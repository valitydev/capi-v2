-module(capi_handler_payment_institutions).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-define(PAYMENT_INSTITUTION_REF(PaymentInstitutionID), #domain_PaymentInstitutionRef{id = PaymentInstitutionID}).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('GetPaymentInstitutions' = OperationID, Req, Context) ->
    Authorize = mk_authorize_operation(OperationID, Context),
    Process = fun() ->
        try
            Residence = capi_handler_encoder:encode_residence(genlib_map:get(residence, Req)),
            Realm = genlib_map:get(realm, Req),
            {ok, PaymentInstObjects} = capi_domain:get_payment_institutions(Context),
            Resp = filter_payment_institutions(Realm, Residence, PaymentInstObjects),
            {ok, {200, #{}, Resp}}
        catch
            throw:{encode_residence, invalid_residence} ->
                {ok, logic_error('invalidRequest', <<"Invalid residence">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetPaymentInstitutionByRef' = OperationID, Req, Context) ->
    Authorize = mk_authorize_operation(OperationID, Context),
    Process = fun() ->
        PaymentInstitutionID = genlib:to_int(maps:get('paymentInstitutionID', Req)),
        PaymentInstitutionRef = ?PAYMENT_INSTITUTION_REF(PaymentInstitutionID),
        case capi_domain:get({payment_institution, PaymentInstitutionRef}, Context) of
            {ok, PaymentInstitution} ->
                {ok, {200, #{}, decode_payment_institution(PaymentInstitution)}};
            {error, not_found} ->
                {ok, general_error(404, <<"Payment institution not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetPaymentInstitutionPaymentTerms' = OperationID, Req, Context) ->
    Authorize = mk_authorize_operation(OperationID, Context),
    Process = fun() ->
        PaymentInstitutionID = genlib:to_int(maps:get('paymentInstitutionID', Req)),
        case compute_payment_institution_terms(PaymentInstitutionID, #payproc_Varset{}, Context) of
            {ok, #domain_TermSet{payments = PaymentTerms}} ->
                {ok, {200, #{}, decode_payment_terms(PaymentTerms)}};
            {error, #payproc_PaymentInstitutionNotFound{}} ->
                {ok, general_error(404, <<"Payment institution not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetServiceProviderByID' = OperationID, Req, Context) ->
    Authorize = mk_authorize_operation(OperationID, Context),
    Process = fun() ->
        ServiceProviderID = maps:get('serviceProviderID', Req),
        PaymentServiceRef = {payment_service, #domain_PaymentServiceRef{id = ServiceProviderID}},
        case capi_domain:get(PaymentServiceRef, Context) of
            {ok, #domain_PaymentServiceObject{data = PaymentService}} ->
                {ok, {200, #{}, decode_payment_service(ServiceProviderID, PaymentService)}};
            {error, not_found} ->
                {ok, general_error(404, <<"Service provider not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

mk_authorize_operation(OperationID, Context) ->
    fun() ->
        Prototypes = [
            {operation, #{id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end.

filter_payment_institutions(Realm, Residence, PaymentInstObjects) ->
    lists:filtermap(
        fun(P) ->
            case check_payment_institution(Realm, Residence, P) of
                true ->
                    {true, decode_payment_institution(P)};
                false ->
                    false
            end
        end,
        PaymentInstObjects
    ).

check_payment_institution(Realm, Residence, PaymentInstitution) ->
    check_payment_institution_realm(Realm, PaymentInstitution) andalso
        check_payment_institution_residence(Residence, PaymentInstitution).

check_payment_institution_realm(undefined, _) ->
    true;
check_payment_institution_realm(Realm1, #domain_PaymentInstitutionObject{
    data = #domain_PaymentInstitution{realm = Realm2}
}) ->
    Realm1 =:= Realm2.

check_payment_institution_residence(undefined, _) ->
    true;
check_payment_institution_residence(Residence, #domain_PaymentInstitutionObject{
    data = #domain_PaymentInstitution{residences = Residences}
}) ->
    ordsets:is_element(Residence, Residences).

compute_payment_institution_terms(PaymentInstitutionID, VS, Context) ->
    Ref = ?PAYMENT_INSTITUTION_REF(PaymentInstitutionID),
    capi_party:compute_payment_institution_terms(Ref, VS, Context).

%

decode_payment_institution(#domain_PaymentInstitutionObject{ref = Ref, data = Data}) ->
    genlib_map:compact(#{
        <<"id">> => Ref#domain_PaymentInstitutionRef.id,
        <<"name">> => Data#domain_PaymentInstitution.name,
        <<"description">> => Data#domain_PaymentInstitution.description,
        <<"realm">> => genlib:to_binary(Data#domain_PaymentInstitution.realm),
        <<"residences">> => [
            capi_handler_decoder_party:decode_residence(R)
         || R <- ordsets:to_list(Data#domain_PaymentInstitution.residences)
        ]
    }).

decode_payment_terms(#domain_PaymentsServiceTerms{currencies = Currencies, categories = Categories}) ->
    genlib_map:compact(#{
        <<"currencies">> => decode_payment_terms(fun capi_handler_decoder_utils:decode_currency/1, Currencies),
        <<"categories">> => decode_payment_terms(fun capi_handler_decoder_utils:decode_category_ref/1, Categories)
    });
decode_payment_terms(undefined) ->
    #{}.

decode_payment_terms(Fun, {value, Val}) when is_list(Val) ->
    [Fun(V) || V <- Val];
decode_payment_terms(_, _) ->
    undefined.

%%

decode_payment_service(ID, #domain_PaymentService{} = PaymentService) ->
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"brandName">> => PaymentService#domain_PaymentService.brand_name,
        <<"category">> => PaymentService#domain_PaymentService.category,
        <<"metadata">> => capi_utils:'maybe'(
            PaymentService#domain_PaymentService.metadata,
            fun capi_handler_decoder_utils:decode_namespaced_metadata/1
        )
    }).
