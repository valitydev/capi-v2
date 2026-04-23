-module(capi_handler_customers).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2, conflict_error/1]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreateCustomer' = OperationID, Req, Context) ->
    CustomerParams = maps:get('CustomerParams', Req),
    PartyID = maps:get(<<"partyID">>, CustomerParams, capi_handler_utils:get_party_id(Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, party => PartyID}}
        ],
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        ThriftParams = encode_customer_params(PartyID, CustomerParams),
        Call = {customer_management, 'Create', {ThriftParams}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, Customer} ->
                {ok, {201, #{}, make_customer_and_token(Customer, Context)}};
            {exception, #customer_CustomerAlreadyExists{id = ExistingID}} ->
                ExternalID = maps:get(<<"externalID">>, CustomerParams, undefined),
                {ok, conflict_error({ExistingID, ExternalID})};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerByExternalID' = OperationID, Req, Context) ->
    ExternalID = maps:get('externalID', Req),
    PartyID = maps:get('partyID', Req, capi_handler_utils:get_party_id(Context)),
    PartyRef = #domain_PartyConfigRef{id = PartyID},
    Call = {customer_management, 'GetByExternalID', {ExternalID, PartyRef}},
    {CustomerID, ResultCustomerState} =
        case capi_handler_utils:service_call(Call, Context) of
            {ok, #customer_CustomerState{customer = C} = State} ->
                {C#customer_Customer.id, State};
            {exception, #customer_CustomerNotFound{}} ->
                {undefined, undefined}
        end,
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, party => PartyID, customer => CustomerID}},
            {cubasty, genlib_map:compact(#{customer => ResultCustomerState})}
        ],
        Resolution = mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultCustomerState, general_error(404, <<"Customer not found">>)),
        Customer = ResultCustomerState#customer_CustomerState.customer,
        {ok, {200, #{}, decode_customer(Customer)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerByID' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    ResultCustomerState = get_customer_state(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{
                id => OperationID, party => get_customer_party_id(ResultCustomerState), customer => CustomerID
            }},
            {cubasty, genlib_map:compact(#{customer => ResultCustomerState})}
        ],
        Resolution = mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultCustomerState, general_error(404, <<"Customer not found">>)),
        Customer = ResultCustomerState#customer_CustomerState.customer,
        {ok, {200, #{}, decode_customer(Customer)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('DeleteCustomer' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    ResultCustomerState = get_customer_state(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{
                id => OperationID, party => get_customer_party_id(ResultCustomerState), customer => CustomerID
            }},
            {cubasty, genlib_map:compact(#{customer => ResultCustomerState})}
        ],
        Resolution = mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultCustomerState, general_error(404, <<"Customer not found">>)),
        Call = {customer_management, 'Delete', {CustomerID}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, #customer_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateCustomerAccessToken' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    ResultCustomerState = get_customer_state(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{
                id => OperationID, party => get_customer_party_id(ResultCustomerState), customer => CustomerID
            }},
            {cubasty, genlib_map:compact(#{customer => ResultCustomerState})}
        ],
        Resolution = mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultCustomerState, general_error(404, <<"Customer not found">>)),
        Customer = ResultCustomerState#customer_CustomerState.customer,
        Response = capi_handler_utils:issue_access_token(Customer, Context),
        {ok, {201, #{}, Response}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerPayments' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    ResultCustomerState = get_customer_state(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{
                id => OperationID, party => get_customer_party_id(ResultCustomerState), customer => CustomerID
            }},
            {cubasty, genlib_map:compact(#{customer => ResultCustomerState})}
        ],
        Resolution = mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultCustomerState, general_error(404, <<"Customer not found">>)),
        Limit = maps:get('limit', Req),
        ContinuationToken = maps:get('continuationToken', Req, undefined),
        Call = {customer_management, 'GetPayments', {CustomerID, Limit, ContinuationToken}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, #customer_CustomerPaymentsResponse{payments = Payments, continuation_token = CT}} ->
                Response = genlib_map:compact(#{
                    <<"result">> => [decode_customer_payment(P) || P <- Payments],
                    <<"continuationToken">> => CT
                }),
                {ok, {200, #{}, Response}};
            {exception, #customer_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerBankCards' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    ResultCustomerState = get_customer_state(CustomerID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{
                id => OperationID, party => get_customer_party_id(ResultCustomerState), customer => CustomerID
            }},
            {cubasty, genlib_map:compact(#{customer => ResultCustomerState})}
        ],
        Resolution = mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(ResultCustomerState, general_error(404, <<"Customer not found">>)),
        Limit = maps:get('limit', Req),
        ContinuationToken = maps:get('continuationToken', Req, undefined),
        Call = {customer_management, 'GetBankCards', {CustomerID, Limit, ContinuationToken}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, #customer_CustomerBankCardsResponse{bank_cards = BankCards, continuation_token = CT}} ->
                Response = genlib_map:compact(#{
                    <<"result">> => [decode_customer_bank_card(BC) || BC <- BankCards],
                    <<"continuationToken">> => CT
                }),
                {ok, {200, #{}, Response}};
            {exception, #customer_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

get_customer_state(CustomerID, Context) ->
    Call = {customer_management, 'Get', {CustomerID}},
    case capi_handler_utils:service_call(Call, Context) of
        {ok, CustomerState} -> CustomerState;
        {exception, #customer_CustomerNotFound{}} -> undefined
    end.

get_customer_party_id(undefined) ->
    undefined;
get_customer_party_id(#customer_CustomerState{customer = Customer}) ->
    Customer#customer_Customer.party_ref#domain_PartyConfigRef.id.

mask_customer_notfound(Resolution) ->
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Customer not found">>)).

encode_customer_params(PartyID, Params) ->
    ContactInfo = maps:get(<<"contactInfo">>, Params, undefined),
    Metadata = maps:get(<<"metadata">>, Params, undefined),
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    #customer_CustomerParams{
        party_ref = #domain_PartyConfigRef{id = PartyID},
        contact_info = encode_contact_info(ContactInfo),
        metadata = encode_metadata(Metadata),
        external_id = ExternalID
    }.

encode_contact_info(undefined) ->
    undefined;
encode_contact_info(ContactInfo) ->
    capi_handler_encoder:encode_contact_info(ContactInfo).

encode_metadata(undefined) ->
    undefined;
encode_metadata(Metadata) ->
    capi_json_marshalling:marshal(Metadata).

%%

make_customer_and_token(Customer, Context) ->
    #{
        <<"customer">> => decode_customer(Customer),
        <<"customerAccessToken">> => capi_handler_utils:issue_access_token(Customer, Context)
    }.

decode_customer(Customer) ->
    genlib_map:compact(#{
        <<"id">> => Customer#customer_Customer.id,
        <<"externalID">> => Customer#customer_Customer.external_id,
        <<"createdAt">> => Customer#customer_Customer.created_at,
        <<"contactInfo">> => decode_contact_info(Customer#customer_Customer.contact_info),
        <<"metadata">> => decode_metadata(Customer#customer_Customer.metadata)
    }).

decode_contact_info(undefined) ->
    undefined;
decode_contact_info(ContactInfo) ->
    capi_handler_decoder_party:decode_contact_info(ContactInfo).

decode_metadata(undefined) ->
    undefined;
decode_metadata(Metadata) ->
    capi_handler_decoder_utils:decode_metadata(Metadata).

decode_customer_payment(Payment) ->
    #{
        <<"invoiceID">> => Payment#customer_CustomerPayment.invoice_id,
        <<"paymentID">> => Payment#customer_CustomerPayment.payment_id,
        <<"createdAt">> => Payment#customer_CustomerPayment.created_at
    }.

decode_customer_bank_card(BankCardInfo) ->
    genlib_map:compact(#{
        <<"id">> => BankCardInfo#customer_BankCardInfo.id,
        <<"cardMask">> => BankCardInfo#customer_BankCardInfo.card_mask,
        <<"createdAt">> => BankCardInfo#customer_BankCardInfo.created_at
    }).
