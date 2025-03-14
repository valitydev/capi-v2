-module(capi_handler_customers).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [
    general_error/2,
    logic_error/1,
    logic_error/2,
    conflict_error/1,
    map_service_result/1
]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreateCustomer' = OperationID, Req, Context) ->
    CustomerParams = maps:get('CustomerParams', Req),
    PartyID = maps:get(<<"partyID">>, CustomerParams, capi_handler_utils:get_party_id(Context)),
    ShopID = maps:get(<<"shopID">>, CustomerParams),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        try
            CustomerID = generate_customer_id(OperationID, PartyID, CustomerParams, Context),
            EncodedCustomerParams = encode_customer_params(CustomerID, PartyID, CustomerParams),
            Call = {customer_management, 'Create', {EncodedCustomerParams}},
            case capi_handler_utils:service_call(Call, Context) of
                {ok, Customer} ->
                    {ok, {201, #{}, make_customer_and_token(Customer, Context)}};
                {exception, #payproc_InvalidPartyStatus{}} ->
                    {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
                {exception, #payproc_InvalidShopStatus{}} ->
                    {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
                {exception, #payproc_ShopNotFound{}} ->
                    {ok, logic_error('invalidShopID', <<"Shop not found">>)};
                {exception, #payproc_PartyNotFound{}} ->
                    {ok, logic_error('invalidPartyID', <<"Party not found">>)};
                {exception, #payproc_OperationNotPermitted{}} ->
                    {ok, logic_error('operationNotPermitted', <<"Operation not permitted">>)}
            end
        catch
            throw:{external_id_conflict, ID, UsedExternalID, _Schema} ->
                {ok, conflict_error({ID, UsedExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerById' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Customer = map_service_result(get_customer_by_id(CustomerID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => Customer}}
        ],
        {ok, mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        case Customer of
            #payproc_Customer{} ->
                {ok, {200, #{}, decode_customer(Customer)}};
            undefined ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('DeleteCustomer' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => CustomerID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Call = {customer_management, 'Delete', {CustomerID}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateCustomerAccessToken' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Customer = map_service_result(get_customer_by_id(CustomerID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => Customer}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case Customer of
            #payproc_Customer{} ->
                Response = capi_handler_utils:issue_access_token(Customer, Context),
                {ok, {201, #{}, Response}};
            undefined ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateBinding' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => CustomerID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Result =
            try
                CustomerBindingParams = maps:get('CustomerBindingParams', Req),

                {CustomerBindingID, RecPaymentToolID} = generate_binding_ids(
                    OperationID,
                    CustomerBindingParams,
                    Context
                ),

                EncodedCustomerBindingParams = encode_customer_binding_params(
                    CustomerBindingID,
                    RecPaymentToolID,
                    CustomerBindingParams
                ),

                Call = {customer_management, 'StartBinding', {CustomerID, EncodedCustomerBindingParams}},
                capi_handler_utils:service_call(Call, Context)
            catch
                throw:invalid_payment_session ->
                    {error, invalid_payment_session};
                throw:Error = {external_id_conflict, _, _, _} ->
                    {error, Error}
            end,
        case Result of
            {ok, CustomerBinding} ->
                {ok, {201, #{}, decode_customer_binding(CustomerBinding)}};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
            {exception, #payproc_InvalidContractStatus{}} ->
                {ok, logic_error('invalidRequest', <<"Invalid contract status">>)};
            {exception, #payproc_OperationNotPermitted{}} ->
                {ok, logic_error('operationNotPermitted', <<"Operation not permitted">>)};
            {error, invalid_payment_session} ->
                {ok, logic_error('invalidPaymentSession', <<"Specified payment session is invalid">>)};
            {error, {external_id_conflict, ID, UsedExternalID, _Schema}} ->
                {ok, conflict_error({ID, UsedExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetBindings' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Customer = map_service_result(get_customer_by_id(CustomerID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => Customer}}
        ],
        {ok, mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        _ = capi_handler:respond_if_undefined(Customer, general_error(404, <<"Customer not found">>)),
        {ok, {200, #{}, [decode_customer_binding(B) || B <- Customer#payproc_Customer.bindings]}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetBinding' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    CustomerBindingID = maps:get('customerBindingID', Req),
    Customer = map_service_result(get_customer_by_id(CustomerID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID, binding => CustomerBindingID}},
            {payproc, #{customer => Customer}}
        ],
        {ok, mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        _ = capi_handler:respond_if_undefined(Customer, general_error(404, <<"Customer not found">>)),
        Bindings = Customer#payproc_Customer.bindings,
        case lists:keyfind(CustomerBindingID, #payproc_CustomerBinding.id, Bindings) of
            #payproc_CustomerBinding{} = B ->
                {ok, {200, #{}, decode_customer_binding(B)}};
            false ->
                {ok, general_error(404, <<"Customer binding not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerEvents' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => CustomerID}}
        ],
        {ok, mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        GetterFun = fun(Range) ->
            capi_handler_utils:service_call(
                {customer_management, 'GetEvents', {CustomerID, Range}},
                Context
            )
        end,
        Result = capi_handler_utils:collect_events(
            maps:get('limit', Req),
            genlib_map:get('eventID', Req),
            GetterFun,
            fun decode_customer_event/1
        ),
        case Result of
            {ok, Events} when is_list(Events) ->
                {ok, {200, #{}, Events}};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)};
            {exception, #payproc_EventNotFound{}} ->
                {ok, general_error(404, <<"Event not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetCustomerPaymentMethods' = OperationID, Req, Context) ->
    CustomerID = maps:get('customerID', Req),
    Customer = map_service_result(get_customer_by_id(CustomerID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, customer => CustomerID}},
            {payproc, #{customer => Customer}}
        ],
        {ok, mask_customer_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Customer, general_error(404, <<"Customer not found">>)),
        PartyID = Customer#payproc_Customer.owner_id,
        % В данном контексте - Party не может не существовать
        {ok, Party} = capi_party:get_party(PartyID, Context),
        Args = {CustomerID, {revision, Party#domain_Party.revision}},
        case capi_handler_utils:get_payment_methods(customer_management, Args, Context) of
            {ok, PaymentMethodRefs} ->
                PaymentMethods0 = capi_handler_decoder_invoicing:decode_payment_methods(PaymentMethodRefs),
                PaymentMethods1 = capi_utils:deduplicate_payment_methods(PaymentMethods0),
                PaymentMethods = capi_handler_utils:emplace_token_provider_data(Customer, PaymentMethods1, Context),
                {ok, {200, #{}, PaymentMethods}};
            {exception, #payproc_CustomerNotFound{}} ->
                {ok, general_error(404, <<"Customer not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

get_customer_by_id(CustomerID, Context) ->
    EventRange = #payproc_EventRange{},
    capi_handler_utils:service_call({customer_management, 'Get', {CustomerID, EventRange}}, Context).

mask_customer_notfound(Resolution) ->
    % ED-206
    % When bouncer says "forbidden" we can't really tell the difference between "forbidden because
    % of no such customer", "forbidden because client has no access to it" and "forbidden because
    % client has no permission to act on it". From the point of view of existing integrations this
    % is not great, so we have to mask specific instances of missing authorization as if specified
    % customer is nonexistent.
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Customer not found">>)).

generate_customer_id(OperationID, PartyID, CustomerParams, #{woody_context := WoodyContext}) ->
    ExternalID = maps:get(<<"externalID">>, CustomerParams, undefined),
    IdempKey = {OperationID, PartyID, ExternalID},
    Identity = capi_bender:make_identity(capi_feature_schemas:customer(), CustomerParams),
    capi_bender:gen_snowflake(IdempKey, Identity, WoodyContext).

encode_customer_params(CustomerID, PartyID, Params) ->
    #payproc_CustomerParams{
        customer_id = CustomerID,
        party_id = PartyID,
        shop_id = genlib_map:get(<<"shopID">>, Params),
        contact_info = capi_handler_encoder:encode_contact_info(genlib_map:get(<<"contactInfo">>, Params)),
        metadata = encode_customer_metadata(genlib_map:get(<<"metadata">>, Params))
    }.

encode_customer_metadata(Meta) ->
    capi_json_marshalling:marshal(Meta).

generate_binding_ids(OperationID, CustomerBindingParams, #{woody_context := WoodyContext} = Context) ->
    ExternalID = maps:get(<<"externalID">>, CustomerBindingParams, undefined),
    PartyID = capi_handler_utils:get_party_id(Context),

    PaymentResource = maps:get(<<"paymentResource">>, CustomerBindingParams),
    PaymentToolToken = maps:get(<<"paymentToolToken">>, PaymentResource),
    PaymentTool = capi_handler_decoder_invoicing:decode_payment_tool(encode_payment_tool_token(PaymentToolToken)),
    CustomerBindingParamsEncrypted =
        maps:put(
            <<"paymentResource">>,
            maps:put(
                <<"paymentTool">>,
                PaymentTool,
                maps:remove(<<"paymentToolToken">>, PaymentResource)
            ),
            CustomerBindingParams
        ),

    Identity = capi_bender:make_identity(
        capi_feature_schemas:customer_binding(),
        CustomerBindingParamsEncrypted
    ),

    OperationIDBin = erlang:atom_to_binary(OperationID),
    CustomerBindingID = capi_bender:gen_snowflake(
        {<<OperationIDBin/binary, "+CustomerBindingID">>, PartyID, ExternalID},
        Identity,
        WoodyContext
    ),
    RecPaymentToolID = capi_bender:gen_snowflake(
        {<<OperationIDBin/binary, "+RecPaymentToolID">>, PartyID, ExternalID},
        Identity,
        WoodyContext
    ),
    {CustomerBindingID, RecPaymentToolID}.

encode_customer_binding_params(
    CustomerBindingID,
    RecPaymentToolID,
    #{<<"paymentResource">> := PaymentResource}
) ->
    PaymentToolToken = maps:get(<<"paymentToolToken">>, PaymentResource),
    PaymentTool = encode_payment_tool_token(PaymentToolToken),

    {ClientInfo, PaymentSession} =
        capi_handler_utils:unwrap_payment_session(maps:get(<<"paymentSession">>, PaymentResource)),

    #payproc_CustomerBindingParams{
        customer_binding_id = CustomerBindingID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = capi_handler_encoder:encode_client_info(ClientInfo)
        }
    }.

encode_payment_tool_token(Token) ->
    case capi_crypto:decode_token(Token) of
        {ok, TokenData} ->
            #{payment_tool := PaymentTool, valid_until := ValidUntil} = TokenData,
            case capi_utils:deadline_is_reached(ValidUntil) of
                true ->
                    logger:info("Payment tool token expired: ~p", [capi_utils:deadline_to_binary(ValidUntil)]),
                    capi_handler:respond(logic_error('invalidPaymentToolToken'));
                _ ->
                    PaymentTool
            end;
        unrecognized ->
            capi_handler:respond(logic_error('invalidPaymentToolToken'));
        {error, {decryption_failed, Error}} ->
            logger:info("Payment tool token decryption failed: ~p", [Error]),
            capi_handler:respond(logic_error('invalidPaymentToolToken'))
    end.

make_customer_and_token(Customer, ProcessingContext) ->
    #{
        <<"customer">> => decode_customer(Customer),
        <<"customerAccessToken">> => capi_handler_utils:issue_access_token(Customer, ProcessingContext)
    }.

decode_customer(Customer) ->
    #{
        <<"id">> => Customer#payproc_Customer.id,
        <<"shopID">> => Customer#payproc_Customer.shop_id,
        <<"status">> => decode_customer_status(Customer#payproc_Customer.status),
        <<"contactInfo">> =>
            capi_handler_decoder_party:decode_contact_info(Customer#payproc_Customer.contact_info),
        <<"metadata">> => decode_customer_metadata(Customer#payproc_Customer.metadata)
    }.

decode_customer_status({Status, _}) ->
    erlang:atom_to_binary(Status, utf8).

decode_customer_metadata(Meta) ->
    capi_json_marshalling:unmarshal(Meta).

decode_customer_binding(CustomerBinding) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => CustomerBinding#payproc_CustomerBinding.id,
            <<"paymentResource">> =>
                capi_handler_decoder_invoicing:decode_disposable_payment_resource(
                    CustomerBinding#payproc_CustomerBinding.payment_resource
                )
        },
        decode_customer_binding_status(CustomerBinding#payproc_CustomerBinding.status)
    ).

decode_customer_binding_status({Status, StatusInfo}) ->
    Error =
        case StatusInfo of
            #payproc_CustomerBindingFailed{failure = OperationFailure} ->
                capi_handler_decoder_utils:decode_operation_failure(OperationFailure);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_customer_event(#payproc_Event{source = {customer_id, _}, payload = Payload} = Event) ->
    case decode_customer_changes(Payload) of
        [_Something | _] = Changes ->
            {true, #{
                <<"id">> => Event#payproc_Event.id,
                <<"createdAt">> => Event#payproc_Event.created_at,
                <<"changes">> => Changes
            }};
        [] ->
            false
    end.

decode_customer_changes({customer_changes, CustomerChanges}) ->
    lists:filtermap(fun decode_customer_change/1, CustomerChanges).

decode_customer_change({customer_binding_changed, CustomerBindingChanged}) ->
    #payproc_CustomerBindingChanged{id = BindingID, payload = Payload} = CustomerBindingChanged,
    decode_customer_binding_change(BindingID, Payload);
decode_customer_change(_) ->
    false.

decode_customer_binding_change(_, {started, Start}) ->
    #payproc_CustomerBindingStarted{binding = CustomerBinding} = Start,
    {true, #{
        <<"changeType">> => <<"CustomerBindingStarted">>,
        <<"customerBinding">> => decode_customer_binding(CustomerBinding)
    }};
decode_customer_binding_change(BindingID, {status_changed, StatusChange}) ->
    #payproc_CustomerBindingStatusChanged{status = Status} = StatusChange,
    {true,
        capi_handler_utils:merge_and_compact(
            #{
                <<"changeType">> => <<"CustomerBindingStatusChanged">>,
                <<"customerBindingID">> => BindingID
            },
            decode_customer_binding_status(Status)
        )};
decode_customer_binding_change(BindingID, {interaction_changed, InteractionChange}) ->
    #payproc_CustomerBindingInteractionChanged{
        interaction = UserInteraction,
        status = Status
    } = InteractionChange,
    ChangeType =
        case Status of
            {requested, _} -> <<"CustomerBindingInteractionRequested">>;
            {completed, _} -> <<"CustomerBindingInteractionCompleted">>;
            undefined -> <<"CustomerBindingInteractionRequested">>
        end,
    {true, #{
        <<"changeType">> => ChangeType,
        <<"customerBindingID">> => BindingID,
        <<"userInteraction">> => capi_handler_decoder_invoicing:decode_user_interaction(UserInteraction)
    }}.
