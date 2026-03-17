-module(capi_handler_antifraud).

-include_lib("fraudbusters_proto/include/fb_proto_fraudbusters_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('InspectUser' = OperationID, Req, Context) ->
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Body = maps:get('UserInspectRequest', Req),
        InspectUserContext = encode_inspect_user_context(Body),
        Call = {inspector, 'InspectUserShops', {InspectUserContext}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, BlockedShops} ->
                {ok, {200, #{}, decode_blocked_shops(BlockedShops)}};
            {exception, #'base_InvalidRequest'{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

encode_inspect_user_context(#{<<"customer">> := Customer, <<"shops">> := Shops}) ->
    #'fraudbusters_InspectUserContext'{
        user_info = encode_client_info(Customer),
        shop_list = [encode_shop_context(S) || S <- Shops]
    }.

encode_client_info(Customer) ->
    Device = maps:get(<<"device">>, Customer, #{}),
    Contact = maps:get(<<"contact">>, Customer, #{}),
    #'fraudbusters_ClientInfo'{
        ip = maps:get(<<"ip">>, Device, undefined),
        fingerprint = maps:get(<<"fingerprint">>, Device, undefined),
        email = maps:get(<<"email">>, Contact, undefined),
        phone = maps:get(<<"phoneNumber">>, Contact, undefined)
    }.

encode_shop_context(#{<<"partyID">> := PartyID, <<"shopID">> := ShopID}) ->
    #'fraudbusters_ShopContext'{
        party_id = PartyID,
        shop_id = ShopID
    }.

decode_blocked_shops(#'fraudbusters_BlockedShops'{shop_list = ShopList}) ->
    #{<<"blockedShops">> => [decode_shop_context(S) || S <- ShopList]}.

decode_shop_context(#'fraudbusters_ShopContext'{party_id = PartyID, shop_id = ShopID}) ->
    #{<<"partyID">> => PartyID, <<"shopID">> => ShopID}.
