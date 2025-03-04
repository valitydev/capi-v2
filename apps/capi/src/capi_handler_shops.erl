-module(capi_handler_shops).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID = 'ActivateShop', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ShopID = maps:get('shopID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:activate_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)};
            {error, #payproc_InvalidShopStatus{status = {suspension, {active, _}}}} ->
                {ok, {204, #{}, undefined}}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'SuspendShop', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ShopID = maps:get('shopID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:suspend_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)};
            {error, #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}}} ->
                {ok, {204, #{}, undefined}}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShops', _Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Party = capi_utils:unwrap(capi_party:get_party(PartyID, Context)),
        {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShopByID', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ShopID = maps:get('shopID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_shop(PartyID, ShopID, Context) of
            {ok, Shop} ->
                {ok, {200, #{}, decode_shop(Shop)}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetShopsForParty', Req, Context) ->
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    ProcessRestricted = fun(Restrictions) ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                Shops = restrict_shops(Party#domain_Party.shops, Restrictions),
                {ok, {200, #{}, decode_shops_map(Shops)}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process, process_restricted => ProcessRestricted}};
prepare(OperationID = 'GetShopByIDForParty', Req, Context) ->
    PartyID = maps:get('partyID', Req),
    ShopID = maps:get('shopID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_shop(PartyID, ShopID, Context) of
            {ok, Shop} ->
                {ok, {200, #{}, decode_shop(Shop)}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'ActivateShopForParty', Req, Context) ->
    PartyID = maps:get('partyID', Req),
    ShopID = maps:get('shopID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:activate_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)};
            {error, #payproc_InvalidShopStatus{status = {suspension, {active, _}}}} ->
                {ok, {204, #{}, undefined}}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'SuspendShopForParty', Req, Context) ->
    PartyID = maps:get('partyID', Req),
    ShopID = maps:get('shopID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID, shop => ShopID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:suspend_shop(PartyID, ShopID, Context) of
            ok ->
                {ok, {204, #{}, undefined}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)};
            {error, #payproc_ShopNotFound{}} ->
                {ok, general_error(404, <<"Shop not found">>)};
            {error, #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}}} ->
                {ok, {204, #{}, undefined}}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

restrict_shops(Shops, Restrictions) ->
    RestrictedShopIDs = capi_bouncer_restrictions:get_restricted_shop_ids(Restrictions),
    maps:filter(fun(Key, _Value) -> lists:member(Key, RestrictedShopIDs) end, Shops).

decode_shops_map(Shops) ->
    capi_handler_decoder_utils:decode_map(Shops, fun decode_shop/1).

decode_shop(Shop) ->
    Currency = capi_utils:'maybe'(
        Shop#domain_Shop.account,
        fun(#domain_ShopAccount{currency = Currency}) ->
            capi_handler_decoder_utils:decode_currency(Currency)
        end
    ),
    genlib_map:compact(#{
        <<"id">> => Shop#domain_Shop.id,
        <<"createdAt">> => Shop#domain_Shop.created_at,
        <<"isBlocked">> => capi_handler_decoder_party:is_blocked(Shop#domain_Shop.blocking),
        <<"isSuspended">> => capi_handler_decoder_party:is_suspended(Shop#domain_Shop.suspension),
        <<"currency">> => Currency,
        <<"categoryID">> => capi_handler_decoder_utils:decode_category_ref(Shop#domain_Shop.category),
        <<"details">> => capi_handler_decoder_party:decode_shop_details(Shop#domain_Shop.details),
        <<"location">> => capi_handler_decoder_party:decode_shop_location(Shop#domain_Shop.location),
        <<"contractID">> => Shop#domain_Shop.contract_id
    }).
