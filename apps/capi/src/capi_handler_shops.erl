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
prepare('GetShopsForParty' = OperationID, Req, Context) ->
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                Shops = get_shops_for_party(Party, Context),
                {ok, {200, #{}, Shops}};
            {error, not_found} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    ProcessRestricted = fun(Restrictions) ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                Shops = get_shops_for_party(Party, Context),
                RestrictedShops = restrict_shops(Shops, Restrictions),
                {ok, {200, #{}, RestrictedShops}};
            {error, not_found} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process, process_restricted => ProcessRestricted}};
prepare('GetShopByIDForParty' = OperationID, Req, Context) ->
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
            {error, not_found} ->
                {ok, general_error(404, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('ActivateShopForParty', _Req, _Context) ->
    {error, noimpl};
prepare('SuspendShopForParty', _Req, _Context) ->
    {error, noimpl};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

get_shops_for_party(#domain_PartyConfig{shops = ShopRefs}, Context) ->
    Shops = lists:foldl(fun(ShopRef, Acc) ->
        case capi_domain:get({shop_config, ShopRef}, capi_domain:head(), Context) of
            {ok, Shop} ->
                [decode_shop(Shop) | Acc];
            {error, not_found} ->
                Acc
        end
    end, [], ShopRefs),
    lists:reverse(Shops).

restrict_shops(Shops, Restrictions) ->
    RestrictedShopIDs = capi_bouncer_restrictions:get_restricted_shop_ids(Restrictions),
    lists:filter(fun(Shop) ->
        ShopID = maps:get(<<"id">>, Shop),
        lists:member(ShopID, RestrictedShopIDs)
    end, Shops).

decode_shop(Shop) ->
    Currency = get_shop_currency(Shop),
    genlib_map:compact(#{
        <<"id">> => Shop#domain_ShopConfig.id,
        <<"createdAt">> => Shop#domain_ShopConfig.created_at,
        <<"isBlocked">> => capi_handler_decoder_party:is_blocked(Shop#domain_ShopConfig.blocking),
        <<"isSuspended">> => capi_handler_decoder_party:is_suspended(Shop#domain_ShopConfig.suspension),
        <<"currency">> => Currency,
        <<"categoryID">> => capi_handler_decoder_utils:decode_category_ref(Shop#domain_ShopConfig.category),
        <<"details">> => capi_handler_decoder_party:decode_shop_details(Shop#domain_ShopConfig.details),
        <<"location">> => capi_handler_decoder_party:decode_shop_location(Shop#domain_ShopConfig.location)
    }).

get_shop_currency(#domain_ShopConfig{currency_configs = Configs}) when is_map(Configs) ->
    %% TODO: fix it when add multi currency support
    [Currency | _] = maps:keys(Configs),
    Currency.