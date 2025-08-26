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
        case get_shops_for_party(PartyID, Context) of
            {ok, Shops} ->
                {ok, {200, #{}, Shops}};
            {error, not_found} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    ProcessRestricted = fun(Restrictions) ->
        case get_shops_for_party(PartyID, Context) of
            {ok, Shops} ->
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
                {ok, {200, #{}, decode_shop(ShopID, Shop)}};
            {error, not_found} ->
                {ok, general_error(404, <<"Shop not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetShopAccount' = _OperationID, _Req, _Context) ->
    {error, noimpl};
prepare('ActivateShopForParty', _Req, _Context) ->
    {error, noimpl};
prepare('SuspendShopForParty', _Req, _Context) ->
    {error, noimpl};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

get_shops_for_party(PartyID, Context) ->
    case capi_party:get_shops(PartyID, Context) of
        {ok, ShopsWithIDs} ->
            {ok,
                lists:map(
                    fun({ShopID, Shop}) -> decode_shop(ShopID, Shop) end,
                    ShopsWithIDs
                )};
        {error, not_found} ->
            {error, not_found}
    end.

restrict_shops(Shops, Restrictions) ->
    RestrictedShopIDs = capi_bouncer_restrictions:get_restricted_shop_ids(Restrictions),
    lists:filter(
        fun(Shop) ->
            ShopID = maps:get(<<"id">>, Shop),
            lists:member(ShopID, RestrictedShopIDs)
        end,
        Shops
    ).

decode_shop(ShopID, Shop) ->
    genlib_map:compact(#{
        <<"id">> => ShopID,
        <<"isBlocked">> => capi_handler_decoder_party:is_blocked(Shop#domain_ShopConfig.block),
        <<"isSuspended">> => capi_handler_decoder_party:is_suspended(Shop#domain_ShopConfig.suspension),
        <<"currency">> => get_shop_currency(Shop#domain_ShopConfig.account),
        <<"categoryID">> => capi_handler_decoder_utils:decode_category_ref(Shop#domain_ShopConfig.category),
        <<"details">> => capi_handler_decoder_party:decode_shop_details(Shop),
        <<"location">> => capi_handler_decoder_party:decode_shop_location(Shop#domain_ShopConfig.location),
        <<"contractID">> => genlib:to_binary(Shop#domain_ShopConfig.terms#domain_TermSetHierarchyRef.id)
    }).

get_shop_currency(#domain_ShopAccount{currency = Currency}) ->
    capi_handler_decoder_utils:decode_currency(Currency).
