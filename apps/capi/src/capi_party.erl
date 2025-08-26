-module(capi_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([get_party/2]).
-export([get_shop/3]).
-export([get_shops/2]).

-export_type([party_id/0]).

-type processing_context() :: capi_handler:processing_context().

-type party_id() :: party_client_thrift:party_id().
-type shop_id() :: party_client_thrift:shop_id().
-type party() :: dmsl_domain_thrift:'PartyConfig'().
-type shop() :: dmsl_domain_thrift:'ShopConfig'().

-spec get_party(party_id(), processing_context()) -> {ok, party()} | {error, not_found}.
get_party(PartyID, Context) ->
    checkout(PartyID, get_party_revision(), Context).

-spec get_party_revision() -> capi_domain:revision().
get_party_revision() ->
    capi_domain:head().

-spec checkout(party_id(), capi_domain:revision(), processing_context()) -> {ok, party()} | {error, not_found}.
checkout(PartyID, Revision, Context) ->
    case capi_domain:get_ext({party_config, #domain_PartyConfigRef{id = PartyID}}, Revision, Context) of
        {error, not_found} = Error ->
            Error;
        Party ->
            Party
    end.

-spec get_shop(party_id(), shop_id(), processing_context()) -> {ok, shop()} | {error, not_found}.
get_shop(PartyID, ShopID, Context) ->
    PartyRef = #domain_PartyConfigRef{id = PartyID},
    ShopRef = #domain_ShopConfigRef{id = ShopID},
    case capi_domain:get_ext({shop_config, ShopRef}, get_party_revision(), Context) of
        {ok, #domain_ShopConfig{party_ref = PartyRef} = Shop} -> {ok, Shop};
        _ -> {error, not_found}
    end.

-spec get_shops(party_id(), processing_context()) -> {ok, [{shop_id(), shop()}]} | {error, not_found}.
get_shops(PartyID, Context) ->
    PartyRef = #domain_PartyConfigRef{id = PartyID},
    case capi_domain:get_with_related({party_config, PartyRef}, get_party_revision(), Context) of
        {ok, _, ReferencedBy, _} ->
            F = fun
                ({shop_config, #domain_ShopConfigObject{ref = #domain_ShopConfigRef{id = ShopID}, data = Shop}}) ->
                    {true, {ShopID, Shop}};
                (_) ->
                    false
            end,
            {ok, lists:filtermap(F, ReferencedBy)};
        {error, not_found} ->
            {error, not_found}
    end.
