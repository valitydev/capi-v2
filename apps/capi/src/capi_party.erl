-module(capi_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-export([get_party/1]).
-export([activate_party/2]).
-export([suspend_party/2]).
-export([get_shop/2]).
-export([activate_shop/3]).
-export([suspend_shop/3]).
-export([compute_payment_institution_terms/3]).

-type result() :: ok | {ok, woody:result()} | {error, woody_error:business_error()}.
-type processing_context() :: capi_handler:processing_context().

-type party_id() :: party_client_thrift:party_id().
-type payment_institution_ref() :: party_client_thrift:payment_institution_ref().
-type varset() :: party_client_thrift:varset().
-type shop_id() :: party_client_thrift:shop_id().
-type party() :: dmsl_domain_thrift:'PartyConfig'().
-type shop() :: dmsl_domain_thrift:'ShopConfig'().

-spec get_party(party_id()) -> {ok, party()} | {error, not_found}.
get_party(PartyID) ->
    checkout(PartyID, get_party_revision()).

-spec get_party_revision() -> capi_domain:revision() | no_return().
get_party_revision() ->
    capi_domain:head().

-spec checkout(party_id(), capi_domain:revision()) -> {ok, party()} | {error, not_found}.
checkout(PartyID, Revision) ->
    case capi_domain:get(Revision, {party_config, #domain_PartyConfigRef{id = PartyID}}) of
        {object_not_found, _Ref} = Error ->
            Error;
        Party ->
            Party
    end.

-spec activate_party(party_id(), processing_context()) -> result().
activate_party(_PartyID, _Context) ->
    {error, #payproc_PartyNotFound{}}.

-spec suspend_party(party_id(), processing_context()) -> result().
suspend_party(_PartyID, _Context) ->
    {error, #payproc_PartyNotFound{}}.

-spec get_shop(party_id(), shop_id()) -> {ok, shop()} | {error, not_found}.
get_shop(PartyID, ShopID) ->
    {ok, #domain_PartyConfig{shops = Shops}} = get_party(PartyID),
    Ref = #domain_ShopConfigRef{id = ShopID},
    case lists:member(Ref, Shops) of
        true ->
            capi_domain:get(capi_domain:head(), {shop_config, Ref});
        false ->
            undefined
    end.

-spec activate_shop(party_id(), shop_id(), processing_context()) -> result().
activate_shop(_PartyID, _ShopID, _Context) ->
    {error, #payproc_PartyNotFound{}}.

-spec suspend_shop(party_id(), shop_id(), processing_context()) -> result().
suspend_shop(_PartyID, _ShopID, _Context) ->
    {error, #payproc_PartyNotFound{}}.

-spec compute_payment_institution_terms(
    payment_institution_ref(),
    varset(),
    processing_context()
) -> result().
compute_payment_institution_terms(Ref, Varset, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:compute_payment_institution_terms(
        Ref,
        Varset,
        Client,
        ClientContext
    ).

client_context(#{party_client := Client, party_client_context := ClientContext}) ->
    {Client, ClientContext}.
