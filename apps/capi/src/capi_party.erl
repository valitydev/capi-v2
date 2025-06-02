-module(capi_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").

-export([get_party/2]).
-export([get_shop/3]).
-export([compute_payment_institution_terms/3]).

-type result() :: ok | {ok, woody:result()} | {error, woody_error:business_error()}.
-type processing_context() :: capi_handler:processing_context().

-type party_id() :: party_client_thrift:party_id().
-type payment_institution_ref() :: party_client_thrift:payment_institution_ref().
-type varset() :: party_client_thrift:varset().
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
    case capi_domain:get({party_config, #domain_PartyConfigRef{id = PartyID}}, Revision, Context) of
        {error, not_found} = Error ->
            Error;
        Party ->
            Party
    end.

-spec get_shop(party_id(), shop_id(), processing_context()) -> {ok, shop()} | {error, not_found}.
get_shop(PartyID, ShopID, Context) ->
    case get_party(PartyID, Context) of
        {ok, #domain_PartyConfig{shops = ShopRefs}} ->
            Ref = #domain_ShopConfigRef{id = ShopID},
            case lists:member(Ref, ShopRefs) of
                true ->
                    capi_domain:get({shop_config, Ref}, capi_domain:head(), Context);
                false ->
                    {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

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
