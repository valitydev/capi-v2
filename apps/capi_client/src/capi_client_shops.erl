-module(capi_client_shops).

-export([get_shops_for_party/2]).
-export([get_shop_by_id_for_party/3]).
-export([suspend_shop_for_party/3]).
-export([activate_shop_for_party/3]).

-type context() :: capi_client_lib:context().

-spec get_shops_for_party(context(), binary()) -> {ok, term()} | {error, term()}.
get_shops_for_party(Context, PartyID) ->
    Params = #{
        binding => #{
            <<"partyID">> => PartyID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:get_shops_for_party(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_shop_by_id_for_party(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_shop_by_id_for_party(Context, PartyID, ShopID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"shopID">> => ShopID
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:get_shop_by_id_for_party(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec suspend_shop_for_party(context(), binary(), binary()) -> ok | {error, term()}.
suspend_shop_for_party(Context, PartyID, ShopID) ->
    Params = #{
        binding => #{
            <<"partyID">> => PartyID,
            <<"shopID">> => ShopID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:suspend_shop_for_party(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, undefined} -> ok;
        {error, Error} -> {error, Error}
    end.

-spec activate_shop_for_party(context(), binary(), binary()) -> ok | {error, term()}.
activate_shop_for_party(Context, PartyID, ShopID) ->
    Params = #{
        binding => genlib_map:compact(#{
            <<"partyID">> => PartyID,
            <<"shopID">> => ShopID
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_shops_api:activate_shop_for_party(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, undefined} -> ok;
        {error, Error} -> {error, Error}
    end.
