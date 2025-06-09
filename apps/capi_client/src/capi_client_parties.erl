-module(capi_client_parties).

-export([get_party_by_id/2]).

-type context() :: capi_client_lib:context().

-spec get_party_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_party_by_id(Context, PartyID) ->
    Params = #{
        binding => #{
            <<"partyID">> => PartyID
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_parties_api:get_party_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
