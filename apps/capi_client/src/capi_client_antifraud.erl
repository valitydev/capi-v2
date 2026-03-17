-module(capi_client_antifraud).

-export([inspect_user/2]).

-type context() :: capi_client_lib:context().

-spec inspect_user(context(), map()) -> {ok, term()} | {error, term()}.
inspect_user(Context, Request) ->
    Params = #{
        body => Request
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_inspector_api:inspect_user(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
