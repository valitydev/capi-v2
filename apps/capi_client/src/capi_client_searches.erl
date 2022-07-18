-module(capi_client_searches).

-export([search_payments/3]).

-type context() :: capi_client_lib:context().
-type search_query() :: capi_client_lib:search_query().

-spec search_payments(context(), binary(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payments(Context, ShopID, Query) ->
    Qs = capi_client_lib:make_search_query_string(Query),
    Params = #{
        binding => #{<<"shopID">> => ShopID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payments(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} ->
            {error, Error}
    end.
