-module(capi_client_customers).

-export([create_customer/2]).
-export([get_customer_by_id/2]).
-export([get_customer_by_external_id/2]).
-export([get_customer_by_external_id/3]).
-export([delete_customer/2]).
-export([create_customer_access_token/2]).
-export([get_customer_payments/3]).
-export([get_customer_bank_cards/3]).

-type context() :: capi_client_lib:context().

-spec create_customer(context(), map()) -> {ok, term()} | {error, term()}.
create_customer(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:create_customer(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_customer_by_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_customer_by_id(Context, CustomerID) ->
    Params = #{binding => #{<<"customerID">> => CustomerID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_customer_by_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_customer_by_external_id(context(), binary()) -> {ok, term()} | {error, term()}.
get_customer_by_external_id(Context, ExternalID) ->
    get_customer_by_external_id(Context, ExternalID, undefined).

-spec get_customer_by_external_id(context(), binary(), binary() | undefined) -> {ok, term()} | {error, term()}.
get_customer_by_external_id(Context, ExternalID, PartyID) ->
    Params = #{qs_val => genlib_map:compact(#{<<"externalID">> => ExternalID, <<"partyID">> => PartyID})},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_customer_by_external_id(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec delete_customer(context(), binary()) -> {ok, term()} | {error, term()}.
delete_customer(Context, CustomerID) ->
    Params = #{binding => #{<<"customerID">> => CustomerID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:delete_customer(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec create_customer_access_token(context(), binary()) -> {ok, term()} | {error, term()}.
create_customer_access_token(Context, CustomerID) ->
    Params = #{binding => #{<<"customerID">> => CustomerID}},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:create_customer_access_token(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_customer_payments(context(), binary(), map()) -> {ok, term()} | {error, term()}.
get_customer_payments(Context, CustomerID, Qs) ->
    Params = #{
        binding => #{<<"customerID">> => CustomerID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_customer_payments(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_customer_bank_cards(context(), binary(), map()) -> {ok, term()} | {error, term()}.
get_customer_bank_cards(Context, CustomerID, Qs) ->
    Params = #{
        binding => #{<<"customerID">> => CustomerID},
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_customers_api:get_customer_bank_cards(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
