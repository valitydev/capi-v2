-module(capi_auth).

%% API functions

-export([get_subject_id/1]).
-export([get_party_id/1]).
-export([get_user_id/1]).
-export([get_user_email/1]).

-export([preauthorize_api_key/1]).
-export([authorize_api_key/3]).
-export([authorize_operation/2]).
-export([issue_access_token/2]).

-export([get_consumer/1]).

%% API types

-type token_type() :: bearer.
-type preauth_context() :: {unauthorized, {token_type(), token_keeper_client:token()}}.
-type auth_context() :: {authorized, token_keeper_client:auth_data()}.
-type restrictions() :: capi_bouncer_restrictions:t().
-type resolution() ::
    allowed
    | {restricted, restrictions()}
    | forbidden.
-type consumer() :: client | merchant | provider.
-type token_spec() :: #{
    party := binary(),
    scope := {invoice | invoice_template | customer, binary()},
    shop => binary(),
    lifetime => pos_integer() | unlimited,
    metadata => token_keeper_client:metadata()
}.

-export_type([preauth_context/0]).
-export_type([auth_context/0]).
-export_type([resolution/0]).
-export_type([consumer/0]).
-export_type([token_spec/0]).
-export_type([restrictions/0]).

%% Internal types

-define(AUTHORIZED(Ctx), {authorized, Ctx}).
-define(UNAUTHORIZED(Ctx), {unauthorized, Ctx}).

-define(TOKEN_KEEPER_AUTHORITY, generic_access_token).

%%
%% API functions
%%

-spec get_subject_id(auth_context()) -> binary() | undefined.
get_subject_id(AuthContext) ->
    case get_party_id(AuthContext) of
        PartyID when is_binary(PartyID) ->
            PartyID;
        undefined ->
            get_user_id(AuthContext)
    end.

-spec get_party_id(auth_context()) -> binary() | undefined.
get_party_id(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(party_id), Metadata).

-spec get_user_id(auth_context()) -> binary() | undefined.
get_user_id(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(user_id), Metadata).

-spec get_user_email(auth_context()) -> binary() | undefined.
get_user_email(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(user_email), Metadata).

-spec get_consumer(auth_context()) -> consumer().
get_consumer(?AUTHORIZED(#{metadata := Metadata})) ->
    case get_metadata(get_metadata_mapped_key(token_consumer), Metadata) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider;
        _Default -> merchant
    end.

%%

-spec preauthorize_api_key(swag_server:api_key()) -> {ok, preauth_context()} | {error, _Reason}.
preauthorize_api_key(ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, Token} ->
            {ok, ?UNAUTHORIZED(Token)};
        {error, Error} ->
            {error, Error}
    end.

-spec authorize_api_key(preauth_context(), token_keeper_client:token_context(), woody_context:ctx()) ->
    {ok, auth_context()} | {error, _Reason}.
authorize_api_key(?UNAUTHORIZED({TokenType, Token}), TokenContext, WoodyContext) ->
    authorize_token_by_type(TokenType, Token, TokenContext, WoodyContext).

-spec authorize_operation(
    Prototypes :: capi_bouncer_context:prototypes(),
    ProcessingContext :: capi_handler:processing_context()
) -> resolution().
authorize_operation(Prototypes, ProcessingContext) ->
    AuthContext = extract_auth_context(ProcessingContext),
    #{swagger_context := SwagContext, woody_context := WoodyContext} = ProcessingContext,
    IPAddress = get_ip_address(SwagContext),
    Fragments = capi_bouncer:gather_context_fragments(
        get_token_keeper_fragment(AuthContext),
        get_user_id(AuthContext),
        IPAddress,
        WoodyContext
    ),
    Fragments1 = capi_bouncer_context:build(Prototypes, Fragments, WoodyContext),
    capi_bouncer:judge(Fragments1, WoodyContext).

%%

-spec issue_access_token(TokenSpec :: token_spec(), WoodyContext :: woody_context:ctx()) ->
    token_keeper_client:token().
issue_access_token(TokenSpec, WoodyContext) ->
    ContextFragment = create_context_fragment(TokenSpec),
    Metadata = create_metadata(TokenSpec),
    %% @TODO for now access tokens are only ephemeral, fix this for compact stuff
    % %%TODO InvoiceTemplateAccessTokens are technically not ephemeral and should become so in the future
    AuthClient = token_keeper_client:ephemeral_authority(?TOKEN_KEEPER_AUTHORITY, WoodyContext),
    {ok, #{token := Token}} = token_keeper_authority_ephemeral:create(ContextFragment, Metadata, AuthClient),
    Token.

%%
%% Internal functions
%%

-define(DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME, 259200).
-define(DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME, 259200).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").

create_context_fragment(TokenSpec) ->
    AuthContext = resolve_auth_context(TokenSpec),
    ContextFragment0 = bouncer_context_helpers:make_auth_fragment(AuthContext),
    {encoded_fragment, ContextFragment} = bouncer_client:bake_context_fragment(ContextFragment0),
    ContextFragment.

-spec resolve_auth_context(token_spec()) ->
    bouncer_context_helpers:auth_params().
resolve_auth_context(TokenSpec) ->
    Scope = resolve_auth_scope(TokenSpec),
    #{
        method => resolve_auth_method(TokenSpec),
        expiration => resolve_auth_expiration(TokenSpec),
        scope => [Scope]
    }.

resolve_auth_scope(TokenSpec) ->
    maps:fold(
        fun
            (party = Entity, EntityID, Scope) ->
                Scope#{Entity => #{id => EntityID}};
            (scope, {Entity, EntityID}, Scope) ->
                Scope#{Entity => #{id => EntityID}};
            (shop = Entity, EntityID, Scope) ->
                Scope#{Entity => #{id => EntityID}};
            (_Key, _Value, Scope) ->
                Scope
        end,
        #{},
        TokenSpec
    ).

resolve_auth_method(#{scope := {invoice, _}}) -> ?CTX_V1_AUTHMETHOD_INVOICEACCESSTOKEN;
resolve_auth_method(#{scope := {customer, _}}) -> ?CTX_V1_AUTHMETHOD_CUSTOMERACCESSTOKEN;
resolve_auth_method(#{scope := {invoice_template, _}}) -> ?CTX_V1_AUTHMETHOD_INVOICETEMPLATEACCESSTOKEN.

resolve_auth_expiration(TokenSpec) ->
    case get_token_lifetime(TokenSpec) of
        unlimited ->
            undefined;
        LifeTime ->
            Deadline = genlib_time:unow() + LifeTime,
            genlib_rfc3339:format(Deadline, second)
    end.

get_token_lifetime(#{lifetime := LifeTime} = TokenSpec) when LifeTime =/= undefined ->
    ok = verify_token_lifetime(TokenSpec, LifeTime),
    LifeTime;
get_token_lifetime(#{scope := {invoice, _}}) ->
    ?DEFAULT_INVOICE_ACCESS_TOKEN_LIFETIME;
get_token_lifetime(#{scope := {invoice_template, _}}) ->
    unlimited;
get_token_lifetime(#{scope := {customer, _}}) ->
    ?DEFAULT_CUSTOMER_ACCESS_TOKEN_LIFETIME.

%% Forbid creation of unlimited lifetime invoice and customer tokens
verify_token_lifetime(#{scope := {invoice, _}}, LifeTime) when LifeTime =/= unlimited -> ok;
verify_token_lifetime(#{scope := {customer, _}}, LifeTime) when LifeTime =/= unlimited -> ok;
verify_token_lifetime(#{scope := {invoice_template, _}}, _LifeTime) -> ok.

%%

create_metadata(TokenSpec) ->
    PartyID = maps:get(party, TokenSpec),
    Metadata0 = maps:get(metadata, TokenSpec, #{}),
    Metadata1 = put_metadata(get_metadata_mapped_key(party_id), PartyID, Metadata0),
    put_metadata(get_metadata_mapped_key(token_consumer), <<"client">>, Metadata1).

extract_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

get_token_keeper_fragment(?AUTHORIZED(#{context := Context})) ->
    Context.

authorize_token_by_type(bearer, Token, TokenContext, WoodyContext) ->
    Authenticator = token_keeper_client:authenticator(WoodyContext),
    case token_keeper_authenticator:authenticate(Token, TokenContext, Authenticator) of
        {ok, AuthData} ->
            {ok, ?AUTHORIZED(AuthData)};
        {error, TokenKeeperError} ->
            _ = logger:warning("Token keeper authorization failed: ~p", [TokenKeeperError]),
            {error, {auth_failed, TokenKeeperError}}
    end.

parse_api_key(<<"Bearer ", Token/binary>>) ->
    {ok, {bearer, Token}};
parse_api_key(_) ->
    {error, unsupported_auth_scheme}.

%%

get_metadata(Key, Metadata) ->
    maps:get(Key, Metadata, undefined).

put_metadata(Key, Value, Metadata) ->
    maps:put(Key, Value, Metadata).

get_metadata_mapped_key(Key) ->
    maps:get(Key, get_meta_mappings()).

get_meta_mappings() ->
    AuthConfig = genlib_app:env(capi, auth_config),
    maps:get(metadata_mappings, AuthConfig).

get_ip_address(SwagContext) ->
    Request = maps:get(cowboy_req, SwagContext, #{}),
    case get_ip_address_from_request(Request) of
        {ok, IPAddress} ->
            IPAddress;
        {error, _Error} ->
            %% Ignore error, add logging if needed
            undefined
    end.

get_ip_address_from_request(Request) ->
    IPAddressHeader = genlib_app:env(capi, ip_address_header, <<"x-forwarded-for">>),
    case Request of
        #{headers := #{IPAddressHeader := IPAddress}} ->
            parse_header_ip_address(IPAddress);
        #{peer := {IPAddress, _Port}} ->
            {ok, IPAddress};
        _ ->
            {error, no_req_in_swag_context}
    end.

parse_header_ip_address(IPAddress0) ->
    IPAddress1 = erlang:binary_to_list(IPAddress0),
    IPs = [L || L <- string:lexemes(IPAddress1, ", ")],
    Valid = lists:all(fun check_ip/1, IPs),
    case IPs of
        [ClientIP | _Proxies] when Valid ->
            inet:parse_strict_address(ClientIP);
        _ ->
            % empty or malformed value
            {error, malformed}
    end.

check_ip(IP) ->
    case inet:parse_strict_address(IP) of
        {ok, _} ->
            true;
        _Error ->
            % unparseable ip address
            false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec determine_peer_test_() -> [_TestGen].
determine_peer_test_() ->
    [
        ?_assertEqual(
            {ok, {10, 10, 10, 10}},
            parse_header_ip_address(<<"10.10.10.10">>)
        ),
        ?_assertEqual(
            {ok, {17, 71, 0, 1}},
            parse_header_ip_address(<<"17.71.0.1">>)
        ),
        ?_assertEqual(
            {ok, {17, 71, 0, 1}},
            parse_header_ip_address(<<" 17.71.0.1,123.123.123.123 ">>)
        ),
        ?_assertEqual(
            {error, malformed},
            parse_header_ip_address(<<",,,,">>)
        ),
        ?_assertEqual(
            {ok, {1, 1, 1, 1}},
            parse_header_ip_address(<<"1.1.1.1,,, ,,,">>)
        ),
        ?_assertEqual(
            {error, malformed},
            parse_header_ip_address(<<"1.,1.,1.1,">>)
        )
    ].

-endif.
