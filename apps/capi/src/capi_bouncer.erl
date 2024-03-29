-module(capi_bouncer).

-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").

-export([gather_context_fragments/4]).
-export([judge/2]).

%%

-spec gather_context_fragments(
    TokenContextFragment :: token_keeper_client:context_fragment(),
    UserID :: binary() | undefined,
    IPAddress :: inet:ip_address(),
    WoodyContext :: woody_context:ctx()
) -> capi_bouncer_context:fragments().
gather_context_fragments(TokenContextFragment, UserID, IPAddress, WoodyCtx) ->
    {Base, External0} = capi_bouncer_context:new(),
    External1 = External0#{<<"token-keeper">> => {encoded_fragment, TokenContextFragment}},
    {add_requester_context(IPAddress, Base), maybe_add_userorg(UserID, External1, WoodyCtx)}.

-spec judge(capi_bouncer_context:fragments(), woody_context:ctx()) -> capi_auth:resolution().
judge({Acc, External}, WoodyCtx) ->
    % TODO error out early?
    {ok, RulesetID} = application:get_env(capi, bouncer_ruleset_id),
    JudgeContext = #{fragments => External#{<<"capi">> => Acc}},
    bouncer_client:judge(RulesetID, JudgeContext, WoodyCtx).

%%

maybe_add_userorg(undefined, External, _WoodyCtx) ->
    External;
maybe_add_userorg(UserID, External, WoodyCtx) ->
    case bouncer_context_helpers:get_user_orgs_fragment(UserID, WoodyCtx) of
        {ok, UserOrgsFragment} ->
            External#{<<"userorg">> => UserOrgsFragment};
        {error, {user, notfound}} ->
            External
    end.

-spec add_requester_context(inet:ip_address(), capi_bouncer_context:acc()) -> capi_bouncer_context:acc().
add_requester_context(IPAddress, FragmentAcc) ->
    bouncer_context_helpers:add_requester(
        #{ip => IPAddress},
        FragmentAcc
    ).
