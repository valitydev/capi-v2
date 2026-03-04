-module(capi_antifraud_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("fraudbusters_proto/include/fb_proto_fraudbusters_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("capi_bouncer_data.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    inspect_user_ok_test/1,
    inspect_user_no_blocked_shops_test/1,
    inspect_user_invalid_request_test/1,
    inspect_user_forbidden_test/1
]).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [{group, operations_by_user_session_token}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_user_session_token, [], [
            inspect_user_ok_test,
            inspect_user_no_blocked_shops_test,
            inspect_user_invalid_request_test,
            inspect_user_forbidden_test
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(operations_by_user_session_token, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps = capi_ct_helper_token_keeper:mock_user_session_token(SupPid),
    [{context, capi_ct_helper:get_context(?API_TOKEN)}, {group_apps, Apps}, {group_test_sup, SupPid} | Config].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(operations_by_user_session_token, C) ->
    capi_utils:'maybe'(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1);
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-define(PARTY_ID, <<"party-1">>).
-define(SHOP_ID_1, <<"shop-1">>).
-define(SHOP_ID_2, <<"shop-2">>).

-define(INSPECT_USER_REQUEST, #{
    <<"customer">> => #{
        <<"device">> => #{
            <<"fingerprint">> => <<"abc123">>,
            <<"ip">> => <<"192.168.1.1">>
        },
        <<"contact">> => #{
            <<"email">> => <<"test@example.com">>,
            <<"phoneNumber">> => <<"+79001234567">>
        }
    },
    <<"shops">> => [
        #{<<"partyID">> => ?PARTY_ID, <<"shopID">> => ?SHOP_ID_1},
        #{<<"partyID">> => ?PARTY_ID, <<"shopID">> => ?SHOP_ID_2}
    ]
}).

-spec inspect_user_ok_test(config()) -> _.
inspect_user_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {inspector, fun(
                'InspectUserShops',
                {#'fraudbusters_InspectUserContext'{
                    user_info = #'fraudbusters_ClientInfo'{
                        email = <<"test@example.com">>,
                        fingerprint = <<"abc123">>,
                        ip = <<"192.168.1.1">>,
                        phone = <<"+79001234567">>
                    }
                }}
            ) ->
                {ok, #'fraudbusters_BlockedShops'{
                    shop_list = [
                        #'fraudbusters_ShopContext'{party_id = ?PARTY_ID, shop_id = ?SHOP_ID_1}
                    ]
                }}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"InspectUser">>, Config),
    {ok, #{<<"blockedShops">> := [#{<<"partyID">> := ?PARTY_ID, <<"shopID">> := ?SHOP_ID_1}]}} =
        capi_client_antifraud:inspect_user(?config(context, Config), ?INSPECT_USER_REQUEST).

-spec inspect_user_no_blocked_shops_test(config()) -> _.
inspect_user_no_blocked_shops_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {inspector, fun('InspectUserShops', _) ->
                {ok, #'fraudbusters_BlockedShops'{shop_list = []}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"InspectUser">>, Config),
    {ok, #{<<"blockedShops">> := []}} =
        capi_client_antifraud:inspect_user(?config(context, Config), ?INSPECT_USER_REQUEST).

-spec inspect_user_invalid_request_test(config()) -> _.
inspect_user_invalid_request_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {inspector, fun('InspectUserShops', _) ->
                {throwing, #'base_InvalidRequest'{errors = [<<"some error">>]}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"InspectUser">>, Config),
    {error, {400, #{<<"code">> := <<"invalidRequest">>}}} =
        capi_client_antifraud:inspect_user(?config(context, Config), ?INSPECT_USER_REQUEST).

-spec inspect_user_forbidden_test(config()) -> _.
inspect_user_forbidden_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_forbidden(), Config),
    {error, {401, _}} =
        capi_client_antifraud:inspect_user(?config(context, Config), ?INSPECT_USER_REQUEST).
