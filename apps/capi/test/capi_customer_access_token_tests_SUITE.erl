-module(capi_customer_access_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-include_lib("capi_dummy_data.hrl").

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
    get_customer_ok_test/1,
    get_customer_payments_ok_test/1,
    get_customer_bank_cards_ok_test/1
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
    [
        {group, operations_by_customer_access_token_after_customer_creation},
        {group, operations_by_customer_access_token_after_token_creation}
    ].

customer_access_token_tests() ->
    [
        get_customer_ok_test,
        get_customer_payments_ok_test,
        get_customer_bank_cards_ok_test
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_customer_access_token_after_customer_creation, [], customer_access_token_tests()},
        {operations_by_customer_access_token_after_token_creation, [], customer_access_token_tests()}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(operations_by_customer_access_token_after_customer_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Context = capi_ct_helper:get_context(?API_TOKEN),
    _ = capi_ct_helper:mock_services(
        [{customer_management, fun('Create', _) -> {ok, ?CUSTOMER} end}],
        MockServiceSup
    ),
    _ = capi_ct_helper_token_keeper:mock_api_key_token(?STRING, MockServiceSup),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateCustomer">>, ?STRING, MockServiceSup),
    Req = #{
        <<"partyID">> => ?STRING,
        <<"contactInfo">> => #{<<"email">> => <<"test@test.ru">>}
    },
    {ok, #{
        <<"customer">> := _,
        <<"customerAccessToken">> := #{<<"payload">> := CustAccToken}
    }} = capi_client_customers:create_customer(Context, Req),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(CustAccToken)} | Config];
init_per_group(operations_by_customer_access_token_after_token_creation, Config) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Context = capi_ct_helper:get_context(?API_TOKEN),
    _ = capi_ct_helper:mock_services(
        [{customer_management, fun('Get', _) -> {ok, ?CUSTOMER_STATE} end}],
        MockServiceSup
    ),
    _ = capi_ct_helper_token_keeper:mock_api_key_token(?STRING, MockServiceSup),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateCustomerAccessToken">>, ?STRING, MockServiceSup),
    {ok, #{<<"payload">> := CustAccToken}} =
        capi_client_customers:create_customer_access_token(Context, ?STRING),
    capi_ct_helper:stop_mocked_service_sup(MockServiceSup),
    [{context, capi_ct_helper:get_context(CustAccToken)} | Config];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = capi_utils:'maybe'(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    MockServiceSup = capi_ct_helper:start_mocked_service_sup(?MODULE),
    _ = capi_ct_helper_token_keeper:mock_customer_access_token(?STRING, ?STRING, MockServiceSup),
    [{test_sup, MockServiceSup} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec get_customer_ok_test(config()) -> _.
get_customer_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{customer_management, fun('Get', _) -> {ok, ?CUSTOMER_STATE} end}],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetCustomerByID">>, ?STRING, Config),
    {ok, #{
        <<"id">> := ?STRING,
        <<"createdAt">> := ?TIMESTAMP
    }} = capi_client_customers:get_customer_by_id(?config(context, Config), ?STRING).

-spec get_customer_payments_ok_test(config()) -> _.
get_customer_payments_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun
                ('Get', _) -> {ok, ?CUSTOMER_STATE};
                ('GetPayments', _) -> {ok, ?CUSTOMER_PAYMENTS_RESPONSE}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetCustomerPayments">>, ?STRING, Config),
    {ok, #{<<"result">> := [#{<<"invoiceID">> := ?STRING}]}} =
        capi_client_customers:get_customer_payments(
            ?config(context, Config),
            ?STRING,
            #{<<"limit">> => <<"10">>}
        ).

-spec get_customer_bank_cards_ok_test(config()) -> _.
get_customer_bank_cards_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun
                ('Get', _) -> {ok, ?CUSTOMER_STATE};
                ('GetBankCards', _) -> {ok, ?CUSTOMER_BANK_CARDS_RESPONSE}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetCustomerBankCards">>, ?STRING, Config),
    {ok, #{<<"result">> := [#{<<"id">> := ?STRING}]}} =
        capi_client_customers:get_customer_bank_cards(
            ?config(context, Config),
            ?STRING,
            #{<<"limit">> => <<"10">>}
        ).
