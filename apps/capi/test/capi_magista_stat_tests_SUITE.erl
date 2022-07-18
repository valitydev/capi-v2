-module(capi_magista_stat_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("magista_proto/include/magista_magista_thrift.hrl").
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
    search_payments_ok_test/1,
    search_payments_invalid_request_test/1,
    search_payments_invalid_token_test/1,
    search_payments_limit_exceeded_test/1
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
        {group, operations_by_api_key_token},
        {group, operations_by_user_session_token}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_api_key_token, [], [
            {group, operations_by_any_token}
        ]},
        {operations_by_user_session_token, [], [
            {group, operations_by_any_token}
        ]},
        {operations_by_any_token, [], [
            search_payments_ok_test,
            search_payments_invalid_request_test,
            search_payments_invalid_token_test,
            search_payments_limit_exceeded_test
        ]}
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
init_per_group(operations_by_api_key_token, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps = capi_ct_helper_token_keeper:mock_api_key_token(?STRING, SupPid),
    [{context, capi_ct_helper:get_context(?API_TOKEN)}, {group_apps, Apps}, {group_test_sup, SupPid} | Config];
init_per_group(operations_by_user_session_token, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps = capi_ct_helper_token_keeper:mock_user_session_token(SupPid),
    [{context, capi_ct_helper:get_context(?API_TOKEN)}, {group_apps, Apps}, {group_test_sup, SupPid} | Config];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.

end_per_group(Group, C) when
    Group =:= operations_by_api_key_token;
    Group =:= operations_by_user_session_token
->
    capi_utils:maybe(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1);
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

-spec search_payments_ok_test(config()) -> _.
search_payments_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            capi_test_hack:get_invoice_mock(),
            {magista, fun('SearchPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_payment_op_ctx(
        <<"SearchPayments">>,
        ?STRING,
        ?STRING,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        Config
    ),
    {ok, _, _} = make_search_payments_query([{'paymentStatus', <<"pending">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentStatus', <<"processed">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentStatus', <<"captured">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentStatus', <<"cancelled">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentStatus', <<"refunded">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentStatus', <<"failed">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentFlow', <<"instant">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentFlow', <<"hold">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentMethod', <<"bankCard">>}], Config),
    {ok, _, _} = make_search_payments_query([{'paymentMethod', <<"paymentTerminal">>}], Config),
    {ok, _, _} = make_search_payments_query(
        [
            {'payerFingerprint', <<"blablablalbalbal">>},
            {'first6', <<"424242">>},
            {'last4', <<"2222">>},
            {'rrn', <<"090909090909">>},
            {'approvalCode', <<"808080">>},
            {'paymentAmount', 10000}
        ],
        Config
    ).

make_search_payments_query(QueryAdds, Config) ->
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'payerEmail', <<"test@test.ru">>},
        {'payerIP', <<"192.168.0.1">>},
        {'invoiceID', <<"testInvoiceID">>},
        {'paymentID', <<"testPaymentID">>},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    capi_client_searches:search_payments(?config(context, Config), ?STRING, Query ++ QueryAdds).

-spec search_payments_invalid_request_test(config()) -> _.
search_payments_invalid_request_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            capi_test_hack:get_invoice_mock(),
            {magista, fun('SearchPayments', _) -> {throwing, #base_InvalidRequest{errors = [<<"error">>]}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_payment_op_ctx(
        <<"SearchPayments">>,
        ?STRING,
        ?STRING,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'invoiceID', <<"testInvoiceID">>},
        {'paymentID', <<"testPaymentID">>},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    {error, {400, _}} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_payments_invalid_token_test(config()) -> _.
search_payments_invalid_token_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            capi_test_hack:get_invoice_mock(),
            {magista, fun('SearchPayments', _) -> {throwing, #magista_BadContinuationToken{reason = <<"">>}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_payment_op_ctx(
        <<"SearchPayments">>,
        ?STRING,
        ?STRING,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'invoiceID', <<"testInvoiceID">>},
        {'paymentID', <<"testPaymentID">>},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    {error, {400, _}} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_payments_limit_exceeded_test(config()) -> _.
search_payments_limit_exceeded_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            capi_test_hack:get_invoice_mock(),
            {magista, fun('SearchPayments', _) -> {throwing, #magista_LimitExceeded{}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_payment_op_ctx(
        <<"SearchPayments">>,
        ?STRING,
        ?STRING,
        <<"testInvoiceID">>,
        <<"testPaymentID">>,
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'invoiceID', <<"testInvoiceID">>},
        {'paymentID', <<"testPaymentID">>},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    {error, {400, _}} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).
