-module(capi_magista_stat_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
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
    search_invoices_ok_test/1,
    search_invoices_by_invoice_id_ok_test/1,
    search_invoices_by_customer_id_ok_test/1,
    search_payments_without_all_optional_fields_ok_test/1,
    search_payments_ok_test/1,
    search_refunds_ok_test/1,
    search_refunds_by_refund_id_ok_test/1,
    search_refunds_by_invoice_id_ok_test/1,
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
            search_invoices_ok_test,
            search_invoices_by_invoice_id_ok_test,
            search_invoices_by_customer_id_ok_test,
            search_payments_without_all_optional_fields_ok_test,
            search_payments_ok_test,
            search_refunds_ok_test,
            search_refunds_by_refund_id_ok_test,
            search_refunds_by_invoice_id_ok_test,
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

-spec search_invoices_ok_test(config()) -> _.
search_invoices_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchInvoices">>, ?STRING, ?STRING, undefined, undefined),
        #ctx_v1_ContextPaymentProcessing{},
        Config
    ),
    {ok, _, _} = search_invoices([], Config),
    {ok, _, _} = search_invoices([{'offset', 42}], Config),
    {ok, _, _} = search_invoices([{'invoiceStatus', <<"unpaid">>}], Config),
    {ok, _, _} = search_invoices([{'invoiceStatus', <<"cancelled">>}], Config),
    {ok, _, _} = search_invoices([{'invoiceStatus', <<"fulfilled">>}, {'invoiceAmount', 42}], Config),
    {ok, _, _} = search_invoices(
        [
            {'paymentStatus', <<"failed">>},
            {'paymentAmount', 42},
            {'payerEmail', ?EMAIL},
            {'rrn', <<"090909090909">>},
            {'approvalCode', <<"808080">>}
        ],
        Config
    ).

-spec search_invoices_by_invoice_id_ok_test(config()) -> _.
search_invoices_by_invoice_id_ok_test(Config) ->
    InvoiceID = <<"blarg">>,
    PaymentID = <<"blorg">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(InvoiceID)} end},
            {magista, fun('SearchInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}
        ],
        Config
    ),
    SearchCtx = ?CTX_SEARCH_OP(<<"SearchInvoices">>, ?STRING, ?STRING, InvoiceID, PaymentID),
    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #ctx_v1_ContextFragment{
                capi = ?CTX_CAPI(SearchCtx),
                payment_processing = #ctx_v1_ContextPaymentProcessing{
                    invoice = ?CTX_INVOICE(InvoiceID, ?STRING, ?STRING)
                }
            }
        ),
        Config
    ),
    {ok, _, _} = search_invoices([{'invoiceID', InvoiceID}, {'paymentID', PaymentID}], Config).

-spec search_invoices_by_customer_id_ok_test(config()) -> _.
search_invoices_by_customer_id_ok_test(Config) ->
    CustomerID = <<"blerg">>,
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Get', _) -> {ok, ?CUSTOMER(CustomerID)} end},
            {magista, fun('SearchInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}
        ],
        Config
    ),
    SearchCtx = ?CTX_SEARCH_OP(<<"SearchInvoices">>, ?STRING, ?STRING, undefined, undefined, CustomerID, undefined),
    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #ctx_v1_ContextFragment{
                capi = ?CTX_CAPI(SearchCtx),
                payment_processing = #ctx_v1_ContextPaymentProcessing{
                    customer = ?CTX_CUSTOMER(CustomerID, ?STRING, ?STRING)
                }
            }
        ),
        Config
    ),
    {ok, _, _} = search_invoices([{'customerID', CustomerID}], Config).

search_invoices(Query, Config) ->
    BaseQuery = [
        {limit, 42},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    capi_client_searches:search_invoices(?config(context, Config), ?STRING, BaseQuery ++ Query).

-spec search_payments_without_all_optional_fields_ok_test(config()) -> _.
search_payments_without_all_optional_fields_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchPayments">>, ?STRING, ?STRING, undefined, undefined),
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}}
    ],
    {ok, _, _} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_payments_ok_test(config()) -> _.
search_payments_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(<<"testInvoiceID">>)} end},
            {magista, fun('SearchPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}
        ],
        Config
    ),
    SearchCtx = ?CTX_SEARCH_OP(<<"SearchPayments">>, ?STRING, ?STRING, <<"testInvoiceID">>, <<"testPaymentID">>),
    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #ctx_v1_ContextFragment{
                capi = ?CTX_CAPI(SearchCtx),
                payment_processing = #ctx_v1_ContextPaymentProcessing{
                    invoice = ?CTX_INVOICE(<<"testInvoiceID">>, ?STRING, ?STRING)
                }
            }
        ),
        Config
    ),
    {ok, _, _} = search_payments([{'paymentStatus', <<"pending">>}], Config),
    {ok, _, _} = search_payments([{'paymentStatus', <<"processed">>}], Config),
    {ok, _, _} = search_payments([{'paymentStatus', <<"captured">>}], Config),
    {ok, _, _} = search_payments([{'paymentStatus', <<"cancelled">>}], Config),
    {ok, _, _} = search_payments([{'paymentStatus', <<"refunded">>}], Config),
    {ok, _, _} = search_payments([{'paymentStatus', <<"failed">>}], Config),
    {ok, _, _} = search_payments([{'paymentFlow', <<"instant">>}], Config),
    {ok, _, _} = search_payments([{'paymentFlow', <<"hold">>}], Config),
    {ok, _, _} = search_payments([{'paymentMethod', <<"bankCard">>}], Config),
    {ok, _, _} = search_payments([{'paymentMethod', <<"paymentTerminal">>}], Config),
    {ok, _, _} = search_payments(
        [
            {'payerFingerprint', <<"blablablalbalbal">>},
            {'first6', <<"424242">>},
            {'last4', <<"2222">>},
            {'rrn', <<"090909090909">>},
            {'approvalCode', <<"808080">>},
            {'paymentAmount', 10000},
            {'paymentTerminalProvider', <<"NEUROSET">>},
            {'barnkCardPaymentSystem', <<"MONSTERCARD">>},
            {'barnkCardTokenProvider', <<"BLABLAPAY">>}
        ],
        Config
    ).

search_payments(QueryAdds, Config) ->
    Query = [
        {limit, 2},
        {offset, 42},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'payerEmail', <<"test@test.ru">>},
        {'payerIP', <<"192.168.0.1">>},
        {'invoiceID', <<"testInvoiceID">>},
        {'paymentID', <<"testPaymentID">>},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    capi_client_searches:search_payments(?config(context, Config), ?STRING, Query ++ QueryAdds).

-spec search_refunds_ok_test(config()) -> _.
search_refunds_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchRefunds', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchRefunds">>, ?STRING, ?STRING, undefined, undefined),
        #ctx_v1_ContextPaymentProcessing{},
        Config
    ),
    {ok, _, _} = search_refunds([], Config),
    {ok, _, _} = search_refunds([{'refundStatus', <<"pending">>}], Config),
    {ok, _, _} = search_refunds([{'refundStatus', <<"succeeded">>}], Config),
    {ok, _, _} = search_refunds([{'refundStatus', <<"failed">>}], Config),
    {ok, _, _} = search_refunds([{'rrn', <<"090909090909">>}, {'approvalCode', <<"808080">>}], Config).

-spec search_refunds_by_refund_id_ok_test(config()) -> _.
search_refunds_by_refund_id_ok_test(Config) ->
    RefundID = <<"refünd">>,
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchRefunds', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchRefunds">>, ?STRING, ?STRING, undefined, undefined, undefined, RefundID),
        #ctx_v1_ContextPaymentProcessing{},
        Config
    ),
    {ok, _, _} = search_refunds([{'refundID', RefundID}], Config).

-spec search_refunds_by_invoice_id_ok_test(config()) -> _.
search_refunds_by_invoice_id_ok_test(Config) ->
    InvoiceID = <<"blarg">>,
    PaymentID = <<"blorg">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(InvoiceID)} end},
            {magista, fun('SearchRefunds', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}
        ],
        Config
    ),
    SearchCtx = ?CTX_SEARCH_OP(<<"SearchRefunds">>, ?STRING, ?STRING, InvoiceID, PaymentID),
    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #ctx_v1_ContextFragment{
                capi = ?CTX_CAPI(SearchCtx),
                payment_processing = #ctx_v1_ContextPaymentProcessing{
                    invoice = ?CTX_INVOICE(InvoiceID, ?STRING, ?STRING)
                }
            }
        ),
        Config
    ),
    {ok, _, _} = search_refunds([{'invoiceID', InvoiceID}, {'paymentID', PaymentID}], Config).

search_refunds(Query, Config) ->
    BaseQuery = [
        {limit, 42},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    capi_client_searches:search_refunds(?config(context, Config), ?STRING, BaseQuery ++ Query).

-spec search_payments_invalid_request_test(config()) -> _.
search_payments_invalid_request_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchPayments', _) -> {throwing, #base_InvalidRequest{errors = [<<"error">>]}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchPayments">>, ?STRING, ?STRING, undefined, undefined),
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    {error, {400, _}} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_payments_invalid_token_test(config()) -> _.
search_payments_invalid_token_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchPayments', _) -> {throwing, #magista_BadContinuationToken{reason = <<"">>}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchPayments">>, ?STRING, ?STRING, undefined, undefined),
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    {error, {400, _}} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).

-spec search_payments_limit_exceeded_test(config()) -> _.
search_payments_limit_exceeded_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {magista, fun('SearchPayments', _) -> {throwing, #magista_LimitExceeded{}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_search_op_ctx(
        ?CTX_SEARCH_OP(<<"SearchPayments">>, ?STRING, ?STRING, undefined, undefined),
        Config
    ),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {'continuationToken', <<"come_back_next_time">>}
    ],
    {error, {400, _}} = capi_client_searches:search_payments(?config(context, Config), ?STRING, Query).
