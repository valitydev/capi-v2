-module(capi_base_api_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_error_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
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
    create_invoice_ok_test/1,
    create_invoice_rand_amount_ok_test/1,
    create_invoice_autorization_error_test/1,
    get_invoice_by_external_id/1,
    get_invoice_by_external_id_for_party/1,
    get_invoice_by_external_id_not_impl_error/1,
    create_invoice_access_token_ok_test/1,
    create_invoice_template_ok_test/1,
    create_invoice_template_w_randomization_ok_test/1,
    create_invoice_with_template_test/1,
    create_invoice_template_autorization_error_test/1,
    create_customer_ok_test/1,
    create_customer_autorization_error_test/1,
    delete_customer_ok_test/1,
    create_customer_access_token_ok_test/1,
    rescind_invoice_ok_test/1,
    fulfill_invoice_ok_test/1,

    get_payment_status_preauthorization_failed_test/1,
    get_payment_status_payment_tool_rejected_test/1,
    get_payment_status_account_limit_exceeded_test/1,
    get_payment_status_account_blocked_test/1,
    get_payment_status_rejected_by_issuer_test/1,
    get_payment_status_account_not_found_test/1,
    get_payment_status_insufficient_funds_test/1,

    create_payment_ok_test/1,
    create_payment_with_changed_cost_ok_test/1,
    create_refund/1,
    create_refund_blocked_error/1,
    create_refund_expired_error/1,
    create_partial_refund/1,
    create_partial_refund_without_currency/1,
    get_refund_by_id/1,
    get_refunds/1,
    get_chargebacks/1,
    get_chargeback_by_id/1,
    get_refund_by_external_id/1,
    update_invoice_template_ok_test/1,
    delete_invoice_template_ok_test/1,
    get_my_party_ok_test/1,
    get_my_party_lazy_creation_ok_test/1,
    get_my_party_lazy_creation_fail_test/1,
    suspend_my_party_ok_test/1,
    activate_my_party_ok_test/1,
    get_party_by_id_ok_test/1,
    suspend_party_by_id_ok_test/1,
    activate_party_by_id_ok_test/1,
    get_shop_by_id_ok_test/1,
    get_shops_ok_test/1,
    activate_shop_ok_test/1,
    suspend_shop_ok_test/1,
    get_shop_by_id_for_party_ok_test/1,
    get_shops_for_party_ok_test/1,
    get_shops_for_party_restricted_ok_test/1,
    suspend_shop_for_party_ok_test/1,
    activate_shop_for_party_ok_test/1,
    get_shop_by_id_for_party_error_test/1,
    get_shops_for_party_error_test/1,
    suspend_shop_for_party_error_test/1,
    activate_shop_for_party_error_test/1,
    get_contract_by_id_ok_test/1,
    get_contract_by_id_for_party_ok_test/1,
    get_contracts_ok_test/1,
    get_contracts_for_party_ok_test/1,
    get_contract_adjustments_ok_test/1,
    get_contract_adjustments_for_party_ok_test/1,
    get_contract_adjustment_by_id_ok_test/1,
    get_contract_adjustment_by_id_for_party_ok_test/1,
    create_webhook_ok_test/1,
    create_webhook_limit_exceeded_test/1,
    get_webhooks/1,
    get_webhooks_for_party/1,
    get_webhook_by_id/1,
    delete_webhook_by_id/1,
    get_categories_ok_test/1,
    get_category_by_ref_ok_test/1,
    get_payment_institutions/1,
    get_payment_institution_by_ref/1,
    get_payment_institution_payment_terms/1,
    get_service_provider_by_id/1,
    check_no_payment_by_external_id_test/1,
    check_no_internal_id_for_external_id_test/1,
    retrieve_payment_by_external_id_test/1,
    retrieve_payment_by_external_id_for_party_test/1,
    check_no_invoice_by_external_id_test/1,
    retrieve_refund_by_external_id_test/1,
    retrieve_refund_by_external_id_for_party_test/1,
    get_country_by_id_test/1,
    get_country_by_id_not_found_test/1,
    get_countries_test/1,
    get_trade_bloc_by_id_test/1,
    get_trade_bloc_by_id_not_found_test/1,
    get_trade_blocs_test/1,
    different_ip_header/1
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
            get_my_party_lazy_creation_fail_test,
            {group, operations_by_any_token}
        ]},
        {operations_by_user_session_token, [], [
            get_my_party_lazy_creation_ok_test,
            {group, operations_by_any_token}
        ]},
        {operations_by_any_token, [], [
            create_customer_ok_test,
            create_customer_autorization_error_test,
            delete_customer_ok_test,
            create_customer_access_token_ok_test,

            create_invoice_ok_test,
            create_invoice_rand_amount_ok_test,
            create_invoice_autorization_error_test,
            get_invoice_by_external_id,
            get_invoice_by_external_id_for_party,
            get_invoice_by_external_id_not_impl_error,
            check_no_invoice_by_external_id_test,
            create_invoice_access_token_ok_test,
            create_invoice_template_ok_test,
            create_invoice_template_w_randomization_ok_test,
            create_invoice_template_autorization_error_test,
            create_invoice_with_template_test,
            rescind_invoice_ok_test,
            fulfill_invoice_ok_test,
            update_invoice_template_ok_test,
            delete_invoice_template_ok_test,

            get_my_party_ok_test,
            suspend_my_party_ok_test,
            activate_my_party_ok_test,
            get_party_by_id_ok_test,
            suspend_party_by_id_ok_test,
            activate_party_by_id_ok_test,
            get_shop_by_id_ok_test,
            get_shops_ok_test,
            activate_shop_ok_test,
            suspend_shop_ok_test,

            get_categories_ok_test,

            get_contract_by_id_ok_test,
            get_contract_by_id_for_party_ok_test,
            get_contracts_ok_test,
            get_contracts_for_party_ok_test,
            get_contract_adjustments_ok_test,
            get_contract_adjustments_for_party_ok_test,
            get_contract_adjustment_by_id_ok_test,
            get_contract_adjustment_by_id_for_party_ok_test,

            get_shop_by_id_for_party_ok_test,
            get_shop_by_id_for_party_error_test,
            get_shops_for_party_ok_test,
            get_shops_for_party_restricted_ok_test,
            get_shops_for_party_error_test,
            suspend_shop_for_party_ok_test,
            suspend_shop_for_party_error_test,
            activate_shop_for_party_ok_test,
            activate_shop_for_party_error_test,

            create_payment_ok_test,
            create_payment_with_changed_cost_ok_test,
            retrieve_payment_by_external_id_test,
            retrieve_payment_by_external_id_for_party_test,
            check_no_payment_by_external_id_test,
            retrieve_refund_by_external_id_test,
            retrieve_refund_by_external_id_for_party_test,
            create_refund,
            create_refund_blocked_error,
            create_refund_expired_error,
            create_partial_refund,
            create_partial_refund_without_currency,
            get_chargebacks,
            get_chargeback_by_id,
            get_refund_by_id,
            get_refunds,
            get_refund_by_external_id,
            check_no_internal_id_for_external_id_test,
            get_payment_status_preauthorization_failed_test,
            get_payment_status_payment_tool_rejected_test,
            get_payment_status_account_limit_exceeded_test,
            get_payment_status_account_blocked_test,
            get_payment_status_rejected_by_issuer_test,
            get_payment_status_account_not_found_test,
            get_payment_status_insufficient_funds_test,

            get_payment_institutions,
            get_payment_institution_by_ref,
            get_payment_institution_payment_terms,
            get_service_provider_by_id,

            get_category_by_ref_ok_test,
            get_country_by_id_test,
            get_country_by_id_not_found_test,
            get_countries_test,
            get_trade_bloc_by_id_test,
            get_trade_bloc_by_id_not_found_test,
            get_trade_blocs_test,

            create_webhook_ok_test,
            create_webhook_limit_exceeded_test,
            get_webhooks,
            get_webhooks_for_party,
            get_webhook_by_id,
            delete_webhook_by_id,

            different_ip_header
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

-spec create_invoice_ok_test(config()) -> _.
create_invoice_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoice">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_invoices:create_invoice(?config(context, Config), ?INVOICE_PARAMS).

-spec create_invoice_rand_amount_ok_test(config()) -> _.
create_invoice_rand_amount_ok_test(Config) ->
    RandomizedAmount = ?INTEGER + ?SMALLER_INTEGER,
    ExpectedInvoice = ?RECORD_UPDATE(
        #domain_Invoice.mutations,
        [{amount, #domain_InvoiceAmountMutation{original = ?INTEGER, mutated = RandomizedAmount}}],
        ?RECORD_UPDATE(#domain_Invoice.cost, ?CASH(RandomizedAmount), ?INVOICE)
    ),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', _) -> {ok, ?PAYPROC_INVOICE(ExpectedInvoice, [])} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoice">>, ?STRING, ?STRING, Config),
    {ok, Response} = capi_client_invoices:create_invoice(
        ?config(context, Config),
        maps:merge(?INVOICE_PARAMS, #{
            <<"randomizeAmount">> => #{
                <<"deviation">> => ?SMALLER_INTEGER
            }
        })
    ),
    ?assertMatch(
        #{
            <<"invoice">> := #{
                <<"amount">> := RandomizedAmount,
                <<"amountRandomized">> := #{
                    <<"original">> := ?INTEGER,
                    <<"randomized">> := RandomizedAmount
                }
            }
        },
        Response
    ).

-spec create_invoice_autorization_error_test(config()) -> _.
create_invoice_autorization_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Create', {#payproc_InvoiceParams{party_id = <<"WrongPartyID">>}}) ->
                {throwing, #payproc_PartyNotFound{}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoice">>, <<"WrongPartyID">>, ?STRING, Config),
    ?assertMatch(
        {error, {400, #{<<"code">> := <<"invalidPartyID">>}}},
        capi_client_invoices:create_invoice(
            ?config(context, Config),
            maps:merge(?INVOICE_PARAMS, #{<<"partyID">> => <<"WrongPartyID">>})
        )
    ).

-spec get_invoice_by_external_id(config()) -> _.
get_invoice_by_external_id(Config) ->
    ExternalID = <<"merch_id">>,
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{}}),
    InvoiceID = capi_utils:get_unique_id(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(InvoiceID, ExternalID)} end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(InvoiceID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"GetInvoiceByExternalID">>,
        InvoiceID,
        ?STRING,
        ?STRING,
        Config
    ),

    {ok, _} = capi_client_invoices:get_invoice_by_external_id(?config(context, Config), ExternalID).

-spec get_invoice_by_external_id_for_party(config()) -> _.
get_invoice_by_external_id_for_party(Config) ->
    PartyID = capi_utils:get_unique_id(),
    ExternalID = <<"merch_id">>,
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{}}),
    InvoiceID = capi_utils:get_unique_id(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(InvoiceID, ExternalID, PartyID)} end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(InvoiceID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"GetInvoiceByExternalIDForParty">>,
        InvoiceID,
        PartyID,
        ?STRING,
        Config
    ),

    {ok, _} = capi_client_invoices:get_invoice_by_external_id_for_party(?config(context, Config), PartyID, ExternalID).

-spec get_invoice_by_external_id_not_impl_error(config()) -> _.
get_invoice_by_external_id_not_impl_error(Config) ->
    ExternalID = <<"merch_id">>,
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{}}),
    InvoiceID = capi_utils:get_unique_id(),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE_WITH_ID(InvoiceID, ExternalID)} end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(InvoiceID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_restricted_shops([?CTX_ENTITY(<<"Shop1">>)], Config),

    {error, {invalid_response_code, 501}} =
        capi_client_invoices:get_invoice_by_external_id(?config(context, Config), ExternalID).

-spec create_invoice_access_token_ok_test(config()) -> _.
create_invoice_access_token_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"CreateInvoiceAccessToken">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_invoices:create_invoice_access_token(?config(context, Config), ?STRING).

-spec create_invoice_template_ok_test(config()) -> _.
create_invoice_template_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun('Create', {#payproc_InvoiceTemplateCreateParams{party_id = ?STRING}}) ->
                {ok, ?INVOICE_TPL}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoiceTemplate">>, ?STRING, ?STRING, Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    Details0 = #{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => <<"test_invoice_template_product">>,
        <<"price">> => #{
            <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        }
    },
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req#{<<"details">> => Details0}),
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req#{
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS
    }).

-spec create_invoice_template_w_randomization_ok_test(config()) -> _.
create_invoice_template_w_randomization_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun('Create', {#payproc_InvoiceTemplateCreateParams{party_id = ?STRING}}) ->
                {ok,
                    ?RECORD_UPDATE(
                        #domain_InvoiceTemplate.mutations,
                        [
                            {amount,
                                {randomization, #domain_RandomizationMutationParams{
                                    deviation = ?SMALLER_INTEGER,
                                    precision = 2,
                                    direction = both
                                }}}
                        ],
                        ?INVOICE_TPL
                    )}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateInvoiceTemplate">>, ?STRING, ?STRING, Config),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>},
        <<"randomizeAmount">> => #{
            <<"deviation">> => ?SMALLER_INTEGER,
            <<"precision">> => 2,
            <<"direction">> => <<"both">>
        }
    },
    Details0 = #{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => <<"test_invoice_template_product">>,
        <<"price">> => #{
            <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        }
    },
    {ok, _} = capi_client_invoice_templates:create(?config(context, Config), Req#{<<"details">> => Details0}),
    {ok, InvoiceTplWithToken} = capi_client_invoice_templates:create(?config(context, Config), Req#{
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS
    }),
    ?assertMatch(
        #{
            <<"invoiceTemplate">> := #{
                <<"randomizeAmount">> :=
                    #{
                        <<"deviation">> := ?SMALLER_INTEGER,
                        <<"direction">> := <<"both">>,
                        <<"precision">> := 2
                    }
            }
        },
        InvoiceTplWithToken
    ).

-spec create_invoice_template_autorization_error_test(config()) -> _.
create_invoice_template_autorization_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun(
                'Create',
                {#payproc_InvoiceTemplateCreateParams{party_id = <<"WrongPartyID">>}}
            ) ->
                {throwing, #payproc_PartyNotFound{}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"CreateInvoiceTemplate">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    Req = #{
        <<"shopID">> => ?STRING,
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"description">> => <<"test_invoice_template_description">>,
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    ?assertMatch(
        {error, {400, #{<<"code">> := <<"invalidPartyID">>}}},
        capi_client_invoice_templates:create(
            ?config(context, Config),
            Req#{
                <<"partyID">> => <<"WrongPartyID">>,
                <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS
            }
        )
    ).

-spec create_invoice_with_template_test(config()) -> _.
create_invoice_with_template_test(Config) ->
    ExternalID = <<"external_id">>,
    BenderKey = <<"bender_key">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun
                ('Create', _) -> {ok, ?INVOICE_TPL};
                ('Get', _) -> {ok, ?INVOICE_TPL}
            end},
            {invoicing, fun(
                'CreateWithTemplate',
                {#payproc_InvoiceWithTemplateParams{id = ID, external_id = EID}}
            ) ->
                {ok, ?PAYPROC_INVOICE_WITH_ID(ID, EID)}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end},
            {bender, fun('GenerateID', _) -> {ok, capi_ct_helper_bender:get_result(BenderKey)} end}
        ],
        Config
    ),

    Req = #{
        <<"amount">> => ?INTEGER,
        <<"currency">> => ?RUB,
        <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
        <<"externalID">> => ExternalID
    },
    Ctx = ?config(context, Config),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_tpl_op_ctx(
        <<"CreateInvoiceWithTemplate">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),

    {ok, #{<<"invoice">> := Invoice}} =
        capi_client_invoice_templates:create_invoice(Ctx, ?STRING, Req),
    ?assertEqual(BenderKey, maps:get(<<"id">>, Invoice)),
    ?assertEqual(ExternalID, maps:get(<<"externalID">>, Invoice)).

-spec check_no_internal_id_for_external_id_test(config()) -> _.
check_no_internal_id_for_external_id_test(Config) ->
    ExternalID = capi_utils:get_unique_id(),
    _ = capi_ct_helper:mock_services(
        [
            {bender, fun('GetInternalID', _) -> {throwing, capi_ct_helper_bender:no_internal_id()} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetPaymentByExternalID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    %% Ugly test case, but after full integration with bouncer we would expect
    %% {error, {401, #{}}}.
    {error, {_, 500}} = capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec create_customer_ok_test(config()) -> _.
create_customer_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Create', {#payproc_CustomerParams{party_id = ?STRING}}) -> {ok, ?CUSTOMER} end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"CreateCustomer">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_customers:create_customer(?config(context, Config), ?CUSTOMER_PARAMS).

-spec create_customer_autorization_error_test(config()) -> _.
create_customer_autorization_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun('Create', {#payproc_CustomerParams{party_id = <<"WrongPartyID">>}}) ->
                {throwing, #payproc_PartyNotFound{}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(<<"bender_key">>) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"CreateCustomer">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {400, #{<<"code">> := <<"invalidPartyID">>}}},
        capi_client_customers:create_customer(
            ?config(context, Config),
            maps:merge(?CUSTOMER_PARAMS, #{
                <<"partyID">> => <<"WrongPartyID">>
            })
        )
    ).

-spec delete_customer_ok_test(config()) -> _.
delete_customer_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {customer_management, fun
                ('Get', _) -> {ok, ?CUSTOMER};
                ('Delete', _) -> {ok, ok}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_customer_op_ctx(
        <<"DeleteCustomer">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_customers:delete_customer(?config(context, Config), ?STRING).

-spec create_customer_access_token_ok_test(config()) -> _.
create_customer_access_token_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{customer_management, fun('Get', _) -> {ok, ?CUSTOMER} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_customer_op_ctx(
        <<"CreateCustomerAccessToken">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_customers:create_customer_access_token(?config(context, Config), ?STRING).

-spec rescind_invoice_ok_test(config()) -> _.
rescind_invoice_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Rescind', _) -> {ok, ok};
                ('Get', _) -> {ok, ?PAYPROC_INVOICE}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"RescindInvoice">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    ok = capi_client_invoices:rescind_invoice(?config(context, Config), ?STRING, ?STRING).

-spec fulfill_invoice_ok_test(config()) -> _.
fulfill_invoice_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Fulfill', _) -> {ok, ok};
                ('Get', _) -> {ok, ?PAYPROC_INVOICE}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_op_ctx(
        <<"FulfillInvoice">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    ok = capi_client_invoices:fulfill_invoice(?config(context, Config), ?STRING, ?STRING).

-spec get_payment_status_preauthorization_failed_test(config()) -> _.
get_payment_status_preauthorization_failed_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    MappedFailure = #{
        <<"code">> => <<"preauthorization_failed">>,
        <<"subError">> => #{
            <<"code">> => <<"unknown">>
        }
    },
    Failure =
        payproc_errors:construct(
            'PaymentFailure',
            {preauthorization_failed, {unknown, #payproc_error_GeneralFailure{}}},
            <<"Reason">>
        ),
    get_merchant_payment_status_test_impl(MappedFailure, Failure, Config).

-spec get_payment_status_payment_tool_rejected_test(config()) -> _.
get_payment_status_payment_tool_rejected_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    MappedFailure = #{
        <<"code">> => <<"authorization_failed">>,
        <<"subError">> => #{
            <<"code">> => <<"payment_tool_rejected">>,
            <<"subError">> => #{
                <<"code">> => <<"bank_card_rejected">>,
                <<"subError">> => #{<<"code">> => <<"cvv_invalid">>}
            }
        }
    },
    Failure =
        payproc_errors:construct(
            'PaymentFailure',
            {authorization_failed,
                {payment_tool_rejected, {bank_card_rejected, {cvv_invalid, #payproc_error_GeneralFailure{}}}}},
            <<"Reason">>
        ),
    get_merchant_payment_status_test_impl(MappedFailure, Failure, Config).

-spec get_payment_status_account_limit_exceeded_test(config()) -> _.
get_payment_status_account_limit_exceeded_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    MappedFailure = #{
        <<"code">> => <<"authorization_failed">>,
        <<"subError">> => #{
            <<"code">> => <<"account_limit_exceeded">>,
            <<"subError">> => #{
                <<"code">> => <<"unknown">>
            }
        }
    },
    Failure =
        payproc_errors:construct(
            'PaymentFailure',
            {authorization_failed, {account_limit_exceeded, {unknown, #payproc_error_GeneralFailure{}}}},
            <<"Reason">>
        ),
    get_merchant_payment_status_test_impl(MappedFailure, Failure, Config).

-spec get_payment_status_account_blocked_test(config()) -> _.
get_payment_status_account_blocked_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    get_merchant_payment_status_test_(account_blocked, Config).

-spec get_payment_status_rejected_by_issuer_test(config()) -> _.
get_payment_status_rejected_by_issuer_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    get_merchant_payment_status_test_(rejected_by_issuer, Config).

-spec get_payment_status_account_not_found_test(config()) -> _.
get_payment_status_account_not_found_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    get_merchant_payment_status_test_(account_not_found, Config).

-spec get_payment_status_insufficient_funds_test(config()) -> _.
get_payment_status_insufficient_funds_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
    get_merchant_payment_status_test_(insufficient_funds, Config).

get_merchant_payment_status_test_(SubErrorCode, Config) ->
    MappedFailure6 = #{
        <<"code">> => <<"authorization_failed">>,
        <<"subError">> => #{
            <<"code">> => atom_to_binary(SubErrorCode)
        }
    },
    Failure6 =
        payproc_errors:construct(
            'PaymentFailure',
            {authorization_failed, {SubErrorCode, #payproc_error_GeneralFailure{}}},
            <<"Reason">>
        ),
    get_merchant_payment_status_test_impl(MappedFailure6, Failure6, Config).

get_merchant_payment_status_test_impl(MappedFailure, Failure, Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE([?PAYPROC_FAILED_PAYMENT({failure, Failure})])} end}
        ],
        Config
    ),
    ?assertMatch(
        {ok, #{
            <<"status">> := <<"failed">>,
            <<"error">> := MappedFailure
        }},
        capi_client_payments:get_payment_by_id(?config(context, Config), ?STRING, ?STRING)
    ).

-spec create_payment_ok_test(config()) -> _.
create_payment_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE};
                ('StartPayment', {_, PaymentParams}) ->
                    #payproc_InvoicePaymentParams{
                        id = ID,
                        external_id = EID,
                        payer = {payment_resource, _},
                        payer_session_info = ?PAYER_SESSION_INFO,
                        context = ?CONTENT
                    } =
                        PaymentParams,
                    {ok, ?PAYPROC_PAYMENT(?PAYMENT_W_EXTERNAL_ID(ID, EID))}
            end},
            {party_management, fun('GetShop', _) ->
                {ok, ?SHOP}
            end},
            {bender, fun('GenerateID', _) ->
                {ok, capi_ct_helper_bender:get_result(BenderKey)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreatePayment">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    PaymentTool = {bank_card, ?BANK_CARD(<<"visa">>, ?EXP_DATE(2, 2020), <<"Degus">>)},
    PaymentToolToken = capi_crypto:encode_token(#{payment_tool => PaymentTool, valid_until => undefined}),
    Req = ?PAYMENT_PARAMS(ExternalID, PaymentToolToken),
    {ok, Payment} = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING),
    #{<<"transactionInfo">> := #{<<"extra_payment_info">> := _}} = Payment.

-spec create_payment_with_changed_cost_ok_test(config()) -> _.
create_payment_with_changed_cost_ok_test(Config) ->
    BenderKey = <<"bender_key">>,
    ExternalID = <<"merch_id">>,
    ChangedCost = 1000,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE};
                ('StartPayment', {_, PaymentParams}) ->
                    #payproc_InvoicePaymentParams{
                        id = ID,
                        payer = {payment_resource, _},
                        payer_session_info = ?PAYER_SESSION_INFO,
                        context = ?CONTENT
                    } =
                        PaymentParams,
                    {ok, ?PAYPROC_PAYMENT(?PAYMENT_W_CHANGED_COST(ID, ChangedCost))}
            end},
            {party_management, fun('GetShop', _) ->
                {ok, ?SHOP}
            end},
            {bender, fun('GenerateID', _) ->
                {ok, capi_ct_helper_bender:get_result(BenderKey)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreatePayment">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    PaymentTool = {bank_card, ?BANK_CARD(<<"visa">>, ?EXP_DATE(2, 2020), <<"Degus">>)},
    PaymentToolToken = capi_crypto:encode_token(#{payment_tool => PaymentTool, valid_until => undefined}),
    Req = ?PAYMENT_PARAMS(ExternalID, PaymentToolToken),
    {ok, Payment} = capi_client_payments:create_payment(?config(context, Config), Req, ?STRING),
    #{<<"amount">> := ChangedCost} = Payment.

-spec create_refund(config()) -> _.
create_refund(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{<<"reason">> => ?STRING},
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT])};
                ('RefundPayment', _) ->
                    {ok, ?REFUND}
            end},
            {generator, fun('GenerateID', _) ->
                capi_ct_helper_bender:generate_id(BenderKey)
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_refund_blocked_error(config()) -> _.
create_refund_blocked_error(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{<<"reason">> => ?STRING},
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    Invoice = ?PAYPROC_INVOICE,
                    {ok, Invoice#payproc_Invoice{payments = [?PAYPROC_PAYMENT]}};
                ('RefundPayment', {?STRING, _, _}) ->
                    {throwing, #payproc_InvalidPartyStatus{
                        status = {blocking, {blocked, #domain_Blocked{reason = ?STRING, since = ?TIMESTAMP}}}
                    }}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(BenderKey) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {error, {400, _}} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_refund_expired_error(config()) -> _.
create_refund_expired_error(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{<<"reason">> => ?STRING},
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    Invoice = ?PAYPROC_INVOICE,
                    {ok, Invoice#payproc_Invoice{payments = [?PAYPROC_PAYMENT]}};
                ('RefundPayment', {?STRING, _, _}) ->
                    {throwing, #payproc_InvalidContractStatus{status = {expired, #domain_ContractExpired{}}}}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(BenderKey) end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {error, {400, _}} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_partial_refund(config()) -> _.
create_partial_refund(Config) ->
    BenderKey = <<"bender_key">>,
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('Get', _) ->
                    P = ?PAYPROC_PAYMENT,
                    {ok, ?PAYPROC_INVOICE([P])};
                (
                    'RefundPayment',
                    {
                        _,
                        _,
                        #payproc_InvoicePaymentRefundParams{
                            cash = ?CASH,
                            cart = ?INVOICE_CART(#{<<"TaxMode">> := {str, <<"10%">>}})
                        }
                    }
                ) ->
                    {ok, ?REFUND}
            end},
            {generator, fun('GenerateID', _) -> capi_ct_helper_bender:generate_id(BenderKey) end}
        ],
        Config
    ),
    Req = #{
        <<"reason">> => ?STRING,
        <<"currency">> => ?RUB,
        <<"amount">> => ?INTEGER,
        <<"cart">> => ?SWAG_INVOICE_CART
    },
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec create_partial_refund_without_currency(config()) -> _.
create_partial_refund_without_currency(Config) ->
    BenderKey = <<"bender_key">>,
    Req = #{
        <<"reason">> => ?STRING,
        <<"amount">> => ?INTEGER
    },
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun
                ('GetPayment', _) ->
                    {ok, ?PAYPROC_PAYMENT};
                ('Get', _) ->
                    P = ?PAYPROC_PAYMENT,
                    {ok, ?PAYPROC_INVOICE([P])};
                ('RefundPayment', _) ->
                    {ok, ?REFUND}
            end},
            {generator, fun('GenerateID', _) ->
                capi_ct_helper_bender:generate_id(BenderKey)
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"CreateRefund">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING).

-spec get_refund_by_id(config()) -> _.
get_refund_by_id(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT,
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(?STRING, ?STRING)]},
                {ok, ?PAYPROC_INVOICE([Payment])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_refund_op_ctx(
        <<"GetRefundByID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_refund_by_id(?config(context, Config), ?STRING, ?STRING, ?STRING).

-spec get_refunds(config()) -> _.
get_refunds(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT,
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(?STRING, ?STRING)]},
                {ok, ?PAYPROC_INVOICE([Payment])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetRefunds">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_refunds(?config(context, Config), ?STRING, ?STRING).

-spec get_refund_by_external_id(config()) -> _.
get_refund_by_external_id(Config) ->
    capi_ct_helper_bender:with_storage(
        fun(Tid) ->
            _ = capi_ct_helper:mock_services(
                [
                    {invoicing, fun
                        ('Get', _) ->
                            P = ?PAYPROC_PAYMENT,
                            Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(?STRING, ?STRING)]},
                            {ok, ?PAYPROC_INVOICE([Payment])};
                        ('RefundPayment', _) ->
                            {ok, ?REFUND}
                    end},
                    {bender, fun
                        ('GetInternalID', {ID}) ->
                            capi_ct_helper_bender:get_internal_id(Tid, ID);
                        ('GenerateID', {ID, _Schema, Ctx}) ->
                            capi_ct_helper_bender:generate_id(Tid, ID, ?STRING, Ctx)
                    end}
                ],
                Config
            ),
            _ = capi_ct_helper_bouncer:mock_arbiter(capi_ct_helper_bouncer:judge_always_allowed(), Config),
            Req = #{<<"reason">> => ?STRING, <<"externalID">> => ?STRING},
            {ok, _} = capi_client_payments:create_refund(?config(context, Config), Req, ?STRING, ?STRING),
            {ok, _} = capi_client_payments:get_refund_by_external_id(?config(context, Config), ?STRING)
        end
    ).

%

-spec get_chargebacks(config()) -> _.
get_chargebacks(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetChargebacks">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_chargebacks(?config(context, Config), ?STRING, ?STRING).

-spec get_chargeback_by_id(config()) -> _.
get_chargeback_by_id(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT])}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetChargebackByID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_payments:get_chargeback_by_id(?config(context, Config), ?STRING, ?STRING, ?STRING).

%

-spec update_invoice_template_ok_test(config()) -> _.
update_invoice_template_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun
                ('Update', _) -> {ok, ?INVOICE_TPL};
                ('Get', _) -> {ok, ?INVOICE_TPL}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_tpl_op_ctx(
        <<"UpdateInvoiceTemplate">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    Req = #{
        <<"lifetime">> => capi_ct_helper:get_lifetime(),
        <<"metadata">> => #{<<"invoice_template_dummy_metadata">> => <<"test_value">>}
    },
    Details0 = #{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => <<"test_invoice_template_product">>,
        <<"price">> => #{
            <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
            <<"currency">> => ?RUB,
            <<"amount">> => ?INTEGER
        }
    },
    {ok, _} = capi_client_invoice_templates:update(?config(context, Config), ?STRING, Req#{<<"details">> => Details0}),
    Details1 = #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency">> => ?RUB,
        <<"cart">> => [
            #{
                <<"product">> => ?STRING,
                <<"price">> => ?INTEGER,
                <<"quantity">> => ?INTEGER
            }
        ]
    },
    {ok, _} = capi_client_invoice_templates:update(?config(context, Config), ?STRING, Req#{<<"details">> => Details1}).

-spec delete_invoice_template_ok_test(config()) -> _.
delete_invoice_template_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun
                ('Delete', _) -> {ok, ok};
                ('Get', _) -> {ok, ?INVOICE_TPL}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_invoice_tpl_op_ctx(
        <<"DeleteInvoiceTemplate">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),
    ok = capi_client_invoice_templates:delete(?config(context, Config), ?STRING).

-spec get_my_party_ok_test(config()) -> _.
get_my_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetMyParty">>, ?STRING, Config),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)).

-spec get_my_party_lazy_creation_ok_test(config()) -> _.
get_my_party_lazy_creation_ok_test(Config) ->
    TestETS = ets:new(get_my_party_lazy_creation_ok_test, [public]),
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) ->
                    case ets:lookup(TestETS, party_created) of
                        [{party_created, true}] -> {ok, ?INTEGER};
                        _ -> {throwing, #payproc_PartyNotFound{}}
                    end;
                ('Checkout', _) ->
                    case ets:lookup(TestETS, party_created) of
                        [{party_created, true}] -> {ok, ?PARTY};
                        _ -> {throwing, #payproc_PartyNotFound{}}
                    end;
                ('Create', _) ->
                    case ets:insert_new(TestETS, {party_created, true}) of
                        true -> {ok, ok};
                        _ -> {throwing, #payproc_PartyExists{}}
                    end
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetMyParty">>, ?STRING, Config),
    {ok, _} = capi_client_parties:get_my_party(?config(context, Config)),
    true = ets:delete(TestETS).

-spec get_my_party_lazy_creation_fail_test(config()) -> _.
get_my_party_lazy_creation_fail_test(Config) ->
    TestETS = ets:new(get_my_party_lazy_creation_fail_test, [public]),
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) ->
                    case ets:lookup(TestETS, party_created) of
                        [{party_created, true}] -> {ok, ?INTEGER};
                        _ -> {throwing, #payproc_PartyNotFound{}}
                    end;
                ('Checkout', _) ->
                    case ets:lookup(TestETS, party_created) of
                        [{party_created, true}] -> {ok, ?PARTY};
                        _ -> {throwing, #payproc_PartyNotFound{}}
                    end;
                ('Create', _) ->
                    case ets:insert_new(TestETS, {party_created, true}) of
                        true -> {ok, ok};
                        _ -> {throwing, #payproc_PartyExists{}}
                    end
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetMyParty">>, ?STRING, Config),
    ?assertMatch(
        {error, {400, _}},
        capi_client_parties:get_my_party(?config(context, Config))
    ),
    true = ets:delete(TestETS).

-spec suspend_my_party_ok_test(config()) -> _.
suspend_my_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"SuspendMyParty">>, ?STRING, Config),
    ok = capi_client_parties:suspend_my_party(?config(context, Config)).

-spec activate_my_party_ok_test(config()) -> _.
activate_my_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"ActivateMyParty">>, ?STRING, Config),
    ok = capi_client_parties:activate_my_party(?config(context, Config)).

-spec get_party_by_id_ok_test(config()) -> _.
get_party_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetPartyByID">>, ?STRING, Config),
    {ok, _} = capi_client_parties:get_party_by_id(?config(context, Config), ?STRING).

-spec suspend_party_by_id_ok_test(config()) -> _.
suspend_party_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Suspend', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"SuspendPartyByID">>, ?STRING, Config),
    ok = capi_client_parties:suspend_party_by_id(?config(context, Config), ?STRING).

-spec activate_party_by_id_ok_test(config()) -> _.
activate_party_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Activate', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"ActivatePartyByID">>, ?STRING, Config),
    ok = capi_client_parties:activate_party_by_id(?config(context, Config), ?STRING).

-spec get_shop_by_id_ok_test(config()) -> _.
get_shop_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetShop', _) -> {ok, ?SHOP} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"GetShopByID">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shop_by_id(?config(context, Config), ?STRING).

-spec get_shop_by_id_for_party_ok_test(config()) -> _.
get_shop_by_id_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"GetShopByIDForParty">>, ?STRING, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shop_by_id_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_shop_by_id_for_party_error_test(config()) -> _.
get_shop_by_id_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', {<<"WrongPartyID">>, _}) ->
                {throwing, #payproc_PartyNotFound{}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"GetShopByIDForParty">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:get_shop_by_id_for_party(?config(context, Config), <<"WrongPartyID">>, ?STRING)
    ).

-spec get_shops_ok_test(config()) -> _.
get_shops_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetShops">>, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shops(?config(context, Config)).

-spec get_shops_for_party_ok_test(config()) -> _.
get_shops_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetShopsForParty">>, ?STRING, Config),
    {ok, _} = capi_client_shops:get_shops_for_party(?config(context, Config), ?STRING).

-spec get_shops_for_party_restricted_ok_test(config()) -> _.
get_shops_for_party_restricted_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_restricted_shops([?CTX_ENTITY(?USD)], Config),
    {ok, [#{<<"currency">> := ?USD}]} =
        capi_client_shops:get_shops_for_party(?config(context, Config), ?STRING).

-spec get_shops_for_party_error_test(config()) -> _.
get_shops_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [{party_management, fun('GetRevision', {<<"WrongPartyID">>}) -> {throwing, #payproc_PartyNotFound{}} end}],
        Config
    ),

    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetShopsForParty">>, <<"WrongPartyID">>, Config),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:get_shops_for_party(?config(context, Config), <<"WrongPartyID">>)
    ).

-spec activate_shop_ok_test(config()) -> _.
activate_shop_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('ActivateShop', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"ActivateShop">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:activate_shop(?config(context, Config), ?STRING).

-spec activate_shop_for_party_ok_test(config()) -> _.
activate_shop_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ActivateShop', {?STRING, _}) -> {ok, ok} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"ActivateShopForParty">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:activate_shop_for_party(?config(context, Config), ?STRING, ?STRING).

-spec activate_shop_for_party_error_test(config()) -> _.
activate_shop_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ActivateShop', {<<"WrongPartyID">>, _}) ->
                {throwing, #payproc_PartyNotFound{}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"ActivateShopForParty">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:activate_shop_for_party(?config(context, Config), <<"WrongPartyID">>, ?STRING)
    ).

-spec suspend_shop_ok_test(config()) -> _.
suspend_shop_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('SuspendShop', _) -> {ok, ok} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"SuspendShop">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:suspend_shop(?config(context, Config), ?STRING).

-spec suspend_shop_for_party_ok_test(config()) -> _.
suspend_shop_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('SuspendShop', _) -> {ok, ok} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(<<"SuspendShopForParty">>, ?STRING, ?STRING, Config),
    ok = capi_client_shops:suspend_shop_for_party(?config(context, Config), ?STRING, ?STRING).

-spec suspend_shop_for_party_error_test(config()) -> _.
suspend_shop_for_party_error_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('SuspendShop', {<<"WrongPartyID">>, _}) ->
                {throwing, #payproc_PartyNotFound{}}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_shop_op_ctx(
        <<"SuspendShopForParty">>,
        <<"WrongPartyID">>,
        ?STRING,
        Config
    ),
    ?assertMatch(
        {error, {404, _}},
        capi_client_shops:suspend_shop_for_party(?config(context, Config), <<"WrongPartyID">>, ?STRING)
    ).

-spec get_contract_by_id_ok_test(config()) -> _.
get_contract_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY};
                ('GetContract', _) -> {ok, ?CONTRACT}
            end}
        ],
        Config
    ),

    _ = capi_ct_helper_bouncer:mock_arbiter(
        ?assertContextMatches(
            #ctx_v1_ContextFragment{
                capi = ?CTX_CAPI(?CTX_CONTRACT_OP(<<"GetContractByID">>, ?STRING, _))
            }
        ),
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?STRING),
    {ok, _} = capi_client_contracts:get_contract_by_id(?config(context, Config), ?WALLET_CONTRACT_ID).

-spec get_contract_by_id_for_party_ok_test(config()) -> _.
get_contract_by_id_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY};
                ('GetContract', _) -> {ok, ?CONTRACT}
            end}
        ],
        Config
    ),

    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractByIDForParty">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_by_id_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_contracts_ok_test(config()) -> _.
get_contracts_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),

    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetContracts">>, ?STRING, Config),
    {ok, [_First, _Second]} = capi_client_contracts:get_contracts(?config(context, Config)).

-spec get_contracts_for_party_ok_test(config()) -> _.
get_contracts_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetContractsForParty">>, ?STRING, Config),
    {ok, [_First, _Second]} = capi_client_contracts:get_contracts_for_party(?config(context, Config), ?STRING).

-spec get_contract_adjustments_ok_test(config()) -> _.
get_contract_adjustments_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY};
                ('GetContract', _) -> {ok, ?CONTRACT}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustments">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustments(?config(context, Config), ?STRING).

-spec get_contract_adjustments_for_party_ok_test(config()) -> _.
get_contract_adjustments_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustmentsForParty">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustments_for_party(?config(context, Config), ?STRING, ?STRING).

-spec get_contract_adjustment_by_id_ok_test(config()) -> _.
get_contract_adjustment_by_id_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun
                ('GetRevision', _) -> {ok, ?INTEGER};
                ('Checkout', _) -> {ok, ?PARTY};
                ('GetContract', _) -> {ok, ?CONTRACT}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustmentByID">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustment_by_id(?config(context, Config), ?STRING, ?STRING).

-spec get_contract_adjustment_by_id_for_party_ok_test(config()) -> _.
get_contract_adjustment_by_id_for_party_ok_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetContract', _) -> {ok, ?CONTRACT} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_contract_op_ctx(
        <<"GetContractAdjustmentByIDForParty">>,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_contracts:get_contract_adjustment_by_id_for_party(
        ?config(context, Config),
        ?STRING,
        ?STRING,
        ?STRING
    ).

-spec create_webhook_ok_test(config()) -> _.
create_webhook_ok_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end},
            {webhook_manager, fun('Create', _) -> {ok, ?WEBHOOK} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateWebhook">>, ?STRING, Config),
    Req = #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => #{
            <<"topic">> => <<"InvoicesTopic">>,
            <<"shopID">> => ?STRING,
            <<"eventTypes">> => [
                <<"InvoiceCreated">>,
                <<"InvoicePaid">>,
                <<"InvoiceCancelled">>,
                <<"InvoiceFulfilled">>,
                <<"PaymentStarted">>,
                <<"PaymentProcessed">>,
                <<"PaymentCaptured">>,
                <<"PaymentCancelled">>,
                <<"PaymentRefunded">>,
                <<"PaymentFailed">>,
                <<"PaymentRefundCreated">>,
                <<"PaymentRefundFailed">>,
                <<"PaymentRefundSucceeded">>,
                <<"PaymentUserInteractionRequested">>,
                <<"PaymentUserInteractionCompleted">>
            ]
        }
    },
    {ok, _} = capi_client_webhooks:create_webhook(?config(context, Config), Req).

-spec create_webhook_limit_exceeded_test(config()) -> _.
create_webhook_limit_exceeded_test(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('GetShop', _) -> {ok, ?SHOP} end},
            {webhook_manager, fun('Create', _) -> {throwing, #webhooker_LimitExceeded{}} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"CreateWebhook">>, ?STRING, Config),
    Req = #{
        <<"url">> => <<"http://localhost:8080/TODO">>,
        <<"scope">> => #{
            <<"topic">> => <<"InvoicesTopic">>,
            <<"shopID">> => ?STRING,
            <<"eventTypes">> => [
                <<"InvoiceCreated">>,
                <<"InvoicePaid">>,
                <<"InvoiceCancelled">>,
                <<"InvoiceFulfilled">>,
                <<"PaymentStarted">>,
                <<"PaymentProcessed">>,
                <<"PaymentCaptured">>,
                <<"PaymentCancelled">>,
                <<"PaymentRefunded">>,
                <<"PaymentFailed">>,
                <<"PaymentRefundCreated">>,
                <<"PaymentRefundFailed">>,
                <<"PaymentRefundSucceeded">>
            ]
        }
    },
    Body = #{<<"message">> => <<"Webhook limit exceeded">>},
    {error, {429, Body}} = capi_client_webhooks:create_webhook(?config(context, Config), Req).

-spec get_webhooks(config()) -> _.
get_webhooks(Config) ->
    _ = capi_ct_helper:mock_services([{webhook_manager, fun('GetList', _) -> {ok, [?WEBHOOK]} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetWebhooks">>, ?STRING, Config),
    {ok, _} = capi_client_webhooks:get_webhooks(?config(context, Config)).

-spec get_webhooks_for_party(config()) -> _.
get_webhooks_for_party(Config) ->
    PartyID = ?STRING,
    _ = capi_ct_helper:mock_services([{webhook_manager, fun('GetList', _) -> {ok, [?WEBHOOK]} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_party_op_ctx(<<"GetWebhooksForParty">>, ?STRING, Config),
    {ok, _} = capi_client_webhooks:get_webhooks_for_party(?config(context, Config), PartyID).

-spec get_webhook_by_id(config()) -> _.
get_webhook_by_id(Config) ->
    _ = capi_ct_helper:mock_services([{webhook_manager, fun('Get', _) -> {ok, ?WEBHOOK} end}], Config),
    _ = capi_ct_helper_bouncer:mock_assert_webhook_op_ctx(
        <<"GetWebhookByID">>,
        ?INTEGER_BINARY,
        ?STRING,
        Config
    ),
    {ok, _} = capi_client_webhooks:get_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec delete_webhook_by_id(config()) -> _.
delete_webhook_by_id(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {webhook_manager, fun
                ('Get', _) -> {ok, ?WEBHOOK};
                ('Delete', _) -> {ok, ok}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_webhook_op_ctx(
        <<"DeleteWebhookByID">>,
        ?INTEGER_BINARY,
        ?STRING,
        Config
    ),
    ok = capi_client_webhooks:delete_webhook_by_id(?config(context, Config), ?INTEGER_BINARY).

-spec get_categories_ok_test(config()) -> _.
get_categories_ok_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCategories">>, Config),
    {ok, _} = capi_client_categories:get_categories(?config(context, Config)).

-spec get_category_by_ref_ok_test(config()) -> _.
get_category_by_ref_ok_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCategoryByRef">>, Config),
    {ok, _} = capi_client_categories:get_category_by_ref(?config(context, Config), ?INTEGER).

-spec check_no_payment_by_external_id_test(config()) -> _.
check_no_payment_by_external_id_test(Config) ->
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => ?STRING}}),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE}
            end},
            {bender, fun('GetInternalID', _) ->
                InternalKey = capi_utils:get_unique_id(),
                {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetPaymentByExternalID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),

    {error,
        {404, #{
            <<"message">> := <<"Payment not found">>
        }}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec check_no_invoice_by_external_id_test(config()) -> _.
check_no_invoice_by_external_id_test(Config) ->
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{}),
    _ = capi_ct_helper:mock_services(
        [
            {bender, fun('GetInternalID', _) ->
                InternalKey = capi_utils:get_unique_id(),
                {ok, capi_ct_helper_bender:get_internal_id_result(InternalKey, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetPaymentByExternalID">>,
        ?STRING,
        ?STRING,
        ?STRING,
        ?STRING,
        Config
    ),

    {error, {invalid_response_code, 500}} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec retrieve_payment_by_external_id_test(config()) -> _.
retrieve_payment_by_external_id_test(Config) ->
    PaymentID = capi_utils:get_unique_id(),
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => ?STRING}}),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                {ok, ?PAYPROC_INVOICE([?PAYPROC_PAYMENT(?PAYMENT_W_EXTERNAL_ID(PaymentID, ExternalID))])}
            end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(PaymentID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetPaymentByExternalID">>,
        ?STRING,
        PaymentID,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, #{
        <<"externalID">> := ExternalID
    }} =
        capi_client_payments:get_payment_by_external_id(?config(context, Config), ExternalID).

-spec retrieve_payment_by_external_id_for_party_test(config()) -> _.
retrieve_payment_by_external_id_for_party_test(Config) ->
    PartyID = capi_utils:get_unique_id(),
    PaymentID = capi_utils:get_unique_id(),
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{<<"context_data">> => #{<<"invoice_id">> => ?STRING}}),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                Payment = ?PAYPROC_PAYMENT(?PAYMENT_W_EXTERNAL_ID(PaymentID, ExternalID)),
                {ok, ?PAYPROC_INVOICE(?INVOICE(?STRING, undefined, PartyID), [Payment])}
            end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(PaymentID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetPaymentByExternalIDForParty">>,
        ?STRING,
        PaymentID,
        PartyID,
        ?STRING,
        Config
    ),
    {ok, #{<<"externalID">> := ExternalID}} =
        capi_client_payments:get_payment_by_external_id_for_party(?config(context, Config), PartyID, ExternalID).

-spec retrieve_refund_by_external_id_test(config()) -> _.
retrieve_refund_by_external_id_test(Config) ->
    RefundID = capi_utils:get_unique_id(),
    PaymentID = capi_utils:get_unique_id(),
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{
        <<"context_data">> => #{<<"payment_id">> => PaymentID, <<"invoice_id">> => ?STRING}
    }),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT(?PAYMENT_W_EXTERNAL_ID(PaymentID, ?STRING)),
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(RefundID, ExternalID)]},
                {ok, ?PAYPROC_INVOICE([Payment])}
            end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(RefundID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetRefundByExternalID">>,
        ?STRING,
        PaymentID,
        ?STRING,
        ?STRING,
        Config
    ),
    {ok, #{<<"externalID">> := ExternalID}} =
        capi_client_payments:get_refund_by_external_id(?config(context, Config), ExternalID).

-spec retrieve_refund_by_external_id_for_party_test(config()) -> _.
retrieve_refund_by_external_id_for_party_test(Config) ->
    PartyID = capi_utils:get_unique_id(),
    RefundID = capi_utils:get_unique_id(),
    PaymentID = capi_utils:get_unique_id(),
    ExternalID = capi_utils:get_unique_id(),
    BenderContext = capi_msgp_marshalling:marshal(#{
        <<"context_data">> => #{<<"payment_id">> => PaymentID, <<"invoice_id">> => ?STRING}
    }),
    _ = capi_ct_helper:mock_services(
        [
            {invoicing, fun('Get', _) ->
                P = ?PAYPROC_PAYMENT(?PAYMENT_W_EXTERNAL_ID(PaymentID, ?STRING)),
                Payment = P#payproc_InvoicePayment{refunds = [?PAYPROC_REFUND(RefundID, ExternalID)]},
                {ok, ?PAYPROC_INVOICE(?INVOICE(?STRING, undefined, PartyID), [Payment])}
            end},
            {bender, fun('GetInternalID', _) ->
                {ok, capi_ct_helper_bender:get_internal_id_result(RefundID, BenderContext)}
            end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_payment_op_ctx(
        <<"GetRefundByExternalIDForParty">>,
        ?STRING,
        PaymentID,
        PartyID,
        ?STRING,
        Config
    ),
    {ok, #{<<"externalID">> := ExternalID}} =
        capi_client_payments:get_refund_by_external_id_for_party(?config(context, Config), PartyID, ExternalID).

-spec get_payment_institutions(config()) -> _.
get_payment_institutions(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutions">>, Config),
    {ok, [_Something]} = capi_client_payment_institutions:get_payment_institutions(?config(context, Config)),
    {ok, []} =
        capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"live">>),
    {ok, [#{<<"realm">> := <<"test">>}]} =
        capi_client_payment_institutions:get_payment_institutions(?config(context, Config), <<"RUS">>, <<"test">>).

-spec get_payment_institution_by_ref(config()) -> _.
get_payment_institution_by_ref(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutionByRef">>, Config),
    {ok, _} = capi_client_payment_institutions:get_payment_institution_by_ref(?config(context, Config), ?INTEGER).

-spec get_payment_institution_payment_terms(config()) -> _.
get_payment_institution_payment_terms(Config) ->
    _ = capi_ct_helper:mock_services(
        [
            {party_management, fun('ComputePaymentInstitutionTerms', _) -> {ok, ?TERM_SET} end}
        ],
        Config
    ),
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetPaymentInstitutionPaymentTerms">>, Config),
    {ok, _} =
        capi_client_payment_institutions:get_payment_institution_payment_terms(?config(context, Config), ?INTEGER).

-spec get_service_provider_by_id(config()) -> _.
get_service_provider_by_id(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetServiceProviderByID">>, Config),
    ?assertEqual(
        {ok, #{
            <<"id">> => <<"qiwi">>,
            <<"brandName">> => <<"QIWI">>,
            <<"category">> => <<"wallets">>,
            <<"metadata">> => #{
                <<"test.ns">> => #{
                    <<"answer">> => 42,
                    <<"localization">> => #{
                        <<"ru_RU">> => [<<"КИВИ Кошелёк">>]
                    }
                }
            }
        }},
        capi_client_payment_institutions:get_service_provider_by_id(?config(context, Config), <<"qiwi">>)
    ).

-spec get_country_by_id_test(config()) -> _.
get_country_by_id_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCountryByID">>, Config),
    ?assertEqual(
        {ok, #{
            <<"id">> => <<"DEU">>,
            <<"name">> => <<"Germany">>,
            <<"tradeBlocs">> => [<<"EEA">>]
        }},
        capi_client_countries:get_country_by_id(?config(context, Config), <<"DEU">>)
    ).

-spec get_country_by_id_not_found_test(config()) -> _.
get_country_by_id_not_found_test(Config) ->
    _NonExistingCountryCode = xxx,
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCountryByID">>, Config),
    ?assertEqual(
        {error, {404, #{<<"message">> => <<"Country not found">>}}},
        capi_client_countries:get_country_by_id(?config(context, Config), <<"XXX">>)
    ).

-spec get_countries_test(config()) -> _.

get_countries_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetCountries">>, Config),
    ?assertEqual(
        {ok, [
            #{
                <<"id">> => <<"DEU">>,
                <<"name">> => <<"Germany">>,
                <<"tradeBlocs">> => [<<"EEA">>]
            },
            #{
                <<"id">> => <<"RUS">>,
                <<"name">> => <<"Russia">>
            }
        ]},
        capi_client_countries:get_countries(?config(context, Config))
    ).

-spec get_trade_bloc_by_id_test(config()) -> _.
get_trade_bloc_by_id_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetTradeBlocByID">>, Config),
    ?assertEqual(
        {ok, #{
            <<"id">> => <<"EEA">>,
            <<"name">> => <<"European Economic Area">>,
            <<"description">> => <<"Extension of EU">>
        }},
        capi_client_trade_blocs:get_trade_bloc_by_id(?config(context, Config), <<"EEA">>)
    ).

-spec get_trade_bloc_by_id_not_found_test(config()) -> _.
get_trade_bloc_by_id_not_found_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetTradeBlocByID">>, Config),
    ?assertEqual(
        {error, {404, #{<<"message">> => <<"Trade Bloc not found">>}}},
        capi_client_trade_blocs:get_trade_bloc_by_id(?config(context, Config), <<"XXX">>)
    ).

-spec get_trade_blocs_test(config()) -> _.
get_trade_blocs_test(Config) ->
    _ = capi_ct_helper_bouncer:mock_assert_op_ctx(<<"GetTradeBlocs">>, Config),
    ?assertEqual(
        {ok, [
            #{
                <<"id">> => <<"EEA">>,
                <<"name">> => <<"European Economic Area">>,
                <<"description">> => <<"Extension of EU">>
            }
        ]},
        capi_client_trade_blocs:get_trade_blocs(?config(context, Config))
    ).

-spec different_ip_header(config()) -> _.
different_ip_header(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('GetShop', _) -> {ok, ?SHOP} end}], Config),
    IPAddress = <<"192.168.4.2">>,
    _ = capi_ct_helper_bouncer:mock_assert_requester_ctx(
        IPAddress,
        Config
    ),
    Context0 = ?config(context, Config),
    Context1 = Context0#{ip_address => IPAddress},
    {ok, _} = capi_client_shops:get_shop_by_id(Context1, ?STRING).
