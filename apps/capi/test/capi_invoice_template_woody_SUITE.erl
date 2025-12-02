-module(capi_invoice_template_woody_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("capi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
%% -include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_api_ext_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([create_invoice_template_ok_test/1]).
-export([update_invoice_template_ok_test/1]).
-export([get_invoice_template_ok_test/1]).
-export([delete_invoice_template_ok_test/1]).

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
        {group, default}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        %% NOTE Sequential execution due to mocks.
        {default, [sequence], [
            create_invoice_template_ok_test,
            update_invoice_template_ok_test,
            get_invoice_template_ok_test,
            delete_invoice_template_ok_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config0) ->
    capi_ct_helper:init_suite(?MODULE, Config0).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_GroupName, Config) ->
    SupPid = capi_ct_helper:start_mocked_service_sup(?MODULE),
    Apps1 = capi_ct_helper_token_keeper:mock_user_session_token(SupPid),
    [
        {group_apps, Apps1},
        {group_test_sup, SupPid}
        | Config
    ].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = capi_utils:'maybe'(?config(group_test_sup, C), fun capi_ct_helper:stop_mocked_service_sup/1),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%% TESTS

-spec create_invoice_template_ok_test(config()) -> _.
create_invoice_template_ok_test(Config) ->
    Params = #api_ext_InvoiceTemplateCreateParams{
        external_id = ?STRING,
        party_id = #domain_PartyConfigRef{id = <<"2">>},
        shop_id = #domain_ShopConfigRef{id = <<"1">>},
        invoice_lifetime = #domain_LifetimeInterval{days = ?INTEGER, months = ?INTEGER, years = ?INTEGER},
        description = <<"Sample text">>,
        details =
            {cart, #domain_InvoiceCart{
                lines = [
                    #domain_InvoiceLine{
                        product = ?STRING,
                        quantity = ?INTEGER,
                        price = ?CASH,
                        metadata = #{?STRING => {obj, #{}}}
                    },
                    #domain_InvoiceLine{
                        product = ?STRING,
                        quantity = ?INTEGER,
                        price = ?CASH,
                        metadata = #{<<"TaxMode">> => {str, <<"18%">>}}
                    }
                ]
            }},
        context = ?CONTENT
    },
    InvoiceTemplateID = genlib:unique(),
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun(
                'Create',
                {#payproc_InvoiceTemplateCreateParams{template_id = TemplateID}}
            ) ->
                {ok, ?INVOICE_TPL(TemplateID)}
            end},
            {bender, fun('GenerateID', {_Key, _, _Ctx}) ->
                {ok, capi_ct_helper_bender:get_result(InvoiceTemplateID)}
            end}
        ],
        Config
    ),
    ?assertMatch(
        {ok, #api_ext_InvoiceTemplateAndToken{
            invoice_template = #domain_InvoiceTemplate{id = InvoiceTemplateID},
            invoice_template_access_token = #api_ext_AccessToken{payload = ?API_TOKEN}
        }},
        woody_client:call({{dmsl_api_ext_thrift, 'InvoiceTemplating'}, 'Create', {Params}}, #{
            url => "http://localhost:8022/v2/extensions/invoice_templating",
            event_handler => scoper_woody_event_handler
        })
    ).

-spec update_invoice_template_ok_test(config()) -> _.
update_invoice_template_ok_test(Config) ->
    Params = #api_ext_InvoiceTemplateUpdateParams{
        invoice_lifetime = #domain_LifetimeInterval{days = ?INTEGER, months = ?INTEGER, years = ?INTEGER},
        description = <<"Sample text">>,
        details =
            {cart, #domain_InvoiceCart{
                lines = [
                    #domain_InvoiceLine{
                        product = ?STRING,
                        quantity = ?INTEGER,
                        price = ?CASH,
                        metadata = #{?STRING => {obj, #{}}}
                    },
                    #domain_InvoiceLine{
                        product = ?STRING,
                        quantity = ?INTEGER,
                        price = ?CASH,
                        metadata = #{<<"TaxMode">> => {str, <<"18%">>}}
                    }
                ]
            }},
        context = ?CONTENT
    },
    InvoiceTemplateID = genlib:unique(),
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun(
                'Update',
                {TemplateID, #payproc_InvoiceTemplateUpdateParams{
                    invoice_lifetime = _,
                    description = _,
                    details = _,
                    context = ?CONTENT
                }}
            ) ->
                {ok, ?INVOICE_TPL(TemplateID)}
            end}
        ],
        Config
    ),
    ?assertMatch(
        {ok, #domain_InvoiceTemplate{id = InvoiceTemplateID}},
        woody_client:call({{dmsl_api_ext_thrift, 'InvoiceTemplating'}, 'Update', {InvoiceTemplateID, Params}}, #{
            url => "http://localhost:8022/v2/extensions/invoice_templating",
            event_handler => scoper_woody_event_handler
        })
    ).

-spec get_invoice_template_ok_test(config()) -> _.
get_invoice_template_ok_test(Config) ->
    InvoiceTemplateID = genlib:unique(),
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun('Get', {TemplateID}) ->
                {ok, ?INVOICE_TPL(TemplateID)}
            end}
        ],
        Config
    ),
    ?assertMatch(
        {ok, #domain_InvoiceTemplate{id = InvoiceTemplateID}},
        woody_client:call({{dmsl_api_ext_thrift, 'InvoiceTemplating'}, 'Get', {InvoiceTemplateID}}, #{
            url => "http://localhost:8022/v2/extensions/invoice_templating",
            event_handler => scoper_woody_event_handler
        })
    ).

-spec delete_invoice_template_ok_test(config()) -> _.
delete_invoice_template_ok_test(Config) ->
    InvoiceTemplateID = genlib:unique(),
    _ = capi_ct_helper:mock_services(
        [
            {invoice_templating, fun('Delete', {TemplateID}) when TemplateID =:= InvoiceTemplateID ->
                {ok, ok}
            end}
        ],
        Config
    ),
    ?assertMatch(
        {ok, ok},
        woody_client:call({{dmsl_api_ext_thrift, 'InvoiceTemplating'}, 'Delete', {InvoiceTemplateID}}, #{
            url => "http://localhost:8022/v2/extensions/invoice_templating",
            event_handler => scoper_woody_event_handler
        })
    ).
