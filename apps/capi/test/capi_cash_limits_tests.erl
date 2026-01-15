-module(capi_cash_limits_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([test/0]).

-spec test() -> term().
test() ->
    eunit:test(?MODULE).

-spec shop_only_limits_test_() -> term().
shop_only_limits_test_() ->
    {setup, fun setup_mocks/0, fun teardown_mocks/1, fun shop_only_limits_test/0}.

-spec terminal_intersection_test_() -> term().
terminal_intersection_test_() ->
    {setup, fun setup_mocks/0, fun teardown_mocks/1, fun terminal_intersection_test/0}.

-spec real_config_limits_test_() -> term().
real_config_limits_test_() ->
    {setup, fun setup_mocks/0, fun teardown_mocks/1, fun real_config_limits_test/0}.

-spec setup_mocks() -> ok.
setup_mocks() ->
    ok = meck:new(capi_party, [non_strict]),
    ok = meck:new(capi_domain, [non_strict]),
    ok.

-spec teardown_mocks(term()) -> ok.
teardown_mocks(_) ->
    meck:unload(capi_party),
    meck:unload(capi_domain).

-spec shop_only_limits_test() -> ok.
shop_only_limits_test() ->
     Currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
     ShopTerms = #domain_TermSet{
         payments = #domain_PaymentsServiceTerms{
             cash_limit = {value, mk_cash_range(100, 1000, Currency)},
             refunds = #domain_PaymentRefundsServiceTerms{
                 partial_refunds = #domain_PartialRefundsServiceTerms{
                     cash_limit = {value, mk_cash_range(200, 900, Currency)}
                 }
             }
         }
     },
     Shop = mk_shop(Currency, #domain_TermSetHierarchyRef{id = 10}, #domain_PaymentInstitutionRef{id = 77}),
     meck:expect(capi_party, get_shop, fun(_PartyID, _ShopID, _Context) -> {ok, Shop} end),
     meck:expect(capi_domain, head, fun() -> 1 end),
     meck:expect(
         capi_domain,
         get_ext,
         fun({term_set_hierarchy, #domain_TermSetHierarchyRef{id = 10}}, 1, _Context) ->
             {ok, #domain_TermSetHierarchy{term_set = ShopTerms}};
            (_, _, _) ->
             {error, not_found}
         end
     ),
     meck:expect(
         capi_domain,
         get_payment_institution,
         fun(#domain_PaymentInstitutionRef{id = 77}, _Context) ->
             {ok, #domain_PaymentInstitution{payment_routing_rules = undefined}}
         end
     ),
     {ok, Result} = capi_cash_limits:get_shop_limits(<<"party">>, <<"shop">>, #{}),
     Payment = maps:get(<<"payment">>, Result),
     PartialRefund = maps:get(<<"partialRefund">>, Result),
     ?assertEqual(<<"RUB">>, maps:get(<<"currency">>, Payment)),
     ?assertEqual(#{<<"amount">> => 100, <<"inclusive">> => true}, maps:get(<<"lowerBound">>, Payment)),
     ?assertEqual(#{<<"amount">> => 1000, <<"inclusive">> => true}, maps:get(<<"upperBound">>, Payment)),
     ?assertEqual(<<"RUB">>, maps:get(<<"currency">>, PartialRefund)),
     ?assertEqual(#{<<"amount">> => 200, <<"inclusive">> => true}, maps:get(<<"lowerBound">>, PartialRefund)),
     ?assertEqual(#{<<"amount">> => 900, <<"inclusive">> => true}, maps:get(<<"upperBound">>, PartialRefund)).
 
-spec terminal_intersection_test() -> ok.
terminal_intersection_test() ->
     Currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
     ShopTerms = #domain_TermSet{
         payments = #domain_PaymentsServiceTerms{
             cash_limit = {value, mk_cash_range(100, 1000, Currency)}
         }
     },
     Shop = mk_shop(Currency, #domain_TermSetHierarchyRef{id = 20}, #domain_PaymentInstitutionRef{id = 88}),
     ProviderRef1 = #domain_ProviderRef{id = 1},
     ProviderRef2 = #domain_ProviderRef{id = 2},
     TerminalRef1 = #domain_TerminalRef{id = 11},
     TerminalRef2 = #domain_TerminalRef{id = 22},
     ProviderTerms1 = mk_provision_terms(50, 500, Currency),
     ProviderTerms2 = mk_provision_terms(100, 700, Currency),
     TerminalTerms1 = mk_provision_terms(200, 800, Currency),
     TerminalTerms2 = mk_provision_terms(300, 900, Currency),
     RulesetRef = #domain_RoutingRulesetRef{id = 999},
     Ruleset = #domain_RoutingRuleset{
         decisions = {candidates, [
             #domain_RoutingCandidate{allowed = {constant, true}, terminal = TerminalRef1},
             #domain_RoutingCandidate{allowed = {constant, true}, terminal = TerminalRef2}
         ]}
     },
     meck:expect(capi_party, get_shop, fun(_PartyID, _ShopID, _Context) -> {ok, Shop} end),
     meck:expect(capi_domain, head, fun() -> 1 end),
     meck:expect(
         capi_domain,
         get_ext,
         fun({term_set_hierarchy, #domain_TermSetHierarchyRef{id = 20}}, 1, _Context) ->
             {ok, #domain_TermSetHierarchy{term_set = ShopTerms}};
            (_, _, _) ->
             {error, not_found}
         end
     ),
     meck:expect(
         capi_domain,
         get_payment_institution,
         fun(#domain_PaymentInstitutionRef{id = 88}, _Context) ->
             {ok, #domain_PaymentInstitution{
                 payment_routing_rules = #domain_RoutingRules{policies = RulesetRef}
             }}
         end
     ),
     meck:expect(
         capi_domain,
         get,
         fun
             ({routing_rules, #domain_RoutingRulesetRef{id = 999}}, _Context) ->
                 {ok, #domain_RoutingRulesObject{data = Ruleset}};
             ({terminal, #domain_TerminalRef{id = 11}}, _Context) ->
                 {ok, #domain_TerminalObject{data = #domain_Terminal{provider_ref = ProviderRef1, terms = TerminalTerms1}}};
             ({terminal, #domain_TerminalRef{id = 22}}, _Context) ->
                 {ok, #domain_TerminalObject{data = #domain_Terminal{provider_ref = ProviderRef2, terms = TerminalTerms2}}};
             ({provider, #domain_ProviderRef{id = 1}}, _Context) ->
                 {ok, #domain_ProviderObject{data = #domain_Provider{terms = ProviderTerms1}}};
             ({provider, #domain_ProviderRef{id = 2}}, _Context) ->
                 {ok, #domain_ProviderObject{data = #domain_Provider{terms = ProviderTerms2}}};
             (_, _Context) ->
                 {error, not_found}
         end
     ),
     {ok, Result} = capi_cash_limits:get_shop_limits(<<"party">>, <<"shop">>, #{}),
    Payment = maps:get(<<"payment">>, Result),
    PartialRefund = maps:get(<<"partialRefund">>, Result),
    ?assertEqual(Payment, PartialRefund),
    ?assertEqual(<<"RUB">>, maps:get(<<"currency">>, Payment)),
    ?assertEqual(#{<<"amount">> => 200, <<"inclusive">> => true}, maps:get(<<"lowerBound">>, Payment)),
    ?assertEqual(#{<<"amount">> => 700, <<"inclusive">> => true}, maps:get(<<"upperBound">>, Payment)).

-spec real_config_limits_test() -> ok.
real_config_limits_test() ->
    Currency = #domain_CurrencyRef{symbolic_code = <<"KZT">>},
    Shop = mk_shop(Currency, #domain_TermSetHierarchyRef{id = 1000}, #domain_PaymentInstitutionRef{id = 100}),
    RulesetRef = #domain_RoutingRulesetRef{id = 1059},
    Ruleset = #domain_RoutingRuleset{
        decisions = {candidates, [
            #domain_RoutingCandidate{allowed = {constant, true}, terminal = #domain_TerminalRef{id = 15}},
            #domain_RoutingCandidate{allowed = {constant, true}, terminal = #domain_TerminalRef{id = 16}}
        ]}
    },
    ProviderTerms8 = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_limit = mk_cash_limit_decisions([{100, 1000000000, Currency}]),
            refunds = #domain_PaymentRefundsProvisionTerms{
                partial_refunds = #domain_PartialRefundsProvisionTerms{
                    cash_limit = mk_cash_limit_decisions([{100, 1000000000, Currency}])
                }
            }
        }
    },
    ProviderTerms9 = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_limit = mk_cash_limit_value(100, 1000000000, Currency),
            refunds = #domain_PaymentRefundsProvisionTerms{
                partial_refunds = #domain_PartialRefundsProvisionTerms{
                    cash_limit = mk_cash_limit_value(100, 100000000, Currency)
                }
            }
        }
    },
    TerminalTerms15 = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_limit = mk_cash_limit_value(10000, 120000000, Currency)
        }
    },
    TerminalTerms16 = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_limit = mk_cash_limit_decisions([
                {51300, 43609100, Currency},
                {51300, 128262000, Currency}
            ])
        }
    },
    meck:expect(capi_party, get_shop, fun(_PartyID, _ShopID, _Context) -> {ok, Shop} end),
    meck:expect(capi_domain, head, fun() -> 1 end),
    meck:expect(
        capi_domain,
        get_ext,
        fun({term_set_hierarchy, #domain_TermSetHierarchyRef{id = 1000}}, 1, _Context) ->
            {error, not_found};
           (_, _, _) ->
            {error, not_found}
        end
    ),
    meck:expect(
        capi_domain,
        get_payment_institution,
        fun(#domain_PaymentInstitutionRef{id = 100}, _Context) ->
            {ok, #domain_PaymentInstitution{
                payment_routing_rules = #domain_RoutingRules{policies = RulesetRef}
            }}
        end
    ),
    meck:expect(
        capi_domain,
        get,
        fun
            ({routing_rules, #domain_RoutingRulesetRef{id = 1059}}, _Context) ->
                {ok, #domain_RoutingRulesObject{data = Ruleset}};
            ({terminal, #domain_TerminalRef{id = 15}}, _Context) ->
                {ok, #domain_TerminalObject{
                    data = #domain_Terminal{provider_ref = #domain_ProviderRef{id = 8}, terms = TerminalTerms15}
                }};
            ({terminal, #domain_TerminalRef{id = 16}}, _Context) ->
                {ok, #domain_TerminalObject{
                    data = #domain_Terminal{provider_ref = #domain_ProviderRef{id = 9}, terms = TerminalTerms16}
                }};
            ({provider, #domain_ProviderRef{id = 8}}, _Context) ->
                {ok, #domain_ProviderObject{data = #domain_Provider{terms = ProviderTerms8}}};
            ({provider, #domain_ProviderRef{id = 9}}, _Context) ->
                {ok, #domain_ProviderObject{data = #domain_Provider{terms = ProviderTerms9}}};
            (_, _Context) ->
                {error, not_found}
        end
    ),
    {ok, Result} = capi_cash_limits:get_shop_limits(<<"party">>, <<"shop">>, #{}),
    io:format("real_config_limits_result=~p~n", [Result]),
    Payment = maps:get(<<"payment">>, Result),
    PartialRefund = maps:get(<<"partialRefund">>, Result),
    ?assertEqual(<<"KZT">>, maps:get(<<"currency">>, Payment)),
    ?assertEqual(#{<<"amount">> => 10000, <<"inclusive">> => true}, maps:get(<<"lowerBound">>, Payment)),
    ?assertEqual(#{<<"amount">> => 120000000, <<"inclusive">> => true}, maps:get(<<"upperBound">>, Payment)),
    ?assertEqual(Payment, PartialRefund).
 
-spec mk_shop(
    dmsl_domain_thrift:'CurrencyRef'(),
    dmsl_domain_thrift:'TermSetHierarchyRef'(),
    dmsl_domain_thrift:'PaymentInstitutionRef'()
) -> dmsl_domain_thrift:'ShopConfig'().
mk_shop(Currency, TermsRef, PiRef) ->
     #domain_ShopConfig{
         account = #domain_ShopAccount{currency = Currency},
         terms = TermsRef,
         payment_institution = PiRef
     }.
 
-spec mk_provision_terms(
    integer(),
    integer(),
    dmsl_domain_thrift:'CurrencyRef'()
) -> dmsl_domain_thrift:'ProvisionTermSet'().
mk_provision_terms(Lower, Upper, Currency) ->
     #domain_ProvisionTermSet{
         payments = #domain_PaymentsProvisionTerms{
             cash_limit = {value, mk_cash_range(Lower, Upper, Currency)}
         }
     }.
 
-spec mk_cash_range(
    integer(),
    integer(),
    dmsl_domain_thrift:'CurrencyRef'()
) -> dmsl_domain_thrift:'CashRange'().
mk_cash_range(Lower, Upper, Currency) ->
     #domain_CashRange{
         lower = {inclusive, #domain_Cash{amount = Lower, currency = Currency}},
         upper = {inclusive, #domain_Cash{amount = Upper, currency = Currency}}
     }.

-spec mk_cash_limit_value(integer(), integer(), dmsl_domain_thrift:'CurrencyRef'()) ->
    dmsl_domain_thrift:'CashLimitSelector'().
mk_cash_limit_value(Lower, Upper, Currency) ->
    {value, mk_cash_range(Lower, Upper, Currency)}.

-spec mk_cash_limit_decisions([{integer(), integer(), dmsl_domain_thrift:'CurrencyRef'()}]) ->
    dmsl_domain_thrift:'CashLimitSelector'().
mk_cash_limit_decisions(Ranges) ->
    {decisions, [
        #domain_CashLimitDecision{
            if_ = {constant, true},
            then_ = mk_cash_limit_value(Lower, Upper, Currency)
        }
     || {Lower, Upper, Currency} <- Ranges
    ]}.
