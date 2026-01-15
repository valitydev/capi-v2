-module(capi_cash_limits).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-export([get_shop_limits/3]).

-type processing_context() :: capi_handler:processing_context().

-spec get_shop_limits(binary(), binary(), processing_context()) -> {ok, map()} | {error, not_found}.
get_shop_limits(PartyID, ShopID, Context) ->
    logger:debug(
        "Cash limits computed from raw DMT selectors without varset for party=~p shop=~p",
        [PartyID, ShopID]
    ),
    case capi_party:get_shop(PartyID, ShopID, Context) of
        {error, not_found} ->
            {error, not_found};
        {ok, Shop} ->
            Currency = Shop#domain_ShopConfig.account#domain_ShopAccount.currency,
            Revision = capi_domain:head(),
            ShopTerms = compute_shop_terms(Shop#domain_ShopConfig.terms, Revision, Context),
            ShopPayment = extract_payment_limit(ShopTerms, Currency),
            ShopPartialRefund = extract_partial_refund_limit(ShopTerms, Currency),
            TerminalRefs = get_payment_terminal_refs(Shop#domain_ShopConfig.payment_institution, Context),
            {TermPayment, TermPartialRefund} = aggregate_terminal_limits(
                TerminalRefs,
                Currency,
                Context
            ),
            Payment = intersect_optional(ShopPayment, TermPayment),
            PartialRefund = intersect_optional(ShopPartialRefund, TermPartialRefund),
            EffectiveRefund = pick_refund_limit(Payment, PartialRefund),
            {ok, encode_limits(Currency, Payment, EffectiveRefund)}
    end.

compute_shop_terms(TermsRef, Revision, Context) ->
    case capi_domain:get_ext({term_set_hierarchy, TermsRef}, Revision, Context) of
        {ok, #domain_TermSetHierarchy{term_set = TermSet}} ->
            TermSet;
        _ ->
            undefined
    end.

aggregate_terminal_limits(TerminalRefs, Currency, Context) ->
    lists:foldl(
        fun(TerminalRef, {PaymentAcc, PartialRefundAcc}) ->
            {Payment, PartialRefund} = get_terminal_limits(TerminalRef, Currency, Context),
            log_terminal_terms(TerminalRef, Payment, PartialRefund),
            {intersect_optional(PaymentAcc, Payment), intersect_optional(PartialRefundAcc, PartialRefund)}
        end,
        {undefined, undefined},
        TerminalRefs
    ).

get_terminal_limits(TerminalRef, Currency, Context) ->
    case capi_domain:get({terminal, TerminalRef}, Context) of
        {ok, #domain_TerminalObject{data = #domain_Terminal{provider_ref = ProviderRef, terms = TerminalTerms}}} ->
            ProviderTerms = get_provider_terms(ProviderRef, Context),
            Payment = intersect_optional(
                extract_payment_limit(ProviderTerms, Currency),
                extract_payment_limit(TerminalTerms, Currency)
            ),
            PartialRefund = intersect_optional(
                extract_partial_refund_limit(ProviderTerms, Currency),
                extract_partial_refund_limit(TerminalTerms, Currency)
            ),
            {Payment, PartialRefund};
        _ ->
            {undefined, undefined}
    end.

get_provider_terms(ProviderRef, Context) ->
    case capi_domain:get({provider, ProviderRef}, Context) of
        {ok, #domain_ProviderObject{data = #domain_Provider{terms = Terms}}} ->
            Terms;
        _ ->
            undefined
    end.

extract_payment_limit(undefined, _Currency) ->
    undefined;
extract_payment_limit(#domain_TermSet{payments = undefined}, _Currency) ->
    undefined;
extract_payment_limit(#domain_TermSet{payments = Payments}, Currency) ->
    Selector = Payments#domain_PaymentsServiceTerms.cash_limit,
    range_from_selector(Selector, Currency);
extract_payment_limit(#domain_ProvisionTermSet{payments = undefined}, _Currency) ->
    undefined;
extract_payment_limit(#domain_ProvisionTermSet{payments = Payments}, Currency) ->
    Selector = Payments#domain_PaymentsProvisionTerms.cash_limit,
    range_from_selector(Selector, Currency).

extract_partial_refund_limit(undefined, _Currency) ->
    undefined;
extract_partial_refund_limit(#domain_TermSet{payments = undefined}, _Currency) ->
    undefined;
extract_partial_refund_limit(#domain_TermSet{payments = Payments}, Currency) ->
    Refunds = Payments#domain_PaymentsServiceTerms.refunds,
    partial_refund_limit_from_refunds(Refunds, Currency);
extract_partial_refund_limit(#domain_ProvisionTermSet{payments = undefined}, _Currency) ->
    undefined;
extract_partial_refund_limit(#domain_ProvisionTermSet{payments = Payments}, Currency) ->
    Refunds = Payments#domain_PaymentsProvisionTerms.refunds,
    partial_refund_limit_from_refunds(Refunds, Currency).

partial_refund_limit_from_refunds(undefined, _Currency) ->
    undefined;
partial_refund_limit_from_refunds(#domain_PaymentRefundsServiceTerms{partial_refunds = undefined}, _Currency) ->
    undefined;
partial_refund_limit_from_refunds(#domain_PaymentRefundsServiceTerms{partial_refunds = PartialRefunds}, Currency) ->
    Selector = PartialRefunds#domain_PartialRefundsServiceTerms.cash_limit,
    range_from_selector(Selector, Currency);
partial_refund_limit_from_refunds(#domain_PaymentRefundsProvisionTerms{partial_refunds = undefined}, _Currency) ->
    undefined;
partial_refund_limit_from_refunds(#domain_PaymentRefundsProvisionTerms{partial_refunds = PartialRefunds}, Currency) ->
    Selector = PartialRefunds#domain_PartialRefundsProvisionTerms.cash_limit,
    range_from_selector(Selector, Currency).

range_from_selector(undefined, _Currency) ->
    undefined;
range_from_selector(Selector, Currency) ->
    Ranges = ranges_from_selector(Selector, Currency),
    intersect_all(Ranges).

ranges_from_selector({value, #domain_CashRange{} = Range}, Currency) ->
    normalize_range(Range, Currency);
ranges_from_selector({decisions, Decisions}, Currency) when is_list(Decisions) ->
    lists:flatmap(
        fun(#domain_CashLimitDecision{then_ = Then}) ->
            ranges_from_selector(Then, Currency)
        end,
        Decisions
    );
ranges_from_selector(_, _Currency) ->
    [].

normalize_range(#domain_CashRange{lower = Lower, upper = Upper}, #domain_CurrencyRef{symbolic_code = CurrencyCode}) ->
    case {extract_bound(Lower), extract_bound(Upper)} of
        {{ok, {LowerType, LowerAmount, CurrencyCode}}, {ok, {UpperType, UpperAmount, CurrencyCode}}} ->
            [#{
                currency => CurrencyCode,
                lower => {LowerType, LowerAmount},
                upper => {UpperType, UpperAmount}
            }];
        _ ->
            []
    end;
normalize_range(_Range, _Currency) ->
    [].

extract_bound({inclusive, #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Code}}}) ->
    {ok, {inclusive, Amount, Code}};
extract_bound({exclusive, #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Code}}}) ->
    {ok, {exclusive, Amount, Code}};
extract_bound(_) ->
    error.

intersect_all([]) ->
    undefined;
intersect_all([Range | Rest]) ->
    lists:foldl(fun intersect_optional/2, Range, Rest).

intersect_optional(undefined, Range) ->
    Range;
intersect_optional(Range, undefined) ->
    Range;
intersect_optional(#{currency := Currency} = R1, #{currency := Currency} = R2) ->
    intersect_ranges(R1, R2);
intersect_optional(_R1, _R2) ->
    undefined.

intersect_ranges(#{lower := Lower1, upper := Upper1} = R1, #{lower := Lower2, upper := Upper2}) ->
    Lower = max_lower(Lower1, Lower2),
    Upper = min_upper(Upper1, Upper2),
    case valid_range(Lower, Upper) of
        true ->
            R1#{lower => Lower, upper => Upper};
        false ->
            undefined
    end.

max_lower({Type1, Amount1}, {_Type2, Amount2}) when Amount1 > Amount2 ->
    {Type1, Amount1};
max_lower({_Type1, Amount1}, {Type2, Amount2}) when Amount2 > Amount1 ->
    {Type2, Amount2};
max_lower({Type1, Amount}, {Type2, Amount}) ->
    case Type1 =:= exclusive orelse Type2 =:= exclusive of
        true -> {exclusive, Amount};
        false -> {inclusive, Amount}
    end.

min_upper({Type1, Amount1}, {_Type2, Amount2}) when Amount1 < Amount2 ->
    {Type1, Amount1};
min_upper({_Type1, Amount1}, {Type2, Amount2}) when Amount2 < Amount1 ->
    {Type2, Amount2};
min_upper({Type1, Amount}, {Type2, Amount}) ->
    case Type1 =:= exclusive orelse Type2 =:= exclusive of
        true -> {exclusive, Amount};
        false -> {inclusive, Amount}
    end.

valid_range({_, LowerAmount}, {_, UpperAmount}) when LowerAmount < UpperAmount ->
    true;
valid_range({inclusive, Amount}, {inclusive, Amount}) ->
    true;
valid_range(_, _) ->
    false.

pick_refund_limit(undefined, undefined) ->
    undefined;
pick_refund_limit(Payment, undefined) ->
    Payment;
pick_refund_limit(undefined, PartialRefund) ->
    PartialRefund;
pick_refund_limit(Payment, PartialRefund) ->
    intersect_optional(Payment, PartialRefund).

encode_limits(Currency, Payment, PartialRefund) ->
    CurrencyCode = capi_handler_decoder_utils:decode_currency(Currency),
    genlib_map:compact(#{
        <<"payment">> => encode_range(CurrencyCode, Payment),
        <<"partialRefund">> => encode_range(CurrencyCode, PartialRefund)
    }).

encode_range(_CurrencyCode, undefined) ->
    undefined;
encode_range(CurrencyCode, #{lower := Lower, upper := Upper}) ->
    #{
        <<"currency">> => CurrencyCode,
        <<"lowerBound">> => encode_bound(Lower),
        <<"upperBound">> => encode_bound(Upper)
    }.

encode_bound({Type, Amount}) ->
    #{
        <<"amount">> => Amount,
        <<"inclusive">> => Type =:= inclusive
    }.

get_payment_terminal_refs(PiRef, Context) ->
    case capi_domain:get_payment_institution(PiRef, Context) of
        {ok, #domain_PaymentInstitution{payment_routing_rules = Rules}} ->
            lists:usort(collect_ruleset_terminals(Rules, Context));
        _ ->
            []
    end.

collect_ruleset_terminals(undefined, _Context) ->
    [];
collect_ruleset_terminals(#domain_RoutingRules{policies = PoliciesRef}, Context) ->
    collect_ruleset_terminals(PoliciesRef, Context, sets:new());
collect_ruleset_terminals(_, _Context) ->
    [].

collect_ruleset_terminals(#domain_RoutingRulesetRef{} = Ref, Context, Seen) ->
    case sets:is_element(Ref, Seen) of
        true ->
            [];
        false ->
            Seen1 = sets:add_element(Ref, Seen),
            case capi_domain:get({routing_rules, Ref}, Context) of
                {ok, #domain_RoutingRulesObject{data = Ruleset}} ->
                    collect_ruleset_terminals(Ruleset, Context, Seen1);
                _ ->
                    []
            end
    end;
collect_ruleset_terminals(#domain_RoutingRuleset{decisions = Decisions}, Context, Seen) ->
    collect_terminals_from_decisions(Decisions, Context, Seen);
collect_ruleset_terminals(_, _Context, _Seen) ->
    [].

collect_terminals_from_decisions({candidates, Candidates}, _Context, _Seen) ->
    [C#domain_RoutingCandidate.terminal || C <- Candidates];
collect_terminals_from_decisions({delegates, Delegates}, Context, Seen) ->
    lists:flatmap(
        fun(#domain_RoutingDelegate{ruleset = Ref}) ->
            collect_ruleset_terminals(Ref, Context, Seen)
        end,
        Delegates
    );
collect_terminals_from_decisions(_, _Context, _Seen) ->
    [].

log_terminal_terms(TerminalRef, Payment, PartialRefund) ->
    logger:debug(
        "Cash limits for terminal ~p: payment=~p partial_refund=~p",
        [TerminalRef, Payment, PartialRefund]
    ).
