-module(capi_cash_limits).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-export([get_shop_limits/3]).

-type processing_context() :: capi_handler:processing_context().

-spec get_shop_limits(binary(), binary(), processing_context()) -> {ok, [map()]} | {error, not_found}.
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
            ShopTerms = get_shop_terms(Shop#domain_ShopConfig.terms, Revision, Context),
            TerminalRefs = get_payment_terminal_refs(Shop#domain_ShopConfig.payment_institution, Context),
            ShopMethods = extract_payment_methods(ShopTerms),
            TerminalMethods = extract_terminal_payment_methods(TerminalRefs, Context),
            Methods = intersect_methods(ShopMethods, TerminalMethods),
            Limits = lists:foldl(
                fun(Method, Acc) ->
                    ShopPayment = extract_payment_limit(ShopTerms, Currency, Method),
                    ShopPartialRefund = extract_partial_refund_limit(ShopTerms, Currency, Method),
                    {TermPayment, TermPartialRefund} = aggregate_terminal_limits(
                        TerminalRefs,
                        Currency,
                        Method,
                        Context
                    ),
                    Payment = intersect_optional(ShopPayment, TermPayment),
                    PartialRefund = intersect_optional(ShopPartialRefund, TermPartialRefund),
                    EffectiveLimit = pick_refund_limit(Payment, PartialRefund),
                    Acc ++ encode_limits(Currency, Method, EffectiveLimit)
                end,
                [],
                Methods
            ),
            {ok, Limits}
    end.

get_shop_terms(TermsRef, Revision, Context) ->
    case capi_domain:get_ext({term_set_hierarchy, TermsRef}, Revision, Context) of
        {ok, #domain_TermSetHierarchy{term_set = TermSet}} ->
            TermSet;
        _ ->
            undefined
    end.

aggregate_terminal_limits(TerminalRefs, Currency, Method, Context) ->
    {PaymentAcc, PartialRefundAcc} = lists:foldl(
        fun(TerminalRef, {PaymentAcc, PartialRefundAcc}) ->
            {Payment, PartialRefund} = get_terminal_limits(TerminalRef, Currency, Method, Context),
            log_terminal_terms(TerminalRef, Payment, PartialRefund),
            {union_optional(PaymentAcc, Payment), union_optional(PartialRefundAcc, PartialRefund)}
        end,
        {init, init},
        TerminalRefs
    ),
    {normalize_union(PaymentAcc), normalize_union(PartialRefundAcc)}.

get_terminal_limits(TerminalRef, Currency, Method, Context) ->
    case capi_domain:get({terminal, TerminalRef}, Context) of
        {ok, #domain_TerminalObject{data = #domain_Terminal{provider_ref = ProviderRef, terms = TerminalTerms}}} ->
            ProviderTerms = get_provider_terms(ProviderRef, Context),
            Payment = intersect_optional(
                extract_payment_limit(ProviderTerms, Currency, Method),
                extract_payment_limit(TerminalTerms, Currency, Method)
            ),
            PartialRefund = intersect_optional(
                extract_partial_refund_limit(ProviderTerms, Currency, Method),
                extract_partial_refund_limit(TerminalTerms, Currency, Method)
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

extract_payment_limit(undefined, _Currency, _Method) ->
    undefined;
extract_payment_limit(#domain_TermSet{payments = undefined}, _Currency, _Method) ->
    undefined;
extract_payment_limit(#domain_TermSet{payments = Payments}, Currency, Method) ->
    Selector = Payments#domain_PaymentsServiceTerms.cash_limit,
    range_from_selector(Selector, Currency, Method);
extract_payment_limit(#domain_ProvisionTermSet{payments = undefined}, _Currency, _Method) ->
    undefined;
extract_payment_limit(#domain_ProvisionTermSet{payments = Payments}, Currency, Method) ->
    Selector = Payments#domain_PaymentsProvisionTerms.cash_limit,
    range_from_selector(Selector, Currency, Method).

extract_partial_refund_limit(undefined, _Currency, _Method) ->
    undefined;
extract_partial_refund_limit(#domain_TermSet{payments = undefined}, _Currency, _Method) ->
    undefined;
extract_partial_refund_limit(#domain_TermSet{payments = Payments}, Currency, Method) ->
    Refunds = Payments#domain_PaymentsServiceTerms.refunds,
    partial_refund_limit_from_refunds(Refunds, Currency, Method);
extract_partial_refund_limit(#domain_ProvisionTermSet{payments = undefined}, _Currency, _Method) ->
    undefined;
extract_partial_refund_limit(#domain_ProvisionTermSet{payments = Payments}, Currency, Method) ->
    Refunds = Payments#domain_PaymentsProvisionTerms.refunds,
    partial_refund_limit_from_refunds(Refunds, Currency, Method).

partial_refund_limit_from_refunds(undefined, _Currency, _Method) ->
    undefined;
partial_refund_limit_from_refunds(#domain_PaymentRefundsServiceTerms{partial_refunds = undefined}, _Currency, _Method) ->
    undefined;
partial_refund_limit_from_refunds(
    #domain_PaymentRefundsServiceTerms{partial_refunds = PartialRefunds}, Currency, Method
) ->
    Selector = PartialRefunds#domain_PartialRefundsServiceTerms.cash_limit,
    range_from_selector(Selector, Currency, Method);
partial_refund_limit_from_refunds(
    #domain_PaymentRefundsProvisionTerms{partial_refunds = undefined}, _Currency, _Method
) ->
    undefined;
partial_refund_limit_from_refunds(
    #domain_PaymentRefundsProvisionTerms{partial_refunds = PartialRefunds}, Currency, Method
) ->
    Selector = PartialRefunds#domain_PartialRefundsProvisionTerms.cash_limit,
    range_from_selector(Selector, Currency, Method).

range_from_selector(undefined, _Currency, _Method) ->
    undefined;
range_from_selector(Selector, Currency, Method) ->
    Ranges = ranges_from_selector(Selector, Currency, Method),
    intersect_all(Ranges).

ranges_from_selector({value, #domain_CashRange{} = Range}, Currency, _Method) ->
    normalize_range(Range, Currency);
ranges_from_selector({decisions, Decisions}, Currency, Method) when is_list(Decisions) ->
    lists:flatmap(
        fun(#domain_CashLimitDecision{if_ = Predicate, then_ = Then}) ->
            case predicate_matches_method(Predicate, Method) of
                true ->
                    ranges_from_selector(Then, Currency, Method);
                false ->
                    []
            end
        end,
        Decisions
    );
ranges_from_selector(_, _Currency, _Method) ->
    [].

normalize_range(#domain_CashRange{lower = Lower, upper = Upper}, #domain_CurrencyRef{symbolic_code = CurrencyCode}) ->
    case {extract_bound(Lower), extract_bound(Upper)} of
        {{ok, {LowerType, LowerAmount, CurrencyCode}}, {ok, {UpperType, UpperAmount, CurrencyCode}}} ->
            [
                #{
                    currency => CurrencyCode,
                    lower => {LowerType, LowerAmount},
                    upper => {UpperType, UpperAmount}
                }
            ];
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

union_optional(undefined, _Range) ->
    undefined;
union_optional(_Range, undefined) ->
    undefined;
union_optional(init, Range) ->
    Range;
union_optional(Range, init) ->
    Range;
union_optional(#{currency := Currency} = R1, #{currency := Currency} = R2) ->
    union_ranges(R1, R2);
union_optional(_R1, _R2) ->
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

union_ranges(#{lower := Lower1, upper := Upper1} = R1, #{lower := Lower2, upper := Upper2}) ->
    Lower = min_lower(Lower1, Lower2),
    Upper = max_upper(Upper1, Upper2),
    R1#{lower => Lower, upper => Upper}.

max_lower({Type1, Amount1}, {_Type2, Amount2}) when Amount1 > Amount2 ->
    {Type1, Amount1};
max_lower({_Type1, Amount1}, {Type2, Amount2}) when Amount2 > Amount1 ->
    {Type2, Amount2};
max_lower({Type1, Amount}, {Type2, Amount}) ->
    case Type1 =:= exclusive orelse Type2 =:= exclusive of
        true -> {exclusive, Amount};
        false -> {inclusive, Amount}
    end.

min_lower({Type1, Amount1}, {_Type2, Amount2}) when Amount1 < Amount2 ->
    {Type1, Amount1};
min_lower({_Type1, Amount1}, {Type2, Amount2}) when Amount2 < Amount1 ->
    {Type2, Amount2};
min_lower({Type1, Amount}, {Type2, Amount}) ->
    case Type1 =:= inclusive orelse Type2 =:= inclusive of
        true -> {inclusive, Amount};
        false -> {exclusive, Amount}
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

max_upper({Type1, Amount1}, {_Type2, Amount2}) when Amount1 > Amount2 ->
    {Type1, Amount1};
max_upper({_Type1, Amount1}, {Type2, Amount2}) when Amount2 > Amount1 ->
    {Type2, Amount2};
max_upper({Type1, Amount}, {Type2, Amount}) ->
    case Type1 =:= inclusive orelse Type2 =:= inclusive of
        true -> {inclusive, Amount};
        false -> {exclusive, Amount}
    end.

valid_range({_, LowerAmount}, {_, UpperAmount}) when LowerAmount < UpperAmount ->
    true;
valid_range({inclusive, Amount}, {inclusive, Amount}) ->
    true;
valid_range(_, _) ->
    false.

normalize_union(init) ->
    undefined;
normalize_union(Value) ->
    Value.

pick_refund_limit(undefined, undefined) ->
    undefined;
pick_refund_limit(Payment, undefined) ->
    Payment;
pick_refund_limit(undefined, PartialRefund) ->
    PartialRefund;
pick_refund_limit(Payment, PartialRefund) ->
    intersect_optional(Payment, PartialRefund).

encode_limits(Currency, Method, Range) ->
    CurrencyCode = capi_handler_decoder_utils:decode_currency(Currency),
    case encode_range(CurrencyCode, Range) of
        undefined ->
            [];
        Encoded ->
            [Encoded#{<<"paymentMethod">> => encode_payment_method(Method)}]
    end.

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

extract_payment_methods(undefined) ->
    undefined;
extract_payment_methods(#domain_TermSet{payments = undefined}) ->
    undefined;
extract_payment_methods(#domain_TermSet{payments = Payments}) ->
    payment_methods_from_selector(Payments#domain_PaymentsServiceTerms.payment_methods);
extract_payment_methods(#domain_ProvisionTermSet{payments = undefined}) ->
    undefined;
extract_payment_methods(#domain_ProvisionTermSet{payments = Payments}) ->
    payment_methods_from_selector(Payments#domain_PaymentsProvisionTerms.payment_methods).

payment_methods_from_selector(undefined) ->
    undefined;
payment_methods_from_selector({value, PaymentMethodRefs}) ->
    lists:usort([payment_method_kind(Ref) || Ref <- PaymentMethodRefs]);
payment_methods_from_selector(_) ->
    [].

payment_method_kind(#domain_PaymentMethodRef{id = {bank_card, _}}) ->
    bank_card;
payment_method_kind(#domain_PaymentMethodRef{id = {payment_terminal, _}}) ->
    payment_terminal;
payment_method_kind(#domain_PaymentMethodRef{id = {digital_wallet, _}}) ->
    digital_wallet;
payment_method_kind(#domain_PaymentMethodRef{id = {crypto_currency, _}}) ->
    crypto_currency;
payment_method_kind(#domain_PaymentMethodRef{id = {mobile, _}}) ->
    mobile.

extract_terminal_payment_methods(TerminalRefs, Context) ->
    Methods = lists:foldl(
        fun(TerminalRef, Acc) ->
            case capi_domain:get({terminal, TerminalRef}, Context) of
                {ok, #domain_TerminalObject{data = #domain_Terminal{provider_ref = ProviderRef, terms = TerminalTerms}}} ->
                    ProviderTerms = get_provider_terms(ProviderRef, Context),
                    ProviderMethods = extract_payment_methods(ProviderTerms),
                    TerminalMethods = extract_payment_methods(TerminalTerms),
                    TerminalAllowed = intersect_methods(ProviderMethods, TerminalMethods),
                    lists:usort(Acc ++ TerminalAllowed);
                _ ->
                    Acc
            end
        end,
        [],
        TerminalRefs
    ),
    Methods.

intersect_methods(undefined, undefined) ->
    [];
intersect_methods(undefined, Methods) ->
    Methods;
intersect_methods(Methods, undefined) ->
    Methods;
intersect_methods(Methods1, Methods2) ->
    lists:usort([M || M <- Methods1, lists:member(M, Methods2)]).

predicate_matches_method({constant, true}, _Method) ->
    true;
predicate_matches_method({constant, false}, _Method) ->
    false;
predicate_matches_method({condition, {payment_tool, {bank_card, _}}}, bank_card) ->
    true;
predicate_matches_method({condition, {payment_terminal, _}}, payment_terminal) ->
    true;
predicate_matches_method({condition, {digital_wallet, _}}, digital_wallet) ->
    true;
predicate_matches_method({condition, {crypto_currency, _}}, crypto_currency) ->
    true;
predicate_matches_method({condition, {mobile_commerce, _}}, mobile) ->
    true;
predicate_matches_method(_Predicate, _Method) ->
    %% TODO handle complex predicates (all_of/any_of/is_not)
    false.

encode_payment_method(bank_card) ->
    #{<<"method">> => <<"BankCard">>};
encode_payment_method(payment_terminal) ->
    #{<<"method">> => <<"PaymentTerminal">>};
encode_payment_method(digital_wallet) ->
    #{<<"method">> => <<"DigitalWallet">>};
encode_payment_method(crypto_currency) ->
    #{<<"method">> => <<"CryptoWallet">>};
encode_payment_method(mobile) ->
    #{<<"method">> => <<"MobileCommerce">>}.
