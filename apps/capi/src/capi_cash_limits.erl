-module(capi_cash_limits).
%
% ВАЖНО: расчет сделан с допущениями и НЕ покрывает ряд кейсов:
% - selectors {decisions, _} для shop/provider/terminal не обрабатываются
% - exclusive bounds приводятся к inclusive (границы теряют строгость)
% - терминалы с cash_limit=decisions полностью игнорируются (нет fallback на provider)
% - при отсутствии payment_methods ответ пустой, даже если лимит посчитан
%
% Учитываются allow и global_allow:
% - {constant, true} -> разрешено, {constant, false} -> запрещено
% - остальные предикаты (all_of, any_of и т.д.) -> по умолчанию разрешено
% - global_allow у провайдера при false запрещает все терминалы провайдера

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-export([get_shop_limits/3]).

-type processing_context() :: capi_handler:processing_context().

-spec get_shop_limits(binary(), binary(), processing_context()) -> {ok, [map()]} | {error, not_found}.
get_shop_limits(PartyID, ShopID, Context) ->
    case capi_party:get_shop(PartyID, ShopID, Context) of
        {error, not_found} ->
            {error, not_found};
        {ok, Shop} ->
            Currency = Shop#domain_ShopConfig.account#domain_ShopAccount.currency,
            Revision = capi_domain:head(),
            ShopTerms = get_shop_terms(Shop#domain_ShopConfig.terms, Revision, Context),
            ShopMethods = extract_payment_methods(ShopTerms),
            ShopLimit = extract_shop_limit(ShopTerms, Currency),
            TerminalRefs =
                get_payment_terminal_refs(
                    Shop#domain_ShopConfig.payment_institution,
                    PartyID,
                    ShopID,
                    Context
                ),
            TermLimit = aggregate_terminal_limits(TerminalRefs, Currency, Context),
            EffectiveLimit = intersect_optional(ShopLimit, TermLimit),
            Limits = lists:flatmap(
                fun(ShopMethod) ->
                    encode_limits(Currency, ShopMethod, EffectiveLimit)
                end,
                ShopMethods
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

extract_payment_methods(#domain_TermSet{
    payments = #domain_PaymentsServiceTerms{payment_methods = {value, PaymentMethodRefs}}
}) ->
    lists:usort([Type || #domain_PaymentMethodRef{id = {Type, _ID}} <- PaymentMethodRefs]);
extract_payment_methods(_) ->
    [].

extract_shop_limit(undefined, _Currency) ->
    undefined;
extract_shop_limit(#domain_TermSet{payments = undefined}, _Currency) ->
    undefined;
extract_shop_limit(#domain_TermSet{payments = Payments}, Currency) ->
    PaymentSelector = Payments#domain_PaymentsServiceTerms.cash_limit,
    PartialRefundSelector =
        case Payments#domain_PaymentsServiceTerms.refunds of
            undefined ->
                undefined;
            #domain_PaymentRefundsServiceTerms{partial_refunds = undefined} ->
                undefined;
            #domain_PaymentRefundsServiceTerms{partial_refunds = PartialRefunds} ->
                PartialRefunds#domain_PartialRefundsServiceTerms.cash_limit
        end,
    Payment = range_from_selector(PaymentSelector, Currency),
    PartialRefund = range_from_selector(PartialRefundSelector, Currency),
    pick_refund_limit(Payment, PartialRefund).

get_payment_terminal_refs(PiRef, PartyID, ShopID, Context) ->
    case capi_domain:get_payment_institution(PiRef, Context) of
        {ok, #domain_PaymentInstitution{payment_routing_rules = Rules}} ->
            case compute_routing_ruleset(Rules, PartyID, ShopID, Context) of
                {ok, #domain_RoutingRuleset{decisions = {candidates, Candidates}}} ->
                    AllowedCandidates = [
                        C#domain_RoutingCandidate.terminal
                     || C <- Candidates,
                        predicate_allowed(C#domain_RoutingCandidate.allowed)
                    ],
                    lists:usort(AllowedCandidates);
                _ ->
                    []
            end;
        _ ->
            []
    end.

predicate_allowed({constant, false}) ->
    false;
predicate_allowed({all_of, List}) when is_list(List) ->
    lists:all(fun predicate_allowed/1, List);
predicate_allowed(_) ->
    true.

compute_routing_ruleset(undefined, _PartyID, _ShopID, _Context) ->
    undefined;
compute_routing_ruleset(#domain_RoutingRules{policies = RulesetRef}, PartyID, ShopID, Context) ->
    Revision = capi_domain:head(),
    Varset = #payproc_Varset{
        party_ref = #domain_PartyConfigRef{id = PartyID},
        shop_id = ShopID
    },
    #{party_client := Client, party_client_context := PartyClientContext} = Context,
    party_client_thrift:compute_routing_ruleset(
        RulesetRef,
        Revision,
        Varset,
        Client,
        PartyClientContext
    ).

aggregate_terminal_limits([], _Currency, _Context) ->
    undefined;
aggregate_terminal_limits([TerminalRef | TerminalRefs], Currency, Context) ->
    Limit0 = get_terminal_limit(TerminalRef, Currency, Context),
    log_terminal_terms(TerminalRef, Limit0),
    lists:foldl(
        fun(TerminalRef1, LimitAcc) ->
            Limit = get_terminal_limit(TerminalRef1, Currency, Context),
            log_terminal_terms(TerminalRef1, Limit),
            union_optional(LimitAcc, Limit)
        end,
        Limit0,
        TerminalRefs
    ).

log_terminal_terms(TerminalRef, Limit) ->
    logger:debug(
        "Cash limits for terminal ~p: limit=~p",
        [TerminalRef, Limit]
    ).

get_terminal_limit(TerminalRef, Currency, Context) ->
    case capi_domain:get({terminal, TerminalRef}, Context) of
        {ok, #domain_TerminalObject{data = #domain_Terminal{provider_ref = ProviderRef, terms = Terms}}} ->
            ProviderTerms = get_provider_terms(ProviderRef, Context),
            compute_terminal_limit(Terms, ProviderTerms, Currency);
        _ ->
            undefined
    end.

compute_terminal_limit(
    #domain_ProvisionTermSet{payments = #domain_PaymentsProvisionTerms{cash_limit = {value, _}}} = Terms,
    ProviderTerms,
    Currency
) ->
    case terminal_and_provider_allowed(Terms#domain_ProvisionTermSet.payments, ProviderTerms) of
        true ->
            case extract_provider_limit(Terms, Currency) of
                undefined ->
                    extract_provider_limit(ProviderTerms, Currency);
                Limit ->
                    Limit
            end;
        false ->
            undefined
    end;
compute_terminal_limit(_, _ProviderTerms, _Currency) ->
    undefined.

-spec terminal_and_provider_allowed(term(), term()) -> boolean().
terminal_and_provider_allowed(TerminalPayments, ProviderTerms) ->
    TerminalAllowed = predicate_allowed(TerminalPayments#domain_PaymentsProvisionTerms.allow),
    TerminalGlobalAllowed = predicate_allowed(TerminalPayments#domain_PaymentsProvisionTerms.global_allow),
    case ProviderTerms of
        #domain_ProvisionTermSet{payments = #domain_PaymentsProvisionTerms{} = ProviderPayments} ->
            %% global_allow провайдера при false запрещает все терминалы
            ProviderGlobalAllowed = predicate_allowed(ProviderPayments#domain_PaymentsProvisionTerms.global_allow),
            ProviderAllowed = predicate_allowed(ProviderPayments#domain_PaymentsProvisionTerms.allow),
            TerminalAllowed andalso TerminalGlobalAllowed andalso ProviderGlobalAllowed andalso ProviderAllowed;
        _ ->
            TerminalAllowed andalso TerminalGlobalAllowed
    end.

get_provider_terms(ProviderRef, Context) ->
    case capi_domain:get({provider, ProviderRef}, Context) of
        {ok, #domain_ProviderObject{data = #domain_Provider{terms = Terms}}} ->
            Terms;
        _ ->
            undefined
    end.

extract_provider_limit(undefined, _Currency) ->
    undefined;
extract_provider_limit(#domain_ProvisionTermSet{payments = undefined}, _Currency) ->
    undefined;
extract_provider_limit(#domain_ProvisionTermSet{payments = Payments}, Currency) ->
    PaymentSelector = Payments#domain_PaymentsProvisionTerms.cash_limit,
    PartialRefundSelector =
        case Payments#domain_PaymentsProvisionTerms.refunds of
            undefined ->
                undefined;
            #domain_PaymentRefundsProvisionTerms{partial_refunds = undefined} ->
                undefined;
            #domain_PaymentRefundsProvisionTerms{partial_refunds = PartialRefunds} ->
                PartialRefunds#domain_PartialRefundsProvisionTerms.cash_limit
        end,
    Payment = range_from_selector(PaymentSelector, Currency),
    PartialRefund = range_from_selector(PartialRefundSelector, Currency),
    pick_refund_limit(Payment, PartialRefund).

range_from_selector({value, #domain_CashRange{} = Range}, Currency) ->
    normalize_range(Range, Currency);
range_from_selector(_, _Currency) ->
    undefined.

normalize_range(#domain_CashRange{lower = Lower, upper = Upper}, #domain_CurrencyRef{symbolic_code = CurrencyCode}) ->
    {LowerAmount, LowerCode} = extract_bound(Lower),
    {UpperAmount, UpperCode} = extract_bound(Upper),
    case {LowerCode, UpperCode} of
        {CurrencyCode, CurrencyCode} ->
            #{
                currency => CurrencyCode,
                lower => LowerAmount,
                upper => UpperAmount
            };
        _ ->
            undefined
    end.

extract_bound({inclusive, #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Code}}}) ->
    {Amount, Code};
extract_bound({exclusive, #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Code}}}) ->
    {Amount, Code}.

pick_refund_limit(undefined, undefined) ->
    undefined;
pick_refund_limit(Payment, undefined) ->
    Payment;
pick_refund_limit(undefined, PartialRefund) ->
    PartialRefund;
pick_refund_limit(Payment, PartialRefund) ->
    intersect_optional(Payment, PartialRefund).

intersect_optional(undefined, Range) ->
    Range;
intersect_optional(Range, undefined) ->
    Range;
intersect_optional(#{currency := Currency} = R1, #{currency := Currency} = R2) ->
    intersect_ranges(R1, R2).

intersect_ranges(#{lower := Lower1, upper := Upper1} = R1, #{lower := Lower2, upper := Upper2}) ->
    Lower = max(Lower1, Lower2),
    Upper = min(Upper1, Upper2),
    case valid_range(Lower, Upper) of
        true ->
            R1#{lower => Lower, upper => Upper};
        false ->
            undefined
    end.

union_optional(undefined, Range) ->
    Range;
union_optional(Range, undefined) ->
    Range;
union_optional(#{currency := Currency} = R1, #{currency := Currency} = R2) ->
    union_ranges(R1, R2).

union_ranges(#{lower := Lower1, upper := Upper1} = R1, #{lower := Lower2, upper := Upper2}) ->
    Lower = min(Lower1, Lower2),
    Upper = max(Upper1, Upper2),
    R1#{lower => Lower, upper => Upper}.

valid_range(LowerAmount, UpperAmount) when LowerAmount < UpperAmount ->
    true;
valid_range(_, _) ->
    false.

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

encode_bound(Amount) ->
    #{
        <<"amount">> => Amount,
        <<"inclusive">> => true
    }.

encode_payment_method(bank_card) ->
    #{
        <<"method">> => <<"BankCard">>,
        <<"paymentSystems">> => []
    };
encode_payment_method(payment_terminal) ->
    #{<<"method">> => <<"PaymentTerminal">>};
encode_payment_method(digital_wallet) ->
    #{<<"method">> => <<"DigitalWallet">>};
encode_payment_method(crypto_currency) ->
    #{<<"method">> => <<"CryptoWallet">>};
encode_payment_method(mobile) ->
    #{<<"method">> => <<"MobileCommerce">>}.

%%%
%%% EUnit tests
%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec predicate_allowed_undefined_test() -> _.
predicate_allowed_undefined_test() ->
    ?assertEqual(true, predicate_allowed(undefined)).

-spec predicate_allowed_constant_true_test() -> _.
predicate_allowed_constant_true_test() ->
    ?assertEqual(true, predicate_allowed({constant, true})).

-spec predicate_allowed_constant_false_test() -> _.
predicate_allowed_constant_false_test() ->
    ?assertEqual(false, predicate_allowed({constant, false})).

-spec predicate_allowed_other_predicates_test() -> _.
predicate_allowed_other_predicates_test() ->
    ?assertEqual(true, predicate_allowed({all_of, []})),
    ?assertEqual(true, predicate_allowed({all_of, [{constant, true}, {constant, true}]})),
    ?assertEqual(false, predicate_allowed({all_of, [{constant, true}, {constant, false}]})),
    ?assertEqual(false, predicate_allowed({all_of, [{all_of, [{constant, true}, {constant, false}]}]})),
    ?assertEqual(true, predicate_allowed({any_of, []})),
    ?assertEqual(true, predicate_allowed({condition, []})).

-spec terminal_and_provider_allowed_all_true_test() -> _.
terminal_and_provider_allowed_all_true_test() ->
    TerminalPayments = #domain_PaymentsProvisionTerms{
        allow = {constant, true},
        global_allow = {constant, true}
    },
    ProviderTerms = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            allow = {constant, true},
            global_allow = {constant, true}
        }
    },
    ?assertEqual(true, terminal_and_provider_allowed(TerminalPayments, ProviderTerms)).

-spec terminal_and_provider_allowed_terminal_allow_false_test() -> _.
terminal_and_provider_allowed_terminal_allow_false_test() ->
    TerminalPayments = #domain_PaymentsProvisionTerms{
        allow = {constant, false},
        global_allow = {constant, true}
    },
    ProviderTerms = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            allow = {constant, true},
            global_allow = {constant, true}
        }
    },
    ?assertEqual(false, terminal_and_provider_allowed(TerminalPayments, ProviderTerms)).

-spec terminal_and_provider_allowed_provider_global_allow_false_test() -> _.
terminal_and_provider_allowed_provider_global_allow_false_test() ->
    TerminalPayments = #domain_PaymentsProvisionTerms{
        allow = {constant, true},
        global_allow = {constant, true}
    },
    ProviderTerms = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            allow = {constant, true},
            global_allow = {constant, false}
        }
    },
    ?assertEqual(false, terminal_and_provider_allowed(TerminalPayments, ProviderTerms)).

-spec terminal_and_provider_allowed_provider_allow_false_test() -> _.
terminal_and_provider_allowed_provider_allow_false_test() ->
    TerminalPayments = #domain_PaymentsProvisionTerms{
        allow = {constant, true},
        global_allow = {constant, true}
    },
    ProviderTerms = #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            allow = {constant, false},
            global_allow = {constant, true}
        }
    },
    ?assertEqual(false, terminal_and_provider_allowed(TerminalPayments, ProviderTerms)).

-spec terminal_and_provider_allowed_provider_undefined_test() -> _.
terminal_and_provider_allowed_provider_undefined_test() ->
    TerminalPayments = #domain_PaymentsProvisionTerms{
        allow = {constant, true},
        global_allow = {constant, true}
    },
    ?assertEqual(true, terminal_and_provider_allowed(TerminalPayments, undefined)).

-endif.
