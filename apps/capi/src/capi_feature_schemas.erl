-module(capi_feature_schemas).

-type schema() :: feat:schema().

-include_lib("feat/include/feat.hrl").

-define(id, 1).
-define(invoice_id, 2).
-define(make_recurrent, 3).
-define(flow, 4).
-define(hold_exp, 5).
-define(payer, 6).
-define(payment_tool, 7).
-define(token, 8).
-define(bank_card, 9).
-define(exp_date, 10).
-define(terminal, 11).
-define(terminal_type, 12).
-define(wallet, 13).
-define(provider, 14).
-define(crypto, 15).
-define(currency, 16).
-define(mobile_commerce, 17).
-define(operator, 18).
-define(phone, 19).
-define(customer, 20).
-define(recurrent, 21).
-define(invoice, 22).
-define(payment, 23).
-define(shop_id, 24).
-define(amount, 25).
-define(product, 26).
-define(due_date, 27).
-define(cart, 28).
-define(quantity, 29).
-define(price, 30).
-define(tax, 31).
-define(rate, 32).
-define(bank_account, 33).
-define(account, 34).
-define(bank_bik, 35).
-define(payment_resource, 36).
-define(payment_session, 37).
-define(lifetime, 38).
-define(details, 39).
-define(days, 40).
-define(months, 41).
-define(years, 42).
-define(single_line, 43).
-define(multiline, 44).
-define(range, 45).
-define(fixed, 46).
-define(lower_bound, 47).
-define(upper_bound, 48).
-define(invoice_template_id, 49).
-define(contact_info, 50).
-define(email, 51).
-define(phone_number, 52).
-define(allocation, 53).
-define(target, 54).
-define(total, 55).
-define(fee, 56).
-define(share, 57).
-define(matisse, 58).
-define(exponent, 59).
-define(instant, 60).
-define(hold, 61).
-define(vat, 62).
-define(unlimited, 63).
-define(shop, 64).
-define(first_name, 65).
-define(last_name, 66).
-define(country, 67).
-define(state, 68).
-define(city, 69).
-define(address, 70).
-define(zip_code, 71).

-export([payment/0]).
-export([invoice/0]).
-export([invoice_template/0]).
-export([refund/0]).
-export([customer_binding/0]).
-export([customer/0]).

-spec payment() -> schema().
payment() ->
    #{
        ?invoice_id => <<"invoiceID">>,
        ?make_recurrent => <<"makeRecurrent">>,
        ?flow => {
            <<"flow">>,
            {union, <<"type">>, #{
                <<"PaymentFlowInstant">> => {?instant, #{}},
                <<"PaymentFlowHold">> =>
                    {?hold, #{
                        ?hold_exp => <<"onHoldExpiration">>
                    }}
            }}
        },
        ?payer => {
            <<"payer">>,
            {union, <<"payerType">>, #{
                <<"CustomerPayer">> =>
                    {?customer, #{
                        ?customer => <<"customerID">>
                    }},
                <<"RecurrentPayer">> => {
                    ?recurrent, #{
                        ?recurrent => {
                            <<"recurrentParentPayment">>,
                            #{
                                ?invoice => <<"invoiceID">>,
                                ?payment => <<"paymentID">>
                            }
                        }
                    }
                },
                <<"PaymentResourcePayer">> =>
                    {?payment_resource, #{
                        ?payment_tool => {<<"paymentTool">>, payment_tool_schema()}
                    }}
            }}
        }
    }.

-spec invoice() -> schema().
invoice() ->
    #{
        ?shop_id => <<"shopID">>,
        ?amount => <<"amount">>,
        ?currency => <<"currency">>,
        ?product => <<"product">>,
        ?due_date => <<"dueDate">>,
        ?cart => {<<"cart">>, {set, cart_line_schema()}},
        ?bank_account => {<<"bankAccount">>, bank_account_schema()},
        ?invoice_template_id => <<"invoiceTemplateID">>,
        ?allocation => {<<"allocation">>, {set, allocation_transaction()}}
    }.

-spec invoice_template() -> schema().
invoice_template() ->
    #{
        ?shop_id => <<"shopID">>,
        ?lifetime => {<<"lifetime">>, lifetime_schema()},
        ?details => {<<"details">>, invoice_template_details_schema()}
    }.

-spec invoice_template_details_schema() -> schema().
invoice_template_details_schema() ->
    {union, <<"templateType">>, #{
        <<"InvoiceTemplateSingleLine">> =>
            {?single_line, #{
                ?product => <<"product">>,
                ?price => {<<"price">>, invoice_template_line_cost()},
                ?tax => {<<"taxMode">>, tax_mode_schema()}
            }},
        <<"InvoiceTemplateMultiLine">> =>
            {?multiline, #{
                ?currency => <<"currency">>,
                ?cart => {<<"cart">>, {set, cart_line_schema()}}
            }}
    }}.

-spec refund() -> schema().
refund() ->
    #{
        ?amount => <<"amount">>,
        ?currency => <<"currency">>,
        ?cart => {<<"cart">>, {set, cart_line_schema()}},
        ?allocation => {<<"allocation">>, {set, allocation_transaction()}}
    }.

-spec customer() -> schema().
customer() ->
    #{
        ?shop_id => <<"shopID">>,
        ?contact_info => {<<"contactInfo">>, contact_info_schema()}
    }.

-spec customer_binding() -> schema().
customer_binding() ->
    #{
        ?payment_resource => {
            <<"paymentResource">>,
            #{
                ?payment_session => <<"paymentSession">>,
                ?payment_tool => {<<"paymentTool">>, payment_tool_schema()}
            }
        }
    }.

-spec payment_tool_schema() -> schema().
payment_tool_schema() ->
    {union, <<"type">>, #{
        <<"bank_card">> =>
            {?bank_card, #{
                ?token => <<"token">>,
                ?exp_date => <<"exp_date">>
            }},
        <<"payment_terminal">> =>
            {?terminal, #{
                ?terminal_type => <<"terminal_type">>
            }},
        <<"digital_wallet">> =>
            {?wallet, #{
                ?provider => <<"provider">>,
                ?id => <<"id">>,
                ?token => reserved
            }},
        <<"crypto_wallet">> =>
            {?crypto, #{
                ?currency => <<"currency">>
            }},
        <<"mobile_commerce">> =>
            {?mobile_commerce, #{
                ?operator => <<"operator">>,
                ?phone => <<"phone">>
            }}
    }}.

-spec allocation_transaction() -> schema().
allocation_transaction() ->
    #{
        ?target => {<<"target">>, allocation_target()},
        ?cart => {<<"cart">>, {set, cart_line_schema()}},
        ?allocation =>
            {union, <<"allocationBodyType">>, #{
                <<"AllocationBodyAmount">> =>
                    {?amount, #{
                        ?currency => <<"currency">>,
                        ?amount => <<"amount">>
                    }},
                <<"AllocationBodyTotal">> =>
                    {?total, #{
                        ?total => <<"total">>,
                        ?currency => <<"currency">>,
                        ?fee => {<<"fee">>, allocation_fee()}
                    }}
            }}
    }.

-spec allocation_fee() -> schema().
allocation_fee() ->
    #{
        ?target => {<<"target">>, allocation_target()},
        ?fee =>
            {union, <<"allocationFeeType">>, #{
                <<"AllocationFeeFixed">> =>
                    {?fixed, #{?amount => <<"amount">>}},
                <<"AllocationFeeShare">> => {
                    ?share, #{
                        ?amount => <<"amount">>,
                        ?share => {<<"share">>, decimal()}
                    }
                }
            }}
    }.

-spec allocation_target() -> schema().
allocation_target() ->
    {union, <<"allocationTargetType">>, #{
        <<"AllocationTargetShop">> =>
            {?shop, #{
                ?shop_id => <<"shopID">>
            }}
    }}.

-spec decimal() -> schema().
decimal() ->
    #{
        ?matisse => [<<"m">>],
        ?exponent => [<<"exp">>]
    }.

-spec cart_line_schema() -> schema().
cart_line_schema() ->
    #{
        ?product => <<"product">>,
        ?quantity => <<"quantity">>,
        ?price => <<"price">>,
        ?tax => {<<"taxMode">>, tax_mode_schema()}
    }.

-spec tax_mode_schema() -> schema().
tax_mode_schema() ->
    {union, <<"type">>, #{
        <<"InvoiceLineTaxVAT">> => {?vat, #{?rate => <<"rate">>}}
    }}.

-spec bank_account_schema() -> schema().
bank_account_schema() ->
    {union, <<"accountType">>, #{
        <<"InvoiceRussianBankAccount">> =>
            {?bank_account, #{
                ?account => <<"account">>,
                ?bank_bik => <<"bankBik">>
            }}
    }}.

invoice_template_line_cost() ->
    {union, <<"costType">>, #{
        <<"InvoiceTemplateLineCostRange">> => {
            ?range, #{
                ?currency => <<"currency">>,
                ?range => {<<"range">>, cost_amount_range()}
            }
        },
        <<"InvoiceTemplateLineCostFixed">> =>
            {?fixed, #{
                ?currency => <<"currency">>,
                ?amount => <<"amount">>
            }},
        <<"InvoiceTemplateLineCostUnlim">> => {?unlimited, #{}}
    }}.

-spec cost_amount_range() -> schema().
cost_amount_range() ->
    #{
        ?upper_bound => <<"upperBound">>,
        ?lower_bound => <<"lowerBound">>
    }.

-spec lifetime_schema() -> schema().
lifetime_schema() ->
    #{
        ?days => <<"days">>,
        ?months => <<"months">>,
        ?years => <<"years">>
    }.

-spec contact_info_schema() -> schema().
contact_info_schema() ->
    #{
        ?email => <<"email">>,
        ?phone_number => <<"phoneNumber">>,
        ?first_name => <<"firstName">>,
        ?last_name => <<"lastName">>,
        ?country => <<"country">>,
        ?state => <<"state">>,
        ?city => <<"city">>,
        ?address => <<"address">>,
        ?zip_code => <<"zipCode">>
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("capi_dummy_data.hrl").

deep_merge(M1, M2) ->
    maps:fold(
        fun
            (K, V, MAcc) when is_map(V) ->
                Value = deep_merge(maps:get(K, MAcc, #{}), V),
                MAcc#{K => Value};
            (K, V, MAcc) ->
                MAcc#{K => V}
        end,
        M1,
        M2
    ).

deep_fetch(Map, Keys) ->
    lists:foldl(fun(K, M) -> maps:get(K, M) end, Map, Keys).

hash(Term) ->
    feat:hash(Term).

read(Schema, Request) ->
    feat:read(Schema, Request).

compare(Features1, Features2) ->
    feat:compare(Features1, Features2).

list_diff_fields(Schema, Diff) ->
    feat:list_diff_fields(Schema, Diff).

-spec test() -> _.

-spec read_payment_features_test() -> _.

read_payment_features_test() ->
    PayerType = <<"PaymentResourcePayer">>,
    ToolType = <<"bank_card">>,
    Token = <<"cds token">>,
    CardHolder = <<"0x42">>,
    Category = <<"BUSINESS">>,
    ExpDate = {exp_date, 02, 2022},
    Flow = <<"PaymentFlowHold">>,
    Request = #{
        <<"flow">> => #{
            <<"type">> => Flow
        },
        <<"payer">> => #{
            <<"payerType">> => PayerType,
            <<"paymentTool">> => #{
                <<"type">> => ToolType,
                <<"token">> => Token,
                <<"exp_date">> => ExpDate,
                <<"cardholder_name">> => CardHolder,
                <<"category">> => Category
            }
        }
    },
    Payer = #{
        ?invoice_id => undefined,
        ?make_recurrent => undefined,
        ?flow => [?hold, #{?hold_exp => undefined}],
        ?payer => [
            ?payment_resource,
            #{
                ?payment_tool =>
                    [
                        ?bank_card,
                        #{
                            ?exp_date => hash(ExpDate),
                            ?token => hash(Token)
                        }
                    ]
            }
        ]
    },
    Features = read(payment(), Request),
    ?assertEqual(Payer, Features).

-spec compare_payment_bank_card_test() -> _.
compare_payment_bank_card_test() ->
    Token2 = <<"cds token 2">>,
    CardHolder2 = <<"Cake">>,

    PaymentTool1 = bank_card(),
    PaymentTool2 = PaymentTool1#{
        <<"token">> => Token2,
        <<"cardholder_name">> => CardHolder2
    },
    Request1 = payment_params(PaymentTool1),
    Request2 = payment_params(PaymentTool2),

    common_compare_tests(payment(), Request1, Request2, [
        <<"payer.paymentTool.token">>
    ]).

-spec compare_different_payment_tool_test() -> _.
compare_different_payment_tool_test() ->
    ToolType2 = <<"digital_wallet">>,
    Token2 = <<"wallet token">>,
    PaymentTool1 = bank_card(),
    PaymentTool2 = #{
        <<"type">> => ToolType2,
        <<"token">> => Token2
    },
    Request1 = payment_params(PaymentTool1),
    Request2 = payment_params(PaymentTool2),

    common_compare_tests(payment(), Request1, Request2, [<<"payer">>]).

-spec read_payment_customer_features_value_test() -> _.
read_payment_customer_features_value_test() ->
    PayerType = <<"CustomerPayer">>,
    CustomerID = <<"some customer id">>,
    Request = #{
        <<"payer">> => #{
            <<"payerType">> => PayerType,
            <<"customerID">> => CustomerID
        }
    },
    Features = read(payment(), Request),
    ?assertEqual(
        #{
            ?invoice_id => undefined,
            ?make_recurrent => undefined,
            ?flow => undefined,
            ?payer => [?customer, #{?customer => hash(CustomerID)}]
        },
        Features
    ).

-spec read_invoice_features_test() -> _.
read_invoice_features_test() ->
    ShopID = <<"shopus">>,
    Cur = <<"XXX">>,
    Prod1 = <<"yellow duck">>,
    Prod2 = <<"blue duck">>,
    DueDate = <<"2019-08-24T14:15:22Z">>,
    Price1 = 10000,
    Price2 = 20000,
    Quantity = 1,
    Product = #{
        ?product => hash(Prod1),
        ?quantity => hash(Quantity),
        ?price => hash(Price1),
        ?tax => undefined
    },
    Product2 = Product#{
        ?product => hash(Prod2),
        ?price => hash(Price2)
    },
    BankAccount = [
        ?bank_account,
        #{
            ?account => hash(<<"12345678901234567890">>),
            ?bank_bik => hash(<<"123456789">>)
        }
    ],
    Invoice = #{
        ?amount => undefined,
        ?currency => hash(Cur),
        ?shop_id => hash(ShopID),
        ?product => undefined,
        ?due_date => hash(DueDate),
        ?bank_account => BankAccount,
        ?cart => [
            [1, Product],
            [0, Product2]
        ],
        ?invoice_template_id => undefined,
        ?allocation => undefined
    },
    Request = #{
        <<"externalID">> => <<"externalID">>,
        <<"dueDate">> => DueDate,
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"description">> => <<"Wild birds.">>,
        <<"bankAccount">> => #{
            <<"accountType">> => <<"InvoiceRussianBankAccount">>,
            <<"account">> => <<"12345678901234567890">>,
            <<"bankBik">> => <<"123456789">>
        },
        <<"cart">> => [
            #{<<"product">> => Prod2, <<"quantity">> => 1, <<"price">> => Price2},
            #{<<"product">> => Prod1, <<"quantity">> => 1, <<"price">> => Price1, <<"not feature">> => <<"hmm">>}
        ],
        <<"metadata">> => #{}
    },

    Features = read(invoice(), Request),
    ?assertEqual(Invoice, Features),

    TemplateID = <<"42">>,
    RequestWithTemplate = Request#{<<"invoiceTemplateID">> => TemplateID},
    FeaturesWithTemplate = read(invoice(), RequestWithTemplate),
    ?assertEqual(hash(TemplateID), maps:get(?invoice_template_id, FeaturesWithTemplate)).

-spec compare_invoices_features_test() -> _.
compare_invoices_features_test() ->
    ShopID = <<"shopus">>,
    Cur = <<"RUB">>,
    Prod1 = <<"yellow duck">>,
    Prod2 = <<"blue duck">>,
    Price1 = 10000,
    Price2 = 20000,
    Product = #{
        <<"product">> => Prod1,
        <<"quantity">> => 1,
        <<"price">> => Price1,
        <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>,
            <<"rate">> => <<"10%">>
        }
    },
    Request1 = #{
        <<"shopID">> => ShopID,
        <<"currency">> => Cur,
        <<"cart">> => [Product]
    },
    Request2 = deep_merge(Request1, #{
        <<"cart">> => [#{<<"product">> => Prod2, <<"price">> => Price2}]
    }),
    Request3 = deep_merge(Request1, #{
        <<"cart">> => [#{<<"product">> => Prod2, <<"price">> => Price2, <<"quantity">> => undefined}]
    }),
    Schema = invoice(),
    Invoice1 = read(Schema, Request1),
    InvoiceChg1 = read(Schema, Request1#{
        <<"cart">> => [
            Product#{
                <<"price">> => Price2,
                <<"taxMode">> => #{
                    <<"type">> => <<"InvoiceLineTaxVAT">>,
                    <<"rate">> => <<"18%">>
                }
            }
        ]
    }),
    Invoice2 = read(Schema, Request2),
    InvoiceWithFullCart = read(Schema, Request3),
    ?assertEqual(
        {false, #{
            ?cart => ?difference
        }},
        compare(Invoice2, Invoice1)
    ),
    ?assert(compare(Invoice1, Invoice1)),
    %% Feature was deleted
    ?assert(compare(InvoiceWithFullCart, Invoice2)),
    %% Feature was add
    ?assert(compare(Invoice2, InvoiceWithFullCart)),
    %% When second request didn't contain feature, this situation detected as conflict.
    ?assertEqual(
        {false, #{?cart => ?difference}},
        compare(Invoice1#{?cart => undefined}, Invoice1)
    ),

    {false, Diff} = compare(Invoice1, InvoiceChg1),
    ?assertEqual(
        [<<"cart.0.price">>, <<"cart.0.taxMode">>],
        list_diff_fields(Schema, Diff)
    ),
    ?assert(compare(Invoice1, Invoice1#{?cart => undefined})).

-spec read_customer_features_test() -> _.
read_customer_features_test() ->
    Request = ?CUSTOMER_PARAMS,
    Features = #{
        ?shop_id => hash(?STRING),
        ?contact_info => #{
            ?email => hash(<<"bla@bla.ru">>),
            ?phone_number => undefined,
            ?first_name => undefined,
            ?last_name => undefined,
            ?country => undefined,
            ?state => undefined,
            ?city => undefined,
            ?address => undefined,
            ?zip_code => undefined
        }
    },
    ?assertEqual(
        Features,
        read(customer(), Request)
    ).

-spec compare_customer_features_test() -> _.
compare_customer_features_test() ->
    Request = ?CUSTOMER_PARAMS,
    RequestSame = Request#{
        <<"partyID">> => <<"ANOTHER PARTY">>,

        <<"metadata">> => #{<<"text">> => <<"sample text">>}
    },
    RequestDifferent = Request#{
        <<"shopID">> => hash(<<"Another shop">>),
        <<"contactInfo">> => #{
            <<"email">> => hash(<<"bla@example.com">>),
            <<"phoneNumber">> => <<"8-800-555-35-35">>
        }
    },
    common_compare_tests(
        customer(),
        Request,
        RequestSame,
        RequestDifferent,
        all
    ).

-spec read_customer_binding_features_test() -> _.
read_customer_binding_features_test() ->
    Session = ?TEST_PAYMENT_SESSION(<<"Session">>),
    Tool = ?TEST_PAYMENT_TOOL(<<"visa">>, <<"TOKEN">>),
    Request = payment_resource(Session, Tool),
    Features = #{
        ?payment_resource => #{
            ?payment_session => hash(Session),
            ?payment_tool => [
                ?bank_card,
                #{
                    ?token => hash(<<"TOKEN">>),
                    ?exp_date => hash(<<"12/2012">>)
                }
            ]
        }
    },

    ?assertEqual(
        Features,
        read(customer_binding(), Request)
    ).

-spec compare_customer_binding_features_test() -> _.
compare_customer_binding_features_test() ->
    Session1 = ?TEST_PAYMENT_SESSION(<<"Session1">>),
    Tool1 = ?TEST_PAYMENT_TOOL(<<"visa">>),
    Request1 = payment_resource(Session1, Tool1),

    Session2 = ?TEST_PAYMENT_SESSION(<<"Session2">>),
    Tool2 = ?TEST_PAYMENT_TOOL(<<"mastercard">>)#{<<"exp_date">> => <<"01/2020">>},
    Request2 = payment_resource(Session2, Tool2),

    common_compare_tests(customer_binding(), Request1, Request2, [
        <<"paymentResource.paymentTool.exp_date">>,
        <<"paymentResource.paymentSession">>
    ]).

%% Add invoice_template tests

-spec read_invoice_template_features_test() -> _.
read_invoice_template_features_test() ->
    ShopID = <<"1">>,
    Request = #{
        <<"shopID">> => ShopID,
        <<"lifetime">> => lifetime_dummy(1, 2, 3),
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS(42)
    },
    Features = #{
        ?shop_id => hash(ShopID),
        ?lifetime => #{
            ?days => hash(1),
            ?months => hash(2),
            ?years => hash(3)
        },
        ?details => [
            ?multiline,
            #{
                ?currency => hash(<<"RUB">>),
                ?cart => [
                    [
                        1,
                        #{
                            ?product => hash(?STRING),
                            ?quantity => hash(42),
                            ?price => hash(?INTEGER),
                            ?tax => [?vat, #{?rate => hash(<<"18%">>)}]
                        }
                    ],
                    [
                        0,
                        #{
                            ?product => hash(?STRING),
                            ?quantity => hash(42),
                            ?price => hash(?INTEGER),
                            ?tax => undefined
                        }
                    ]
                ]
            }
        ]
    },

    ?assertEqual(
        Features,
        read(invoice_template(), Request)
    ).

-spec compare_invoice_template_features_test() -> _.
compare_invoice_template_features_test() ->
    ShopID1 = <<"1">>,
    ShopID2 = <<"2">>,
    Request1 = #{
        <<"shopID">> => ShopID1,
        <<"lifetime">> => lifetime_dummy(1, 2, 3),
        <<"details">> => ?INVOICE_TMPL_DETAILS_PARAMS(42)
    },
    Request2 = deep_merge(
        Request1,
        #{
            <<"shopID">> => ShopID2,
            <<"lifetime">> => lifetime_dummy(1, 2, 42),
            <<"details">> => #{
                <<"currency">> => ?USD,
                <<"cart">> => [hd(deep_fetch(Request1, [<<"details">>, <<"cart">>]))]
            }
        }
    ),

    common_compare_tests(invoice_template(), Request1, Request2, [
        <<"shopID">>,
        <<"lifetime.years">>,
        <<"details">>
    ]).

-spec read_allocation_transaction_test_() -> _.
read_allocation_transaction_test_() ->
    Request1 = ?ALLOCATION_TRANSACTION_PARAMS,
    AllocationParams = #{
        ?total => hash(?INTEGER),
        ?currency => hash(?USD),
        ?fee => #{
            ?target =>
                [
                    ?shop,
                    #{
                        ?shop_id => hash(?STRING)
                    }
                ],
            ?fee => [
                ?share,
                #{
                    ?amount => hash(?INTEGER),
                    ?share => #{
                        ?matisse => hash(?INTEGER),
                        ?exponent => hash(?INTEGER)
                    }
                }
            ]
        }
    },
    Features1 = #{
        ?target =>
            [
                ?shop,
                #{
                    ?shop_id => hash(?STRING)
                }
            ],
        ?cart => [
            [
                0,
                #{
                    ?product => hash(?STRING),
                    ?quantity => hash(?INTEGER),
                    ?price => hash(?INTEGER),
                    ?tax => undefined
                }
            ]
        ],
        ?allocation => [?total, AllocationParams]
    },
    Request2 = Request1#{
        <<"fee">> => #{
            <<"target">> => ?ALLOCATION_TARGET,
            <<"allocationFeeType">> => <<"AllocationFeeFixed">>,
            <<"amount">> => 1024
        }
    },
    Features2 = Features1#{
        ?allocation =>
            [
                ?total,
                AllocationParams#{
                    ?fee => #{
                        ?target =>
                            [
                                ?shop,
                                #{
                                    ?shop_id => hash(?STRING)
                                }
                            ],
                        ?fee => [?fixed, #{?amount => hash(1024)}]
                    }
                }
            ]
    },
    [
        ?_assertEqual(Features1, read(allocation_transaction(), Request1)),
        ?_assertEqual(Features2, read(allocation_transaction(), Request2))
    ].

-spec compare_allocation_transaction_test() -> _.
compare_allocation_transaction_test() ->
    Request1 = ?ALLOCATION_TRANSACTION_PARAMS,
    Request2 = ?ALLOCATION_TRANSACTION_PARAMS#{
        <<"total">> => 1024,
        <<"amount">> => 512,
        <<"fee">> => #{
            <<"target">> => ?ALLOCATION_TARGET,
            <<"allocationFeeType">> => <<"AllocationFeeFixed">>,
            <<"amount">> => ?INTEGER,
            <<"share">> => undefined
        }
    },
    %% Request3 = #{
    %%     <<"target">> => ?ALLOCATION_TARGET#{<<"shopID">> => <<"SomeShop">>},
    %%     <<"allocationBodyType">> => <<"AllocationBodyAmount">>,
    %%     <<"amount">> => ?INTEGER,
    %%     <<"currency">> => ?RUB,
    %%     <<"cart">> => [
    %%         #{<<"product">> => ?STRING, <<"quantity">> => 1, <<"price">> => ?INTEGER}
    %%     ]
    %% },
    Request4 = Request1#{
        <<"fee">> => deep_merge(maps:get(<<"fee">>, Request1), #{
            <<"amount">> => 1024,
            <<"share">> => #{<<"m">> => 1024, <<"exp">> => 1024}
        })
    },
    common_compare_tests(allocation_transaction(), Request1, Request2, [
        <<"fee">>, <<"total">>
    ]),
    %% common_compare_tests(allocation_transaction(), Request1, Request3, all),
    common_compare_tests(allocation_transaction(), Request1, Request4, [<<"fee">>]).

-spec demo_compare_allocation_transaction_test() -> _.
demo_compare_allocation_transaction_test() ->
    Request1 = ?ALLOCATION_TRANSACTION_PARAMS,
    Request2 = #{
        <<"allocationBodyType">> => <<"AllocationBodyAmount">>
    },
    %% Request3 = #{
    %%     <<"fee">> => deep_merge(maps:get(<<"fee">>, Request1), #{
    %%         <<"allocationFeeType">> => <<"AllocationFeeFixed">>
    %%     })
    %% },
    common_compare_tests(allocation_transaction(), Request1, Request2, all)
%% common_compare_tests(allocation_transaction(), Request1, Request3, [
%%     <<"fee">>
%% ])
.

%%

payment_resource(Session, Tool) ->
    #{
        <<"paymentResource">> => #{
            <<"paymentSession">> => Session,
            <<"paymentTool">> => Tool
        }
    }.

payment_params(PaymentTool) ->
    deep_merge(
        payment_params(<<"EID">>, <<"Jwe">>, #{}, false),
        #{<<"payer">> => #{<<"paymentTool">> => PaymentTool}}
    ).

payment_params(ExternalID, Jwe, ContactInfo, MakeRecurrent) ->
    genlib_map:compact(#{
        <<"externalID">> => ExternalID,
        <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
        <<"makeRecurrent">> => MakeRecurrent,
        <<"metadata">> => #{<<"bla">> => <<"*">>},
        <<"processingDeadline">> => <<"5m">>,
        <<"payer">> => #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"paymentSession">> => <<"payment.session">>,
            <<"paymentToolToken">> => Jwe,
            <<"contactInfo">> => ContactInfo
        }
    }).

bank_card() ->
    #{
        <<"type">> => <<"bank_card">>,
        <<"token">> => <<"cds token">>,
        <<"payment_system">> => <<"visa">>,
        <<"bin">> => <<"411111">>,
        <<"last_digits">> => <<"1111">>,
        <<"exp_date">> => <<"2019-08-24T14:15:22Z">>,
        <<"cardholder_name">> => <<"Degus Degusovich">>,
        <<"is_cvv_empty">> => false
    }.

lifetime_dummy(Days, Months, Years) ->
    #{
        <<"days">> => Days,
        <<"months">> => Months,
        <<"years">> => Years
    }.

%% compare_equal_test(Schema, Request, AnotherRequest) ->
%%     Features = read(Schema, Request),
%%     AnotherFeatures = read(Schema, AnotherRequest),
%%     ?assertEqual(true, compare(Features, AnotherFeatures)).

common_compare_tests(Schema, Request, RequestDifferent, DiffFeatures) ->
    common_compare_tests(Schema, Request, Request, RequestDifferent, DiffFeatures).

common_compare_tests(Schema, Request, RequestWithIgnoredFields, RequestDifferent, DiffFeatures) ->
    Features = read(Schema, Request),
    %% Equal to self
    ?assertEqual(true, compare(Features, Features)),

    %% Equal to feature-wise same request
    case Request =:= RequestWithIgnoredFields of
        true ->
            ok;
        false ->
            FeaturesIgnored = read(Schema, RequestWithIgnoredFields),
            ?assertEqual(true, compare(Features, FeaturesIgnored))
    end,

    %% Has correct diff with different request
    FeaturesDifferent = read(Schema, RequestDifferent),
    Result = compare(Features, FeaturesDifferent),
    ?assertMatch({false, _}, Result),

    {false, Diff} = Result,
    ActualDiffFeatures = list_diff_fields(Schema, Diff),

    case ActualDiffFeatures =:= all orelse DiffFeatures =:= all of
        true -> ?assertEqual(DiffFeatures, ActualDiffFeatures);
        false -> ?assertEqual(lists:sort(DiffFeatures), lists:sort(ActualDiffFeatures))
    end.
-endif.
