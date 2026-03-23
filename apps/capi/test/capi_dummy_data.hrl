-ifndef(__CAPI_DUMMY_DATA_HRL__).
-define(__CAPI_DUMMY_DATA_HRL__, 42).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").
-include_lib("damsel/include/dmsl_customer_thrift.hrl").

-define(RECORD_UPDATE(FieldIndex, Value, Record), erlang:setelement(FieldIndex, Record, Value)).

-define(STRING, <<"TEST">>).
-define(RUB, <<"RUB">>).
-define(USD, <<"USD">>).
-define(KZT, <<"KZT">>).
-define(BANKID_RU, <<"PUTIN">>).
-define(BANKID_US, <<"TRAMP">>).
-define(WALLET_TOOL, <<"TOOL">>).
-define(PI_ACCOUNT_TOOL, <<"PITOOL">>).
-define(JSON, #{<<"bla">> => [42]}).
-define(JSON_SERIAL, <<"{\"bla\":[42]}">>).
-define(INTEGER, 10000).
-define(SMALLER_INTEGER, 1000).
-define(INTEGER_BINARY, <<"10000">>).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).
-define(URL, <<"https://url.io/1337">>).
-define(MD5, <<"033BD94B1168D7E4F0D644C3C95E35BF">>).
-define(SHA256, <<"94EE059335E587E501CC4BF90613E0814F00A7B08BC7C648FD865A2AF6A22CC2">>).
-define(TEST_USER_REALM, <<"external">>).
-define(TEST_RULESET_ID, <<"test/api">>).
-define(API_TOKEN, <<"letmein">>).
-define(EMAIL, <<"test@test.ru">>).

-define(KZT_PARTY_ID, <<"95a158c2-343a-40c9-b690-247dbee3fa40">>).
-define(KZT_SHOP_ID, <<"bbe49f63-0ff8-4cc4-99e8-00892a683cec">>).
-define(KZT_PI_ID, 100).
-define(KZT_TERMS_ID, 1000).
-define(KZT_RULESET_ID, 1059).
-define(KZT_PROHIBITIONS_ID, 1060).
-define(KZT_PROVIDER_8_ID, 8).
-define(KZT_PROVIDER_9_ID, 9).
-define(KZT_PROVIDER_10_ID, 10).
-define(KZT_TERMINAL_15_ID, 15).
-define(KZT_TERMINAL_16_ID, 16).
-define(KZT_TERMINAL_17_ID, 17).

-define(RATIONAL, #base_Rational{p = ?INTEGER, q = ?INTEGER}).

-define(DETAILS, #domain_InvoiceDetails{
    product = ?STRING,
    description = ?STRING,
    cart = ?INVOICE_CART,
    bank_account = ?INVOICE_BANK_ACCOUNT
}).

-define(CASH(Amount), #domain_Cash{
    amount = Amount,
    currency = #domain_CurrencyRef{
        symbolic_code = ?RUB
    }
}).

-define(CASH, ?CASH(?INTEGER)).

-define(CONTENT, #base_Content{
    type = <<"application/json">>,
    data = ?JSON_SERIAL
}).

-define(LIFETIME_INTERVAL, #domain_LifetimeInterval{
    years = ?INTEGER,
    months = ?INTEGER,
    days = ?INTEGER
}).

-define(TPL_CASH, {fixed, ?CASH}).

-define(INVOICE_STATUS(Status),
    erlang:apply(
        fun
            (unpaid) ->
                {unpaid, #domain_InvoiceUnpaid{}};
            (paid) ->
                {paid, #domain_InvoicePaid{}};
            (cancelled) ->
                {cancelled, #domain_InvoiceCancelled{details = ?STRING}};
            (fulfilled) ->
                {fulfilled, #domain_InvoiceFulfilled{details = ?STRING}}
        end,
        [Status]
    )
).

-define(INVOICE, ?INVOICE(?STRING, undefined)).

-define(INVOICE(ID, EID), ?INVOICE(ID, EID, ?STRING)).

-define(INVOICE(ID, EID, OwnerID), #domain_Invoice{
    id = ID,
    domain_revision = ?INTEGER,
    created_at = ?TIMESTAMP,
    status = ?INVOICE_STATUS(unpaid),
    due = ?TIMESTAMP,
    details = ?DETAILS,
    cost = ?CASH,
    context = ?CONTENT,
    shop_ref = #domain_ShopConfigRef{id = ?STRING},
    party_ref = #domain_PartyConfigRef{id = OwnerID},
    template_id = ?STRING,
    external_id = EID
}).

-define(SWAG_INVOICE_CART, [
    #{
        <<"product">> => ?STRING,
        <<"price">> => ?INTEGER,
        <<"quantity">> => ?INTEGER,
        <<"taxMode">> => #{
            <<"type">> => <<"InvoiceLineTaxVAT">>,
            <<"rate">> => <<"10%">>
        }
    }
]).

-define(INVOICE_BANK_ACCOUNT,
    {russian, #domain_InvoiceRussianBankAccount{
        account = <<"12345678901234567890">>,
        bank_bik = <<"123456789">>
    }}
).

-define(PAYPROC_INVOICE(Payments), ?PAYPROC_INVOICE(?INVOICE, Payments)).

-define(PAYPROC_INVOICE(Invoice, Payments), #payproc_Invoice{
    latest_event_id = ?INTEGER,
    invoice = Invoice,
    payments = Payments
}).

-define(PAYPROC_INVOICE, ?PAYPROC_INVOICE([])).

-define(PAYPROC_INVOICE_WITH_ID(ID), ?PAYPROC_INVOICE_WITH_ID(ID, undefined, ?STRING)).

-define(PAYPROC_INVOICE_WITH_ID(ID, EID), ?PAYPROC_INVOICE_WITH_ID(ID, EID, ?STRING)).

-define(PAYPROC_INVOICE_WITH_ID(ID, EID, OwnerID), #payproc_Invoice{
    latest_event_id = ?INTEGER,
    invoice = ?INVOICE(ID, EID, OwnerID),
    payments = []
}).

-define(INVOICE_LINE, #domain_InvoiceLine{
    product = ?STRING,
    quantity = ?INTEGER,
    price = ?CASH,
    metadata = #{?STRING := {obj, #{}}}
}).

-define(ALLOCATION_CART, #domain_InvoiceCart{
    lines = [
        #domain_InvoiceLine{
            product = ?STRING,
            quantity = ?INTEGER,
            price = ?CASH,
            metadata = #{<<"TaxMode">> => {str, <<"10%">>}}
        }
    ]
}).

-define(ALLOCATION, #domain_Allocation{
    transactions = [
        #domain_AllocationTransaction{
            id = ?STRING,
            target =
                {shop, #domain_AllocationTransactionTargetShop{
                    party_ref = #domain_PartyConfigRef{id = ?STRING},
                    shop_ref = #domain_ShopConfigRef{id = ?STRING}
                }},
            amount = ?CASH,
            body = #domain_AllocationTransactionBodyTotal{
                fee_target =
                    {shop, #domain_AllocationTransactionTargetShop{
                        party_ref = #domain_PartyConfigRef{id = ?STRING},
                        shop_ref = #domain_ShopConfigRef{id = ?STRING}
                    }},
                total = ?CASH,
                fee_amount = ?CASH,
                fee = #domain_AllocationTransactionFeeShare{
                    parts = ?RATIONAL
                }
            },
            details = #domain_AllocationTransactionDetails{
                cart = ?ALLOCATION_CART
            }
        }
    ]
}).

-define(INVOICE_CART, ?INVOICE_CART(#{<<"TaxMode">> => {str, <<"10%">>}})).
-define(INVOICE_CART(MD), #domain_InvoiceCart{
    lines = [
        #domain_InvoiceLine{
            product = ?STRING,
            quantity = ?INTEGER,
            price = ?CASH,
            metadata = MD
        }
    ]
}).

-define(INVOICE_TPL, ?INVOICE_TPL(?STRING)).
-define(INVOICE_TPL(InvoiceID), #domain_InvoiceTemplate{
    id = InvoiceID,
    details =
        {product, #domain_InvoiceTemplateProduct{
            product = ?STRING,
            price = ?TPL_CASH,
            metadata = #{?STRING => {obj, #{}}}
        }},
    product = ?STRING,
    context = ?CONTENT,
    shop_ref = #domain_ShopConfigRef{id = ?STRING},
    party_ref = #domain_PartyConfigRef{id = ?STRING},
    invoice_lifetime = ?LIFETIME_INTERVAL
}).

-define(BANK_CARD, ?BANK_CARD(<<"visa">>)).

-define(BANK_CARD(PS), #domain_BankCard{
    token = PS,
    payment_system = #domain_PaymentSystemRef{id = PS},
    bin = <<"411111">>,
    last_digits = <<"1111">>
}).

-define(BANK_CARD(PS, ExpDate), ?BANK_CARD(PS, ExpDate, <<"CARD HODLER">>)).
-define(BANK_CARD(PS, ExpDate, CardHolder), ?BANK_CARD(PS, ExpDate, CardHolder, undefined)).
-define(BANK_CARD(PS, ExpDate, CardHolder, Category), #domain_BankCard{
    token = PS,
    payment_system = #domain_PaymentSystemRef{id = PS},
    exp_date = ExpDate,
    cardholder_name = CardHolder,
    category = Category,
    bin = <<"411111">>,
    last_digits = <<"1111">>
}).

-define(DIGITAL_WALLET(Provider, ID, Token), #domain_DigitalWallet{
    payment_service = #domain_PaymentServiceRef{id = Provider},
    id = ID,
    token = Token
}).

-define(MOBILE_COMMERCE(Operator, CC, CTN), #domain_MobileCommerce{
    operator = #domain_MobileOperatorRef{id = Operator},
    phone = #domain_MobilePhone{
        cc = CC,
        ctn = CTN
    }
}).

-define(CRYPTO_CURRENCY_BTC, #domain_CryptoCurrencyRef{id = <<"bitcoin">>}).

-define(CONTACT_INFO, #domain_ContactInfo{
    phone_number = ?STRING,
    email = ?EMAIL,
    first_name = <<"FirstName">>,
    last_name = <<"LastName">>,
    country = <<"RUS">>,
    state = <<"State">>,
    city = <<"City">>,
    address = <<"Address">>,
    postal_code = <<"PostalCode">>,
    date_of_birth = <<"1970-01-01">>,
    document_id = <<"DocumentId">>
}).

-define(EXP_DATE(Month, Year), #domain_BankCardExpDate{
    month = Month,
    year = Year
}).

-define(DISP_PAYMENT_RESOURCE(PT), #domain_DisposablePaymentResource{
    payment_tool = PT,
    payment_session_id = ?STRING,
    client_info = #domain_ClientInfo{
        fingerprint = ?STRING,
        ip_address = ?STRING
    }
}).

-define(DISP_PAYMENT_RESOURCE, ?DISP_PAYMENT_RESOURCE({bank_card, ?BANK_CARD})).

-define(RECURRENT_PAYER,
    {recurrent, #domain_RecurrentPayer{
        payment_tool = {bank_card, ?BANK_CARD},
        recurrent_parent = #domain_RecurrentParentPayment{
            invoice_id = ?STRING,
            payment_id = ?STRING
        },
        contact_info = ?CONTACT_INFO
    }}
).

-define(PAYER(PT),
    {payment_resource, #domain_PaymentResourcePayer{
        resource = ?DISP_PAYMENT_RESOURCE(PT),
        contact_info = ?CONTACT_INFO
    }}
).

-define(PAYER, ?PAYER({bank_card, ?BANK_CARD})).

-define(PAYER_SESSION_INFO, #domain_PayerSessionInfo{
    redirect_url = ?URL
}).

-define(PAYMENT_STATUS_PENDING, {pending, #domain_InvoicePaymentPending{}}).
-define(PAYMENT_STATUS_FAILED(F), {failed, #domain_InvoicePaymentFailed{failure = F}}).

-define(PAYMENT(ID, Status, Payer, ExternalID, ChangedCost), #domain_InvoicePayment{
    id = ID,
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    status = Status,
    payer = Payer,
    payer_session_info = ?PAYER_SESSION_INFO,
    cost = ?CASH,
    changed_cost = ChangedCost,
    flow = {instant, #domain_InvoicePaymentFlowInstant{}},
    context = ?CONTENT,
    make_recurrent = false,
    external_id = ExternalID
}).

-define(PAYMENT(ID, Status, Payer), ?PAYMENT(ID, Status, Payer, undefined, undefined)).
-define(PAYMENT, ?PAYMENT(?STRING, ?PAYMENT_STATUS_PENDING, ?PAYER)).

-define(PAYMENT_WITH_RECURRENT_PAYER, ?PAYMENT(?STRING, ?PAYMENT_STATUS_PENDING, ?RECURRENT_PAYER)).

-define(PAYMENT_W_EXTERNAL_ID(ID, ExternalID), ?PAYMENT(ID, ?PAYMENT_STATUS_PENDING, ?PAYER, ExternalID, undefined)).

-define(PAYMENT_W_CHANGED_COST(ID, Amount), ?PAYMENT(ID, ?PAYMENT_STATUS_PENDING, ?PAYER, undefined, ?CASH(Amount))).

-define(PAYMENT_W_CUSTOMER(ID, CustomerID), (?PAYMENT(ID, ?PAYMENT_STATUS_PENDING, ?PAYER))#domain_InvoicePayment{
    customer_id = CustomerID
}).

-define(RECURRENT_PAYMENT(Status), #domain_InvoicePayment{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    status = Status,
    payer = ?RECURRENT_PAYER,
    cost = ?CASH,
    flow = {instant, #domain_InvoicePaymentFlowInstant{}},
    context = ?CONTENT,
    make_recurrent = true
}).

-define(RECURRENT_PAYMENT, ?RECURRENT_PAYMENT({pending, #domain_InvoicePaymentPending{}})).

-define(PAYPROC_PAYMENT(Payment, Refunds, Adjustments, Chargebacks), #payproc_InvoicePayment{
    payment = Payment,
    refunds = [#payproc_InvoicePaymentRefund{refund = R, sessions = []} || R <- Refunds],
    chargebacks = Chargebacks,
    sessions = [],
    legacy_refunds = Refunds,
    adjustments = Adjustments,
    last_transaction_info = ?TX_INFO,
    allocation = ?ALLOCATION
}).

-define(PAYPROC_PAYMENT(Payment), ?PAYPROC_PAYMENT(Payment, [?REFUND], [?ADJUSTMENT], [?PAYPROC_CHARGEBACK])).

-define(PAYPROC_PAYMENT, ?PAYPROC_PAYMENT(?PAYMENT, [?REFUND], [?ADJUSTMENT], [?PAYPROC_CHARGEBACK])).

-define(FAILED_PAYMENT(Failure), ?PAYMENT(?STRING, ?PAYMENT_STATUS_FAILED(Failure), ?PAYER)).

-define(PAYPROC_FAILED_PAYMENT(Failure), ?PAYPROC_PAYMENT(?FAILED_PAYMENT(Failure), [?REFUND], [], [])).

-define(ACCOUNT_STATE, #payproc_AccountState{
    account_id = ?INTEGER,
    own_amount = ?INTEGER,
    available_amount = ?INTEGER,
    currency = #domain_Currency{
        name = ?STRING,
        symbolic_code = ?RUB,
        numeric_code = ?INTEGER,
        exponent = ?INTEGER
    }
}).

-define(REFUND, ?REFUND(?STRING, ?STRING)).

-define(REFUND(ID, EID), #domain_InvoicePaymentRefund{
    id = ID,
    status = {pending, #domain_InvoicePaymentRefundPending{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?STRING,
    cash = ?CASH,
    external_id = EID
}).

-define(PAYPROC_REFUND(ID, EID), #payproc_InvoicePaymentRefund{
    refund = ?REFUND(ID, EID),
    sessions = []
}).

-define(CHARGEBACK, ?CHARGEBACK(?STRING)).

-define(PAYPROC_CHARGEBACK, ?PAYPROC_CHARGEBACK(?STRING)).

-define(PAYPROC_CHARGEBACK(ID), #payproc_InvoicePaymentChargeback{
    chargeback = ?CHARGEBACK(ID)
}).

-define(CHARGEBACK(ID), #domain_InvoicePaymentChargeback{
    id = ID,
    status = {pending, #domain_InvoicePaymentChargebackPending{}},
    stage = {chargeback, #domain_InvoicePaymentChargebackStageChargeback{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?CHARGEBACK_REASON,
    body = ?CASH,
    levy = ?CASH
}).

-define(CHARGEBACK_REASON, #domain_InvoicePaymentChargebackReason{
    code = <<"C0D3">>,
    category = {dispute, #'domain_InvoicePaymentChargebackCategoryDispute'{}}
}).

-define(BLOCKING,
    {unblocked, #domain_Unblocked{
        reason = ?STRING,
        since = ?TIMESTAMP
    }}
).

-define(SUSPENTION, {active, #domain_Active{since = ?TIMESTAMP}}).

-define(SHOP(Currency), #domain_ShopConfig{
    name = ?STRING,
    block = ?BLOCKING,
    suspension = ?SUSPENTION,
    payment_institution = #domain_PaymentInstitutionRef{id = ?INTEGER},
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER},
    account = #domain_ShopAccount{
        currency = #domain_CurrencyRef{symbolic_code = Currency},
        settlement = ?INTEGER,
        guarantee = ?INTEGER
    },
    party_ref = #domain_PartyConfigRef{id = ?STRING},
    location = ?SHOP_LOCATION,
    category = #domain_CategoryRef{id = ?INTEGER}
}).

-define(SHOP, ?SHOP(?RUB)).

-define(SHOP_LOCATION, {url, ?URL}).

-define(SHOP_DETAILS, #domain_Details{name = ?STRING}).

-define(PARTY, #domain_PartyConfig{
    name = <<"PARTY">>,
    block = ?BLOCKING,
    suspension = ?SUSPENTION,
    contact_info = #domain_PartyContactInfo{registration_email = ?STRING}
}).

-define(PARTY_WITH_SHOPS, #domain_PartyConfig{
    name = <<"PARTY_WITH_SHOPS">>,
    block = ?BLOCKING,
    suspension = ?SUSPENTION,
    contact_info = #domain_PartyContactInfo{registration_email = ?STRING}
}).

-define(ADJUSTMENT, #domain_InvoicePaymentAdjustment{
    id = ?STRING,
    status = {pending, #domain_InvoicePaymentAdjustmentPending{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?STRING,
    new_cash_flow = [],
    old_cash_flow_inverse = []
}).

-define(PAYMENT_INSTITUTION_ACCOUNT,
    {payment_institution_account, #domain_PaymentInstitutionAccount{}}
).

-define(WALLET_INFO,
    {wallet_info, #domain_WalletInfo{
        wallet_id = ?STRING
    }}
).

-define(WEBHOOK, #webhooker_Webhook{
    id = ?INTEGER,
    party_ref = #domain_PartyConfigRef{id = ?STRING},
    event_filter =
        {invoice, #webhooker_InvoiceEventFilter{
            shop_ref = #domain_ShopConfigRef{id = ?STRING},
            types = ordsets:from_list([
                {created, #webhooker_InvoiceCreated{}},

                {status_changed, #webhooker_InvoiceStatusChanged{
                    value = {
                        paid,
                        #webhooker_InvoicePaid{}
                    }
                }},
                {status_changed, #webhooker_InvoiceStatusChanged{
                    value = {
                        cancelled,
                        #webhooker_InvoiceCancelled{}
                    }
                }},
                {status_changed, #webhooker_InvoiceStatusChanged{
                    value = {
                        fulfilled,
                        #webhooker_InvoiceFulfilled{}
                    }
                }},

                {payment,
                    {
                        created,
                        #webhooker_InvoicePaymentCreated{}
                    }},
                {payment,
                    {
                        status_changed,
                        #webhooker_InvoicePaymentStatusChanged{
                            value = {
                                processed,
                                #webhooker_InvoicePaymentProcessed{}
                            }
                        }
                    }},
                {payment,
                    {
                        status_changed,
                        #webhooker_InvoicePaymentStatusChanged{
                            value = {
                                captured,
                                #webhooker_InvoicePaymentCaptured{}
                            }
                        }
                    }},
                {payment,
                    {
                        status_changed,
                        #webhooker_InvoicePaymentStatusChanged{
                            value = {
                                cancelled,
                                #webhooker_InvoicePaymentCancelled{}
                            }
                        }
                    }},
                {payment,
                    {
                        status_changed,
                        #webhooker_InvoicePaymentStatusChanged{
                            value = {
                                refunded,
                                #webhooker_InvoicePaymentRefunded{}
                            }
                        }
                    }},
                {payment,
                    {
                        status_changed,
                        #webhooker_InvoicePaymentStatusChanged{
                            value = {
                                failed,
                                #webhooker_InvoicePaymentFailed{}
                            }
                        }
                    }},
                {payment,
                    {
                        invoice_payment_refund_change,
                        {
                            invoice_payment_refund_created,
                            #webhooker_InvoicePaymentRefundCreated{}
                        }
                    }},
                {payment,
                    {
                        invoice_payment_refund_change,
                        {
                            invoice_payment_refund_status_changed,
                            #webhooker_InvoicePaymentRefundStatusChanged{
                                value = {
                                    failed,
                                    #webhooker_InvoicePaymentRefundFailed{}
                                }
                            }
                        }
                    }},
                {payment,
                    {
                        invoice_payment_refund_change,
                        {
                            invoice_payment_refund_status_changed,
                            #webhooker_InvoicePaymentRefundStatusChanged{
                                value = {
                                    succeeded,
                                    #webhooker_InvoicePaymentRefundSucceeded{}
                                }
                            }
                        }
                    }},
                {payment,
                    {
                        user_interaction,
                        #webhooker_InvoicePaymentUserInteractionChange{
                            status = {requested, #webhooker_UserInteractionStatusRequested{}}
                        }
                    }},
                {payment,
                    {
                        user_interaction,
                        #webhooker_InvoicePaymentUserInteractionChange{
                            status = {completed, #webhooker_UserInteractionStatusCompleted{}}
                        }
                    }}
            ])
        }},
    url = ?URL,
    pub_key = ?STRING,
    enabled = true
}).

-define(TX_INFO, #domain_TransactionInfo{
    id = ?STRING,
    timestamp = ?TIMESTAMP,
    extra = #{},
    additional_info = ?ADDITIONAL_TX_INFO
}).

-define(ADDITIONAL_TX_INFO, #domain_AdditionalTransactionInfo{
    rrn = <<"090909090909">>,
    approval_code = <<"808080">>,
    extra_payment_info = #{<<"test_key">> => <<"test_value">>}
}).

-define(STAT_PAYMENT_STATUS_PENDING, {pending, #domain_InvoicePaymentPending{}}).

-define(STAT_PAYMENT_STATUS_CAPTURED, {captured, #domain_InvoicePaymentCaptured{}}).

-define(STAT_PAYMENT_STATUS_FAILED,
    {failed, #domain_InvoicePaymentFailed{failure = {failure, #domain_Failure{code = <<"error_code">>}}}}
).

-define(ALL_OBJECTS, #{
    {category, #domain_CategoryRef{id = ?INTEGER}} =>
        {category, #domain_CategoryObject{
            ref = #domain_CategoryRef{id = ?INTEGER},
            data = #domain_Category{
                name = ?STRING,
                description = ?STRING
            }
        }},
    {business_schedule, #domain_BusinessScheduleRef{id = ?INTEGER}} =>
        {business_schedule, #domain_BusinessScheduleObject{
            ref = #domain_BusinessScheduleRef{id = ?INTEGER},
            data = #domain_BusinessSchedule{
                name = ?STRING,
                description = ?STRING,
                schedule = #'base_Schedule'{
                    year = {every, #'base_ScheduleEvery'{}},
                    month = {every, #'base_ScheduleEvery'{}},
                    day_of_month = {every, #'base_ScheduleEvery'{}},
                    day_of_week = {every, #'base_ScheduleEvery'{}},
                    hour = {every, #'base_ScheduleEvery'{}},
                    minute = {every, #'base_ScheduleEvery'{}},
                    second = {every, #'base_ScheduleEvery'{}}
                },
                delay = #'base_TimeSpan'{}
            }
        }},
    {globals, #domain_GlobalsRef{}} =>
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, #domain_ExternalAccountSetRef{id = ?INTEGER}},
                payment_institutions = [#domain_PaymentInstitutionRef{id = ?INTEGER}]
            }
        }},
    {payment_institution, #domain_PaymentInstitutionRef{id = ?INTEGER}} =>
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = #domain_PaymentInstitutionRef{id = ?INTEGER},
            data = #domain_PaymentInstitution{
                name = ?STRING,
                description = ?STRING,
                system_account_set = {value, #domain_SystemAccountSetRef{id = ?INTEGER}},
                inspector = {value, #domain_InspectorRef{id = ?INTEGER}},
                realm = test,
                residences = [rus]
            }
        }},
    {country, #domain_CountryRef{id = rus}} =>
        {country, #domain_CountryObject{
            ref = #domain_CountryRef{id = rus},
            data = #domain_Country{
                name = <<"Russia">>
            }
        }},
    {party_config, #domain_PartyConfigRef{id = ?STRING}} =>
        {party_config, #domain_PartyConfigObject{
            ref = #domain_PartyConfigRef{id = ?STRING},
            data = ?PARTY_WITH_SHOPS
        }},
    {shop_config, #domain_ShopConfigRef{id = ?STRING}} =>
        {shop_config, #domain_ShopConfigObject{
            ref = #domain_ShopConfigRef{id = ?STRING},
            data = ?SHOP(?USD)
        }},
    {country, #domain_CountryRef{id = deu}} =>
        {country, #domain_CountryObject{
            ref = #domain_CountryRef{id = deu},
            data = #domain_Country{
                name = <<"Germany">>,
                trade_blocs = ordsets:from_list(
                    [#domain_TradeBlocRef{id = <<"EEA">>}]
                )
            }
        }},
    {trade_bloc, #domain_TradeBlocRef{id = <<"EEA">>}} =>
        {trade_bloc, #domain_TradeBlocObject{
            ref = #domain_TradeBlocRef{id = <<"EEA">>},
            data = #domain_TradeBloc{
                name = <<"European Economic Area">>,
                description = <<"Extension of EU">>
            }
        }},

    {payment_system, #domain_PaymentSystemRef{id = <<"visa">>}} =>
        {payment_system, #domain_PaymentSystemObject{
            ref = #domain_PaymentSystemRef{id = <<"visa">>},
            data = #domain_PaymentSystem{name = <<"Visa">>}
        }},

    {payment_system, #domain_PaymentSystemRef{id = <<"mastercard">>}} =>
        {payment_system, #domain_PaymentSystemObject{
            ref = #domain_PaymentSystemRef{id = <<"mastercard">>},
            data = #domain_PaymentSystem{name = <<"Mastercard">>}
        }},

    {payment_service, #domain_PaymentServiceRef{id = <<"qiwi">>}} =>
        {payment_service, #domain_PaymentServiceObject{
            ref = #domain_PaymentServiceRef{id = <<"qiwi">>},
            data = #domain_PaymentService{
                name = <<"Qiwi">>,
                brand_name = <<"QIWI">>,
                category = <<"wallets">>,
                metadata = #{
                    <<"test.ns">> =>
                        {obj, #{
                            <<"answer">> => {i, 42},
                            <<"localization">> =>
                                {obj, #{
                                    <<"ru_RU">> => {arr, [{str, <<"КИВИ Кошелёк">>}]}
                                }}
                        }}
                }
            }
        }},

    {payment_institution, #domain_PaymentInstitutionRef{id = ?KZT_PI_ID}} =>
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = #domain_PaymentInstitutionRef{id = ?KZT_PI_ID},
            data = #domain_PaymentInstitution{
                name = ?STRING,
                description = ?STRING,
                system_account_set = {value, #domain_SystemAccountSetRef{id = ?INTEGER}},
                inspector = {value, #domain_InspectorRef{id = ?INTEGER}},
                realm = test,
                residences = [rus],
                payment_routing_rules = #domain_RoutingRules{
                    policies = #domain_RoutingRulesetRef{id = ?KZT_RULESET_ID},
                    prohibitions = #domain_RoutingRulesetRef{id = ?KZT_PROHIBITIONS_ID}
                }
            }
        }},

    {routing_rules, #domain_RoutingRulesetRef{id = ?KZT_RULESET_ID}} =>
        {routing_rules, #domain_RoutingRulesObject{
            ref = #domain_RoutingRulesetRef{id = ?KZT_RULESET_ID},
            data = #domain_RoutingRuleset{
                name = ?STRING,
                decisions =
                    {candidates, [
                        #domain_RoutingCandidate{
                            allowed = {constant, true},
                            terminal = #domain_TerminalRef{id = ?KZT_TERMINAL_15_ID}
                        },
                        #domain_RoutingCandidate{
                            allowed = {constant, true},
                            terminal = #domain_TerminalRef{id = ?KZT_TERMINAL_16_ID}
                        }
                    ]}
            }
        }},

    {routing_rules, #domain_RoutingRulesetRef{id = ?KZT_PROHIBITIONS_ID}} =>
        {routing_rules, #domain_RoutingRulesObject{
            ref = #domain_RoutingRulesetRef{id = ?KZT_PROHIBITIONS_ID},
            data = #domain_RoutingRuleset{
                name = ?STRING,
                decisions = {candidates, []}
            }
        }},

    {provider, #domain_ProviderRef{id = ?KZT_PROVIDER_8_ID}} =>
        {provider, #domain_ProviderObject{
            ref = #domain_ProviderRef{id = ?KZT_PROVIDER_8_ID},
            data = #domain_Provider{
                name = ?STRING,
                description = ?STRING,
                proxy = #domain_Proxy{ref = #domain_ProxyRef{id = ?INTEGER}, additional = #{}},
                realm = test,
                terms =
                    #domain_ProvisionTermSet{
                        payments =
                            #domain_PaymentsProvisionTerms{
                                payment_methods =
                                    {value, [
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"MASTERCARD">>},
                                                    is_cvv_empty = false
                                                }}
                                        },
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
                                                    is_cvv_empty = false
                                                }}
                                        }
                                    ]},
                                cash_limit =
                                    {decisions, [
                                        #domain_CashLimitDecision{
                                            if_ = {constant, true},
                                            then_ =
                                                {value, #domain_CashRange{
                                                    lower =
                                                        {inclusive, #domain_Cash{
                                                            amount = 100,
                                                            currency =
                                                                #domain_CurrencyRef{symbolic_code = ?KZT}
                                                        }},
                                                    upper =
                                                        {inclusive, #domain_Cash{
                                                            amount = 1000000000,
                                                            currency =
                                                                #domain_CurrencyRef{symbolic_code = ?KZT}
                                                        }}
                                                }}
                                        }
                                    ]},
                                refunds =
                                    #domain_PaymentRefundsProvisionTerms{
                                        cash_flow = {value, []},
                                        partial_refunds =
                                            #domain_PartialRefundsProvisionTerms{
                                                cash_limit =
                                                    {decisions, [
                                                        #domain_CashLimitDecision{
                                                            if_ = {constant, true},
                                                            then_ =
                                                                {value, #domain_CashRange{
                                                                    lower =
                                                                        {inclusive, #domain_Cash{
                                                                            amount = 100,
                                                                            currency =
                                                                                #domain_CurrencyRef{
                                                                                    symbolic_code = ?KZT
                                                                                }
                                                                        }},
                                                                    upper =
                                                                        {inclusive, #domain_Cash{
                                                                            amount = 1000000000,
                                                                            currency =
                                                                                #domain_CurrencyRef{
                                                                                    symbolic_code = ?KZT
                                                                                }
                                                                        }}
                                                                }}
                                                        }
                                                    ]}
                                            }
                                    }
                            }
                    }
            }
        }},

    {provider, #domain_ProviderRef{id = ?KZT_PROVIDER_9_ID}} =>
        {provider, #domain_ProviderObject{
            ref = #domain_ProviderRef{id = ?KZT_PROVIDER_9_ID},
            data = #domain_Provider{
                name = ?STRING,
                description = ?STRING,
                proxy = #domain_Proxy{ref = #domain_ProxyRef{id = ?INTEGER}, additional = #{}},
                realm = test,
                terms =
                    #domain_ProvisionTermSet{
                        payments =
                            #domain_PaymentsProvisionTerms{
                                payment_methods =
                                    {value, [
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"MASTERCARD">>},
                                                    is_cvv_empty = false
                                                }}
                                        },
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
                                                    is_cvv_empty = false
                                                }}
                                        }
                                    ]},
                                cash_limit =
                                    {value, #domain_CashRange{
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 100,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }},
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 1000000000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }}
                                    }},
                                refunds =
                                    #domain_PaymentRefundsProvisionTerms{
                                        cash_flow = {value, []},
                                        partial_refunds =
                                            #domain_PartialRefundsProvisionTerms{
                                                cash_limit =
                                                    {value, #domain_CashRange{
                                                        lower =
                                                            {inclusive, #domain_Cash{
                                                                amount = 100,
                                                                currency =
                                                                    #domain_CurrencyRef{
                                                                        symbolic_code = ?KZT
                                                                    }
                                                            }},
                                                        upper =
                                                            {inclusive, #domain_Cash{
                                                                amount = 100000000,
                                                                currency =
                                                                    #domain_CurrencyRef{
                                                                        symbolic_code = ?KZT
                                                                    }
                                                            }}
                                                    }}
                                            }
                                    }
                            }
                    }
            }
        }},

    %% Провайдер с global_allow=false — для теста блокировки терминала
    {provider, #domain_ProviderRef{id = ?KZT_PROVIDER_10_ID}} =>
        {provider, #domain_ProviderObject{
            ref = #domain_ProviderRef{id = ?KZT_PROVIDER_10_ID},
            data = #domain_Provider{
                name = ?STRING,
                description = ?STRING,
                proxy = #domain_Proxy{ref = #domain_ProxyRef{id = ?INTEGER}, additional = #{}},
                realm = test,
                terms =
                    #domain_ProvisionTermSet{
                        payments =
                            #domain_PaymentsProvisionTerms{
                                allow = {constant, true},
                                global_allow = {constant, false},
                                payment_methods =
                                    {value, [
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
                                                    is_cvv_empty = false
                                                }}
                                        }
                                    ]},
                                cash_limit =
                                    {value, #domain_CashRange{
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 50000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }},
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 50000000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }}
                                    }}
                            }
                    }
            }
        }},

    {terminal, #domain_TerminalRef{id = ?KZT_TERMINAL_15_ID}} =>
        {terminal, #domain_TerminalObject{
            ref = #domain_TerminalRef{id = ?KZT_TERMINAL_15_ID},
            data = #domain_Terminal{
                name = ?STRING,
                description = ?STRING,
                provider_ref = #domain_ProviderRef{id = ?KZT_PROVIDER_8_ID},
                terms =
                    #domain_ProvisionTermSet{
                        payments =
                            #domain_PaymentsProvisionTerms{
                                payment_methods =
                                    {value, [
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"MASTERCARD">>},
                                                    is_cvv_empty = false
                                                }}
                                        },
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
                                                    is_cvv_empty = false
                                                }}
                                        }
                                    ]},
                                cash_limit =
                                    {value, #domain_CashRange{
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 10000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }},
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 120000000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }}
                                    }}
                            }
                    }
            }
        }},

    {terminal, #domain_TerminalRef{id = ?KZT_TERMINAL_16_ID}} =>
        {terminal, #domain_TerminalObject{
            ref = #domain_TerminalRef{id = ?KZT_TERMINAL_16_ID},
            data = #domain_Terminal{
                name = ?STRING,
                description = ?STRING,
                provider_ref = #domain_ProviderRef{id = ?KZT_PROVIDER_9_ID},
                terms =
                    #domain_ProvisionTermSet{
                        payments =
                            #domain_PaymentsProvisionTerms{
                                cash_limit =
                                    {decisions, [
                                        #domain_CashLimitDecision{
                                            if_ = {constant, true},
                                            then_ =
                                                {value, #domain_CashRange{
                                                    lower =
                                                        {inclusive, #domain_Cash{
                                                            amount = 51300,
                                                            currency =
                                                                #domain_CurrencyRef{symbolic_code = ?KZT}
                                                        }},
                                                    upper =
                                                        {inclusive, #domain_Cash{
                                                            amount = 43609100,
                                                            currency =
                                                                #domain_CurrencyRef{symbolic_code = ?KZT}
                                                        }}
                                                }}
                                        },
                                        #domain_CashLimitDecision{
                                            if_ = {constant, true},
                                            then_ =
                                                {value, #domain_CashRange{
                                                    lower =
                                                        {inclusive, #domain_Cash{
                                                            amount = 51300,
                                                            currency =
                                                                #domain_CurrencyRef{symbolic_code = ?KZT}
                                                        }},
                                                    upper =
                                                        {inclusive, #domain_Cash{
                                                            amount = 128262000,
                                                            currency =
                                                                #domain_CurrencyRef{symbolic_code = ?KZT}
                                                        }}
                                                }}
                                        }
                                    ]}
                            }
                    }
            }
        }},

    %% Терминал с провайдером global_allow=false — для теста блокировки
    {terminal, #domain_TerminalRef{id = ?KZT_TERMINAL_17_ID}} =>
        {terminal, #domain_TerminalObject{
            ref = #domain_TerminalRef{id = ?KZT_TERMINAL_17_ID},
            data = #domain_Terminal{
                name = ?STRING,
                description = ?STRING,
                provider_ref = #domain_ProviderRef{id = ?KZT_PROVIDER_10_ID},
                terms =
                    #domain_ProvisionTermSet{
                        payments =
                            #domain_PaymentsProvisionTerms{
                                allow = {constant, true},
                                global_allow = {constant, true},
                                payment_methods =
                                    {value, [
                                        #domain_PaymentMethodRef{
                                            id =
                                                {bank_card, #domain_BankCardPaymentMethod{
                                                    payment_system = #domain_PaymentSystemRef{id = <<"VISA">>},
                                                    is_cvv_empty = false
                                                }}
                                        }
                                    ]},
                                cash_limit =
                                    {value, #domain_CashRange{
                                        lower =
                                            {inclusive, #domain_Cash{
                                                amount = 50000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }},
                                        upper =
                                            {inclusive, #domain_Cash{
                                                amount = 50000000,
                                                currency = #domain_CurrencyRef{symbolic_code = ?KZT}
                                            }}
                                    }}
                            }
                    }
            }
        }},

    {term_set_hierarchy, #domain_TermSetHierarchyRef{id = ?KZT_TERMS_ID}} =>
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = #domain_TermSetHierarchyRef{id = ?KZT_TERMS_ID},
            data = #domain_TermSetHierarchy{
                term_set =
                    #domain_TermSet{
                        payments =
                            #domain_PaymentsServiceTerms{
                                payment_methods =
                                    {value,
                                        ordsets:from_list([
                                            #domain_PaymentMethodRef{
                                                id =
                                                    {bank_card, #domain_BankCardPaymentMethod{
                                                        payment_system = #domain_PaymentSystemRef{
                                                            id = <<"VISA">>
                                                        }
                                                    }}
                                            }
                                        ])}
                            }
                    }
            }
        }},

    {shop_config, #domain_ShopConfigRef{id = ?KZT_SHOP_ID}} =>
        {shop_config, #domain_ShopConfigObject{
            ref = #domain_ShopConfigRef{id = ?KZT_SHOP_ID},
            data = #domain_ShopConfig{
                name = ?STRING,
                block = ?BLOCKING,
                suspension = ?SUSPENTION,
                payment_institution = #domain_PaymentInstitutionRef{id = ?KZT_PI_ID},
                terms = #domain_TermSetHierarchyRef{id = ?KZT_TERMS_ID},
                account = #domain_ShopAccount{
                    currency = #domain_CurrencyRef{symbolic_code = ?KZT},
                    settlement = ?INTEGER,
                    guarantee = ?INTEGER
                },
                party_ref = #domain_PartyConfigRef{id = ?KZT_PARTY_ID},
                location = ?SHOP_LOCATION,
                category = #domain_CategoryRef{id = ?INTEGER}
            }
        }}
}).

-define(USER_INTERACTION,
    {redirect,
        {post_request, #user_interaction_BrowserPostRequest{
            uri = ?URL,
            form = #{
                <<"redirect">> => ?URL
            }
        }}}
).

-define(USER_INTERACTION_REQUESTED, {requested, #user_interaction_Requested{}}).
-define(USER_INTERACTION_COMPLETED, {completed, #user_interaction_Completed{}}).

-define(INVOICE_PAYMENT_CHANGE(Payload),
    {invoice_payment_change, #payproc_InvoicePaymentChange{
        id = ?STRING,
        payload = Payload
    }}
).

-define(SESSION_CHANGE(Target, Payload),
    {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
        target = Target,
        payload = Payload
    }}
).

-define(INVOICE_EVENT(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    payload =
        {invoice_changes, [
            {invoice_created, #payproc_InvoiceCreated{invoice = ?INVOICE}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(unpaid)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(paid)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(cancelled)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(fulfilled)}},
            ?INVOICE_PAYMENT_CHANGE(
                ?SESSION_CHANGE(
                    {processed, #domain_InvoicePaymentProcessed{}},
                    {session_interaction_changed, #payproc_SessionInteractionChanged{
                        interaction = ?USER_INTERACTION
                    }}
                )
            ),
            ?INVOICE_PAYMENT_CHANGE(
                ?SESSION_CHANGE(
                    {processed, #domain_InvoicePaymentProcessed{}},
                    {session_interaction_changed, #payproc_SessionInteractionChanged{
                        interaction = ?USER_INTERACTION,
                        status = ?USER_INTERACTION_REQUESTED
                    }}
                )
            ),
            ?INVOICE_PAYMENT_CHANGE(
                ?SESSION_CHANGE(
                    {processed, #domain_InvoicePaymentProcessed{}},
                    {session_interaction_changed, #payproc_SessionInteractionChanged{
                        interaction = ?USER_INTERACTION,
                        status = ?USER_INTERACTION_COMPLETED
                    }}
                )
            )
        ]},
    source = {invoice_id, ?STRING}
}).

-define(INVOICE_EVENT_PRIVATE(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    payload =
        {invoice_changes, [
            ?INVOICE_PAYMENT_CHANGE(
                ?SESSION_CHANGE(
                    {processed, #domain_InvoicePaymentProcessed{}},
                    {session_started, #payproc_SessionStarted{}}
                )
            )
        ]},
    source = {invoice_id, ?STRING}
}).

-define(TERM_SET, #domain_TermSet{
    payments = ?PAYMENTS_SERVICE_TERMS
}).

-define(PAYMENTS_SERVICE_TERMS, #domain_PaymentsServiceTerms{
    payment_methods =
        {value,
            ordsets:from_list([
                #domain_PaymentMethodRef{
                    id =
                        {bank_card, #domain_BankCardPaymentMethod{
                            payment_system = #domain_PaymentSystemRef{id = <<"mastercard">>}
                        }}
                },
                #domain_PaymentMethodRef{
                    id =
                        {bank_card, #domain_BankCardPaymentMethod{
                            payment_system = #domain_PaymentSystemRef{id = <<"visa">>}
                        }}
                },
                #domain_PaymentMethodRef{
                    id = {crypto_currency, ?CRYPTO_CURRENCY_BTC}
                },
                #domain_PaymentMethodRef{
                    id = {crypto_currency, #domain_CryptoCurrencyRef{id = <<"bitcoin_cash">>}}
                },
                #domain_PaymentMethodRef{
                    id =
                        {bank_card, #domain_BankCardPaymentMethod{
                            payment_system = #domain_PaymentSystemRef{id = <<"mastercard">>},
                            payment_token = #domain_BankCardTokenServiceRef{id = <<"applepay">>}
                        }}
                },
                #domain_PaymentMethodRef{
                    id =
                        {bank_card, #domain_BankCardPaymentMethod{
                            payment_system = #domain_PaymentSystemRef{id = <<"visa">>},
                            payment_token = #domain_BankCardTokenServiceRef{id = <<"applepay">>}
                        }}
                },
                #domain_PaymentMethodRef{
                    id =
                        {bank_card, #domain_BankCardPaymentMethod{
                            payment_system = #domain_PaymentSystemRef{id = <<"mastercard">>},
                            payment_token = #domain_BankCardTokenServiceRef{id = <<"applepay">>},
                            tokenization_method = dpan
                        }}
                },
                #domain_PaymentMethodRef{
                    id =
                        {bank_card, #domain_BankCardPaymentMethod{
                            payment_system = #domain_PaymentSystemRef{id = <<"visa">>},
                            payment_token = #domain_BankCardTokenServiceRef{id = <<"applepay">>},
                            tokenization_method = dpan
                        }}
                },
                #domain_PaymentMethodRef{
                    id = {digital_wallet, #domain_PaymentServiceRef{id = <<"qiwi">>}}
                },
                #domain_PaymentMethodRef{
                    id = {mobile, #domain_MobileOperatorRef{id = <<"tele2">>}}
                },
                #domain_PaymentMethodRef{
                    id = {payment_terminal, #domain_PaymentServiceRef{id = <<"euroset">>}}
                }
            ])}
}).

-define(TEST_PAYMENT_TOKEN, ?STRING).

-define(TEST_PAYMENT_TOOL, ?TEST_PAYMENT_TOOL(<<"visa">>)).
-define(TEST_PAYMENT_TOOL(PaymentSystem), ?TEST_PAYMENT_TOOL(PaymentSystem, ?STRING)).
-define(TEST_PAYMENT_TOOL(PaymentSystem, Token), #{
    <<"type">> => <<"bank_card">>,
    <<"token">> => Token,
    <<"payment_system">> => PaymentSystem,
    <<"bin">> => <<"411111">>,
    <<"masked_pan">> => <<"1111">>,
    <<"exp_date">> => <<"12/2012">>
}).

-define(TEST_PAYMENT_SESSION, ?TEST_PAYMENT_SESSION(?STRING)).

-define(TEST_PAYMENT_SESSION(Session),
    capi_utils:map_to_base64url(#{
        <<"paymentSession">> => Session,
        <<"clientInfo">> => #{
            <<"fingerprint">> => <<"test fingerprint">>,
            <<"ip">> => <<"::ffff:127.0.0.1">>,
            <<"peer_ip">> => <<"::ffff:127.0.0.1">>,
            <<"user_ip">> => <<"::ffff:127.127.0.1">>
        }
    })
).

-define(INVOICE_TMPL_DETAILS_PARAMS, ?INVOICE_TMPL_DETAILS_PARAMS(?INTEGER)).
-define(INVOICE_TMPL_DETAILS_PARAMS(Quantity), #{
    <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
    <<"currency">> => ?RUB,
    <<"cart">> => [
        #{
            <<"product">> => ?STRING,
            <<"price">> => ?INTEGER,
            <<"quantity">> => Quantity
        },
        #{
            <<"product">> => ?STRING,
            <<"price">> => ?INTEGER,
            <<"quantity">> => Quantity,
            <<"taxMode">> => #{
                <<"type">> => <<"InvoiceLineTaxVAT">>,
                <<"rate">> => <<"18%">>
            }
        }
    ]
}).

-define(INVOICE_PARAMS, #{
    <<"shopID">> => ?STRING,
    <<"partyID">> => ?STRING,
    <<"amount">> => ?INTEGER,
    <<"currency">> => ?RUB,
    <<"metadata">> => #{<<"invoice_dummy_metadata">> => <<"test_value">>},
    <<"dueDate">> => ?TIMESTAMP,
    <<"product">> => <<"test_product">>,
    <<"description">> => <<"test_invoice_description">>,
    <<"bankAccount">> => #{
        <<"accountType">> => <<"InvoiceRussianBankAccount">>,
        <<"account">> => <<"12345678901234567890">>,
        <<"bankBik">> => <<"123456789">>
    }
}).

-define(PAYMENT_PARAMS(EID, Token), #{
    <<"externalID">> => EID,
    <<"flow">> => #{<<"type">> => <<"PaymentFlowInstant">>},
    <<"payer">> => #{
        <<"payerType">> => <<"PaymentResourcePayer">>,
        <<"paymentSession">> => ?TEST_PAYMENT_SESSION,
        <<"paymentToolToken">> => Token,
        <<"sessionInfo">> => #{
            <<"redirectUrl">> => ?URL
        },
        <<"contactInfo">> => #{
            <<"email">> => <<"bla@bla.ru">>
        }
    },
    <<"metadata">> => ?JSON,
    <<"processingDeadline">> => <<"5m">>
}).

-define(ALLOCATION_TARGET, #{
    <<"allocationTargetType">> => <<"AllocationTargetShop">>,
    <<"shopID">> => ?STRING
}).

-define(ALLOCATION_TRANSACTION_PARAMS, #{
    <<"target">> => ?ALLOCATION_TARGET,
    <<"allocationBodyType">> => <<"AllocationBodyTotal">>,
    <<"total">> => ?INTEGER,
    <<"currency">> => ?USD,
    <<"fee">> => #{
        <<"target">> => ?ALLOCATION_TARGET,
        <<"allocationFeeType">> => <<"AllocationFeeShare">>,
        <<"amount">> => ?INTEGER,
        <<"share">> => #{<<"m">> => ?INTEGER, <<"exp">> => ?INTEGER}
    },
    <<"cart">> => [
        #{<<"product">> => ?STRING, <<"quantity">> => ?INTEGER, <<"price">> => ?INTEGER}
    ]
}).

%% Customer

-define(CUSTOMER, #customer_Customer{
    id = ?STRING,
    party_ref = #domain_PartyConfigRef{id = ?STRING},
    created_at = ?TIMESTAMP,
    status = {active, #customer_CustomerActive{}},
    contact_info = ?CONTACT_INFO,
    metadata = {obj, #{<<"key">> => {str, <<"value">>}}}
}).

-define(CUSTOMER_STATE, #customer_CustomerState{
    customer = ?CUSTOMER,
    bank_card_refs = [],
    payment_refs = []
}).

-define(CUSTOMER_PAYMENT, #customer_CustomerPayment{
    invoice_id = ?STRING,
    payment_id = ?STRING,
    created_at = ?TIMESTAMP
}).

-define(CUSTOMER_PAYMENTS_RESPONSE, #customer_CustomerPaymentsResponse{
    payments = [?CUSTOMER_PAYMENT],
    continuation_token = undefined
}).

-define(BANK_CARD_INFO, #customer_BankCardInfo{
    id = ?STRING,
    card_mask = <<"424242******4242">>,
    created_at = ?TIMESTAMP,
    recurrent_providers = []
}).

-define(CUSTOMER_BANK_CARDS_RESPONSE, #customer_CustomerBankCardsResponse{
    bank_cards = [?BANK_CARD_INFO],
    continuation_token = undefined
}).

-endif.
