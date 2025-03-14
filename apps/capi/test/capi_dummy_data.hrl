-ifndef(__CAPI_DUMMY_DATA_HRL__).
-define(__CAPI_DUMMY_DATA_HRL__, 42).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_webhooker_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").
-include_lib("magista_proto/include/magista_magista_thrift.hrl").

-define(RECORD_UPDATE(FieldIndex, Value, Record), erlang:setelement(FieldIndex, Record, Value)).

-define(STRING, <<"TEST">>).
-define(RUB, <<"RUB">>).
-define(USD, <<"USD">>).
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
    created_at = ?TIMESTAMP,
    status = ?INVOICE_STATUS(unpaid),
    due = ?TIMESTAMP,
    details = ?DETAILS,
    cost = ?CASH,
    context = ?CONTENT,
    shop_id = ?STRING,
    owner_id = OwnerID,
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
    invoice = Invoice,
    payments = Payments
}).

-define(PAYPROC_INVOICE, ?PAYPROC_INVOICE([])).

-define(PAYPROC_INVOICE_WITH_ID(ID), ?PAYPROC_INVOICE_WITH_ID(ID, undefined, ?STRING)).

-define(PAYPROC_INVOICE_WITH_ID(ID, EID), ?PAYPROC_INVOICE_WITH_ID(ID, EID, ?STRING)).

-define(PAYPROC_INVOICE_WITH_ID(ID, EID, OwnerID), #payproc_Invoice{
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
                    owner_id = ?STRING,
                    shop_id = ?STRING
                }},
            amount = ?CASH,
            body = #domain_AllocationTransactionBodyTotal{
                fee_target =
                    {shop, #domain_AllocationTransactionTargetShop{
                        owner_id = ?STRING,
                        shop_id = ?STRING
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
    shop_id = ?STRING,
    owner_id = ?STRING,
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

-define(CUSTOMER_PAYER,
    {customer, #domain_CustomerPayer{
        customer_id = ?STRING,
        customer_binding_id = ?STRING,
        rec_payment_tool_id = ?STRING,
        payment_tool = {bank_card, ?BANK_CARD},
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
-define(PAYMENT_WITH_CUSTOMER_PAYER, ?PAYMENT(?STRING, ?PAYMENT_STATUS_PENDING, ?CUSTOMER_PAYER)).

-define(PAYMENT_W_EXTERNAL_ID(ID, ExternalID), ?PAYMENT(ID, ?PAYMENT_STATUS_PENDING, ?PAYER, ExternalID, undefined)).

-define(PAYMENT_W_CHANGED_COST(ID, Amount), ?PAYMENT(ID, ?PAYMENT_STATUS_PENDING, ?PAYER, undefined, ?CASH(Amount))).

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

-define(CONTRACT, #domain_Contract{
    id = ?STRING,
    contractor = ?CONTRACTOR_REGISTERED_USER,
    payment_institution = #domain_PaymentInstitutionRef{id = ?INTEGER},
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    status = {active, #domain_ContractActive{}},
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER},
    adjustments = [?CONTRACT_ADJUSTMENT],
    payout_tools = [],
    legal_agreement = ?CONTRACT_LEGAL_AGREEMENT,
    report_preferences = ?CONTRACT_REPORT_PREFS
}).

-define(CONTRACT_LEGAL_AGREEMENT, #domain_LegalAgreement{
    signed_at = ?TIMESTAMP,
    legal_agreement_id = ?STRING,
    valid_until = ?TIMESTAMP
}).

-define(CONTRACT_REPORT_PREFS, #domain_ReportPreferences{
    service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
        schedule = #domain_BusinessScheduleRef{id = ?INTEGER},
        signer = #domain_Representative{
            position = ?STRING,
            full_name = ?STRING,
            document = {articles_of_association, #domain_ArticlesOfAssociation{}}
        }
    }
}).

-define(CONTRACTOR_REGISTERED_USER, {registered_user, #domain_RegisteredUser{email = ?STRING}}).

-define(BLOCKING,
    {unblocked, #domain_Unblocked{
        reason = ?STRING,
        since = ?TIMESTAMP
    }}
).

-define(SUSPENTION, {active, #domain_Active{since = ?TIMESTAMP}}).

-define(SHOP(Account), #domain_Shop{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    details = ?SHOP_DETAILS,
    location = ?SHOP_LOCATION,
    category = #domain_CategoryRef{id = ?INTEGER},
    contract_id = ?STRING,
    account = Account
}).

-define(SHOP, ?SHOP(undefined)).

-define(SHOP_ACCOUNT(Currency), #domain_ShopAccount{
    currency = #domain_CurrencyRef{symbolic_code = Currency},
    settlement = ?INTEGER,
    guarantee = ?INTEGER,
    payout = ?INTEGER
}).

-define(SHOP_CONTRACT, #payproc_ShopContract{
    shop = ?SHOP,
    contract = ?CONTRACT
}).

-define(SHOP_LOCATION, {url, ?URL}).

-define(SHOP_DETAILS, #domain_ShopDetails{name = ?STRING}).

-define(PARTY_CONTRACTOR, #domain_PartyContractor{
    id = ?STRING,
    contractor =
        {private_entity,
            {russian_private_entity, #domain_RussianPrivateEntity{
                first_name = ?STRING,
                second_name = ?STRING,
                middle_name = ?STRING,
                contact_info = ?CONTACT_INFO
            }}},
    status = none,
    identity_documents = []
}).

-define(WALLET_CONTRACT_ID, <<"WALLET_CONTRACT_ID">>).

-define(WALLET_CONTRACT, #domain_Contract{
    id = ?WALLET_CONTRACT_ID,
    contractor_id = ?STRING,
    payment_institution = #domain_PaymentInstitutionRef{id = ?INTEGER},
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    status = {active, #domain_ContractActive{}},
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER},
    adjustments = [],
    payout_tools = []
}).

-define(WALLET, #domain_Wallet{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    contract = ?WALLET_CONTRACT_ID
}).

-define(PARTY, #domain_Party{
    id = ?STRING,
    contact_info = #domain_PartyContactInfo{registration_email = ?STRING},
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    contracts = #{
        ?STRING => ?CONTRACT,
        ?WALLET_CONTRACT_ID => ?WALLET_CONTRACT
    },
    shops = #{
        ?STRING => ?SHOP,
        ?USD => ?SHOP(?SHOP_ACCOUNT(?USD))
    },
    contractors = #{?STRING => ?PARTY_CONTRACTOR},
    wallets = #{?STRING => ?WALLET},
    revision = 0
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

-define(CONTRACT_ADJUSTMENT, #domain_ContractAdjustment{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER}
}).

-define(PAYOUT_TOOL(ID, ToolInfo), #domain_PayoutTool{
    id = ID,
    created_at = ?TIMESTAMP,
    currency = #domain_CurrencyRef{symbolic_code = ?RUB},
    payout_tool_info = ToolInfo
}).

-define(PAYMENT_INSTITUTION_ACCOUNT,
    {payment_institution_account, #domain_PaymentInstitutionAccount{}}
).

-define(RUSSIAN_BANK_ACCOUNT,
    {russian_bank_account, #domain_RussianBankAccount{
        account = <<"12345678901234567890">>,
        bank_name = ?STRING,
        bank_post_account = <<"12345678901234567890">>,
        bank_bik = <<"123456789">>
    }}
).

-define(INTERNATIONAL_BANK_ACCOUNT,
    {international_bank_account, #domain_InternationalBankAccount{
        number = <<"12345678901234567890">>,
        bank = ?INTERNATIONAL_BANK_DETAILS,
        correspondent_account = #domain_InternationalBankAccount{number = <<"00000000000000000000">>},
        iban = <<"GR1601101250000000012300695">>,
        account_holder = ?STRING
    }}
).

-define(INTERNATIONAL_BANK_DETAILS, #domain_InternationalBankDetails{
    %% In reality either bic or aba_rtn should be used, not both.
    bic = <<"DEUTDEFF500">>,
    country = usa,
    name = ?STRING,
    address = ?STRING,
    aba_rtn = <<"129131673">>
}).

-define(WALLET_INFO,
    {wallet_info, #domain_WalletInfo{
        wallet_id = ?STRING
    }}
).

-define(WEBHOOK, #webhooker_Webhook{
    id = ?INTEGER,
    party_id = ?STRING,
    event_filter =
        {invoice, #webhooker_InvoiceEventFilter{
            shop_id = ?STRING,
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

-define(STAT_RESPONSE_INVOICES, #magista_StatInvoiceResponse{
    invoices = [
        ?STAT_INVOICE({unpaid, #domain_InvoiceUnpaid{}}),
        ?STAT_INVOICE({paid, #domain_InvoicePaid{}}),
        ?STAT_INVOICE({cancelled, #domain_InvoiceCancelled{details = ?STRING}}),
        ?STAT_INVOICE({fulfilled, #domain_InvoiceFulfilled{details = ?STRING}})
    ],
    continuation_token = ?STRING
}).

-define(STAT_INVOICE(Status), #magista_StatInvoice{
    id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = Status,
    product = ?STRING,
    description = ?STRING,
    due = ?TIMESTAMP,
    amount = ?INTEGER,
    currency_symbolic_code = ?RUB,
    context = ?CONTENT,
    cart = ?INVOICE_CART,
    external_id = ?STRING,
    status_changed_at = ?TIMESTAMP
}).

-define(STAT_RESPONSE_PAYMENTS, #magista_StatPaymentResponse{
    payments = [
        ?STAT_PAYMENT(
            ?STAT_CUSTOMER_PAYER({digital_wallet, ?DIGITAL_WALLET(?STRING, ?STRING, ?STRING)}),
            ?STAT_PAYMENT_STATUS_PENDING
        ),
        ?STAT_PAYMENT(?STAT_CUSTOMER_PAYER({bank_card, ?BANK_CARD}), ?STAT_PAYMENT_STATUS_FAILED),
        ?STAT_PAYMENT(?RECURRENT_PAYER, ?STAT_PAYMENT_STATUS_PENDING),
        ?STAT_PAYMENT(?PAYER, ?STAT_PAYMENT_STATUS_CAPTURED),
        ?STAT_PAYMENT(
            ?PAYER,
            ?STAT_PAYMENT_STATUS_PENDING,
            {hold, #magista_InvoicePaymentFlowHold{on_hold_expiration = cancel, held_until = ?TIMESTAMP}}
        )
    ],
    continuation_token = ?STRING
}).

-define(STAT_PAYMENT(Payer, Status, Flow), #magista_StatPayment{
    id = ?STRING,
    invoice_id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = Status,
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    payer = Payer,
    context = ?CONTENT,
    flow = Flow,
    domain_revision = ?INTEGER,
    additional_transaction_info = ?ADDITIONAL_TX_INFO
}).

-define(STAT_PAYMENT(Payer, Status), ?STAT_PAYMENT(Payer, Status, {instant, #magista_InvoicePaymentFlowInstant{}})).

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

-define(STAT_CUSTOMER_PAYER(PaymentTool),
    {customer, #magista_CustomerPayer{
        customer_id = ?STRING,
        payment_tool = PaymentTool,
        contact_info = ?CONTACT_INFO
    }}
).

-define(STAT_PAYMENT_STATUS_PENDING, {pending, #domain_InvoicePaymentPending{}}).

-define(STAT_PAYMENT_STATUS_CAPTURED, {captured, #domain_InvoicePaymentCaptured{}}).

-define(STAT_PAYMENT_STATUS_FAILED,
    {failed, #domain_InvoicePaymentFailed{failure = {failure, #domain_Failure{code = <<"error_code">>}}}}
).

-define(STAT_RESPONSE_REFUNDS, #magista_StatRefundResponse{
    refunds = [
        ?STAT_REFUND({pending, #domain_InvoicePaymentRefundPending{}}),
        ?STAT_REFUND({succeeded, #domain_InvoicePaymentRefundSucceeded{}}),
        ?STAT_REFUND(
            {failed, #domain_InvoicePaymentRefundFailed{
                failure = {operation_timeout, #domain_OperationTimeout{}}
            }}
        )
    ],
    continuation_token = ?STRING
}).

-define(STAT_REFUND(Status), #magista_StatRefund{
    id = ?STRING,
    payment_id = ?STRING,
    invoice_id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    status = Status,
    created_at = ?TIMESTAMP,
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    reason = ?STRING,
    cart = ?INVOICE_CART,
    external_id = ?STRING,
    status_changed_at = ?TIMESTAMP
}).

-define(SNAPSHOT, #'domain_conf_Snapshot'{
    version = ?INTEGER,
    domain = #{
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
                    default_contract_template = {value, #domain_ContractTemplateRef{id = ?INTEGER}},
                    providers = {value, []},
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
            }}
    }
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

-define(CUSTOMER, ?CUSTOMER(?STRING)).
-define(CUSTOMER(ID), #payproc_Customer{
    id = ID,
    owner_id = ?STRING,
    shop_id = ?STRING,
    status = ?CUSTOMER_READY,
    created_at = ?TIMESTAMP,
    bindings = [?CUSTOMER_BINDING],
    contact_info = ?CONTACT_INFO,
    metadata = {obj, #{}}
}).

-define(CUSTOMER_READY, {ready, #payproc_CustomerReady{}}).

-define(CUSTOMER_BINDING, ?CUSTOMER_BINDING(?STRING, ?STRING)).

-define(CUSTOMER_BINDING(ID, RECID), #payproc_CustomerBinding{
    id = ID,
    rec_payment_tool_id = RECID,
    payment_resource = ?DISP_PAYMENT_RESOURCE,
    status = {succeeded, #payproc_CustomerBindingSucceeded{}}
}).

-define(CUSTOMER_EVENT(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    source = {customer_id, ?STRING},
    payload =
        {customer_changes, [
            {customer_created, #payproc_CustomerCreated{
                customer_id = ?STRING,
                owner_id = ?STRING,
                shop_id = ?STRING,
                created_at = ?TIMESTAMP,
                contact_info = ?CONTACT_INFO,
                metadata = {obj, #{}}
            }},
            {customer_status_changed, #payproc_CustomerStatusChanged{
                status = ?CUSTOMER_READY
            }},
            {customer_binding_changed, #payproc_CustomerBindingChanged{
                id = ?STRING,
                payload =
                    {started, #payproc_CustomerBindingStarted{
                        binding = ?CUSTOMER_BINDING
                    }}
            }},
            {customer_binding_changed, #payproc_CustomerBindingChanged{
                id = ?STRING,
                payload =
                    {status_changed, #payproc_CustomerBindingStatusChanged{
                        status =
                            {failed, #payproc_CustomerBindingFailed{
                                failure = {failure, #domain_Failure{code = <<"error_code">>}}
                            }}
                    }}
            }},
            {customer_binding_changed, #payproc_CustomerBindingChanged{
                id = ?STRING,
                payload =
                    {interaction_changed, #payproc_CustomerBindingInteractionChanged{
                        interaction = ?USER_INTERACTION
                    }}
            }},
            {customer_binding_changed, #payproc_CustomerBindingChanged{
                id = ?STRING,
                payload =
                    {interaction_changed, #payproc_CustomerBindingInteractionChanged{
                        interaction = ?USER_INTERACTION,
                        status = ?USER_INTERACTION_COMPLETED
                    }}
            }}
        ]}
}).

-define(PAYOUT, ?PAYOUT(?PI_ACCOUNT_TOOL, ?STRING)).
-define(PAYOUT(ToolID), ?PAYOUT(ToolID, ?STRING)).

-define(PAYOUT(ToolID, PartyID), #payouts_Payout{
    payout_id = ?STRING,
    created_at = ?TIMESTAMP,
    party_id = PartyID,
    shop_id = ?STRING,
    status = {paid, #payouts_PayoutPaid{}},
    cash_flow = [],
    payout_tool_id = ToolID,
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency = #payouts_CurrencyRef{
        symbolic_code = ?RUB
    }
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

-define(CUSTOMER_PARAMS, #{
    <<"shopID">> => ?STRING,
    <<"contactInfo">> => #{<<"email">> => <<"bla@bla.ru">>},
    <<"metadata">> => #{<<"text">> => [<<"SOMESHIT">>, 42]}
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

-endif.
