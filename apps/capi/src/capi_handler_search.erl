-module(capi_handler_search).

-include_lib("magista_proto/include/magista_magista_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID, Req, Context) when OperationID =:= 'SearchInvoices' ->
    Prototypes = build_prototypes(OperationID, Context, Req),
    Authorize = fun() -> {ok, capi_auth:authorize_operation(Prototypes, Context)} end,
    Process = fun() ->
        Query = make_invoices_search_query(Context, Req),
        process_search_request('SearchInvoices', Query, Req, Context)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'SearchPayments' ->
    Prototypes = build_prototypes(OperationID, Context, Req),
    Authorize = fun() -> {ok, capi_auth:authorize_operation(Prototypes, Context)} end,
    Process = fun() ->
        Query = make_payments_search_query(Context, Req),
        process_search_request('SearchPayments', Query, Req, Context)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'SearchRefunds' ->
    Prototypes = build_prototypes(OperationID, Context, Req),
    Authorize = fun() -> {ok, capi_auth:authorize_operation(Prototypes, Context)} end,
    Process = fun() ->
        Query = make_refunds_search_query(Context, Req),
        process_search_request('SearchRefunds', Query, Req, Context)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

make_invoices_search_query(Context, Req) ->
    #magista_InvoiceSearchQuery{
        common_search_query_params = make_common_query_params(Context, Req),
        payment_params = make_payment_query_params(Req),
        invoice_ids = encode_invoice_ids(genlib_map:get('invoiceID', Req)),
        invoice_status = encode_invoice_status(genlib_map:get('invoiceStatus', Req)),
        invoice_amount = genlib_map:get('invoiceAmount', Req)
    }.

make_payments_search_query(Context, Req) ->
    #magista_PaymentSearchQuery{
        common_search_query_params = make_common_query_params(Context, Req),
        payment_params = make_payment_query_params(Req),
        invoice_ids = encode_invoice_ids(genlib_map:get('invoiceID', Req))
    }.

make_refunds_search_query(Context, Req) ->
    #magista_RefundSearchQuery{
        common_search_query_params = make_common_query_params(Context, Req),
        invoice_ids = encode_invoice_ids(genlib_map:get('invoiceID', Req)),
        payment_id = genlib_map:get('paymentID', Req),
        refund_id = genlib_map:get('refundID', Req),
        refund_status = encode_refund_status(genlib_map:get('refundStatus', Req))
    }.

make_common_query_params(Context, Req) ->
    #magista_CommonSearchQueryParams{
        to_time = capi_handler_utils:get_time('toTime', Req),
        from_time = capi_handler_utils:get_time('fromTime', Req),
        shop_ids = [genlib_map:get('shopID', Req)],
        party_id = capi_handler_utils:get_party_id(Context),
        continuation_token = genlib_map:get('continuationToken', Req),
        limit = genlib_map:get('limit', Req)
    }.

make_payment_query_params(Req) ->
    #magista_PaymentParams{
        payment_id = genlib_map:get('paymentID', Req),
        payment_status = encode_payment_status(genlib_map:get('paymentStatus', Req)),
        payment_flow = encode_payment_flow(genlib_map:get('paymentFlow', Req)),
        payment_tool = encode_payment_method(genlib_map:get('paymentMethod', Req)),
        payment_terminal_provider = encode_terminal_provider(genlib_map:get('paymentTerminalProvider', Req)),
        payment_email = genlib_map:get('payerEmail', Req),
        payment_ip = genlib_map:get('payerIP', Req),
        payment_fingerprint = genlib_map:get('payerFingerprint', Req),
        payment_first6 = genlib_map:get('first6', Req),
        payment_system = encode_payment_system_ref(genlib_map:get('bankCardPaymentSystem', Req)),
        payment_last4 = genlib_map:get('last4', Req),
        payment_customer_id = genlib_map:get('customerID', Req),
        payment_amount = genlib_map:get('paymentAmount', Req),
        payment_rrn = genlib_map:get('rrn', Req),
        payment_approval_code = genlib_map:get('approvalCode', Req),
        payment_token_provider = encode_payment_token_provider(genlib_map:get('bankCardTokenProvider', Req))
    }.

process_search_request(Function, Query, _Req, Context) ->
    Call = {magista, Function, {Query}},
    process_search_request_result(Function, capi_handler_utils:service_call(Call, Context), Context).

process_search_request_result(Function, Result, Context) ->
    case Result of
        {ok, Response} ->
            {Results, ContinuationToken} = decode_search_response(Function, Response, Context),
            Resp = genlib_map:compact(#{
                <<"result">> => Results,
                <<"totalCount">> => length(Results),
                <<"continuationToken">> => ContinuationToken
            }),
            {ok, {200, #{}, Resp}};
        {exception, #base_InvalidRequest{errors = Errors}} ->
            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
            {ok, logic_error('invalidRequest', FormattedErrors)};
        {exception, #magista_LimitExceeded{}} ->
            {ok, logic_error('invalidRequest', <<"Invalid limit">>)};
        {exception, #magista_BadContinuationToken{}} ->
            {ok, logic_error('invalidRequest', <<"Invalid token">>)}
    end.

decode_search_response(
    'SearchInvoices',
    #magista_StatInvoiceResponse{
        invoices = Invoices,
        continuation_token = ContinuationToken
    },
    _Context
) ->
    {
        [decode_stat_invoice(Invoice) || Invoice <- Invoices],
        ContinuationToken
    };
decode_search_response(
    'SearchPayments',
    #magista_StatPaymentResponse{
        payments = Payments,
        continuation_token = ContinuationToken
    },
    Context
) ->
    {
        [decode_stat_payment(Payment, Context) || Payment <- Payments],
        ContinuationToken
    };
decode_search_response(
    'SearchRefunds',
    #magista_StatRefundResponse{
        refunds = Refunds,
        continuation_token = ContinuationToken
    },
    _Context
) ->
    {[decode_stat_refund(Refund) || Refund <- Refunds], ContinuationToken}.

%%

encode_invoice_status(unpaid) -> unpaid;
encode_invoice_status(cancelled) -> cancelled;
encode_invoice_status(paid) -> paid;
encode_invoice_status(fulfilled) -> fulfilled;
encode_invoice_status(undefined) -> undefined.

encode_payment_status(pending) -> pending;
encode_payment_status(processed) -> processed;
encode_payment_status(captured) -> captured;
encode_payment_status(cancelled) -> cancelled;
encode_payment_status(refunded) -> refunded;
encode_payment_status(failed) -> failed;
encode_payment_status(undefined) -> undefined.

encode_refund_status(pending) -> pending;
encode_refund_status(succeeded) -> succeeded;
encode_refund_status(failed) -> failed;
encode_refund_status(undefined) -> undefined.

encode_payment_flow(instant) -> instant;
encode_payment_flow(hold) -> hold;
encode_payment_flow(undefined) -> undefined.

encode_payment_method('bankCard') -> bank_card;
encode_payment_method('paymentTerminal') -> payment_terminal;
encode_payment_method(undefined) -> undefined.

encode_terminal_provider(ID) -> encode_payment_service_ref(ID).

encode_payment_service_ref(ID) when is_binary(ID) -> #domain_PaymentServiceRef{id = ID};
encode_payment_service_ref(undefined) -> undefined.

encode_payment_system_ref(ID) when is_binary(ID) -> #domain_PaymentSystemRef{id = ID};
encode_payment_system_ref(undefined) -> undefined.

encode_payment_token_provider(ID) when is_binary(ID) -> #domain_BankCardTokenServiceRef{id = ID};
encode_payment_token_provider(undefined) -> undefined.

encode_invoice_ids(ID) when is_binary(ID) -> [ID];
encode_invoice_ids(undefined) -> undefined.

%%

decode_stat_invoice(Stat) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Stat#magista_StatInvoice.id,
            <<"externalID">> => Stat#magista_StatInvoice.external_id,
            <<"shopID">> => Stat#magista_StatInvoice.shop_id,
            <<"createdAt">> => Stat#magista_StatInvoice.created_at,
            <<"dueDate">> => Stat#magista_StatInvoice.due,
            <<"amount">> => Stat#magista_StatInvoice.amount,
            <<"currency">> => Stat#magista_StatInvoice.currency_symbolic_code,
            <<"metadata">> => capi_handler_decoder_utils:decode_context(Stat#magista_StatInvoice.context),
            <<"product">> => Stat#magista_StatInvoice.product,
            <<"description">> => Stat#magista_StatInvoice.description,
            <<"cart">> => capi_handler_decoder_invoicing:decode_invoice_cart(Stat#magista_StatInvoice.cart)
        },
        capi_handler_decoder_invoicing:decode_invoice_status(Stat#magista_StatInvoice.status)
    ).

decode_stat_payment(Stat, Context) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Stat#magista_StatPayment.id,
            <<"shortID">> => Stat#magista_StatPayment.short_id,
            <<"invoiceID">> => Stat#magista_StatPayment.invoice_id,
            <<"shopID">> => Stat#magista_StatPayment.shop_id,
            <<"createdAt">> => Stat#magista_StatPayment.created_at,
            <<"amount">> => Stat#magista_StatPayment.amount,
            <<"fee">> => Stat#magista_StatPayment.fee,
            <<"currency">> => Stat#magista_StatPayment.currency_symbolic_code,
            <<"payer">> => decode_stat_payer(Stat#magista_StatPayment.payer),
            <<"flow">> => decode_stat_payment_flow(Stat#magista_StatPayment.flow),
            <<"metadata">> => capi_handler_decoder_utils:decode_context(Stat#magista_StatPayment.context),
            <<"transactionInfo">> => decode_stat_tx_info(Stat#magista_StatPayment.additional_transaction_info),
            <<"statusChangedAt">> => Stat#magista_StatPayment.status_changed_at,
            <<"makeRecurrent">> => capi_handler_decoder_invoicing:decode_make_recurrent(
                Stat#magista_StatPayment.make_recurrent
            ),
            <<"cart">> => capi_handler_decoder_invoicing:decode_invoice_cart(Stat#magista_StatPayment.cart)
        },
        decode_stat_payment_status(Stat#magista_StatPayment.status, Context)
    ).

decode_stat_refund(Stat) ->
    Status = capi_handler_decoder_invoicing:decode_refund_status(Stat#magista_StatRefund.status),
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Stat#magista_StatRefund.id,
            <<"externalID">> => Stat#magista_StatRefund.external_id,
            <<"invoiceID">> => Stat#magista_StatRefund.invoice_id,
            <<"paymentID">> => Stat#magista_StatRefund.payment_id,
            <<"createdAt">> => Stat#magista_StatRefund.created_at,
            <<"reason">> => Stat#magista_StatRefund.reason,
            <<"amount">> => Stat#magista_StatRefund.amount,
            <<"currency">> => Stat#magista_StatRefund.currency_symbolic_code,
            <<"cart">> => capi_handler_decoder_invoicing:decode_invoice_cart(Stat#magista_StatRefund.cart)
        },
        Status
    ).

decode_stat_tx_info(undefined) ->
    undefined;
decode_stat_tx_info(TransactionInfo) ->
    RRN = TransactionInfo#domain_AdditionalTransactionInfo.rrn,
    AAC = TransactionInfo#domain_AdditionalTransactionInfo.approval_code,
    ParsedTransactionInfo = #{
        <<"rrn">> => RRN,
        <<"approvalCode">> => AAC
    },
    genlib_map:compact(ParsedTransactionInfo).

decode_stat_payer(
    {customer, #magista_CustomerPayer{
        customer_id = ID,
        payment_tool = PaymentTool
    }}
) ->
    #{
        <<"payerType">> => <<"CustomerPayer">>,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"customerID">> => ID
    };
decode_stat_payer(
    {recurrent, #domain_RecurrentPayer{
        payment_tool = PaymentTool,
        recurrent_parent = RecurrentParent,
        contact_info = #domain_ContactInfo{
            phone_number = PhoneNumber,
            email = Email,
            first_name = FirstName,
            last_name = LastName,
            country = Country,
            state = State,
            city = City,
            address = Address,
            postal_code = PostalCode,
            date_of_birth = DateOfBirth,
            document_id = DocumentId
        }
    }}
) ->
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email">> => Email,
            <<"firstName">> => FirstName,
            <<"lastName">> => LastName,
            <<"country">> => Country,
            <<"state">> => State,
            <<"city">> => City,
            <<"address">> => Address,
            <<"postalCode">> => PostalCode,
            <<"dateOfBirth">> => DateOfBirth,
            <<"documentId">> => DocumentId
        }),
        <<"recurrentParentPayment">> => capi_handler_decoder_invoicing:decode_recurrent_parent(RecurrentParent)
    };
decode_stat_payer(
    {payment_resource, #domain_PaymentResourcePayer{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = #domain_ClientInfo{
                ip_address = IP,
                fingerprint = Fingerprint
            }
        },
        contact_info = #domain_ContactInfo{
            phone_number = PhoneNumber,
            email = Email,
            first_name = FirstName,
            last_name = LastName,
            country = Country,
            state = State,
            city = City,
            address = Address,
            postal_code = PostalCode,
            date_of_birth = DateOfBirth,
            document_id = DocumentId
        }
    }}
) ->
    genlib_map:compact(#{
        <<"payerType">> => <<"PaymentResourcePayer">>,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"paymentSession">> => PaymentSession,
        <<"clientInfo">> => genlib_map:compact(#{
            <<"ip">> => IP,
            <<"fingerprint">> => Fingerprint
        }),
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email">> => Email,
            <<"firstName">> => FirstName,
            <<"lastName">> => LastName,
            <<"country">> => Country,
            <<"state">> => State,
            <<"city">> => City,
            <<"address">> => Address,
            <<"postalCode">> => PostalCode,
            <<"dateOfBirth">> => DateOfBirth,
            <<"documentId">> => DocumentId
        })
    }).

decode_stat_payment_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};
decode_stat_payment_flow(
    {hold, #magista_InvoicePaymentFlowHold{
        on_hold_expiration = OnHoldExpiration,
        held_until = HeldUntil
    }}
) ->
    #{
        <<"type">> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil">> => HeldUntil
    }.

decode_stat_payment_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentFailed{failure = OperationFailure} ->
                capi_handler_decoder_invoicing:decode_payment_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

decode_stat_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_stat_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_stat_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>});
decode_stat_payment_tool_details({crypto_currency, CryptoCurrency}) ->
    #{
        <<"detailsType">> => <<"PaymentToolDetailsCryptoWallet">>,
        <<"cryptoCurrency">> => decode_crypto_wallet_details(CryptoCurrency)
    };
decode_stat_payment_tool_details({mobile_commerce, MobileCommerce}) ->
    #domain_MobileCommerce{
        phone = Phone
    } = MobileCommerce,
    PhoneNumber = gen_phone_number(decode_mobile_phone(Phone)),
    #{
        <<"detailsType">> => <<"PaymentToolDetailsMobileCommerce">>,
        <<"phoneNumber">> => mask_phone_number(PhoneNumber)
    }.

decode_bank_card_details(BankCard, V) ->
    LastDigits = BankCard#domain_BankCard.last_digits,
    Bin = capi_handler_decoder_utils:decode_bank_card_bin(BankCard#domain_BankCard.bin),
    PaymentSystem = capi_handler_decoder_utils:decode_payment_system_ref(BankCard#domain_BankCard.payment_system),
    BankCardTokenServiceRef = capi_utils:'maybe'(
        BankCard#domain_BankCard.payment_token,
        fun capi_handler_decoder_utils:decode_bank_card_token_service_ref/1
    ),
    capi_handler_utils:merge_and_compact(V, #{
        <<"last4">> => LastDigits,
        <<"first6">> => Bin,
        <<"cardNumberMask">> => capi_handler_decoder_utils:decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem">> => PaymentSystem,
        <<"tokenProvider">> => BankCardTokenServiceRef
    }).

decode_payment_terminal_details(
    #domain_PaymentTerminal{payment_service = #domain_PaymentServiceRef{id = Provider}},
    V
) ->
    V#{
        <<"provider">> => Provider
    }.

decode_digital_wallet_details(#domain_DigitalWallet{payment_service = #domain_PaymentServiceRef{id = Provider}}, V) ->
    V#{
        <<"provider">> => Provider
    }.

decode_crypto_wallet_details(#domain_CryptoCurrencyRef{id = Provider}) ->
    Provider.

mask_phone_number(PhoneNumber) ->
    capi_utils:redact(PhoneNumber, <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>).

decode_mobile_phone(#domain_MobilePhone{cc = Cc, ctn = Ctn}) ->
    #{<<"cc">> => Cc, <<"ctn">> => Ctn}.

gen_phone_number(#{<<"cc">> := Cc, <<"ctn">> := Ctn}) ->
    <<"+", Cc/binary, Ctn/binary>>.

build_prototypes(OperationID, Context, Req) ->
    InvoiceID = genlib_map:get('invoiceID', Req),
    CustomerID = genlib_map:get('customerID', Req),
    [
        {operation, #{
            id => OperationID,
            party => capi_handler_utils:get_party_id(Context),
            shop => genlib_map:get('shopID', Req),
            invoice => InvoiceID,
            payment => genlib_map:get('paymentID', Req),
            customer => CustomerID,
            refund => genlib_map:get('refundID', Req)
        }},
        {payproc, #{
            invoice => InvoiceID,
            customer => CustomerID
        }}
    ].
