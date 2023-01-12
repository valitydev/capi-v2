-module(capi_handler_decoder_invoicing).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

-export([decode_user_interaction_form/1]).
-export([decode_user_interaction/1]).
-export([decode_invoice_payment/3]).
-export([decode_payment/3]).
-export([decode_chargeback/2]).
-export([decode_refund/1]).
-export([decode_invoice/1]).
-export([decode_invoice_status/1]).
-export([decode_invoice_cart/1]).
-export([decode_invoice_bank_account/1]).
-export([decode_invoice_line_tax_mode/1]).
-export([decode_payment_tool/1]).
-export([decode_disposable_payment_resource/1]).
-export([decode_payment_methods/1]).
-export([decode_payment_status/2]).
-export([decode_payment_operation_failure/2]).
-export([decode_refund_status/1]).
-export([decode_recurrent_parent/1]).
-export([decode_make_recurrent/1]).

-export([make_invoice_and_token/2]).

-type processing_context() :: capi_handler:processing_context().
-type decode_data() :: capi_handler_decoder_utils:decode_data().

%%

-spec decode_user_interaction({atom(), _}) -> decode_data().
decode_user_interaction({payment_terminal_reciept, TerminalReceipt}) ->
    #{
        <<"interactionType">> => <<"PaymentTerminalReceipt">>,
        <<"shortPaymentID">> => TerminalReceipt#user_interaction_PaymentTerminalReceipt.short_payment_id,
        <<"dueDate">> => TerminalReceipt#user_interaction_PaymentTerminalReceipt.due
    };
decode_user_interaction({redirect, BrowserRequest}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => decode_browser_request(BrowserRequest)
    };
decode_user_interaction({qr_code_display_request, QrCodeDisplayRequest}) ->
    #{
        <<"interactionType">> => <<"QrCodeDisplayRequest">>,
        <<"qrCode">> => decode_qr_code(QrCodeDisplayRequest)
    };
decode_user_interaction({crypto_currency_transfer_request, CryptoCurrencyTransferRequest}) ->
    #{
        <<"interactionType">> => <<"CryptoCurrencyTransferRequest">>,
        <<"cryptoAddress">> =>
            CryptoCurrencyTransferRequest#'user_interaction_CryptoCurrencyTransferRequest'.crypto_address,
        <<"symbolicCode">> => decode_crypto_symcode(CryptoCurrencyTransferRequest),
        <<"cryptoAmount">> => decode_crypto_amount(CryptoCurrencyTransferRequest)
    }.

decode_browser_request({get_request, #'user_interaction_BrowserGetRequest'{uri = UriTemplate}}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => UriTemplate
    };
decode_browser_request(
    {post_request, #'user_interaction_BrowserPostRequest'{uri = UriTemplate, form = UserInteractionForm}}
) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => UriTemplate,
        <<"form">> => decode_user_interaction_form(UserInteractionForm)
    }.

decode_qr_code(#'user_interaction_QrCodeDisplayRequest'{qr_code = QrCode}) ->
    QrCode#'user_interaction_QrCode'.payload.

decode_crypto_symcode(#'user_interaction_CryptoCurrencyTransferRequest'{crypto_cash = Cash}) ->
    Cash#'user_interaction_CryptoCash'.crypto_symbolic_code.

decode_crypto_amount(#'user_interaction_CryptoCurrencyTransferRequest'{crypto_cash = Cash}) ->
    % apparently Q is always a power of ten
    Amount = Cash#'user_interaction_CryptoCash'.crypto_amount,
    ok = ensure_correct_exponent(Amount),
    Integral = decode_integral_part(Amount),
    Fractional = decode_fractional_part(Amount),
    build_decoded_crypto_amount(Integral, Fractional).

ensure_correct_exponent(#base_Rational{q = Q}) ->
    Log = math:log10(Q),
    case Log - trunc(Log) of
        0.0 -> ok;
        _ -> error('expected a power of 10 denominator')
    end.

decode_integral_part(#base_Rational{p = P, q = Q}) ->
    erlang:integer_to_binary(P div Q).

decode_fractional_part(#base_Rational{p = P, q = Q}) ->
    Exponent = get_exponent(Q),
    build_fractional(P rem Q, Exponent).

get_exponent(Q) ->
    erlang:trunc(math:log10(Q)).

build_fractional(_Fractional, _Exponent = 0) ->
    <<>>;
build_fractional(Fractional, Exponent) ->
    BinaryFractional = erlang:integer_to_binary(Fractional),
    strip_trailing_zeroes(genlib_string:pad_numeric(BinaryFractional, Exponent)).

strip_trailing_zeroes(Fractional) ->
    ByteSize = byte_size(Fractional) - 1,
    case Fractional of
        <<Prefix:ByteSize/bytes, "0">> -> strip_trailing_zeroes(Prefix);
        Fractional -> Fractional
    end.

build_decoded_crypto_amount(Integral, <<>>) ->
    Integral;
build_decoded_crypto_amount(Integral, Fractional) ->
    <<Integral/binary, ".", Fractional/binary>>.

-spec decode_user_interaction_form(map()) -> decode_data().
decode_user_interaction_form(Form) ->
    maps:fold(
        fun(K, V, Acc) ->
            F = #{
                <<"key">> => K,
                <<"template">> => V
            },
            [F | Acc]
        end,
        [],
        Form
    ).

-spec decode_payment(binary(), capi_handler_encoder:encode_data(), processing_context()) ->
    decode_data().
decode_payment(InvoiceID, Payment, Context) ->
    #domain_Cash{
        amount = Amount,
        currency = Currency
    } = Payment#domain_InvoicePayment.cost,
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Payment#domain_InvoicePayment.id,
            <<"externalID">> => Payment#domain_InvoicePayment.external_id,
            <<"invoiceID">> => InvoiceID,
            <<"createdAt">> => Payment#domain_InvoicePayment.created_at,
            <<"flow">> => decode_flow(Payment#domain_InvoicePayment.flow),
            <<"amount">> => Amount,
            <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
            <<"payer">> => maps:merge(
                decode_payer(Payment#domain_InvoicePayment.payer),
                decode_payer_session_info(Payment#domain_InvoicePayment.payer_session_info)
            ),
            <<"makeRecurrent">> => decode_make_recurrent(Payment#domain_InvoicePayment.make_recurrent),
            <<"metadata">> => capi_handler_decoder_utils:decode_context(Payment#domain_InvoicePayment.context)
        },
        decode_payment_status(Payment#domain_InvoicePayment.status, Context)
    ).

-spec decode_invoice_payment(binary(), capi_handler_encoder:encode_data(), processing_context()) ->
    decode_data().
decode_invoice_payment(InvoiceID, InvoicePayment = #payproc_InvoicePayment{payment = Payment}, Context) ->
    capi_handler_utils:merge_and_compact(
        decode_payment(InvoiceID, Payment, Context),
        #{
            <<"transactionInfo">> => decode_last_tx_info(InvoicePayment#payproc_InvoicePayment.last_transaction_info),
            <<"allocation">> => capi_allocation:decode(InvoicePayment#payproc_InvoicePayment.allocation)
        }
    ).

decode_last_tx_info(undefined) ->
    undefined;
decode_last_tx_info(TransactionInfo) ->
    decode_additional_tx_info(TransactionInfo#domain_TransactionInfo.additional_info).

decode_additional_tx_info(undefined) ->
    undefined;
decode_additional_tx_info(AdditionalTransactionInfo) ->
    genlib_map:compact(#{
        <<"rrn">> => AdditionalTransactionInfo#domain_AdditionalTransactionInfo.rrn,
        <<"approvalCode">> => AdditionalTransactionInfo#domain_AdditionalTransactionInfo.approval_code,
        <<"extra_payment_info">> => AdditionalTransactionInfo#domain_AdditionalTransactionInfo.extra_payment_info
    }).

decode_payer(
    {customer, #domain_CustomerPayer{
        payment_tool = PaymentTool,
        customer_id = ID
    }}
) ->
    #{
        <<"payerType">> => <<"CustomerPayer">>,
        <<"customerID">> => ID,
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool)
    };
decode_payer(
    {recurrent, #domain_RecurrentPayer{
        payment_tool = PaymentTool,
        recurrent_parent = RecurrentParent,
        contact_info = ContactInfo
    }}
) ->
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"contactInfo">> => capi_handler_decoder_party:decode_contact_info(ContactInfo),
        <<"recurrentParentPayment">> => decode_recurrent_parent(RecurrentParent)
    };
decode_payer(
    {payment_resource, #domain_PaymentResourcePayer{
        resource = Resource,
        contact_info = ContactInfo
    }}
) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"payerType">> => <<"PaymentResourcePayer">>,
            <<"contactInfo">> => capi_handler_decoder_party:decode_contact_info(ContactInfo)
        },
        decode_disposable_payment_resource(Resource)
    ).

-spec decode_payment_tool(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_payment_tool({bank_card, BankCard}) ->
    decode_bank_card(BankCard);
decode_payment_tool({payment_terminal, PaymentTerminal}) ->
    decode_payment_terminal(PaymentTerminal);
decode_payment_tool({digital_wallet, DigitalWallet}) ->
    decode_digital_wallet(DigitalWallet);
decode_payment_tool({mobile_commerce, MobileCommerce}) ->
    decode_mobile_commerce(MobileCommerce);
decode_payment_tool({crypto_currency, CryptoCurrency}) ->
    decode_crypto_wallet(CryptoCurrency).

decode_bank_card(#domain_BankCard{
    'token' = Token,
    'payment_system' = PaymentSystem,
    'exp_date' = ExpDate
}) ->
    genlib_map:compact(#{
        <<"type">> => <<"bank_card">>,
        <<"token">> => Token,
        <<"payment_system">> => capi_handler_decoder_utils:decode_payment_system_ref(PaymentSystem),
        <<"exp_date">> => ExpDate
    }).

decode_payment_terminal(#domain_PaymentTerminal{payment_service = PaymentService}) ->
    #{
        <<"type">> => <<"payment_terminal">>,
        <<"terminal_type">> => capi_handler_decoder_utils:decode_payment_service_ref(PaymentService)
    }.

decode_digital_wallet(#domain_DigitalWallet{
    payment_service = PaymentService,
    id = ID,
    token = Token
}) ->
    genlib_map:compact(#{
        <<"type">> => <<"digital_wallet">>,
        <<"provider">> => capi_handler_decoder_utils:decode_payment_service_ref(PaymentService),
        <<"id">> => ID,
        <<"token">> => Token
    }).

decode_crypto_wallet(CryptoCurrency) ->
    #{
        <<"type">> => <<"crypto_wallet">>,
        <<"crypto_currency">> => capi_handler_decoder_utils:decode_crypto_currency_ref(CryptoCurrency)
    }.

decode_mobile_commerce(MobileCommerce) ->
    #domain_MobileCommerce{
        operator = MobileOperator,
        phone = #domain_MobilePhone{
            cc = Cc,
            ctn = Ctn
        }
    } = MobileCommerce,
    Phone = #{<<"cc">> => Cc, <<"ctn">> => Ctn},
    #{
        <<"type">> => <<"mobile_commerce">>,
        <<"phone">> => Phone,
        <<"operator">> => capi_handler_decoder_utils:decode_mobile_operator_ref(MobileOperator)
    }.

-spec decode_disposable_payment_resource(capi_handler_encoder:encode_data()) ->
    capi_handler_decoder_utils:decode_data().
decode_disposable_payment_resource(#domain_DisposablePaymentResource{
    payment_tool = PaymentTool,
    client_info = ClientInfo
}) ->
    #{
        <<"paymentToolDetails">> => decode_payment_tool_details(PaymentTool),
        <<"clientInfo">> => decode_client_info(ClientInfo)
    }.

decode_client_info(undefined) ->
    undefined;
decode_client_info(ClientInfo) ->
    #{
        <<"fingerprint">> => ClientInfo#domain_ClientInfo.fingerprint,
        <<"ip">> => ClientInfo#domain_ClientInfo.ip_address
    }.

decode_payer_session_info(#domain_PayerSessionInfo{
    redirect_url = RedirectURL
}) ->
    #{
        <<"sessionInfo">> => genlib_map:compact(#{
            <<"redirectUrl">> => RedirectURL
        })
    };
decode_payer_session_info(undefined) ->
    #{}.

-spec decode_payment_status({atom(), _}, processing_context()) -> decode_data().
decode_payment_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentFailed{failure = OperationFailure} ->
                decode_payment_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

-spec decode_payment_operation_failure({atom(), _}, processing_context()) -> decode_data().
decode_payment_operation_failure({operation_timeout, _}, _) ->
    payment_error(<<"timeout">>);
decode_payment_operation_failure({failure, Failure}, Context) ->
    AuthContext = capi_handler_utils:get_auth_context(Context),
    case capi_auth:get_consumer(AuthContext) of
        client ->
            payment_error(payproc_errors:match('PaymentFailure', Failure, fun payment_error_client_maping/1));
        merchant ->
            % чтобы не городить ещё один обход дерева как в payproc_errors проще отформатировать в текст,
            % а потом уже в json
            decode_payment_operation_failure_(
                binary:split(erlang:list_to_binary(payproc_errors:format_raw(Failure)), <<":">>, [global])
            )
    end.

decode_payment_operation_failure_([H | T]) ->
    R = payment_error(H),
    case T of
        [] -> R;
        _ -> R#{<<"subError">> => decode_payment_operation_failure_(T)}
    end.

decode_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};
decode_flow({hold, #domain_InvoicePaymentFlowHold{on_hold_expiration = OnHoldExpiration, held_until = HeldUntil}}) ->
    #{
        <<"type">> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil">> => HeldUntil
    }.

-spec decode_make_recurrent(undefined | boolean()) -> boolean().
decode_make_recurrent(undefined) ->
    false;
decode_make_recurrent(Value) when is_boolean(Value) ->
    Value.

-spec decode_recurrent_parent(capi_handler_encoder:encode_data()) -> decode_data().
decode_recurrent_parent(#domain_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    }.

payment_error(Code) ->
    #{<<"code">> => Code}.

%% client error mapping
%% @see https://github.com/petrkozorezov/swag/blob/master/spec/definitions/PaymentError.yaml
-spec payment_error_client_maping(capi_handler_encoder:encode_data()) -> binary().
payment_error_client_maping({preauthorization_failed, _}) ->
    <<"PreauthorizationFailed">>;
payment_error_client_maping({authorization_failed, {account_blocked, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {rejected_by_issuer, _}}) ->
    <<"RejectedByIssuer">>;
payment_error_client_maping({authorization_failed, {payment_tool_rejected, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_not_found, _}}) ->
    <<"InvalidPaymentTool">>;
payment_error_client_maping({authorization_failed, {account_limit_exceeded, _}}) ->
    <<"AccountLimitsExceeded">>;
payment_error_client_maping({authorization_failed, {insufficient_funds, _}}) ->
    <<"InsufficientFunds">>;
payment_error_client_maping(_) ->
    <<"PaymentRejected">>.

-spec decode_refund(dmsl_domain_thrift:'InvoicePaymentRefund'()) -> decode_data().
decode_refund(Refund) ->
    #domain_Cash{amount = Amount, currency = Currency} = Refund#domain_InvoicePaymentRefund.cash,
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Refund#domain_InvoicePaymentRefund.id,
            <<"createdAt">> => Refund#domain_InvoicePaymentRefund.created_at,
            <<"reason">> => Refund#domain_InvoicePaymentRefund.reason,
            <<"amount">> => Amount,
            <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
            <<"externalID">> => Refund#domain_InvoicePaymentRefund.external_id,
            <<"cart">> => decode_invoice_cart(Refund#domain_InvoicePaymentRefund.cart),
            <<"allocation">> => capi_allocation:decode(Refund#domain_InvoicePaymentRefund.allocation)
        },
        decode_refund_status(Refund#domain_InvoicePaymentRefund.status)
    ).

-spec decode_refund_status(dmsl_domain_thrift:'InvoicePaymentRefundStatus'()) -> decode_data().
decode_refund_status({Status, StatusInfo}) ->
    Error =
        case StatusInfo of
            #domain_InvoicePaymentRefundFailed{failure = OperationFailure} ->
                capi_handler_decoder_utils:decode_operation_failure(OperationFailure);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error">> => Error
    }.

-spec decode_chargeback(capi_handler_encoder:encode_data(), processing_context()) ->
    decode_data().
decode_chargeback(#payproc_InvoicePaymentChargeback{chargeback = Chargeback}, Context) ->
    decode_chargeback(Chargeback, Context);
decode_chargeback(#domain_InvoicePaymentChargeback{} = Chargeback, _Context) ->
    #domain_Cash{amount = Body, currency = Currency} = Chargeback#domain_InvoicePaymentChargeback.body,
    #domain_Cash{amount = Levy, currency = Currency} = Chargeback#domain_InvoicePaymentChargeback.levy,
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Chargeback#domain_InvoicePaymentChargeback.id,
            <<"createdAt">> => Chargeback#domain_InvoicePaymentChargeback.created_at,
            <<"status">> => decode_chargeback_status(Chargeback#domain_InvoicePaymentChargeback.status),
            <<"stage">> => decode_chargeback_stage(Chargeback#domain_InvoicePaymentChargeback.stage),
            <<"body">> => Body,
            <<"levy">> => Levy,
            <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)
        },
        decode_chargeback_reason_code(Chargeback#domain_InvoicePaymentChargeback.reason)
    ).

decode_chargeback_status({pending, _StatusDetails}) ->
    <<"pending">>;
decode_chargeback_status({rejected, _StatusDetails}) ->
    <<"rejected">>;
decode_chargeback_status({accepted, _StatusDetails}) ->
    <<"accepted">>;
decode_chargeback_status({cancelled, _StatusDetails}) ->
    <<"cancelled">>.

decode_chargeback_stage({chargeback, _StageDetails}) ->
    <<"chargeback">>;
decode_chargeback_stage({pre_arbitration, _StageDetails}) ->
    <<"pre-arbitration">>;
decode_chargeback_stage({arbitration, _StageDetails}) ->
    <<"arbitration">>.

decode_chargeback_reason_code(#domain_InvoicePaymentChargebackReason{code = Code}) ->
    #{<<"reasonCode">> => Code}.

-spec decode_invoice(dmsl_domain_thrift:'Invoice'()) -> decode_data().
decode_invoice(Invoice) ->
    #domain_Cash{amount = Amount, currency = Currency} = Invoice#domain_Invoice.cost,
    Details = Invoice#domain_Invoice.details,
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Invoice#domain_Invoice.id,
            <<"externalID">> => Invoice#domain_Invoice.external_id,
            <<"shopID">> => Invoice#domain_Invoice.shop_id,
            <<"createdAt">> => Invoice#domain_Invoice.created_at,
            <<"dueDate">> => Invoice#domain_Invoice.due,
            <<"amount">> => Amount,
            <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
            <<"metadata">> => capi_handler_decoder_utils:decode_context(Invoice#domain_Invoice.context),
            <<"product">> => Details#domain_InvoiceDetails.product,
            <<"description">> => Details#domain_InvoiceDetails.description,
            <<"cart">> => decode_invoice_cart(Details#domain_InvoiceDetails.cart),
            <<"bankAccount">> => decode_invoice_bank_account(Details#domain_InvoiceDetails.bank_account),
            <<"invoiceTemplateID">> => Invoice#domain_Invoice.template_id,
            <<"allocation">> => capi_allocation:decode(Invoice#domain_Invoice.allocation)
        },
        decode_invoice_status(Invoice#domain_Invoice.status)
    ).

-spec decode_invoice_status(dmsl_domain_thrift:'InvoiceStatus'()) -> decode_data().
decode_invoice_status({Status, StatusInfo}) ->
    Reason =
        case StatusInfo of
            #domain_InvoiceCancelled{details = Details} -> Details;
            #domain_InvoiceFulfilled{details = Details} -> Details;
            _ -> undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"reason">> => Reason
    }.

-spec decode_invoice_cart(capi_handler_encoder:encode_data() | undefined) ->
    [capi_handler_decoder_utils:decode_data()] | undefined.
decode_invoice_cart(#domain_InvoiceCart{lines = Lines}) ->
    [decode_invoice_line(L) || L <- Lines];
decode_invoice_cart(undefined) ->
    undefined.

decode_invoice_line(InvoiceLine = #domain_InvoiceLine{quantity = Quantity, price = #domain_Cash{amount = Price}}) ->
    genlib_map:compact(#{
        <<"product">> => InvoiceLine#domain_InvoiceLine.product,
        <<"quantity">> => Quantity,
        <<"price">> => Price,
        <<"cost">> => Price * Quantity,
        <<"taxMode">> => decode_invoice_line_tax_mode(InvoiceLine#domain_InvoiceLine.metadata)
    }).

-spec decode_invoice_bank_account(dmsl_domain_thrift:'InvoiceBankAccount'() | undefined) ->
    decode_data() | undefined.
decode_invoice_bank_account({russian, Russian}) ->
    genlib_map:compact(#{
        <<"accountType">> => <<"InvoiceRussianBankAccount">>,
        <<"account">> => Russian#domain_InvoiceRussianBankAccount.account,
        <<"bankBik">> => Russian#domain_InvoiceRussianBankAccount.bank_bik
    });
decode_invoice_bank_account(undefined) ->
    undefined.

-spec decode_invoice_line_tax_mode(map()) -> decode_data() | undefined.
decode_invoice_line_tax_mode(#{<<"TaxMode">> := {str, TM}}) ->
    #{
        <<"type">> => <<"InvoiceLineTaxVAT">>,
        <<"rate">> => TM
    };
decode_invoice_line_tax_mode(_) ->
    undefined.

-spec decode_payment_methods(undefined | list()) -> list(decode_data()).
decode_payment_methods(undefined) ->
    [];
decode_payment_methods(PaymentMethodRefs) ->
    PaymentMethods = [ID || #domain_PaymentMethodRef{id = ID} <- PaymentMethodRefs],
    lists:foldl(
        fun(Method, Acc) ->
            {_, MethodTerms} = lists:unzip(proplists:lookup_all(Method, PaymentMethods)),
            decode_payment_method(Method, MethodTerms) ++ Acc
        end,
        [],
        proplists:get_keys(PaymentMethods)
    ).

decode_payment_method(bank_card, Cards) ->
    {Regular, Tokenized} =
        lists:partition(
            fun(#domain_BankCardPaymentMethod{payment_token = TP}) -> TP =:= undefined end,
            Cards
        ),
    [
        #{
            <<"method">> => <<"BankCard">>,
            <<"paymentSystems">> => lists:map(fun decode_bank_card_method/1, Regular)
        }
        | decode_tokenized_bank_cards(Tokenized)
    ];
decode_payment_method(payment_terminal, Providers) ->
    [
        #{
            <<"method">> => <<"PaymentTerminal">>,
            <<"providers">> => [
                capi_handler_decoder_utils:decode_payment_service_ref(Provider)
             || Provider <- Providers
            ]
        }
    ];
decode_payment_method(digital_wallet, Providers) ->
    [
        #{
            <<"method">> => <<"DigitalWallet">>,
            <<"providers">> => [
                capi_handler_decoder_utils:decode_payment_service_ref(Provider)
             || Provider <- Providers
            ]
        }
    ];
decode_payment_method(crypto_currency, CryptoCurrencies) ->
    [
        #{
            <<"method">> => <<"CryptoWallet">>,
            <<"cryptoCurrencies">> => [
                capi_handler_decoder_utils:decode_crypto_currency_ref(Currency)
             || Currency <- CryptoCurrencies
            ]
        }
    ];
decode_payment_method(mobile, MobileOperators) ->
    [
        #{
            <<"method">> => <<"MobileCommerce">>,
            <<"operators">> => [
                capi_handler_decoder_utils:decode_mobile_operator_ref(Operator)
             || Operator <- MobileOperators
            ]
        }
    ].

decode_bank_card_method(#domain_BankCardPaymentMethod{payment_system = PS}) ->
    capi_handler_decoder_utils:decode_payment_system_ref(PS).

decode_tokenized_bank_cards([#domain_BankCardPaymentMethod{} | _] = TokenizedBankCards) ->
    PropTokenizedBankCards = [
        {TP, PS}
     || #domain_BankCardPaymentMethod{payment_system = PS, payment_token = TP} <-
            TokenizedBankCards
    ],
    do_decode_tokenized_bank_cards(PropTokenizedBankCards);
decode_tokenized_bank_cards([]) ->
    [].

do_decode_tokenized_bank_cards(PropTokenizedBankCards) ->
    lists:map(
        fun(TokenProvider) ->
            {_, PaymentSystems} = lists:unzip(proplists:lookup_all(TokenProvider, PropTokenizedBankCards)),
            decode_tokenized_bank_card(TokenProvider, PaymentSystems)
        end,
        proplists:get_keys(PropTokenizedBankCards)
    ).

decode_tokenized_bank_card(TokenProvider, PaymentSystems) ->
    #{
        <<"method">> => <<"BankCard">>,
        <<"paymentSystems">> =>
            lists:map(fun capi_handler_decoder_utils:decode_payment_system_ref/1, PaymentSystems),
        <<"tokenProviders">> =>
            [capi_handler_decoder_utils:decode_bank_card_token_service_ref(TokenProvider)]
    }.

-spec decode_payment_tool_details(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>});
decode_payment_tool_details({crypto_currency, #domain_CryptoCurrencyRef{id = CryptoCurrency}}) ->
    #{
        <<"detailsType">> => <<"PaymentToolDetailsCryptoWallet">>,
        <<"cryptoCurrency">> => CryptoCurrency
    };
decode_payment_tool_details({mobile_commerce, MobileCommerce}) ->
    #domain_MobileCommerce{
        phone = Phone
    } = MobileCommerce,
    PhoneNumber = gen_phone_number(decode_mobile_phone(Phone)),
    #{
        <<"detailsType">> => <<"PaymentToolDetailsMobileCommerce">>,
        <<"phoneNumber">> => mask_phone_number(PhoneNumber)
    }.

mask_phone_number(PhoneNumber) ->
    capi_utils:redact(PhoneNumber, <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>).

decode_bank_card_details(BankCard, V) ->
    LastDigits = capi_handler_decoder_utils:decode_last_digits(BankCard#domain_BankCard.last_digits),
    Bin = capi_handler_decoder_utils:decode_bank_card_bin(BankCard#domain_BankCard.bin),
    PaymentSystem = BankCard#domain_BankCard.payment_system,
    TokenProvider = BankCard#domain_BankCard.payment_token,
    capi_handler_utils:merge_and_compact(V, #{
        <<"last4">> => LastDigits,
        <<"first6">> => Bin,
        <<"cardNumberMask">> => capi_handler_decoder_utils:decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem">> => capi_handler_decoder_utils:decode_payment_system_ref(PaymentSystem),
        <<"tokenProvider">> => capi_utils:maybe(
            TokenProvider,
            fun capi_handler_decoder_utils:decode_bank_card_token_service_ref/1
        )
        % TODO: Uncomment or delete this when we negotiate deploying non-breaking changes
        % <<"tokenization_method">> => TokenizationMethod
    }).

decode_payment_terminal_details(
    #domain_PaymentTerminal{
        payment_service = PaymentService
    },
    V
) ->
    V#{
        <<"provider">> => capi_handler_decoder_utils:decode_payment_service_ref(PaymentService)
    }.

decode_digital_wallet_details(#domain_DigitalWallet{payment_service = Provider}, V) ->
    V#{
        <<"provider">> => Provider#domain_PaymentServiceRef.id
    }.

decode_mobile_phone(#domain_MobilePhone{cc = Cc, ctn = Ctn}) ->
    #{<<"cc">> => Cc, <<"ctn">> => Ctn}.

gen_phone_number(#{<<"cc">> := Cc, <<"ctn">> := Ctn}) ->
    <<"+", Cc/binary, Ctn/binary>>.

-spec make_invoice_and_token(capi_handler_encoder:encode_data(), processing_context()) ->
    capi_handler_decoder_utils:decode_data().
make_invoice_and_token(Invoice, ProcessingContext) ->
    #{
        <<"invoice">> => decode_invoice(Invoice),
        <<"invoiceAccessToken">> => capi_handler_utils:issue_access_token(Invoice, ProcessingContext)
    }.

%%

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec crypto_amount_decoder_test() -> _.
crypto_amount_decoder_test() ->
    ?assertError('expected a power of 10 denominator', decode_crypto_amount(build_request(1, 2))),
    ?assertEqual(<<"1100000007">>, decode_crypto_amount(build_request(1100000007, 1))),
    ?assertEqual(<<"1">>, decode_crypto_amount(build_request(100000000, 100000000))),
    ?assertEqual(<<"1.1">>, decode_crypto_amount(build_request(110000000, 100000000))),
    ?assertEqual(<<"11.00000007">>, decode_crypto_amount(build_request(1100000007, 100000000))),
    ?assertEqual(<<"0.11000007">>, decode_crypto_amount(build_request(11000007, 100000000))),
    ?assertEqual(<<"0.110007">>, decode_crypto_amount(build_request(11000700, 100000000))).

build_request(P, Q) ->
    Amount = #base_Rational{p = P, q = Q},
    Cash = #'user_interaction_CryptoCash'{crypto_amount = Amount, crypto_symbolic_code = <<>>},
    #'user_interaction_CryptoCurrencyTransferRequest'{crypto_address = <<>>, crypto_cash = Cash}.

-spec mask_phone_number_test_() -> [_TestCase].
mask_phone_number_test_() ->
    [
        ?_assertEqual(<<"+7******7890">>, mask_phone_number(<<"+71234567890">>)),
        ?_assertEqual(<<"+7*23">>, mask_phone_number(<<"+7123">>)),
        ?_assertEqual(<<"+1NOTANUMBER">>, mask_phone_number(<<"+1NOTANUMBER">>))
    ].

-endif.
