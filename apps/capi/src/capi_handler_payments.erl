-module(capi_handler_payments).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/1, logic_error/2, conflict_error/1]).

-define(DEFAULT_PROCESSING_DEADLINE, <<"30m">>).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreatePayment' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    PaymentParams = maps:get('PaymentParams', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        try
            capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
            DomainInvoice = Invoice#payproc_Invoice.invoice,
            Result = create_payment(DomainInvoice, PaymentParams, Context, OperationID),
            case Result of
                {ok, Payment} ->
                    {ok, {201, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
                {exception, #payproc_InvalidInvoiceStatus{}} ->
                    {ok, logic_error('invalidInvoiceStatus', <<"Invalid invoice status">>)};
                {exception, #payproc_InvoicePaymentPending{}} ->
                    {ok, logic_error('invoicePaymentPending', <<"Invoice payment pending">>)};
                {exception, #base_InvalidRequest{errors = Errors}} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error('invalidRequest', FormattedErrors)};
                {exception, #payproc_InvalidPartyStatus{}} ->
                    {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
                {exception, #payproc_InvalidShopStatus{}} ->
                    {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
                {exception, #payproc_InvalidContractStatus{}} ->
                    {ok, logic_error('invalidContractStatus', <<"Invalid contract status">>)};
                {exception, #payproc_InvalidRecurrentParentPayment{}} ->
                    {ok, logic_error('invalidRecurrentParent', <<"Specified recurrent parent is invalid">>)};
                {exception, #payproc_InvoiceNotFound{}} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
        catch
            throw:invalid_payment_session ->
                {ok, logic_error('invalidPaymentSession', <<"Specified payment session is invalid">>)};
            throw:invalid_processing_deadline ->
                {ok, logic_error('invalidProcessingDeadline', <<"Specified processing deadline is invalid">>)};
            throw:{external_id_conflict, PaymentID, ExternalID, _Schema} ->
                {ok, conflict_error({PaymentID, ExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetPayments' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        #'payproc_Invoice'{payments = Payments} = Invoice,
        {ok, {200, #{}, [decode_invoice_payment(InvoiceID, P, Context) || P <- Payments]}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetPaymentByID' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        case find_payment_by_id(PaymentID, Invoice) of
            Payment when Payment /= undefined ->
                {ok, {200, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
            undefined ->
                {ok, general_error(404, <<"Payment not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, #{'partyID' := PartyID, 'externalID' := ExternalID}, Context) when
    OperationID =:= 'GetPaymentByExternalID';
    OperationID =:= 'GetPaymentByExternalIDForParty'
->
    InternalID = get_payment_by_external_id(PartyID, ExternalID, Context),
    Invoice = 'maybe'(
        InternalID,
        fun({InvoiceID, _}) -> get_invoice_by_id(InvoiceID, Context) end
    ),

    OperationPrototype = 'maybe'(
        InternalID,
        fun({InvoiceID, PaymentID}) ->
            #{id => OperationID, invoice => InvoiceID, payment => PaymentID}
        end
    ),

    Authorize = fun() ->
        Prototypes = [
            {operation, genlib:define(OperationPrototype, #{id => OperationID})},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, mask_payment_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(InternalID, general_error(404, <<"Payment not found">>)),
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        {InvoiceID, PaymentID} = InternalID,
        case find_payment_by_id(PaymentID, Invoice) of
            Payment when Payment /= undefined ->
                {ok, {200, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
            undefined ->
                {ok, general_error(404, <<"Payment not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetPaymentByExternalID' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Req1 = maps:put('partyID', PartyID, Req),
    prepare(OperationID, Req1, Context);
prepare('CapturePayment' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    PartyID = Invoice#payproc_Invoice.invoice#domain_Invoice.owner_id,
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Params = maps:get('CaptureParams', Req),
        try
            Allocation = maps:get(<<"allocation">>, Params, undefined),
            ok = validate_allocation(Allocation),
            CaptureParams = #payproc_InvoicePaymentCaptureParams{
                reason = maps:get(<<"reason">>, Params),
                cash = encode_optional_cash(Params, InvoiceID, PaymentID, Context),
                cart = capi_handler_encoder:encode_invoice_cart(Params),
                allocation = capi_allocation:encode(Allocation, PartyID)
            },
            CallArgs = {InvoiceID, PaymentID, CaptureParams},
            Call = {invoicing, 'CapturePayment', CallArgs},
            capi_handler_utils:service_call(Call, Context)
        of
            {ok, _} ->
                {ok, {202, #{}, undefined}};
            {exception, #payproc_InvoicePaymentNotFound{}} ->
                {ok, general_error(404, <<"Payment not found">>)};
            {exception, #payproc_InvalidPaymentStatus{}} ->
                {ok, logic_error('invalidPaymentStatus', <<"Invalid payment status">>)};
            {exception, #payproc_InvoiceNotFound{}} ->
                {ok, general_error(404, <<"Invoice not found">>)};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)};
            {exception, #payproc_OperationNotPermitted{}} ->
                {ok, logic_error('operationNotPermitted', <<"Operation not permitted">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
            {exception, #payproc_InconsistentCaptureCurrency{payment_currency = PaymentCurrency}} ->
                {ok,
                    logic_error(
                        'inconsistentCaptureCurrency',
                        io_lib:format("Correct currency: ~p", [PaymentCurrency])
                    )};
            {exception, #payproc_AmountExceededCaptureBalance{payment_amount = PaymentAmount}} ->
                {ok,
                    logic_error(
                        'amountExceededCaptureBalance',
                        io_lib:format("Max amount: ~p", [PaymentAmount])
                    )};
            {exception, #payproc_AllocationNotAllowed{}} ->
                {ok, logic_error('allocationNotPermitted', <<"Not allowed">>)};
            {exception, #payproc_AllocationExceededPaymentAmount{}} ->
                {ok, logic_error('invalidAllocation', <<"Exceeded payment amount">>)};
            {exception, #payproc_AllocationInvalidTransaction{} = InvalidTransaction} ->
                Message = capi_allocation:transaction_error(InvalidTransaction),
                {ok, logic_error('invalidAllocation', Message)}
        catch
            throw:invoice_cart_empty ->
                {ok, logic_error('invalidInvoiceCart', <<"Wrong size. Path to item: cart">>)};
            throw:allocation_wrong_cart ->
                {ok, logic_error('invalidAllocation', <<"Wrong cart">>)};
            throw:allocation_duplicate ->
                {ok, logic_error('invalidAllocation', <<"Duplicate shop">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CancelPayment' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => InvoiceID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Reason = maps:get(<<"reason">>, maps:get('Reason', Req)),
        CallArgs = {InvoiceID, PaymentID, Reason},
        Call = {invoicing, 'CancelPayment', CallArgs},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, _} ->
                {ok, {202, #{}, undefined}};
            {exception, #payproc_InvoicePaymentNotFound{}} ->
                {ok, general_error(404, <<"Payment not found">>)};
            {exception, #payproc_InvalidPaymentStatus{}} ->
                {ok, logic_error('invalidPaymentStatus', <<"Invalid payment status">>)};
            {exception, #payproc_InvoiceNotFound{}} ->
                {ok, general_error(404, <<"Invoice not found">>)};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)};
            {exception, #payproc_OperationNotPermitted{}} ->
                {ok, logic_error('operationNotPermitted', <<"Operation not permitted">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateRefund' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    RefundParams = maps:get('RefundParams', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    PartyID = Invoice#payproc_Invoice.invoice#domain_Invoice.owner_id,
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => InvoiceID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        try
            ok = validate_refund(RefundParams),
            create_refund(PartyID, InvoiceID, PaymentID, RefundParams, Context, OperationID)
        of
            {ok, Refund} ->
                {ok, {201, #{}, capi_handler_decoder_invoicing:decode_refund(Refund)}};
            {exception, #payproc_InvoicePaymentNotFound{}} ->
                {ok, general_error(404, <<"Payment not found">>)};
            {exception, #payproc_InvoiceNotFound{}} ->
                {ok, general_error(404, <<"Invoice not found">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
            {exception, #payproc_InvalidContractStatus{}} ->
                {ok, logic_error('invalidContractStatus', <<"Invalid contract status">>)};
            {exception, #payproc_OperationNotPermitted{}} ->
                {ok, logic_error('operationNotPermitted', <<"Operation not permitted">>)};
            {exception, #payproc_InvalidPaymentStatus{}} ->
                {ok, logic_error('invalidPaymentStatus', <<"Invalid invoice payment status">>)};
            {exception, #payproc_InsufficientAccountBalance{}} ->
                ErrResp = logic_error(
                    'insufficentAccountBalance',
                    <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                ),
                {ok, ErrResp};
            {exception, #payproc_InvoicePaymentAmountExceeded{}} ->
                {ok, logic_error('invoicePaymentAmountExceeded', <<"Payment amount exceeded">>)};
            {exception, #payproc_InconsistentRefundCurrency{}} ->
                {ok, logic_error('inconsistentRefundCurrency', <<"Inconsistent refund currency">>)};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)};
            {exception, #payproc_AllocationNotAllowed{}} ->
                {ok, logic_error('allocationNotPermitted', <<"Not allowed">>)};
            {exception, #payproc_AllocationExceededPaymentAmount{}} ->
                {ok, logic_error('invalidAllocation', <<"Exceeded payment amount">>)};
            {exception, #payproc_AllocationInvalidTransaction{} = InvalidTransaction} ->
                Message = capi_allocation:transaction_error(InvalidTransaction),
                {ok, logic_error('invalidAllocation', Message)};
            {exception, #payproc_AllocationNotFound{}} ->
                {ok, logic_error('invalidAllocation', <<"Not found">>)}
        catch
            throw:invoice_cart_empty ->
                {ok, logic_error('invalidInvoiceCart', <<"Wrong size. Path to item: cart">>)};
            throw:{external_id_conflict, RefundID, ExternalID, _Schema} ->
                {ok, conflict_error({RefundID, ExternalID})};
            throw:allocation_duplicate ->
                {ok, logic_error('invalidAllocation', <<"Duplicate shop">>)};
            throw:allocation_wrong_cart ->
                {ok, logic_error('invalidAllocation', <<"Wrong cart">>)};
            throw:refund_cart_conflict ->
                {ok, logic_error('refundCartConflict', <<"Inconsistent Refund Cart">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetRefunds' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        case find_payment_by_id(PaymentID, Invoice) of
            #payproc_InvoicePayment{refunds = Refunds} ->
                {ok,
                    {200, #{}, [
                        capi_handler_decoder_invoicing:decode_refund(R)
                     || #payproc_InvoicePaymentRefund{refund = R} <- Refunds
                    ]}};
            undefined ->
                {ok, general_error(404, <<"Payment not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetRefundByID' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    RefundID = maps:get('refundID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    Payment = find_payment_by_id(PaymentID, Invoice),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID, refund => RefundID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, mask_invoice_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,

    Process = fun() ->
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        capi_handler:respond_if_undefined(Payment, general_error(404, <<"Payment not found">>)),
        case find_refund_by_id(RefundID, Payment) of
            #payproc_InvoicePaymentRefund{refund = Refund} ->
                {ok, {200, #{}, capi_handler_decoder_invoicing:decode_refund(Refund)}};
            undefined ->
                {ok, general_error(404, <<"Invoice payment refund not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, #{'partyID' := PartyID, 'externalID' := ExternalID}, Context) when
    OperationID =:= 'GetRefundByExternalID';
    OperationID =:= 'GetRefundByExternalIDForParty'
->
    InternalID = get_refund_by_external_id(PartyID, ExternalID, Context),
    Invoice = 'maybe'(
        InternalID,
        fun({InvoiceID, _PaymentID, _RefundID}) -> get_invoice_by_id(InvoiceID, Context) end
    ),
    OperationPrototype = 'maybe'(
        InternalID,
        fun({InvoiceID, PaymentID, RefundID}) ->
            #{id => OperationID, invoice => InvoiceID, payment => PaymentID, refund => RefundID}
        end
    ),

    Authorize = fun() ->
        Prototypes = [
            {operation, genlib:define(OperationPrototype, #{id => OperationID})},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, mask_refund_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,

    Process = fun() ->
        capi_handler:respond_if_undefined(InternalID, general_error(404, <<"Refund not found">>)),
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        {_InvoiceID, PaymentID, RefundID} = InternalID,
        Payment = find_payment_by_id(PaymentID, Invoice),
        capi_handler:respond_if_undefined(Payment, general_error(404, <<"Payment not found">>)),
        case find_refund_by_id(RefundID, Payment) of
            #payproc_InvoicePaymentRefund{refund = Refund} ->
                {ok, {200, #{}, capi_handler_decoder_invoicing:decode_refund(Refund)}};
            undefined ->
                {ok, general_error(404, <<"Invoice payment refund not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetRefundByExternalID' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Req1 = maps:put('partyID', PartyID, Req),
    prepare(OperationID, Req1, Context);
prepare('GetChargebacks' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        DecodeChargebackFun = fun(C) ->
            capi_handler_decoder_invoicing:decode_chargeback(C#payproc_InvoicePaymentChargeback.chargeback, Context)
        end,
        case find_payment_by_id(PaymentID, Invoice) of
            #payproc_InvoicePayment{chargebacks = Chargebacks} ->
                {ok, {200, #{}, [DecodeChargebackFun(C) || C <- Chargebacks]}};
            undefined ->
                {ok, general_error(404, <<"Payment not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetChargebackByID' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentID = maps:get('paymentID', Req),
    ChargebackID = maps:get('chargebackID', Req),
    Invoice = get_invoice_by_id(InvoiceID, Context),
    Payment = find_payment_by_id(PaymentID, Invoice),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{id => OperationID, invoice => InvoiceID, payment => PaymentID}},
            {payproc, #{invoice => Invoice}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(Invoice, general_error(404, <<"Invoice not found">>)),
        capi_handler:respond_if_undefined(Payment, general_error(404, <<"Payment not found">>)),
        case find_chargeback_by_id(ChargebackID, Payment) of
            {ok, Chargeback} ->
                {ok, {200, #{}, capi_handler_decoder_invoicing:decode_chargeback(Chargeback, Context)}};
            {error, chargeback_not_found} ->
                {ok, general_error(404, <<"Invoice payment chargeback not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

validate_allocation(Allocation) ->
    case capi_allocation:validate(Allocation) of
        ok -> ok;
        Error -> throw(Error)
    end.

validate_refund(Params) ->
    Allocation = maps:get(<<"allocation">>, Params, undefined),
    ok = validate_allocation(Allocation),
    RefundCart = maps:get(<<"cart">>, Params, undefined),
    case {RefundCart, Allocation} of
        {undefined, _} -> ok;
        {_, undefined} -> ok;
        _ -> throw(refund_cart_conflict)
    end.

create_payment(Invoice, PaymentParams, Context, OperationID) ->
    PaymentToken = decode_payment_token(PaymentParams),
    PaymentTool = capi_utils:'maybe'(PaymentToken, fun(#{payment_tool := V}) -> V end),

    InvoiceID = Invoice#domain_Invoice.id,
    PaymentID = create_payment_id(Invoice, PaymentParams, Context, OperationID, PaymentTool),
    ExternalID = maps:get(<<"externalID">>, PaymentParams, undefined),
    InvoicePaymentParams = encode_invoice_payment_params(PaymentID, ExternalID, PaymentParams, PaymentTool),
    Call = {invoicing, 'StartPayment', {InvoiceID, InvoicePaymentParams}},
    capi_handler_utils:service_call(Call, Context).

create_payment_id(Invoice, PaymentParams0, Context, OperationID, PaymentToolThrift) ->
    InvoiceID = Invoice#domain_Invoice.id,
    PartyID = Invoice#domain_Invoice.owner_id,
    Payer = maps:get(<<"payer">>, PaymentParams0),
    PaymentTool = capi_utils:'maybe'(PaymentToolThrift, fun capi_handler_decoder_invoicing:decode_payment_tool/1),
    PaymentParams = PaymentParams0#{
        % Требуется для последующей кодировки параметров плательщика
        <<"invoiceID">> => InvoiceID,
        % Заменяем на структуру без токена
        <<"payer">> => Payer#{<<"paymentTool">> => PaymentTool}
    },
    ExternalID = maps:get(<<"externalID">>, PaymentParams, undefined),
    BenderPrefix = OperationID,
    IdempotentKey = {BenderPrefix, PartyID, ExternalID},

    SequenceID = InvoiceID,
    Identity = capi_bender:make_identity(capi_feature_schemas:payment(), PaymentParams),
    SequenceParams = #{},
    #{woody_context := WoodyCtx} = Context,
    %% We put `invoice_id` in a context here because `get_payment_by_external_id/3` needs it to work
    CtxData = #{<<"invoice_id">> => InvoiceID},
    capi_bender:gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyCtx, CtxData).

find_payment_by_id(PaymentID, #payproc_Invoice{payments = Payments}) ->
    Fun = fun(#payproc_InvoicePayment{payment = #domain_InvoicePayment{id = ID}}) ->
        PaymentID == ID
    end,
    find_by(Fun, genlib:define(Payments, [])).

find_refund_by_id(RefundID, #payproc_InvoicePayment{refunds = Refunds}) ->
    Fun = fun(#payproc_InvoicePaymentRefund{refund = Refund}) ->
        Refund#domain_InvoicePaymentRefund.id == RefundID
    end,
    find_by(Fun, genlib:define(Refunds, [])).

find_chargeback_by_id(ChargebackID, #payproc_InvoicePayment{chargebacks = Chargebacks}) ->
    Fun = fun(#payproc_InvoicePaymentChargeback{chargeback = Chargeback}) ->
        Chargeback#domain_InvoicePaymentChargeback.id == ChargebackID
    end,
    case find_by(Fun, genlib:define(Chargebacks, [])) of
        undefined ->
            {error, chargeback_not_found};
        Chargeback ->
            {ok, Chargeback}
    end.

find_by(Fun, [E | Rest]) ->
    case Fun(E) of
        true -> E;
        false -> find_by(Fun, Rest)
    end;
find_by(_, []) ->
    undefined.

get_invoice_by_id(InvoiceID, Context) ->
    case capi_handler_utils:get_invoice_by_id(InvoiceID, Context) of
        {ok, Invoice} ->
            Invoice;
        {exception, #payproc_InvoiceNotFound{}} ->
            undefined
    end.

encode_invoice_payment_params(ID, ExternalID, PaymentParams, PaymentTool) ->
    Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
    Payer = genlib_map:get(<<"payer">>, PaymentParams),
    #payproc_InvoicePaymentParams{
        id = ID,
        external_id = ExternalID,
        payer = encode_payer_params(Payer, PaymentTool),
        payer_session_info = encode_payer_session_info(Payer),
        flow = encode_flow(Flow),
        make_recurrent = genlib_map:get(<<"makeRecurrent">>, PaymentParams, false),
        context = capi_handler_encoder:encode_payment_context(PaymentParams),
        processing_deadline = encode_processing_deadline(
            genlib_map:get(<<"processingDeadline">>, PaymentParams, default_processing_deadline())
        )
    }.

encode_payer_params(
    #{
        <<"payerType">> := <<"CustomerPayer">>,
        <<"customerID">> := ID
    },
    _
) ->
    {customer, #payproc_CustomerPayerParams{customer_id = ID}};
encode_payer_params(
    #{
        <<"payerType">> := <<"PaymentResourcePayer">>,
        <<"paymentSession">> := EncodedSession,
        <<"contactInfo">> := ContactInfo
    },
    PaymentTool
) ->
    {ClientInfo, PaymentSession} = capi_handler_utils:unwrap_payment_session(EncodedSession),
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = capi_handler_encoder:encode_client_info(ClientInfo)
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }};
encode_payer_params(
    #{
        <<"payerType">> := <<"RecurrentPayer">>,
        <<"recurrentParentPayment">> := RecurrentParent,
        <<"contactInfo">> := ContactInfo
    },
    _
) ->
    #{
        <<"invoiceID">> := InvoiceID,
        <<"paymentID">> := PaymentID
    } = RecurrentParent,
    {recurrent, #payproc_RecurrentPayerParams{
        recurrent_parent = #domain_RecurrentParentPayment{
            invoice_id = InvoiceID,
            payment_id = PaymentID
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }}.

encode_payer_session_info(#{<<"sessionInfo">> := SessionInfo}) ->
    #domain_PayerSessionInfo{
        redirect_url = maps:get(<<"redirectUrl">>, SessionInfo, undefined)
    };
encode_payer_session_info(#{}) ->
    undefined.

encode_flow(#{<<"type">> := <<"PaymentFlowInstant">>}) ->
    {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
encode_flow(#{<<"type">> := <<"PaymentFlowHold">>} = Entity) ->
    OnHoldExpiration = maps:get(<<"onHoldExpiration">>, Entity, <<"cancel">>),
    {hold, #payproc_InvoicePaymentParamsFlowHold{
        on_hold_expiration = binary_to_existing_atom(OnHoldExpiration, utf8)
    }}.

encode_optional_cash(#{<<"amount">> := _, <<"currency">> := _} = Params, _, _, _) ->
    capi_handler_encoder:encode_cash(Params);
encode_optional_cash(#{<<"amount">> := _} = Params, InvoiceID, PaymentID, Context) ->
    {ok, #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{
            cost = #domain_Cash{currency = Currency}
        }
    }} = capi_handler_utils:get_payment_by_id(InvoiceID, PaymentID, Context),
    capi_handler_encoder:encode_cash(Params#{<<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)});
encode_optional_cash(_, _, _, _) ->
    undefined.

%%

decode_payment_token(#{<<"payer">> := Payer}) ->
    decode_payment_token(Payer);
decode_payment_token(#{<<"paymentToolToken">> := Token}) ->
    case capi_crypto:decode_token(Token) of
        % TODO #ED-162 Проверка времени жизни будет в bouncer, возможно тут её следует убрать вместе с тестами
        {ok, #{valid_until := ValidUntil} = TokenData} ->
            case capi_utils:deadline_is_reached(ValidUntil) of
                true ->
                    logger:warning("Payment tool token expired: ~p", [capi_utils:deadline_to_binary(ValidUntil)]),
                    capi_handler:respond(logic_error('invalidPaymentToolToken'));
                _ ->
                    TokenData
            end;
        unrecognized ->
            capi_handler:respond(logic_error('invalidPaymentToolToken'));
        {error, {decryption_failed, Error}} ->
            logger:warning("Payment tool token decryption failed: ~p", [Error]),
            capi_handler:respond(logic_error('invalidPaymentToolToken'))
    end;
decode_payment_token(_Other) ->
    undefined.

decode_invoice_payment(InvoiceID, InvoicePayment, Context) ->
    capi_handler_decoder_invoicing:decode_invoice_payment(InvoiceID, InvoicePayment, Context).

get_refund_by_external_id(PartyID, ExternalID, #{woody_context := WoodyContext}) ->
    IdempotentKey = {'CreateRefund', PartyID, ExternalID},
    case capi_bender:get_internal_id(IdempotentKey, WoodyContext) of
        {ok, RefundID, CtxData} ->
            InvoiceID = maps:get(<<"invoice_id">>, CtxData),
            PaymentID = maps:get(<<"payment_id">>, CtxData),
            {InvoiceID, PaymentID, RefundID};
        {error, internal_id_not_found} ->
            undefined
    end.

-spec get_payment_by_external_id(binary(), binary(), capi_handler:processing_context()) ->
    {binary(), binary()} | undefined.
get_payment_by_external_id(PartyID, ExternalID, #{woody_context := WoodyContext}) ->
    IdempotentKey = {'CreatePayment', PartyID, ExternalID},
    case capi_bender:get_internal_id(IdempotentKey, WoodyContext) of
        {ok, PaymentID, CtxData} ->
            InvoiceID = maps:get(<<"invoice_id">>, CtxData),
            {InvoiceID, PaymentID};
        {error, internal_id_not_found} ->
            undefined
    end.

encode_processing_deadline(Deadline) ->
    case capi_utils:parse_deadline(Deadline) of
        {ok, undefined} ->
            undefined;
        {error, bad_deadline} ->
            throw(invalid_processing_deadline);
        {ok, ProcessingDeadline} ->
            woody_deadline:to_binary(ProcessingDeadline)
    end.

default_processing_deadline() ->
    genlib_app:env(capi, default_processing_deadline, ?DEFAULT_PROCESSING_DEADLINE).

create_refund(PartyID, InvoiceID, PaymentID, RefundParams0, Context, BenderPrefix) ->
    RefundParams = RefundParams0#{<<"invoiceID">> => InvoiceID, <<"paymentID">> => PaymentID},
    ExternalID = maps:get(<<"externalID">>, RefundParams, undefined),
    IdempotentKey = {BenderPrefix, PartyID, ExternalID},
    Identity = capi_bender:make_identity(capi_feature_schemas:refund(), RefundParams),
    SequenceID = create_sequence_id([InvoiceID, PaymentID], BenderPrefix),
    SequenceParams = #{minimum => 100},
    #{woody_context := WoodyCtx} = Context,
    %% We put `invoice_id` and `payment_id` in a context here because `get_refund_by_external_id/3` needs it to work
    CtxData = #{<<"invoice_id">> => InvoiceID, <<"payment_id">> => PaymentID},
    RefundID = capi_bender:gen_sequence(IdempotentKey, Identity, SequenceID, SequenceParams, WoodyCtx, CtxData),
    refund_payment(PartyID, RefundID, InvoiceID, PaymentID, RefundParams, Context).

refund_payment(PartyID, RefundID, InvoiceID, PaymentID, RefundParams, Context) ->
    ExternalID = maps:get(<<"externalID">>, RefundParams, undefined),
    Allocation = maps:get(<<"allocation">>, RefundParams, undefined),
    Params = #payproc_InvoicePaymentRefundParams{
        external_id = ExternalID,
        reason = genlib_map:get(<<"reason">>, RefundParams),
        cash = encode_optional_cash(RefundParams, InvoiceID, PaymentID, Context),
        cart = capi_handler_encoder:encode_invoice_cart(RefundParams),
        allocation = capi_allocation:encode(Allocation, PartyID)
    },
    CallArgs = {
        InvoiceID,
        PaymentID,
        Params#payproc_InvoicePaymentRefundParams{id = RefundID}
    },
    Call = {invoicing, 'RefundPayment', CallArgs},
    capi_handler_utils:service_call(Call, Context).

%% ED-206
%% When bouncer says "forbidden" we can't really tell the difference between "forbidden because
%% of no such invoice", "forbidden because client has no access to it" and "forbidden because
%% client has no permission to act on it". From the point of view of existing integrations this
%% is not great, so we have to mask specific instances of missing authorization as if specified
%% invoice / payment / refund is nonexistent.

mask_invoice_notfound(Resolution) ->
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Invoice not found">>)).

mask_payment_notfound(Resolution) ->
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Payment not found">>)).

mask_refund_notfound(Resolution) ->
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Invoice payment refund not found">>)).

%%

create_sequence_id([Identifier | Rest], BenderPrefix) ->
    Next = create_sequence_id(Rest, BenderPrefix),
    <<Identifier/binary, ".", Next/binary>>;
create_sequence_id([], BenderPrefix) ->
    genlib:to_binary(BenderPrefix).

'maybe'(undefined, _) ->
    undefined;
'maybe'(V, Fun) ->
    Fun(V).
