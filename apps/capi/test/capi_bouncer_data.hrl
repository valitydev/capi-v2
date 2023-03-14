-ifndef(capi_bouncer_data_included__).
-define(capi_bouncer_data_included__, ok).

-include_lib("stdlib/include/assert.hrl").

-include_lib("bouncer_proto/include/bouncer_decision_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_rstn_thrift.hrl").

-define(JUDGEMENT(Resolution), #decision_Judgement{resolution = Resolution}).
-define(ALLOWED, {allowed, #decision_ResolutionAllowed{}}).
-define(FORBIDDEN, {forbidden, #decision_ResolutionForbidden{}}).
-define(RESTRICTED(Restrictions),
    {restricted, #decision_ResolutionRestricted{
        restrictions = Restrictions
    }}
).

-define(CAPI_SHOP_RESTRICTIONS(Shops), #rstn_Restrictions{
    capi = #rstn_RestrictionsCommonAPI{
        op = #rstn_CommonAPIOperationRestrictions{
            shops = Shops
        }
    }
}).

-define(CTX_ENTITY(ID), #base_Entity{id = ID}).

-define(CTX_CAPI(Op), #ctx_v1_ContextCommonAPI{op = Op}).

-define(CTX_CAPI_OP(ID), #ctx_v1_CommonAPIOperation{id = ID}).

-define(CTX_PARTY_OP(ID, PartyID), #ctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_SHOP_OP(ID, PartyID, ShopID), #ctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_CONTRACT_OP(ID, PartyID, ContractID), #ctx_v1_CommonAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    contract = ?CTX_ENTITY(ContractID)
}).

-define(CTX_INVOICE_OP(ID, InvoiceID), #ctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID)
}).

-define(CTX_PAYMENT_OP(ID, InvoiceID, PaymentID), #ctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID)
}).

-define(CTX_PAYMENT_OP(ID, InvoiceID), #ctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID)
}).

-define(CTX_REFUND_OP(ID, InvoiceID, PaymentID, RefundID), #ctx_v1_CommonAPIOperation{
    id = ID,
    invoice = ?CTX_ENTITY(InvoiceID),
    payment = ?CTX_ENTITY(PaymentID),
    refund = ?CTX_ENTITY(RefundID)
}).

-define(CTX_INVOICE_TPL_OP(ID, InvoiceTemplateID), #ctx_v1_CommonAPIOperation{
    id = ID,
    invoice_template = ?CTX_ENTITY(InvoiceTemplateID)
}).

-define(CTX_CUSTOMER_OP(ID, CustomerID), #ctx_v1_CommonAPIOperation{
    id = ID,
    customer = ?CTX_ENTITY(CustomerID)
}).

-define(CTX_BINDING_OP(ID, CustomerID, BindingID), #ctx_v1_CommonAPIOperation{
    id = ID,
    customer = ?CTX_ENTITY(CustomerID),
    binding = ?CTX_ENTITY(BindingID)
}).

-define(CTX_WEBHOOK_OP(ID, WebhookID), #ctx_v1_CommonAPIOperation{
    id = ID,
    webhook = ?CTX_ENTITY(WebhookID)
}).

-define(CTX_PAYOUT_OP(ID, PayoutID), #ctx_v1_CommonAPIOperation{
    id = ID,
    payout = ?CTX_ENTITY(PayoutID)
}).

-define(CTX_SEARCH_OP(ID, PartyID, ShopID, InvoiceID, PaymentID),
    ?CTX_SEARCH_OP(
        ID,
        PartyID,
        ShopID,
        InvoiceID,
        PaymentID,
        undefined,
        undefined
    )
).

-define(CTX_SEARCH_OP(
    ID,
    PartyID,
    ShopID,
    InvoiceID,
    PaymentID,
    CustomerID,
    RefundID
),
    #ctx_v1_CommonAPIOperation{
        id = ID,
        party = ?CTX_ENTITY(PartyID),
        shop = ?CTX_ENTITY(ShopID),
        invoice = capi_utils:maybe(InvoiceID, fun(V) -> ?CTX_ENTITY(V) end),
        payment = capi_utils:maybe(PaymentID, fun(V) -> ?CTX_ENTITY(V) end),
        customer = capi_utils:maybe(CustomerID, fun(V) -> ?CTX_ENTITY(V) end),
        refund = capi_utils:maybe(RefundID, fun(V) -> ?CTX_ENTITY(V) end)
    }
).

-define(CTX_INVOICE(ID, PartyID, ShopID), #ctx_v1_Invoice{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_INVOICE(ID, PartyID, ShopID, Payments), #ctx_v1_Invoice{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    payments = Payments
}).

-define(CTX_PAYMENT(ID), #ctx_v1_Payment{id = ID}).

-define(CTX_INVOICE_TPL(ID, PartyID, ShopID), #ctx_v1_InvoiceTemplate{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_CUSTOMER(ID, PartyID, ShopID), #ctx_v1_Customer{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_WEBHOOK(ID, PartyID), #ctx_v1_Webhook{
    id = ID,
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_PAYOUT(ID, PartyID, ContractID, ShopID), #ctx_v1_Payout{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    contract = ?CTX_ENTITY(ContractID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(assertContextMatches(Expect), fun(Context) ->
    try
        ?assertMatch(Expect, Context),
        {ok, ?JUDGEMENT(?ALLOWED)}
    catch
        error:AssertMatchError:Stacktrace ->
            logger:error("failed ~p at ~p", [AssertMatchError, Stacktrace]),
            {throwing, #decision_InvalidContext{}}
    end
end).

-endif.
