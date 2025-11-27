// NOTE "proto/base.thrift" is ambiguous, because both damsel and bouncer-proto
// have such thrift-file.
// include "proto/base.thrift"
include "proto/domain.thrift"
include "proto/payment_processing.thrift"

namespace java dev.vality.capi
namespace erlang capi.ext

// NOTE Stolen from damsel/proto/base.thrift
exception InvalidRequest {
    1: required list<string> errors
}

// Based on `payment_processing.InvoiceTemplateCreateParams`
struct InvoiceTemplateCreateParams {
    1: optional string external_id
    2: required domain.PartyConfigRef party_id
    3: required domain.ShopConfigRef shop_id
    4: required domain.LifetimeInterval invoice_lifetime
    5: optional string name
    6: optional string description
    7: required domain.InvoiceTemplateDetails details
    8: required domain.InvoiceContext context
}

// Based on `payment_processing.InvoiceTemplateUpdateParams`
struct InvoiceTemplateUpdateParams {
    1: optional domain.LifetimeInterval invoice_lifetime
    2: optional string name
    3: optional string description
    4: optional domain.InvoiceTemplateDetails details
    5: optional domain.InvoiceContext context
}

struct AccessToken {
    1: required string payload
}

struct InvoiceTemplateAndToken {
    1: required domain.InvoiceTemplate invoice_template
    2: required AccessToken invoice_template_access_token
}

service InvoiceTemplating {

    InvoiceTemplateAndToken Create (1: InvoiceTemplateCreateParams params)
        throws (
            1: payment_processing.PartyNotFound ex1,
            2: payment_processing.InvalidPartyStatus ex2,
            3: payment_processing.ShopNotFound ex3,
            4: payment_processing.InvalidShopStatus ex4,
            5: InvalidRequest ex5
        )

    domain.InvoiceTemplate Get (2: domain.InvoiceTemplateID id)
        throws (
            1: payment_processing.InvoiceTemplateNotFound ex1,
            2: payment_processing.InvoiceTemplateRemoved ex2
        )

    domain.InvoiceTemplate Update (2: domain.InvoiceTemplateID id, 3: InvoiceTemplateUpdateParams params)
        throws (
            1: payment_processing.InvoiceTemplateNotFound ex1,
            2: payment_processing.InvoiceTemplateRemoved ex2,
            3: payment_processing.InvalidPartyStatus ex3,
            4: payment_processing.InvalidShopStatus ex4,
            5: InvalidRequest ex5
        )

    void Delete (2: domain.InvoiceTemplateID id)
        throws (
            1: payment_processing.InvoiceTemplateNotFound ex1,
            2: payment_processing.InvoiceTemplateRemoved ex2,
            3: payment_processing.InvalidPartyStatus ex3,
            4: payment_processing.InvalidShopStatus ex4
        )
}
