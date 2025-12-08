-module(capi_handler_invoice_templates).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_api_ext_thrift.hrl").

-behaviour(capi_handler).
-export([prepare/3]).

-behaviour(woody_server_thrift_handler).
-export([handle_function/4]).

-import(capi_handler_utils, [general_error/2, logic_error/2, conflict_error/1, map_service_result/1]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('CreateInvoiceTemplate' = OperationID, Req, Context) ->
    InvoiceTemplateParams = maps:get('InvoiceTemplateCreateParams', Req),
    PartyID = maps:get(<<"partyID">>, InvoiceTemplateParams, capi_handler_utils:get_party_id(Context)),
    Authorize = fun() ->
        ShopID = maps:get(<<"shopID">>, InvoiceTemplateParams),
        Prototypes = [{operation, #{party => PartyID, shop => ShopID, id => OperationID}}],
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        try
            InvoiceTemplateID = generate_invoice_template_id(OperationID, InvoiceTemplateParams, PartyID, Context),
            CallArgs = {encode_invoice_tpl_create_params(InvoiceTemplateID, PartyID, InvoiceTemplateParams)},
            capi_handler_utils:service_call(
                {invoice_templating, 'Create', CallArgs},
                Context
            )
        of
            {ok, InvoiceTpl} ->
                {ok, {201, #{}, make_invoice_tpl_and_token(InvoiceTpl, Context)}};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)};
            {exception, #payproc_PartyNotFound{}} ->
                {ok, logic_error('invalidPartyID', <<"Party not found">>)};
            {exception, #payproc_ShopNotFound{}} ->
                {ok, logic_error('invalidShopID', <<"Shop not found">>)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)}
        catch
            throw:invoice_cart_empty ->
                {ok, logic_error('invalidInvoiceCart', <<"Wrong size. Path to item: cart">>)};
            throw:zero_invoice_lifetime ->
                {ok, logic_error('invalidRequest', <<"Lifetime cannot be zero">>)};
            throw:{external_id_conflict, ID, UsedExternalID, _Schema} ->
                {ok, conflict_error({ID, UsedExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoiceTemplateByID' = OperationID, Req, Context) ->
    InvoiceTemplateID = maps:get('invoiceTemplateID', Req),
    InvoiceTpl = map_service_result(get_invoice_template(InvoiceTemplateID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{invoice_template => InvoiceTemplateID, id => OperationID}},
            {payproc, #{invoice_template => InvoiceTpl}}
        ],
        Resolution = mask_invoice_template_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(InvoiceTpl, general_error(404, <<"Invoice template not found">>)),
        {ok, {200, #{}, decode_invoice_tpl(InvoiceTpl)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('UpdateInvoiceTemplate' = OperationID, Req, Context) ->
    InvoiceTemplateID = maps:get('invoiceTemplateID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{invoice_template => InvoiceTemplateID, id => OperationID}},
            {payproc, #{invoice_template => InvoiceTemplateID}}
        ],
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        try
            Params = encode_invoice_tpl_update_params(maps:get('InvoiceTemplateUpdateParams', Req)),
            Call = {invoice_templating, 'Update', {InvoiceTemplateID, Params}},
            capi_handler_utils:service_call(Call, Context)
        of
            {ok, UpdatedInvoiceTpl} ->
                {ok, {200, #{}, decode_invoice_tpl(UpdatedInvoiceTpl)}};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
            {exception, #payproc_InvoiceTemplateNotFound{}} ->
                {ok, general_error(404, <<"Invoice Template not found">>)};
            {exception, #payproc_InvoiceTemplateRemoved{}} ->
                {ok, general_error(404, <<"Invoice Template not found">>)}
        catch
            throw:#payproc_InvoiceTemplateNotFound{} ->
                {ok, general_error(404, <<"Invoice Template not found">>)};
            throw:#payproc_InvoiceTemplateRemoved{} ->
                {ok, general_error(404, <<"Invoice Template not found">>)};
            throw:invoice_cart_empty ->
                {ok, logic_error('invalidInvoiceCart', <<"Wrong size. Path to item: cart">>)};
            throw:zero_invoice_lifetime ->
                {ok, logic_error('invalidRequest', <<"Lifetime cannot be zero">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('DeleteInvoiceTemplate' = OperationID, Req, Context) ->
    InvoiceTemplateID = maps:get('invoiceTemplateID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{invoice_template => InvoiceTemplateID, id => OperationID}},
            {payproc, #{invoice_template => InvoiceTemplateID}}
        ],
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        Call = {invoice_templating, 'Delete', {InvoiceTemplateID}},
        case capi_handler_utils:service_call(Call, Context) of
            {ok, _R} ->
                {ok, {204, #{}, undefined}};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
            {exception, #payproc_InvoiceTemplateNotFound{}} ->
                {ok, general_error(404, <<"Invoice Template not found">>)};
            {exception, #payproc_InvoiceTemplateRemoved{}} ->
                {ok, general_error(404, <<"Invoice Template not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('CreateInvoiceWithTemplate' = OperationID, Req, Context) ->
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    InvoiceTpl = map_service_result(get_invoice_template(InvoiceTplID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{invoice_template => InvoiceTplID, id => OperationID}},
            {payproc, #{invoice_template => InvoiceTpl}}
        ],
        Resolution = capi_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(InvoiceTpl, general_error(404, <<"Invoice template not found">>)),
        InvoiceParams = maps:get('InvoiceParamsWithTemplate', Req),
        PartyID = InvoiceTpl#domain_InvoiceTemplate.party_ref#domain_PartyConfigRef.id,
        try create_invoice(PartyID, InvoiceTplID, InvoiceParams, Context, OperationID) of
            {ok, #'payproc_Invoice'{invoice = Invoice}} ->
                {ok, {201, #{}, capi_handler_decoder_invoicing:make_invoice_and_token(Invoice, Context)}};
            {exception, #base_InvalidRequest{errors = Errors}} ->
                FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                {ok, logic_error('invalidRequest', FormattedErrors)};
            {exception, #payproc_InvalidPartyStatus{}} ->
                {ok, logic_error('invalidPartyStatus', <<"Invalid party status">>)};
            {exception, #payproc_InvalidShopStatus{}} ->
                {ok, logic_error('invalidShopStatus', <<"Invalid shop status">>)};
            {exception, #payproc_InvoiceTemplateNotFound{}} ->
                {ok, general_error(404, <<"Invoice Template not found">>)};
            {exception, #payproc_InvoiceTemplateRemoved{}} ->
                {ok, general_error(404, <<"Invoice Template not found">>)};
            {exception, #payproc_InvoiceTermsViolated{}} ->
                {ok, logic_error('invoiceTermsViolated', <<"Invoice parameters violate terms">>)}
        catch
            throw:{bad_invoice_params, currency_no_amount} ->
                {ok, logic_error('invalidRequest', <<"Amount is required for the currency">>)};
            throw:{bad_invoice_params, amount_no_currency} ->
                {ok, logic_error('invalidRequest', <<"Currency is required for the amount">>)};
            throw:{external_id_conflict, InvoiceID, ExternalID, _Schema} ->
                {ok, conflict_error({InvoiceID, ExternalID})}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetInvoicePaymentMethodsByTemplateID' = OperationID, Req, Context) ->
    InvoiceTemplateID = maps:get('invoiceTemplateID', Req),
    InvoiceTemplate = map_service_result(get_invoice_template(InvoiceTemplateID, Context)),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{invoice_template => InvoiceTemplateID, id => OperationID}},
            {payproc, #{invoice_template => InvoiceTemplate}}
        ],
        Resolution = mask_invoice_template_notfound(capi_auth:authorize_operation(Prototypes, Context)),
        {ok, Resolution}
    end,
    Process = fun() ->
        capi_handler:respond_if_undefined(InvoiceTemplate, general_error(404, <<"Invoice template not found">>)),
        case capi_handler_utils:get_payment_methods(invoice_templating, {InvoiceTemplateID}, Context) of
            {ok, PaymentMethodRefs} ->
                PaymentMethods0 = capi_handler_decoder_invoicing:decode_payment_methods(PaymentMethodRefs),
                PaymentMethods1 = capi_utils:deduplicate_payment_methods(PaymentMethods0),
                PaymentMethods = capi_handler_utils:emplace_token_provider_data(
                    InvoiceTemplate, PaymentMethods1, Context
                ),
                {ok, {200, #{}, PaymentMethods}};
            {exception, E} when
                E == #payproc_InvoiceTemplateNotFound{};
                E == #payproc_InvoiceTemplateRemoved{}
            ->
                {ok, general_error(404, <<"Invoice template not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), _) ->
    {ok, term()} | no_return().
handle_function(Function, Args, WoodyContext, Opts) ->
    scoper:scope(
        invoice_templating,
        fun() ->
            try handle_function_(Function, Args, WoodyContext, Opts) of
                {ok, _} = Result ->
                    Result;
                {exception, #payproc_InvoiceTemplateNotFound{} = Exception} ->
                    woody_error:raise(business, Exception);
                {exception, #payproc_InvoiceTemplateRemoved{} = Exception} ->
                    woody_error:raise(business, Exception);
                {exception, #payproc_PartyNotFound{} = Exception} ->
                    woody_error:raise(business, Exception);
                {exception, #payproc_ShopNotFound{} = Exception} ->
                    woody_error:raise(business, Exception);
                {exception, #payproc_InvalidPartyStatus{} = Exception} ->
                    woody_error:raise(business, Exception);
                {exception, #payproc_InvalidShopStatus{} = Exception} ->
                    woody_error:raise(business, Exception);
                {exception, #base_InvalidRequest{errors = Errors}} ->
                    woody_error:raise(business, #base_InvalidRequest{errors = Errors})
            catch
                throw:(#payproc_InvoiceTemplateNotFound{} = Exception) ->
                    woody_error:raise(business, Exception);
                throw:(#payproc_InvoiceTemplateRemoved{} = Exception) ->
                    woody_error:raise(business, Exception);
                throw:invoice_cart_empty ->
                    woody_error:raise(business, #base_InvalidRequest{errors = [<<"Wrong size. Path to item: cart">>]});
                throw:zero_invoice_lifetime ->
                    woody_error:raise(business, #base_InvalidRequest{errors = [<<"Lifetime cannot be zero">>]});
                throw:{external_id_conflict, _ID, _UsedExternalID, _Schema} ->
                    woody_error:raise(business, #base_InvalidRequest{
                        errors = [<<"This 'externalID' has been used by another request">>]
                    })
            end
        end
    ).

handle_function_('Create', {InvoiceTemplateParams}, WoodyContext, _Opts) ->
    %% NOTE Use same operation ID as the original in swagger/JSON API
    InvoiceTemplateID = generate_thrift_invoice_template_id(
        'CreateInvoiceTemplate', InvoiceTemplateParams, WoodyContext
    ),
    CallArgs = {encode_thrift_invoice_tpl_create_params(InvoiceTemplateID, InvoiceTemplateParams)},
    case capi_woody_client:call_service(invoice_templating, 'Create', CallArgs, WoodyContext) of
        {ok, InvoiceTpl} ->
            {ok, make_thrift_invoice_tpl_and_token(InvoiceTpl, WoodyContext)};
        Passthrough ->
            Passthrough
    end;
handle_function_('Get', {InvoiceTemplateID}, WoodyContext, _Opts) ->
    capi_woody_client:call_service(invoice_templating, 'Get', {InvoiceTemplateID}, WoodyContext);
handle_function_('Update', {InvoiceTemplateID, InvoiceTemplateParams}, WoodyContext, _Opts) ->
    Params = encode_thrift_invoice_tpl_update_params(InvoiceTemplateParams),
    capi_woody_client:call_service(invoice_templating, 'Update', {InvoiceTemplateID, Params}, WoodyContext);
handle_function_('Delete', {InvoiceTemplateID}, WoodyContext, _Opts) ->
    capi_woody_client:call_service(invoice_templating, 'Delete', {InvoiceTemplateID}, WoodyContext).

mask_invoice_template_notfound(Resolution) ->
    % ED-206
    % When bouncer says "forbidden" we can't really tell the difference between "forbidden because
    % of no such invoice teplate", "forbidden because client has no access to it" and "forbidden
    % because client has no permission to act on it". From the point of view of existing integrations
    % this is not great, so we have to mask specific instances of missing authorization as if
    % specified invoice template is nonexistent.
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Invoice template not found">>)).

%%

create_invoice(PartyID, InvoiceTplID, InvoiceParams, Context, BenderPrefix) ->
    #{woody_context := WoodyCtx} = Context,
    % CAPI#344: Since the prefixes are different, it's possible to create 2 copies of the same Invoice with the same
    % externalId by using `CreateInvoice` and `CreateInvoiceWithTemplate` together
    ExternalID = maps:get(<<"externalID">>, InvoiceParams, undefined),
    IdempotentKey = {BenderPrefix, PartyID, ExternalID},
    InvoiceParamsWithTemplate = maps:put(<<"invoiceTemplateID">>, InvoiceTplID, InvoiceParams),
    Identity = capi_bender:make_identity(capi_feature_schemas:invoice(), InvoiceParamsWithTemplate),
    InvoiceID = capi_bender:gen_snowflake(IdempotentKey, Identity, WoodyCtx),
    CallArgs = {encode_invoice_params_with_tpl(InvoiceID, InvoiceTplID, InvoiceParams)},
    Call = {invoicing, 'CreateWithTemplate', CallArgs},
    capi_handler_utils:service_call(Call, Context).

get_invoice_template(ID, Context) ->
    Call = {invoice_templating, 'Get', {ID}},
    capi_handler_utils:service_call(Call, Context).

generate_invoice_template_id(OperationID, TemplateParams, PartyID, #{woody_context := WoodyContext}) ->
    ExternalID = maps:get(<<"externalID">>, TemplateParams, undefined),
    IdempKey = {OperationID, PartyID, ExternalID},
    Identity = capi_bender:make_identity(capi_feature_schemas:invoice_template(), TemplateParams),
    capi_bender:gen_snowflake(IdempKey, Identity, WoodyContext).

generate_thrift_invoice_template_id(
    OperationID,
    #api_ext_InvoiceTemplateCreateParams{external_id = ExternalID, party_id = #domain_PartyConfigRef{id = PartyID}} =
        TemplateParams,
    WoodyContext
) ->
    IdempKey = {OperationID, PartyID, ExternalID},
    Identity = capi_bender:make_identity(
        capi_feature_schemas:invoice_template(),
        decode_to_feature_container(TemplateParams)
    ),
    capi_bender:gen_snowflake(IdempKey, Identity, WoodyContext).

decode_to_feature_container(#api_ext_InvoiceTemplateCreateParams{
    shop_id = #domain_ShopConfigRef{id = ShopID},
    invoice_lifetime = #domain_LifetimeInterval{days = DD, months = MM, years = YY},
    details = Details
}) ->
    #{
        <<"shopID">> => ShopID,
        <<"lifetime">> => #{<<"days">> => DD, <<"months">> => MM, <<"years">> => YY},
        <<"details">> => encode_details_to_feature_container(Details)
    }.

encode_details_to_feature_container(
    {product, #domain_InvoiceTemplateProduct{
        product = Product,
        price = Price,
        metadata = Metadata
    }}
) ->
    genlib_map:compact(#{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => Product,
        <<"price">> => encode_price_to_feature_container(Price),
        <<"taxMode">> => encode_tax_metadata_to_feature_container(Metadata)
    });
encode_details_to_feature_container({cart, #domain_InvoiceCart{lines = Lines}}) ->
    {Cart, Currency} = encode_cart_lines_to_feature_container(Lines),
    #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency">> => Currency,
        <<"cart">> => Cart
    }.

encode_cart_lines_to_feature_container([]) ->
    throw(invoice_cart_empty);
encode_cart_lines_to_feature_container(Lines) ->
    {Cart, Currency} = lists:foldl(
        fun(
            #domain_InvoiceLine{
                product = Product,
                quantity = Quantity,
                price = #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Curr}},
                metadata = Metadata
            },
            {Items, _}
        ) ->
            {
                [
                    genlib_map:compact(#{
                        <<"product">> => Product,
                        <<"quantity">> => Quantity,
                        <<"price">> => Amount,
                        <<"taxMode">> => encode_tax_metadata_to_feature_container(Metadata)
                    })
                    | Items
                ],
                Curr
            }
        end,
        {[], undefined},
        Lines
    ),
    {lists:reverse(Cart), Currency}.

encode_price_to_feature_container({unlim, #domain_InvoiceTemplateCostUnlimited{}}) ->
    #{<<"costType">> => <<"InvoiceTemplateLineCostUnlim">>};
encode_price_to_feature_container(
    {fixed, #domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Currency}}}
) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
        <<"amount">> => Amount,
        <<"currency">> => Currency
    };
encode_price_to_feature_container(
    {range, #domain_CashRange{
        lower = {_, #domain_Cash{currency = #domain_CurrencyRef{symbolic_code = Currency}}} = LowerBound,
        upper = UpperBound
    }}
) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostRange">>,
        <<"currency">> => Currency,
        <<"range">> => #{
            <<"lowerBound">> => encode_bound_to_feature_container(LowerBound, 1),
            <<"upperBound">> => encode_bound_to_feature_container(UpperBound, -1)
        }
    }.

encode_bound_to_feature_container({inclusive, #domain_Cash{amount = Bound}}, _Delta) ->
    Bound;
encode_bound_to_feature_container({exclusive, #domain_Cash{amount = Bound}}, Delta) ->
    Bound + Delta.

encode_tax_metadata_to_feature_container(#{<<"TaxMode">> := {str, TM}}) ->
    #{
        <<"type">> => <<"InvoiceLineTaxVAT">>,
        <<"rate">> => TM
    };
encode_tax_metadata_to_feature_container(#{}) ->
    undefined.

encode_invoice_tpl_create_params(InvoiceTemplateID, PartyID, Params) when is_map(Params) ->
    Details = encode_invoice_tpl_details(genlib_map:get(<<"details">>, Params)),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateCreateParams{
        template_id = InvoiceTemplateID,
        party_id = #domain_PartyConfigRef{id = PartyID},
        shop_id = #domain_ShopConfigRef{id = genlib_map:get(<<"shopID">>, Params)},
        invoice_lifetime = encode_lifetime(Params),
        product = Product,
        name = genlib_map:get(<<"name">>, Params),
        description = genlib_map:get(<<"description">>, Params),
        details = Details,
        context = capi_handler_encoder:encode_invoice_context(Params),
        mutations = capi_mutation:encode_amount_randomization_params(genlib_map:get(<<"randomizeAmount">>, Params))
    }.

encode_invoice_tpl_update_params(Params) ->
    Details = encode_invoice_tpl_details(genlib_map:get(<<"details">>, Params)),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = encode_lifetime(Params),
        product = Product,
        name = genlib_map:get(<<"name">>, Params),
        description = genlib_map:get(<<"description">>, Params),
        details = Details,
        context = encode_optional_context(Params),
        mutations = capi_mutation:encode_amount_randomization_params(genlib_map:get(<<"randomizeAmount">>, Params))
    }.

encode_thrift_invoice_tpl_update_params(#api_ext_InvoiceTemplateUpdateParams{
    invoice_lifetime = InvoiceLifetime,
    name = Name,
    description = Description,
    details = Details,
    context = Context
}) ->
    ok = assert_cart_is_not_empty(Details),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = InvoiceLifetime,
        product = Product,
        name = Name,
        description = Description,
        details = Details,
        context = Context
    }.

assert_cart_is_not_empty({cart, #domain_InvoiceCart{lines = []}}) ->
    throw(invoice_cart_empty);
assert_cart_is_not_empty(_) ->
    ok.

make_invoice_tpl_and_token(InvoiceTpl, ProcessingContext) ->
    #{
        <<"invoiceTemplate">> => decode_invoice_tpl(InvoiceTpl),
        <<"invoiceTemplateAccessToken">> => capi_handler_utils:issue_access_token(InvoiceTpl, ProcessingContext)
    }.

encode_thrift_invoice_tpl_create_params(InvoiceTemplateID, #api_ext_InvoiceTemplateCreateParams{
    party_id = PartyID,
    shop_id = ShopID,
    invoice_lifetime = InvoiceLifetime,
    name = Name,
    description = Description,
    details = Details,
    context = Context
}) ->
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateCreateParams{
        template_id = InvoiceTemplateID,
        party_id = PartyID,
        shop_id = ShopID,
        invoice_lifetime = InvoiceLifetime,
        product = Product,
        name = Name,
        description = Description,
        details = Details,
        context = Context
    }.

make_thrift_invoice_tpl_and_token(InvoiceTpl, WoodyContext) ->
    TokenSpec = #{
        party => InvoiceTpl#domain_InvoiceTemplate.party_ref#domain_PartyConfigRef.id,
        scope => {invoice_template, InvoiceTpl#domain_InvoiceTemplate.id},
        shop => InvoiceTpl#domain_InvoiceTemplate.shop_ref#domain_ShopConfigRef.id
    },
    TokenPayload = capi_auth:issue_access_token(TokenSpec, WoodyContext),
    #api_ext_InvoiceTemplateAndToken{
        invoice_template = InvoiceTpl,
        invoice_template_access_token = #api_ext_AccessToken{payload = TokenPayload}
    }.

encode_invoice_tpl_details(#{<<"templateType">> := <<"InvoiceTemplateSingleLine">>} = Details) ->
    {product, encode_invoice_tpl_product(Details)};
encode_invoice_tpl_details(#{<<"templateType">> := <<"InvoiceTemplateMultiLine">>} = Details) ->
    {cart, capi_handler_encoder:encode_invoice_cart(Details)};
encode_invoice_tpl_details(undefined) ->
    undefined.

get_product_from_tpl_details({product, #domain_InvoiceTemplateProduct{product = Product}}) ->
    Product;
get_product_from_tpl_details({cart, #domain_InvoiceCart{lines = [FirstLine | _]}}) ->
    #domain_InvoiceLine{product = Product} = FirstLine,
    Product;
get_product_from_tpl_details(undefined) ->
    undefined.

encode_optional_context(#{<<"metadata">> := _} = Params) ->
    capi_handler_encoder:encode_invoice_context(Params);
encode_optional_context(#{}) ->
    undefined.

encode_lifetime(#{<<"lifetime">> := Lifetime}) ->
    encode_lifetime(
        genlib_map:get(<<"days">>, Lifetime),
        genlib_map:get(<<"months">>, Lifetime),
        genlib_map:get(<<"years">>, Lifetime)
    );
encode_lifetime(_) ->
    undefined.

encode_lifetime(0, 0, 0) ->
    throw(zero_invoice_lifetime);
encode_lifetime(DD, MM, YY) ->
    #domain_LifetimeInterval{
        days = DD,
        months = MM,
        years = YY
    }.

encode_invoice_params_with_tpl(InvoiceID, InvoiceTplID, InvoiceParams) ->
    #payproc_InvoiceWithTemplateParams{
        id = InvoiceID,
        external_id = genlib_map:get(<<"externalID">>, InvoiceParams),
        template_id = InvoiceTplID,
        cost = encode_optional_invoice_cost(InvoiceParams),
        context = encode_optional_context(InvoiceParams)
    }.

encode_invoice_tpl_product(Details) ->
    #domain_InvoiceTemplateProduct{
        product = genlib_map:get(<<"product">>, Details),
        price = encode_invoice_tpl_line_cost(genlib_map:get(<<"price">>, Details)),
        metadata = capi_handler_encoder:encode_invoice_line_meta(Details)
    }.

encode_optional_invoice_cost(#{<<"amount">> := _, <<"currency">> := _} = Params) ->
    capi_handler_encoder:encode_cash(Params);
encode_optional_invoice_cost(#{<<"amount">> := _}) ->
    throw({bad_invoice_params, amount_no_currency});
encode_optional_invoice_cost(#{<<"currency">> := _}) ->
    throw({bad_invoice_params, currency_no_amount});
encode_optional_invoice_cost(_) ->
    undefined.

encode_invoice_tpl_line_cost(#{<<"costType">> := CostType} = Cost) ->
    encode_invoice_tpl_line_cost(CostType, Cost);
encode_invoice_tpl_line_cost(_) ->
    undefined.

encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostUnlim">>, _Cost) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}};
encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostFixed">>, Cost) ->
    {fixed, capi_handler_encoder:encode_cash(Cost)};
encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostRange">>, Cost) ->
    Range = genlib_map:get(<<"range">>, Cost),
    {range, #domain_CashRange{
        lower =
            {inclusive,
                capi_handler_encoder:encode_cash(
                    Cost#{<<"amount">> => genlib_map:get(<<"lowerBound">>, Range)}
                )},
        upper =
            {inclusive,
                capi_handler_encoder:encode_cash(
                    Cost#{<<"amount">> => genlib_map:get(<<"upperBound">>, Range)}
                )}
    }}.

decode_invoice_tpl(InvoiceTpl) ->
    #domain_LifetimeInterval{days = DD, months = MM, years = YY} = InvoiceTpl#domain_InvoiceTemplate.invoice_lifetime,
    genlib_map:compact(#{
        <<"id">> => InvoiceTpl#domain_InvoiceTemplate.id,
        <<"shopID">> => InvoiceTpl#domain_InvoiceTemplate.shop_ref#domain_ShopConfigRef.id,
        <<"name">> => InvoiceTpl#domain_InvoiceTemplate.name,
        <<"description">> => InvoiceTpl#domain_InvoiceTemplate.description,
        <<"createdAt">> => InvoiceTpl#domain_InvoiceTemplate.created_at,
        <<"lifetime">> => #{
            <<"days">> => undef_to_zero(DD),
            <<"months">> => undef_to_zero(MM),
            <<"years">> => undef_to_zero(YY)
        },
        <<"details">> => decode_invoice_tpl_details(InvoiceTpl#domain_InvoiceTemplate.details),
        <<"metadata">> => capi_handler_decoder_utils:decode_context(InvoiceTpl#domain_InvoiceTemplate.context),
        <<"randomizeAmount">> => capi_mutation:decode_amount_randomization_params(
            InvoiceTpl#domain_InvoiceTemplate.mutations
        )
    }).

undef_to_zero(undefined) -> 0;
undef_to_zero(Int) -> Int.

decode_invoice_tpl_details({cart, Cart}) ->
    #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency">> => get_currency_from_cart(Cart),
        <<"cart">> => capi_handler_decoder_invoicing:decode_invoice_cart(Cart)
    };
decode_invoice_tpl_details({product, Product}) ->
    genlib_map:compact(#{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product">> => Product#domain_InvoiceTemplateProduct.product,
        <<"price">> => decode_invoice_tpl_line_cost(Product#domain_InvoiceTemplateProduct.price),
        <<"taxMode">> => capi_handler_decoder_invoicing:decode_invoice_line_tax_mode(
            Product#domain_InvoiceTemplateProduct.metadata
        )
    }).

get_currency_from_cart(#domain_InvoiceCart{lines = [FirstLine | _]}) ->
    #domain_InvoiceLine{price = #domain_Cash{currency = Currency}} = FirstLine,
    capi_handler_decoder_utils:decode_currency(Currency).

decode_invoice_tpl_line_cost({unlim, _}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostUnlim">>
    };
decode_invoice_tpl_line_cost({fixed, #domain_Cash{amount = Amount, currency = Currency}}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"amount">> => Amount
    };
decode_invoice_tpl_line_cost({range, #domain_CashRange{upper = {_, UpperCashBound}, lower = {_, LowerCashBound}}}) ->
    #domain_Cash{amount = UpperBound, currency = Currency} = UpperCashBound,
    #domain_Cash{amount = LowerBound, currency = Currency} = LowerCashBound,
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostRange">>,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"range">> => #{
            <<"upperBound">> => UpperBound,
            <<"lowerBound">> => LowerBound
        }
    }.
