-module(capi_handler_encoder).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-export([encode_contact_info/1]).
-export([encode_client_info/1]).
-export([encode_cash/1]).
-export([encode_cash/2]).
-export([encode_currency/1]).
-export([encode_invoice_cart/1]).
-export([encode_invoice_cart/2]).
-export([encode_invoice_bank_account/1]).
-export([encode_invoice_context/1]).
-export([encode_payment_context/1]).
-export([encode_invoice_line_meta/1]).
-export([encode_residence/1]).
-export([encode_content/2]).

-export_type([encode_data/0]).

-type request_data() :: capi_handler:request_data().
-type encode_data() :: tuple().

-spec encode_contact_info(request_data()) -> encode_data().
encode_contact_info(ContactInfo) ->
    #domain_ContactInfo{
        phone_number = genlib_map:get(<<"phoneNumber">>, ContactInfo),
        email = genlib_map:get(<<"email">>, ContactInfo),
        first_name = genlib_map:get(<<"firstName">>, ContactInfo),
        last_name = genlib_map:get(<<"lastName">>, ContactInfo),
        country = genlib_map:get(<<"country">>, ContactInfo),
        state = genlib_map:get(<<"state">>, ContactInfo),
        city = genlib_map:get(<<"city">>, ContactInfo),
        address = genlib_map:get(<<"address">>, ContactInfo),
        postal_code = genlib_map:get(<<"postalCode">>, ContactInfo)
    }.

-spec encode_client_info(request_data()) -> encode_data().
encode_client_info(ClientInfo) ->
    #domain_ClientInfo{
        fingerprint = maps:get(<<"fingerprint">>, ClientInfo),
        ip_address = maps:get(<<"ip">>, ClientInfo),
        peer_ip_address = maps:get(<<"peer_ip">>, ClientInfo, undefined),
        user_ip_address = maps:get(<<"user_ip">>, ClientInfo, undefined)
    }.

-spec encode_residence(binary() | undefined) -> atom().
encode_residence(undefined) ->
    undefined;
encode_residence(Residence) when is_binary(Residence) ->
    case capi_domain:encode_enum('CountryCode', string:lowercase(Residence)) of
        {ok, EncodedResidence} -> EncodedResidence;
        {error, _} -> throw({encode_residence, invalid_residence})
    end.

-spec encode_cash(request_data()) -> encode_data().
encode_cash(Params) ->
    Amount = genlib_map:get(<<"amount">>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_cash(Amount, Currency).

-spec encode_cash(integer(), binary()) -> encode_data().
encode_cash(Amount, Currency) ->
    #domain_Cash{
        amount = Amount,
        currency = encode_currency(Currency)
    }.

-spec encode_currency(binary()) -> encode_data().
encode_currency(SymbolicCode) ->
    #domain_CurrencyRef{symbolic_code = SymbolicCode}.

-spec encode_invoice_cart(request_data()) -> encode_data().
encode_invoice_cart(Params) ->
    Cart = genlib_map:get(<<"cart">>, Params),
    Currency = genlib_map:get(<<"currency">>, Params),
    encode_invoice_cart(Cart, Currency).

-spec encode_invoice_cart(list(), binary()) -> encode_data().
encode_invoice_cart(Cart, Currency) when Cart =/= undefined, Cart =/= [] ->
    #domain_InvoiceCart{
        lines = [encode_invoice_line(Line, Currency) || Line <- Cart]
    };
encode_invoice_cart([], _) ->
    throw(invoice_cart_empty);
encode_invoice_cart(undefined, _) ->
    undefined.

encode_invoice_line(Line, Currency) ->
    Metadata = encode_invoice_line_meta(Line),
    Price = encode_cash(genlib_map:get(<<"price">>, Line), Currency),
    #domain_InvoiceLine{
        product = genlib_map:get(<<"product">>, Line),
        quantity = genlib_map:get(<<"quantity">>, Line),
        price = Price,
        metadata = Metadata
    }.

-spec encode_invoice_line_meta(request_data()) -> #{binary() => {str, _}}.

-define(DEFAULT_INVOICE_LINE_META, #{}).

encode_invoice_line_meta(Line) ->
    case genlib_map:get(<<"taxMode">>, Line) of
        TaxMode when TaxMode =/= undefined ->
            TM = encode_invoice_line_tax_mode(TaxMode),
            #{<<"TaxMode">> => {str, TM}};
        undefined ->
            ?DEFAULT_INVOICE_LINE_META
    end.

encode_invoice_line_tax_mode(#{<<"type">> := <<"InvoiceLineTaxVAT">>} = TaxMode) ->
    genlib_map:get(<<"rate">>, TaxMode).

-spec encode_invoice_bank_account(request_data()) -> dmsl_domain_thrift:'InvoiceBankAccount'() | undefined.
encode_invoice_bank_account(Params) ->
    do_encode_invoice_bank_account(genlib_map:get(<<"bankAccount">>, Params)).

do_encode_invoice_bank_account(#{<<"accountType">> := <<"InvoiceRussianBankAccount">>} = Account) ->
    {russian, #domain_InvoiceRussianBankAccount{
        account = maps:get(<<"account">>, Account),
        bank_bik = maps:get(<<"bankBik">>, Account)
    }};
do_encode_invoice_bank_account(undefined) ->
    undefined.

-define(DEFAULT_INVOICE_META, #{}).

-spec encode_invoice_context(request_data()) -> encode_data().
encode_invoice_context(Params) ->
    encode_invoice_context(Params, ?DEFAULT_INVOICE_META).

encode_invoice_context(Params, DefaultMeta) ->
    Context = genlib_map:get(<<"metadata">>, Params, DefaultMeta),
    encode_content(json, Context).

-spec encode_payment_context(request_data()) -> encode_data() | undefined.
encode_payment_context(#{<<"metadata">> := Context}) ->
    encode_content(json, Context);
encode_payment_context(#{}) ->
    undefined.

-spec encode_content(json, term()) -> encode_data().
encode_content(json, Data) ->
    #base_Content{
        type = <<"application/json">>,
        data = jsx:encode(Data)
    }.
