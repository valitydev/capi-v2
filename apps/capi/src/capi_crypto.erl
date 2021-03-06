-module(capi_crypto).

-include_lib("damsel/include/dmsl_paytool_token_thrift.hrl").

-type token() :: binary().
-type token_data() :: #{
    payment_tool := payment_tool(),
    valid_until := deadline()
}.
-type payment_tool() :: dmsl_domain_thrift:'PaymentTool'().
-type payment_tool_token() :: dmsl_paytool_token_thrift:'PaymentToolToken'().
-type payment_tool_token_payload() :: dmsl_paytool_token_thrift:'PaymentToolTokenPayload'().
-type deadline() :: capi_utils:deadline().

-export_type([token/0]).
-export_type([token_data/0]).

-export([encode_token/1]).
-export([decode_token/1]).

-spec encode_token(token_data()) -> token().
encode_token(TokenData) ->
    PaymentToolToken = encode_payment_tool_token(TokenData),
    ThriftType = {struct, struct, {dmsl_paytool_token_thrift, 'PaymentToolToken'}},
    {ok, EncodedToken} = lechiffre:encode(ThriftType, PaymentToolToken),
    TokenVersion = token_version(),
    <<TokenVersion/binary, ".", EncodedToken/binary>>.

-spec decode_token(token()) -> {ok, token_data()} | unrecognized | {error, lechiffre:decoding_error()}.
decode_token(Token) ->
    Ver = token_version(),
    Size = byte_size(Ver),
    case Token of
        <<Ver:Size/binary, $., EncryptedPaymentToolToken/binary>> ->
            decrypt_token(EncryptedPaymentToolToken);
        _ ->
            unrecognized
    end.

%% Internal

token_version() ->
    <<"v2">>.

decrypt_token(EncryptedPaymentToolToken) ->
    ThriftType = {struct, struct, {dmsl_paytool_token_thrift, 'PaymentToolToken'}},
    case lechiffre:decode(ThriftType, EncryptedPaymentToolToken) of
        {ok, PaymentToolToken} ->
            Payload = PaymentToolToken#paytool_token_PaymentToolToken.payload,
            ValidUntil = PaymentToolToken#paytool_token_PaymentToolToken.valid_until,
            {ok, #{
                payment_tool => decode_payment_tool_token_payload(Payload),
                valid_until => decode_deadline(ValidUntil)
            }};
        {error, _} = Error ->
            Error
    end.

-spec encode_payment_tool_token(token_data()) -> payment_tool_token().
encode_payment_tool_token(TokenData) ->
    Payload = maps:get(payment_tool, TokenData),
    ValidUntil = maps:get(valid_until, TokenData),
    #paytool_token_PaymentToolToken{
        payload = encode_payment_tool_token_payload(Payload),
        valid_until = encode_deadline(ValidUntil)
    }.

-spec encode_deadline(deadline()) -> binary() | undefined.
encode_deadline(undefined) ->
    undefined;
encode_deadline(Deadline) ->
    capi_utils:deadline_to_binary(Deadline).

-spec encode_payment_tool_token_payload(payment_tool()) -> payment_tool_token_payload().
encode_payment_tool_token_payload({bank_card, BankCard}) ->
    {bank_card_payload, #paytool_token_BankCardPayload{
        bank_card = BankCard
    }};
encode_payment_tool_token_payload({payment_terminal, PaymentTerminal}) ->
    {payment_terminal_payload, #paytool_token_PaymentTerminalPayload{
        payment_terminal = PaymentTerminal
    }};
encode_payment_tool_token_payload({digital_wallet, DigitalWallet}) ->
    {digital_wallet_payload, #paytool_token_DigitalWalletPayload{
        digital_wallet = DigitalWallet
    }};
encode_payment_tool_token_payload({crypto_currency, CryptoCurrency}) ->
    {crypto_currency_payload, #paytool_token_CryptoCurrencyPayload{
        crypto_currency = CryptoCurrency
    }};
encode_payment_tool_token_payload({mobile_commerce, MobileCommerce}) ->
    {mobile_commerce_payload, #paytool_token_MobileCommercePayload{
        mobile_commerce = MobileCommerce
    }}.

-spec decode_deadline(binary()) -> deadline() | undefined.
decode_deadline(undefined) ->
    undefined;
decode_deadline(Deadline) ->
    capi_utils:deadline_from_binary(Deadline).

-spec decode_payment_tool_token_payload(payment_tool_token_payload()) -> payment_tool().
decode_payment_tool_token_payload(PaymentToolToken) ->
    case PaymentToolToken of
        {bank_card_payload, Payload} ->
            {bank_card, Payload#paytool_token_BankCardPayload.bank_card};
        {payment_terminal_payload, Payload} ->
            {payment_terminal, Payload#paytool_token_PaymentTerminalPayload.payment_terminal};
        {digital_wallet_payload, Payload} ->
            {digital_wallet, Payload#paytool_token_DigitalWalletPayload.digital_wallet};
        {crypto_currency_payload, Payload} ->
            {crypto_currency, Payload#paytool_token_CryptoCurrencyPayload.crypto_currency};
        {mobile_commerce_payload, Payload} ->
            {mobile_commerce, Payload#paytool_token_MobileCommercePayload.mobile_commerce}
    end.
