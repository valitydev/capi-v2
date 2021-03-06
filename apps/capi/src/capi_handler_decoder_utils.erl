-module(capi_handler_decoder_utils).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").

-export([decode_map/2]).
-export([decode_currency/1]).
-export([decode_payment_system_ref/1]).
-export([decode_payment_service_ref/1]).
-export([decode_crypto_currency_ref/1]).
-export([decode_bank_card_token_service_ref/1]).
-export([decode_mobile_operator_ref/1]).
-export([decode_business_schedule_ref/1]).
-export([decode_bank_card_bin/1]).
-export([decode_last_digits/1]).
-export([decode_masked_pan/2]).
-export([decode_operation_failure/1]).
-export([decode_category_ref/1]).
-export([decode_context/1]).
-export([decode_optional/2]).
-export([decode_metadata/1]).
-export([decode_namespaced_metadata/1]).

-export_type([decode_data/0]).

-type decode_data() :: #{binary() => term()}.
-type encoded_currency() :: dmsl_domain_thrift:'Currency'() | dmsl_domain_thrift:'CurrencyRef'().
-type encoded_payment_system_ref() :: dmsl_domain_thrift:'PaymentSystemRef'().
-type encoded_payment_service_ref() :: dmsl_domain_thrift:'PaymentServiceRef'().
-type encoded_crypto_currency_ref() :: dmsl_domain_thrift:'CryptoCurrencyRef'().
-type encoded_bank_card_token_service_ref() :: dmsl_domain_thrift:'BankCardTokenServiceRef'().
-type encoded_mobile_operator_ref() :: dmsl_domain_thrift:'MobileOperatorRef'().

-spec decode_map(map(), fun((_) -> any())) -> [any()].
decode_map(Items, Fun) ->
    lists:map(Fun, maps:values(Items)).

-spec decode_currency(encoded_currency()) -> binary().
decode_currency(#domain_Currency{symbolic_code = SymbolicCode}) -> SymbolicCode;
decode_currency(#domain_CurrencyRef{symbolic_code = SymbolicCode}) -> SymbolicCode.

-spec decode_payment_system_ref(encoded_payment_system_ref()) -> binary().
decode_payment_system_ref(#domain_PaymentSystemRef{id = ID}) -> ID.

-spec decode_payment_service_ref(encoded_payment_service_ref()) -> binary().
decode_payment_service_ref(#domain_PaymentServiceRef{id = ID}) -> ID.

-spec decode_crypto_currency_ref(encoded_crypto_currency_ref()) -> binary().
decode_crypto_currency_ref(#domain_CryptoCurrencyRef{id = ID}) -> ID.

-spec decode_bank_card_token_service_ref(encoded_bank_card_token_service_ref()) -> binary().
decode_bank_card_token_service_ref(#domain_BankCardTokenServiceRef{id = ID}) -> ID.

-spec decode_mobile_operator_ref(encoded_mobile_operator_ref()) -> binary().
decode_mobile_operator_ref(#domain_MobileOperatorRef{id = ID}) -> ID.

-spec decode_business_schedule_ref(capi_handler_encoder:encode_data()) -> binary() | undefined.
decode_business_schedule_ref(#domain_BusinessScheduleRef{id = ID}) when ID /= undefined ->
    ID;
decode_business_schedule_ref(undefined) ->
    undefined.

-spec decode_bank_card_bin(binary()) -> binary() | undefined.
decode_bank_card_bin(<<>>) ->
    undefined;
decode_bank_card_bin(Bin) when byte_size(Bin) > 6 ->
    %%backwards compatibility with old data
    binary:part(Bin, {0, 6});
decode_bank_card_bin(Bin) ->
    Bin.

-define(PAN_LENGTH, 16).

-spec decode_masked_pan(binary() | undefined, binary()) -> binary().
decode_masked_pan(undefined, LastDigits) ->
    decode_masked_pan(<<>>, LastDigits);
decode_masked_pan(Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, ?PAN_LENGTH - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec decode_last_digits(binary()) -> binary().
decode_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_last_digits(MaskedPan) ->
    MaskedPan.

-spec decode_operation_failure(_) -> decode_data().
decode_operation_failure({operation_timeout, _}) ->
    logic_error(timeout, <<"timeout">>);
decode_operation_failure({failure, #domain_Failure{code = Code, reason = Reason}}) ->
    logic_error(Code, Reason).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

-spec decode_category_ref(capi_handler_encoder:encode_data()) -> integer().
decode_category_ref(#domain_CategoryRef{id = CategoryRef}) ->
    CategoryRef.

-spec decode_context(capi_handler_encoder:encode_data()) -> decode_data() | undefined.
decode_context(#base_Content{type = <<"application/json">>, data = InvoiceContext}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext, [return_maps]);
decode_context(undefined) ->
    undefined.

-spec decode_optional(any() | undefined, fun((any()) -> decode_data())) -> decode_data().
decode_optional(Arg, DecodeFun) when Arg /= undefined ->
    DecodeFun(Arg);
decode_optional(undefined, _) ->
    undefined.

-spec decode_metadata(dmsl_domain_thrift:'Metadata'()) -> capi_json_marshalling:value().
decode_metadata(MD) ->
    capi_json_marshalling:unmarshal(MD).

-spec decode_namespaced_metadata(#{NS => dmsl_domain_thrift:'Metadata'()}) ->
    #{NS => capi_json_marshalling:value()}
when
    NS :: binary().
decode_namespaced_metadata(NamespacedMD) ->
    maps:map(fun(_NS, MD) -> decode_metadata(MD) end, NamespacedMD).
