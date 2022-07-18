-module(capi_handler_decoder_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([decode_shop_location/1]).
-export([decode_shop_details/1]).
-export([decode_contact_info/1]).
-export([decode_party/1]).
-export([is_blocked/1]).
-export([is_suspended/1]).
-export([decode_contractor/1]).
-export([decode_legal_agreement/1]).
-export([decode_reporting_preferences/1]).
-export([decode_residence/1]).
-export([decode_payment_institution_ref/1]).
-export([decode_disposable_payment_resource/1]).
-export([decode_payout_tool_details/1]).
-export([decode_payment_tool/1]).
-export([decode_payment_tool_details/1]).

%%

-spec decode_shop_location(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_shop_location({url, Location}) ->
    #{
        <<"locationType">> => <<"ShopLocationUrl">>,
        <<"url">> => Location
    }.

-spec decode_shop_details(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_shop_details(#domain_ShopDetails{name = Name, description = Description}) ->
    genlib_map:compact(#{
        <<"name">> => Name,
        <<"description">> => Description
    }).

-spec decode_contact_info(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_contact_info(#domain_ContactInfo{phone_number = PhoneNumber, email = Email}) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => PhoneNumber,
        <<"email">> => Email
    }).

-spec decode_party(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_party(#domain_Party{id = PartyID, blocking = Blocking, suspension = Suspension}) ->
    #{
        <<"id">> => PartyID,
        <<"isBlocked">> => is_blocked(Blocking),
        <<"isSuspended">> => is_suspended(Suspension)
    }.

-spec is_blocked({blocked | unblocked, _}) -> true | false.
is_blocked({blocked, _}) -> true;
is_blocked({unblocked, _}) -> false.

-spec is_suspended({suspended | active, _}) -> true | false.
is_suspended({suspended, _}) -> true;
is_suspended({active, _}) -> false.

-spec decode_contractor(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_contractor({legal_entity, LegalEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"LegalEntity">>}, decode_legal_entity(LegalEntity));
decode_contractor({private_entity, PrivateEntity}) ->
    maps:merge(#{<<"contractorType">> => <<"PrivateEntity">>}, decode_private_entity(PrivateEntity));
decode_contractor({registered_user, RegisteredUser}) ->
    maps:merge(#{<<"contractorType">> => <<"RegisteredUser">>}, decode_registered_user(RegisteredUser)).

decode_legal_entity({russian_legal_entity, LegalEntity}) ->
    #{
        <<"entityType">> => <<"RussianLegalEntity">>,
        <<"registeredName">> => LegalEntity#domain_RussianLegalEntity.registered_name,
        <<"registeredNumber">> => LegalEntity#domain_RussianLegalEntity.registered_number,
        <<"inn">> => LegalEntity#domain_RussianLegalEntity.inn,
        <<"actualAddress">> => LegalEntity#domain_RussianLegalEntity.actual_address,
        <<"postAddress">> => LegalEntity#domain_RussianLegalEntity.post_address,
        <<"representativePosition">> => LegalEntity#domain_RussianLegalEntity.representative_position,
        <<"representativeFullName">> => LegalEntity#domain_RussianLegalEntity.representative_full_name,
        <<"representativeDocument">> => LegalEntity#domain_RussianLegalEntity.representative_document,
        <<"bankAccount">> =>
            decode_russian_bank_account(LegalEntity#domain_RussianLegalEntity.russian_bank_account, #{})
    };
decode_legal_entity({international_legal_entity, LegalEntity}) ->
    genlib_map:compact(#{
        <<"entityType">> => <<"InternationalLegalEntity">>,
        <<"legalName">> => LegalEntity#domain_InternationalLegalEntity.legal_name,
        <<"tradingName">> => LegalEntity#domain_InternationalLegalEntity.trading_name,
        <<"registeredOffice">> => LegalEntity#domain_InternationalLegalEntity.registered_address,
        <<"principalPlaceOfBusiness">> => LegalEntity#domain_InternationalLegalEntity.actual_address,
        <<"registeredNumber">> => LegalEntity#domain_InternationalLegalEntity.registered_number
    }).

decode_private_entity({russian_private_entity, PrivateEntity}) ->
    #{
        <<"entityType">> => <<"RussianPrivateEntity">>,
        <<"firstName">> => PrivateEntity#domain_RussianPrivateEntity.first_name,
        <<"secondName">> => PrivateEntity#domain_RussianPrivateEntity.second_name,
        <<"middleName">> => PrivateEntity#domain_RussianPrivateEntity.middle_name,
        <<"contactInfo">> => decode_contact_info(PrivateEntity#domain_RussianPrivateEntity.contact_info)
    }.

decode_registered_user(#domain_RegisteredUser{email = Email}) ->
    #{<<"email">> => Email}.

-spec decode_legal_agreement(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_legal_agreement(
    #domain_LegalAgreement{
        signed_at = SignedAt,
        legal_agreement_id = ID,
        valid_until = ValidUntil
    }
) ->
    genlib_map:compact(#{
        <<"id">> => ID,
        <<"signedAt">> => SignedAt,
        <<"validUntil">> => ValidUntil
    }).

-spec decode_reporting_preferences(capi_handler_encoder:encode_data()) -> capi_handler_decoder_utils:decode_data().
decode_reporting_preferences(#domain_ReportPreferences{
    service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
        schedule = ScheduleRef,
        signer = Signer
    }
}) ->
    #{
        <<"serviceAcceptanceActPreferences">> => #{
            <<"scheduleID">> => capi_handler_decoder_utils:decode_business_schedule_ref(ScheduleRef),
            <<"signer">> => decode_representative(Signer)
        }
    };
decode_reporting_preferences(#domain_ReportPreferences{service_acceptance_act_preferences = undefined}) ->
    #{}.

decode_representative(#domain_Representative{
    position = Position,
    full_name = Name,
    document = Document
}) ->
    #{
        <<"position">> => Position,
        <<"fullName">> => Name,
        <<"document">> => decode_representative_document(Document)
    }.

decode_representative_document({articles_of_association, #domain_ArticlesOfAssociation{}}) ->
    #{
        <<"representativeDocumentType">> => <<"ArticlesOfAssociation">>
    };
decode_representative_document({power_of_attorney, LegalAgreement}) ->
    maps:merge(
        #{<<"representativeDocumentType">> => <<"PowerOfAttorney">>},
        decode_legal_agreement(LegalAgreement)
    ).

-spec decode_residence(atom() | undefined) -> binary().
decode_residence(undefined) ->
    undefined;
decode_residence(Residence) when is_atom(Residence) ->
    list_to_binary(string:to_upper(atom_to_list(Residence))).

-spec decode_payment_institution_ref(capi_handler_encoder:encode_data()) -> integer().
decode_payment_institution_ref(#domain_PaymentInstitutionRef{id = Ref}) ->
    Ref.

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

-spec decode_payout_tool_details({atom(), _}) -> capi_handler_decoder_utils:decode_data().
decode_payout_tool_details({russian_bank_account, V}) ->
    decode_russian_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankAccount">>});
decode_payout_tool_details({international_bank_account, V}) ->
    decode_international_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>});
decode_payout_tool_details({wallet_info, V}) ->
    #{
        <<"detailsType">> => <<"PayoutToolDetailsWalletInfo">>,
        <<"walletID">> => V#domain_WalletInfo.wallet_id
    };
decode_payout_tool_details({payment_institution_account, _V}) ->
    #{
        <<"detailsType">> => <<"PayoutToolDetailsPaymentInstitutionAccount">>
    }.

decode_russian_bank_account(BankAccount, V) ->
    V#{
        <<"account">> => BankAccount#domain_RussianBankAccount.account,
        <<"bankName">> => BankAccount#domain_RussianBankAccount.bank_name,
        <<"bankPostAccount">> => BankAccount#domain_RussianBankAccount.bank_post_account,
        <<"bankBik">> => BankAccount#domain_RussianBankAccount.bank_bik
    }.

decode_international_bank_account(undefined, _) ->
    undefined;
decode_international_bank_account(BankAccount, V) ->
    genlib_map:compact(V#{
        <<"number">> => BankAccount#domain_InternationalBankAccount.number,
        <<"iban">> => BankAccount#domain_InternationalBankAccount.iban,
        <<"bankDetails">> => decode_international_bank_details(
            BankAccount#domain_InternationalBankAccount.bank
        ),
        <<"correspondentBankAccount">> => decode_international_bank_account(
            BankAccount#domain_InternationalBankAccount.correspondent_account,
            #{}
        )
    }).

decode_international_bank_details(undefined) ->
    undefined;
decode_international_bank_details(Bank) ->
    genlib_map:compact(#{
        <<"bic">> => Bank#domain_InternationalBankDetails.bic,
        <<"abartn">> => Bank#domain_InternationalBankDetails.aba_rtn,
        <<"name">> => Bank#domain_InternationalBankDetails.name,
        <<"countryCode">> => decode_residence(Bank#domain_InternationalBankDetails.country),
        <<"address">> => Bank#domain_InternationalBankDetails.address
    }).

decode_mobile_phone(#domain_MobilePhone{cc = Cc, ctn = Ctn}) ->
    #{<<"cc">> => Cc, <<"ctn">> => Ctn}.

gen_phone_number(#{<<"cc">> := Cc, <<"ctn">> := Ctn}) ->
    <<"+", Cc/binary, Ctn/binary>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec mask_phone_number_test_() -> [_TestCase].
mask_phone_number_test_() ->
    [
        ?_assertEqual(<<"+7******7890">>, mask_phone_number(<<"+71234567890">>)),
        ?_assertEqual(<<"+7*23">>, mask_phone_number(<<"+7123">>)),
        ?_assertEqual(<<"+1NOTANUMBER">>, mask_phone_number(<<"+1NOTANUMBER">>))
    ].

-endif.
