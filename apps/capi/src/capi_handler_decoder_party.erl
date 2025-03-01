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
decode_contact_info(#domain_ContactInfo{
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
}) ->
    genlib_map:compact(#{
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

decode_russian_bank_account(BankAccount, V) ->
    V#{
        <<"account">> => BankAccount#domain_RussianBankAccount.account,
        <<"bankName">> => BankAccount#domain_RussianBankAccount.bank_name,
        <<"bankPostAccount">> => BankAccount#domain_RussianBankAccount.bank_post_account,
        <<"bankBik">> => BankAccount#domain_RussianBankAccount.bank_bik
    }.
