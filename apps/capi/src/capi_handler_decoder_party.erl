-module(capi_handler_decoder_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([decode_shop_location/1]).
-export([decode_shop_details/1]).
-export([decode_contact_info/1]).
-export([decode_party/1]).
-export([is_blocked/1]).
-export([is_suspended/1]).
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
decode_party(#domain_PartyConfig{id = PartyID, blocking = Blocking, suspension = Suspension}) ->
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

-spec decode_residence(atom() | undefined) -> binary().
decode_residence(undefined) ->
    undefined;
decode_residence(Residence) when is_atom(Residence) ->
    list_to_binary(string:to_upper(atom_to_list(Residence))).

-spec decode_payment_institution_ref(capi_handler_encoder:encode_data()) -> integer().
decode_payment_institution_ref(#domain_PaymentInstitutionRef{id = Ref}) ->
    Ref.
