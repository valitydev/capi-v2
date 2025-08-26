-module(capi_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_conf_v2_thrift.hrl").

-export([head/0]).
-export([get_payment_institution/2]).
-export([get_payment_institutions/1]).
-export([get_country/2]).
-export([get/2]).
-export([get/3]).
-export([get_ext/3]).
-export([get_with_related/3]).
-export([get_objects_by_type/2]).
-export([encode_enum/2]).
-export([encode_enum/3]).
-export([extract_type/1]).

-type processing_context() :: capi_handler:processing_context().
-type country_code() :: dmsl_domain_thrift:'CountryCode'().
-type country_object() :: dmsl_domain_thrift:'CountryObject'().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type data() :: _.
-type revision() :: dmt_client:version().

-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type payment_institution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().
-type payment_institution_object() :: dmsl_domain_thrift:'PaymentInstitutionObject'().
-type realm() :: dmsl_domain_thrift:'PaymentInstitutionRealm'().

-export_type([realm/0]).
-export_type([revision/0]).

-spec head() -> revision().
head() ->
    dmt_client:get_latest_version().

-spec get_payment_institution(payment_institution_ref(), processing_context()) ->
    {ok, payment_institution()} | {error, not_found}.
get_payment_institution(Ref, Context) ->
    case get({payment_institution, Ref}, Context) of
        {ok, PiObject} ->
            {ok, PiObject#domain_PaymentInstitutionObject.data};
        Error ->
            Error
    end.

-spec get_payment_institutions(processing_context()) -> {ok, [payment_institution_object()]} | {error, not_found}.
get_payment_institutions(Context) ->
    try
        Opts = make_opts(Context),

        {Version, #domain_GlobalsObject{data = Globals}} =
            unwrap_versioned(dmt_client:checkout_object(latest, globals(), Opts)),

        PaymentInstitutionRefs =
            case Globals#domain_Globals.payment_institutions of
                undefined -> [];
                List -> List
            end,

        PaymentInstitutions =
            lists:map(
                fun(Ref) ->
                    {_, Object} =
                        unwrap_versioned(dmt_client:checkout_object(Version, {payment_institution, Ref}, Opts)),
                    Object
                end,
                PaymentInstitutionRefs
            ),

        {ok, PaymentInstitutions}
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            {error, not_found}
    end.

-spec get_country(country_code(), processing_context() | undefined) -> {ok, country_object()} | {error, not_found}.
get_country(CountryCode, Context) ->
    Ref = {country, #domain_CountryRef{id = CountryCode}},
    try
        get(Ref, Context)
    catch
        error:{woody_error, {internal, result_unexpected, _}} ->
            %% NOTE Object not exists if woody fails to encode country code
            {error, not_found}
    end.

-spec get(ref(), processing_context() | undefined) -> {ok, data()} | {error, not_found}.
get(Ref, Context) ->
    get(Ref, latest, Context).

-spec get(ref(), revision(), processing_context() | undefined) -> {ok, data()} | {error, not_found}.
get(Ref, Revision, Context) ->
    try
        Opts = make_opts(Context),
        {_, Object} = unwrap_versioned(dmt_client:checkout_object(Revision, Ref, Opts)),
        {ok, Object}
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            {error, not_found}
    end.

%% FIXME Naming. It supposed to return unwrapped `Data` from object
%% structured as `{Type, {Tag, Ref, Data}}`.
-spec get_ext(ref(), revision(), processing_context() | undefined) -> {ok, data()} | {error, not_found}.
get_ext(Ref, Revision, Context) ->
    try
        Opts = make_opts(Context),
        {ok, extract_data(unwrap_versioned(dmt_client:checkout_object(Revision, Ref, Opts)))}
    catch
        throw:#domain_conf_v2_ObjectNotFound{} ->
            {error, not_found}
    end.

extract_data({_Tag, {_Name, _Ref, Data}}) ->
    Data.

-spec get_with_related(ref(), revision(), processing_context() | undefined) ->
    {
        ok,
        dmt_client:domain_object(),
        ReferencedBy :: [dmt_client:domain_object()],
        ReferencesTo :: [dmt_client:domain_object()]
    }
    | {error, not_found}.
get_with_related(Ref, Revision, Context) ->
    try
        Opts = make_opts(Context),
        #domain_conf_v2_VersionedObjectWithReferences{
            object = #domain_conf_v2_VersionedObject{object = Object},
            referenced_by = ReferencedBy,
            references_to = ReferencesTo
        } =
            dmt_client:checkout_object_with_references(Revision, Ref, Opts),
        Unwrapper = fun(#domain_conf_v2_VersionedObject{object = O}) -> O end,
        {ok, Object, lists:map(Unwrapper, ReferencedBy), lists:map(Unwrapper, ReferencesTo)}
    catch
        error:version_not_found ->
            {error, not_found};
        throw:#domain_conf_v2_ObjectNotFound{} ->
            {error, not_found}
    end.

-spec encode_enum(Type :: atom(), binary()) -> {ok, atom()} | {error, unknown_atom | unknown_variant}.
encode_enum(Type, Binary) ->
    encode_enum(dmsl_domain_thrift, Type, Binary).
-spec encode_enum(Module :: atom(), Type :: atom(), binary()) -> {ok, atom()} | {error, unknown_atom | unknown_variant}.
encode_enum(Module, Type, Binary) ->
    try erlang:binary_to_existing_atom(Binary, utf8) of
        Atom ->
            {enum, Variants} = Module:enum_info(Type),
            case lists:keyfind(Atom, 1, Variants) of
                false -> {error, unknown_variant};
                _ -> {ok, Atom}
            end
    catch
        error:badarg ->
            {error, unknown_atom}
    end.

-type type() :: atom().
-type type_info() ::
    type() | {module(), type()} | dmsl_domain_thrift:struct_info().

-spec extract_type(type_info()) -> {module(), type()}.
extract_type(Name) when is_atom(Name) ->
    {dmsl_domain_thrift, Name};
extract_type({_ID, _Kind, TypeInfo, _Field, _Default}) ->
    extract_type(TypeInfo);
extract_type({Module, Name} = Type) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({struct, struct, Type = {Module, Name}}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type({enum, Type = {Module, Name}}) when is_atom(Module), is_atom(Name) ->
    Type;
extract_type(_) ->
    error(badarg).

globals() ->
    {globals, #domain_GlobalsRef{}}.

-spec get_objects_by_type(Type :: atom(), processing_context()) -> {ok, [dmsl_domain_thrift:'DomainObject'()]}.
get_objects_by_type(Type, Context) ->
    Opts = make_opts(Context),
    Objects = lists:map(
        fun(VersionedObject) ->
            {_, Object} = unwrap_versioned(VersionedObject),
            Object
        end,
        dmt_client:checkout_objects_by_type(latest, Type, Opts)
    ),
    {ok, Objects}.

-spec make_opts(processing_context()) -> dmt_client:opts().
make_opts(#{woody_context := WoodyContext}) ->
    #{woody_context => WoodyContext};
make_opts(_) ->
    #{}.

unwrap_versioned(#domain_conf_v2_VersionedObject{
    info = #domain_conf_v2_VersionedObjectInfo{version = Version},
    object = {_Type, Object}
}) ->
    {Version, Object}.
