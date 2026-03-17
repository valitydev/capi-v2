-module(capi_handler_parties).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).

-type processing_context() :: capi_handler:processing_context().

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('GetPartyByID' = OperationID, Req, Context) ->
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        {ok, mask_party_notfound(capi_auth:authorize_operation(Prototypes, Context))}
    end,
    Process = fun() ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                DecodedParty = capi_handler_decoder_party:decode_party(PartyID, Party),
                {ok, {200, #{}, DecodedParty}};
            {error, not_found} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('ActivatePartyByID', _Req, _Context) ->
    {error, noimpl};
prepare('SuspendPartyByID', _Req, _Context) ->
    {error, noimpl};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

mask_party_notfound(Resolution) ->
    % ED-206
    % When bouncer says "forbidden" we can't really tell the difference between "forbidden because
    % of no such party", "forbidden because client has no access to it" and "forbidden because
    % client has no permission to act on it". From the point of view of existing integrations this
    % is not great, so we have to mask specific instances of missing authorization as if specified
    % party is nonexistent.
    capi_handler:respond_if_forbidden(Resolution, general_error(404, <<"Party not found">>)).
