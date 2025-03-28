-module(capi_handler_contracts).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare('GetContracts' = OperationID, _Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        {ok, Party} = capi_party:get_party(PartyID, Context),
        {ok, {200, #{}, decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractByID' = OperationID, Req, Context) ->
    ContractID = maps:get('contractID', Req),
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, contract => ContractID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        % Получение Party требуется для извлечения domain_Party.contractors
        {ok, Party} = capi_party:get_party(PartyID, Context),
        case genlib_map:get(ContractID, Party#domain_Party.contracts) of
            undefined ->
                {ok, general_error(404, <<"Contract not found">>)};
            Contract ->
                {ok, {200, #{}, decode_contract(Contract, Party#domain_Party.contractors)}}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractAdjustments' = OperationID, Req, Context) ->
    ContractID = maps:get('contractID', Req),
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, contract => ContractID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Contract = get_contract_or_fail(PartyID, ContractID, Context),
        Resp = [decode_contract_adjustment(A) || A <- Contract#domain_Contract.adjustments],
        {ok, {200, #{}, Resp}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractAdjustmentByID' = OperationID, Req, Context) ->
    ContractID = maps:get('contractID', Req),
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, contract => ContractID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Contract = get_contract_or_fail(PartyID, ContractID, Context),
        AdjustmentID = maps:get('adjustmentID', Req),
        Adjustments = Contract#domain_Contract.adjustments,
        case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
            #domain_ContractAdjustment{} = A ->
                {ok, {200, #{}, decode_contract_adjustment(A)}};
            false ->
                {ok, general_error(404, <<"Adjustment not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractsForParty' = OperationID, Req, Context) ->
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        case capi_party:get_party(PartyID, Context) of
            {ok, Party} ->
                {ok, {200, #{}, decode_contracts_map(Party#domain_Party.contracts, Party#domain_Party.contractors)}};
            {error, #payproc_PartyNotFound{}} ->
                {ok, general_error(404, <<"Party not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractByIDForParty' = OperationID, Req, Context) ->
    ContractID = maps:get('contractID', Req),
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, contract => ContractID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Contract = get_contract_or_fail(PartyID, ContractID, Context),
        % Получение Party требуется для извлечения domain_Party.contractors
        {ok, Party} = capi_party:get_party(PartyID, Context),
        {ok, {200, #{}, decode_contract(Contract, Party#domain_Party.contractors)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractAdjustmentsForParty' = OperationID, Req, Context) ->
    ContractID = maps:get('contractID', Req),
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, contract => ContractID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Contract = get_contract_or_fail(PartyID, ContractID, Context),
        Resp = [decode_contract_adjustment(A) || A <- Contract#domain_Contract.adjustments],
        {ok, {200, #{}, Resp}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetContractAdjustmentByIDForParty' = OperationID, Req, Context) ->
    ContractID = maps:get('contractID', Req),
    PartyID = maps:get('partyID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, contract => ContractID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun() ->
        Contract = get_contract_or_fail(PartyID, ContractID, Context),
        AdjustmentID = maps:get('adjustmentID', Req),
        Adjustments = Contract#domain_Contract.adjustments,
        case lists:keyfind(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
            #domain_ContractAdjustment{} = A ->
                {ok, {200, #{}, decode_contract_adjustment(A)}};
            false ->
                {ok, general_error(404, <<"Adjustment not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

get_contract_or_fail(PartyID, ContractID, Context) ->
    case capi_party:get_contract(PartyID, ContractID, Context) of
        {ok, Contract} ->
            Contract;
        {error, #payproc_PartyNotFound{}} ->
            capi_handler:respond(general_error(404, <<"Party not found">>));
        {error, #payproc_ContractNotFound{}} ->
            capi_handler:respond(general_error(404, <<"Contract not found">>))
    end.

%%

decode_contracts_map(Contracts, Contractors) ->
    capi_handler_decoder_utils:decode_map(Contracts, fun(C) -> decode_contract(C, Contractors) end).

decode_contract(Contract, Contractors) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Contract#domain_Contract.id,
            <<"createdAt">> => Contract#domain_Contract.created_at,
            <<"contractor">> => capi_handler_decoder_party:decode_contractor(
                get_contractor(Contract, Contractors)
            ),
            <<"paymentInstitutionID">> =>
                capi_handler_decoder_party:decode_payment_institution_ref(Contract#domain_Contract.payment_institution),
            <<"validSince">> => Contract#domain_Contract.valid_since,
            <<"validUntil">> => Contract#domain_Contract.valid_until,
            <<"legalAgreement">> => capi_handler_decoder_utils:decode_optional(
                Contract#domain_Contract.legal_agreement,
                fun capi_handler_decoder_party:decode_legal_agreement/1
            ),
            <<"reportingPreferences">> => capi_handler_decoder_utils:decode_optional(
                Contract#domain_Contract.report_preferences,
                fun capi_handler_decoder_party:decode_reporting_preferences/1
            )
        },
        decode_contract_status(Contract#domain_Contract.status)
    ).

decode_contract_status({active, _}) ->
    #{
        <<"status">> => <<"active">>
    };
decode_contract_status({terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}) ->
    #{
        <<"status">> => <<"terminated">>,
        <<"terminatedAt">> => TerminatedAt
    }.

get_contractor(#domain_Contract{contractor = Contractor}, _) when Contractor =/= undefined ->
    Contractor;
get_contractor(#domain_Contract{contractor_id = ContractorID}, Contractors) ->
    #domain_PartyContractor{
        contractor = Contractor
    } = maps:get(ContractorID, Contractors),
    Contractor.

decode_contract_adjustment(ContractAdjustment) ->
    genlib_map:compact(#{
        <<"id">> => ContractAdjustment#domain_ContractAdjustment.id,
        <<"createdAt">> => ContractAdjustment#domain_ContractAdjustment.created_at,
        <<"validSince">> => ContractAdjustment#domain_ContractAdjustment.valid_since,
        <<"validUntil">> => ContractAdjustment#domain_ContractAdjustment.valid_until
    }).
