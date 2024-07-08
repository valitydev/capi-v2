-module(capi_mutation).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([encode_amount_randomization_params/1]).
-export([decode_amount_randomization_params/1]).
-export([decode_invoice_amount_mutation/1]).

%%

-type mutation_param() :: dmsl_payproc_thrift:'InvoiceMutationParams'().
-type mutation() :: dmsl_domain_thrift:'InvoiceMutation'().

-spec encode_amount_randomization_params(map() | undefined) -> [mutation_param()] | undefined.
encode_amount_randomization_params(Opts) when is_map(Opts) ->
    [
        {amount,
            {randomization, #domain_RandomizationMutationParams{
                deviation = maps:get(<<"deviation">>, Opts),
                precision = maps:get(<<"precision">>, Opts, 2),
                direction = binary_to_existing_atom(maps:get(<<"direction">>, Opts, <<"both">>)),
                min_amount_condition = maps:get(<<"minAmountCondition">>, Opts, undefined),
                max_amount_condition = maps:get(<<"maxAmountCondition">>, Opts, 2),
                amount_multiplicity_condition = maps:get(<<"amountMultiplicityCondition">>, Opts, undefined)
            }}}
    ];
encode_amount_randomization_params(_) ->
    undefined.

-spec decode_amount_randomization_params([mutation_param()] | undefined) -> map() | undefined.
decode_amount_randomization_params(Mutations) when is_list(Mutations) ->
    lists:foldl(
        fun
            (
                {amount,
                    {randomization, #domain_RandomizationMutationParams{
                        deviation = Deviation,
                        precision = Precision,
                        direction = Direction,
                        min_amount_condition = MinAmountCondition,
                        max_amount_condition = MaxAmountCondition,
                        amount_multiplicity_condition = AmountMultiplicityCondition
                    }}},
                _
            ) ->
                #{
                    <<"deviation">> => Deviation,
                    <<"precision">> => Precision,
                    <<"direction">> => atom_to_binary(Direction),
                    <<"minAmountCondition">> => MinAmountCondition,
                    <<"maxAmountCondition">> => MaxAmountCondition,
                    <<"amountMultiplicityCondition">> => AmountMultiplicityCondition
                };
            (_, Opts) ->
                Opts
        end,
        undefined,
        Mutations
    );
decode_amount_randomization_params(_) ->
    undefined.

-spec decode_invoice_amount_mutation([mutation()] | undefined) -> map() | undefined.
decode_invoice_amount_mutation(Mutations) when is_list(Mutations) ->
    lists:foldl(
        fun
            ({amount, #domain_InvoiceAmountMutation{original = Original, mutated = Mutated}}, _) ->
                #{
                    <<"original">> => Original,
                    <<"randomized">> => Mutated
                };
            (_, AmountRandomized) ->
                AmountRandomized
        end,
        undefined,
        Mutations
    );
decode_invoice_amount_mutation(_) ->
    undefined.
