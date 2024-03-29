-module(capi_bouncer_restrictions).

-include_lib("bouncer_proto/include/bouncer_rstn_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").

%% API
-export([get_restricted_shop_ids/1]).

-export_type([t/0]).

-type t() :: bouncer_rstn_thrift:'Restrictions'().

-spec get_restricted_shop_ids(t()) -> [binary()].
get_restricted_shop_ids(Restrictions) ->
    #rstn_Restrictions{
        capi = #rstn_RestrictionsCommonAPI{
            op = #rstn_CommonAPIOperationRestrictions{
                shops = ShopEntities
            }
        }
    } = Restrictions,
    [ID || #base_Entity{id = ID} <- ordsets:to_list(ShopEntities)].
