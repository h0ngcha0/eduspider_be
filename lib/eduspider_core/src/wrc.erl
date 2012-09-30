%% -------------------------------------------------------------------
%%
%% Copyright (c) 2009-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Wrapper for Riak clients.  Supports HTTP and Protocol Buffers.

%%%_* Module declaration ===============================================
-module(wrc).

%%%_* Exports ==========================================================
-export([ delete/3
        , delete/4
        , disconnect/1
        , get/3
        , get/4
        , get_bucket/2
        , get_client_id/1
        , get_server_info/1
        , list_buckets/1
        , list_keys/2
        , mapred/3
        , mapred/4
        , mapred_bucket/3
        , mapred_bucket/4
        , mapred_bucket_stream/5
        , mapred_stream/4
        , mapred_stream/5
        , ping/1
        , put/2
        , put/3
        , run/1
        , run/2
        , set_bucket/3
        , set_client_id/2
        , stream_list_keys/2
        ]).

%%%_* Records ==========================================================
-record(wrc, {module, client}).

%%%_* Macros ===========================================================
%-define(DEFAULT_RIAK, {http, {"127.0.0.1", 8098, "riak"}}).
-define(DEFAULT_RIAK,        {pb, {"127.0.0.1", 8087}}).

-define(PASS0(RC, Command),
        (RC#wrc.module):Command(RC#wrc.client)).
-define(PASS1(RC, Command, P1),
        (RC#wrc.module):Command(RC#wrc.client, P1)).
-define(PASS2(RC, Command, P1, P2),
        (RC#wrc.module):Command(RC#wrc.client, P1, P2)).
-define(PASS3(RC, Command, P1, P2, P3),
        (RC#wrc.module):Command(RC#wrc.client, P1, P2, P3)).
-define(PASS4(RC, Command, P1, P2, P3, P4),
        (RC#wrc.module):Command(RC#wrc.client, P1, P2, P3, P4)).

%%%_* Code =============================================================

run(JobFun)                                    ->
  run(random_client_id(), JobFun).
run(ClientId, JobFun) when is_binary(ClientId) ->
  RunJobFun = fun(Client) ->
                  {ok, WrcClient} =
                    case eduspider_core:get_app_env(riak, ?DEFAULT_RIAK) of
                      {http, _Node} ->
                        connect_http(Client, ClientId);
                      {pb,   _Node} ->
                        connect_pb(Client, ClientId)
                    end,
                  JobFun(WrcClient)
              end,
  pooler_run(RunJobFun).

pooler_run(RunJobFun) ->
  Client = pooler:take_member(),
  Result = RunJobFun(Client),
  pooler:return_member(Client, ok),
  Result.

connect_http(Client, _ClientId) ->
  %% C = rhc:create(IP, Port, Prefix, [{client_id, ClientId}]),
  {ok, #wrc{module = rhc,
            client = Client}}.

connect_pb(Client, ClientId) ->
  ok = riakc_pb_socket:set_client_id(Client, ClientId),
  {ok, #wrc{module = riakc_pb_socket,
            client = Client}}.

disconnect(#wrc{module=riakc_pb_socket, client=C}) ->
  riakc_pb_socket:stop(C);
disconnect(_WRC)                                   ->
  ok.

ping(RC) ->
  ?PASS0(RC, ping).

get_client_id(RC) ->
  ?PASS0(RC, get_client_id).

set_client_id(#wrc{module=rhc, client=C}, ClientId) ->
  connect_http(ClientId,
               {rhc:ip(C), rhc:port(C), rhc:prefix(C)});
set_client_id(WRC=#wrc{module=riakc_pb_socket, client=C},
              ClientId) ->
  riakc_pb_socket:set_client_id(C, ClientId),
  {ok, WRC}.

get_server_info(RC) ->
  ?PASS0(RC, get_server_info).

get(RC, Bucket, Key)          ->
  to_wobj(?PASS2(RC, get, Bucket, Key)).
get(RC, Bucket, Key, Options) ->
  to_wobj(?PASS3(RC, get, Bucket, Key, Options)).

to_wobj({ok, RCObj}) ->
  {ok, wobj:from_riakc_obj(RCObj)};
to_wobj(Error)       ->
  Error.

put(RC, Obj)          ->
  ?PASS1(RC, put, wobj:to_riakc_obj(Obj)).
put(RC, Obj, Options) ->
  ?PASS2(RC, put, wobj:to_riakc_obj(Obj), Options).

delete(RC, Bucket, Key)          ->
  ?PASS2(RC, delete, Bucket, Key).
delete(RC, Bucket, Key, Options) ->
  ?PASS3(RC, delete, Bucket, Key, Options).

list_buckets(RC) ->
  ?PASS0(RC, list_buckets).

list_keys(RC, Bucket) ->
  ?PASS1(RC, list_keys, Bucket).

stream_list_keys(RC, Bucket) ->
  ?PASS1(RC, stream_list_keys, Bucket).

get_bucket(RC, Bucket)              ->
  ?PASS1(RC, get_bucket, Bucket).

set_bucket(RC, Bucket, BucketProps) ->
  ?PASS2(RC, set_bucket, Bucket, BucketProps).

mapred(RC, Inputs, Query)          ->
  ?PASS2(RC, mapred, Inputs, Query).
mapred(RC, Inputs, Query, Timeout) ->
  ?PASS3(RC, mapred, Inputs, Query, Timeout).

mapred_stream(RC, Inputs, Query, ClientPid)          ->
  ?PASS3(RC, mapred_stream, Inputs, Query, ClientPid).
mapred_stream(RC, Inputs, Query, ClientPid, Timeout) ->
  ?PASS4(RC, mapred_stream, Inputs, Query, ClientPid, Timeout).

mapred_bucket(RC, Bucket, Query)          ->
  ?PASS2(RC, mapred_bucket, Bucket, Query).
mapred_bucket(RC, Bucket, Query, Timeout) ->
  ?PASS3(RC, mapred_bucket, Bucket, Query, Timeout).

mapred_bucket_stream(RC, Bucket, Query, ClientPid, Timeout) ->
  ?PASS4(RC, mapred_bucket_stream, Bucket, Query, ClientPid, Timeout).

%% INTERNAL
random_client_id() ->
  {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
  {_,_,NowPart} = now(),
  Id = erlang:phash2([Y,Mo,D,H,Mi,S,node(),NowPart]),
  base64:encode(<<Id:32>>).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
