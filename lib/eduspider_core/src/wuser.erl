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

%%%_* Module declaration ===============================================
-module(wuser).

%%%_* Exports ==========================================================
-export( [ create/2
         , fetch/1
         , get_email/1
         , get_name/1
         , set_login_time/2
         , store/1
         ]).

%%%_* Includes =========================================================
-include_lib("eduspider_core/include/eduspider_core.hrl").

%%%_* Macros ===========================================================
-define(F_EMAIL,      <<"email">>).
-define(F_NAME,       <<"name">>).
-define(F_LOGIN_TIME, <<"login_time">>).

%%%_* Code =============================================================

%% @doc fetch a user based on the user id @end
-spec fetch(UserId :: binary()) ->
               {ok, User :: binary()} | {error, term()}.
fetch(UserId) ->
  Fun = fun(Client) ->
            fetch(Client, UserId)
        end,
  wrc:run(Fun).

fetch(Client, UserId) when is_list(UserId) ->
  fetch(Client, list_to_binary(UserId));
fetch(Client, UserId) when is_binary(UserId) ->
  case wrc:get(Client, ?B_USER, UserId) of
    {ok, Wobj} ->
      lager:info("fetched user:~p", [Wobj]),
      {ok, wobj:to_json(<<"user">>, [Wobj])};
    Error      ->
      lager:error("error when fetching user:~p", [Error]),
      Error
  end.

%% @doc store a new user @end
-spec store(UserJson :: binary()) ->
               ok | {ok, tuple()} | {ok, binary()} | {error, term()}.
store(UserJson) ->
  Fun = fun(Client) ->
            store(Client, UserJson)
        end,
  wrc:run(Fun).

store(Client0, UserJson) ->
  WUser        = to_wobj(UserJson),
  {ok, Client} = wrc:set_client_id(Client0, wobj:key(WUser)),
  lager:debug("store user:~p", [UserJson]),
  wrc:put(Client, WUser).

create(Username, DecodedJsonStruct) ->
  wobj:create(?B_USER, Username, DecodedJsonStruct).

get_name(User) ->
  case wobj:get_json_field(User, ?F_NAME) of
    {struct, Name} ->
      wobj:get_json_field(Name, <<"formated">>);
    Name ->
      Name
  end.

get_email(User) ->
  wobj:get_json_field(User, ?F_EMAIL).

set_login_time(User, Ts) ->
  Utc = calendar:now_to_universal_time(Ts),
  Sec = calendar:datetime_to_gregorian_seconds(Utc),
  wobj:set_json_field(User, ?F_LOGIN_TIME, Sec).

to_wobj(UserJson) ->
  {struct, [{<<"user">>, {struct, UserFields}}]} = mochijson2:decode(UserJson),
  UserId = proplists:get_value(<<"key">>, UserFields),
  create(UserId, {struct, UserFields}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
