%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc eduspider core
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_core).

%%%_* Exports ==========================================================
-export([ start/0
        , start_link/0
        , stop/0 ]).

-export([ get_app_env/2
        , read_mapred_js/0
        , search_enabled/0
        , set_bucket_props/0]).

%%%_* Includes =========================================================
-include_lib("eduspider_core/include/eduspider_core.hrl").

%%%_* Code =============================================================
%% @doc get the value of the app env variable @end
get_app_env(Env, Default) ->
  case application:get_env(eduspider_core, Env) of
    {ok, Val} -> Val;
    undefined -> Default
  end.

start()           ->
  start_common(),
  application:start(eduspider_core).

start_link()      ->
  start_common(),
  eduspider_core_sup:start_link().

stop()            ->
  Res = application:stop(eduspider_core),
  stop_common(),
  Res.

ensure_start(App) ->
  case application:start(App) of
    {error, {already_started, App}} -> ok;
    ok                              -> ok
  end.

%% Read each file in PRIV/mapred/<filename>.js and set its content
%% as wriaki application env <filename>
read_mapred_js() ->
  Dir = filename:join(code:priv_dir(eduspider_core), "mapred"),
  Filenames = filelib:wildcard("*.js", Dir),
  lists:foreach(
    fun(Name) ->
        {ok, Content} = file:read_file(filename:join(Dir, Name)),
        Atom = list_to_atom(hd(string:tokens(Name, "."))),
        application:set_env(eduspider_core, Atom, Content)
    end,
    Filenames).

set_bucket_props() ->
  Fun = fun(Client) ->
            ok = wrc:set_bucket( Client
                               , ?B_ARTICLE
                               , [{allow_mult, true}|search_hook()]),
            ok = wrc:set_bucket( Client
                               , ?B_HISTORY
                               , [{allow_mult, true}])
        end,
  wrc:run(Fun).

search_hook() ->
  case search_enabled() of
    true ->
      [{precommit, [{struct,[{<<"mod">>,<<"riak_search_kv_hook">>},
                             {<<"fun">>,<<"precommit">>}]}]}];
    _ ->
      []
  end.

search_enabled() ->
  get_app_env(search_enabled, false) == true.

start_common() ->
  lists:foreach( fun(App) ->
                     ensure_start(App)
                 end
               , dependent_applications()).

stop_common() ->
  lists:foreach( fun(App) ->
                     application:stop(App)
                 end
               , lists:reverse(dependent_applications())).

dependent_applications() ->
  [ crypto
  , compiler
  , syntax_tools
  , lager
  , public_key
  , ssl
  , inets
  , pooler
  , webmachine
  ].

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
