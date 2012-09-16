%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc eduspider core supervisor
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(eduspider_core_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
-export([start_link/0]).
-export([init/1]).

%%%_* Macros ===========================================================
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%_* Code =============================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Ip     = eduspider_core:get_app_env(web_ip, "0.0.0.0"),
  Port   = eduspider_core:get_app_env(web_port, 8001),
  LogDir = eduspider_core:get_app_env(log_dir, "priv/log"),

  lager:info("eduspider core port:~p", [Port]),

  {ok, Dispatch} = file:consult( filename:join( code:priv_dir(eduspider_core)
                                              , "dispatch.conf")),

  WebConfig = [ {ip       , Ip}
              , {port     , Port}
              , {log_dir  , LogDir}
              , {dispatch , Dispatch}],

  eduspider_core:set_bucket_props(),
  eduspider_core:read_mapred_js(),

  Child = { webmachine_mochiweb
          , {webmachine_mochiweb, start, [WebConfig]}
          ,  permanent, 5000, worker, [webmachine_mochiweb]},

  {ok, {{one_for_one, 5, 10}, [Child]}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
