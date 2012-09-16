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
  eduspider_core:set_bucket_props(),
  eduspider_core:read_mapred_js(),
  Child = ?CHILD(eduspider_core_server, worker),
  {ok, { {one_for_one, 5, 10}, [Child]}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
