%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc eduspider server-side functionalities
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(eduspider_core_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
%% gen_server Function Exports
-export([ start_link/0
        ]).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

%% Business APIs

%%%_* Macros ===========================================================
-define(SERVER, ?MODULE).

%%%_* Code =============================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call(_, _, State)                                    ->
  {noreply, State}.

handle_cast(_Msg, State)                                    ->
  {noreply, State}.

handle_info(_Info, State)                                   ->
  {noreply, State}.

terminate(_Reason, _State)                                  ->
  ok.

code_change(_OldVsn, State, _Extra)                         ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
