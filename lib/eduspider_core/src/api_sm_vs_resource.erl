%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc web machine resource for study materials version history
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(api_sm_vs_resource).

%%%_* Exports ==================================================================
-export( [ allowed_methods/2
         , init/1
         , is_authorized/2
         , to_html/2
         ]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Macros ===================================================================
%%%_* Records ==================================================================
-record(ctx, { uid :: string()
             }).

%%%_* Code =====================================================================
init([]) -> {ok, #ctx{}}.

%% only 'GET'
allowed_methods(RD, Ctx) ->
  {['GET'], RD, Ctx}.

is_authorized(RD, Ctx) ->
  UserId          = api_resource_lib:get_user(RD),
  AuthorizeStatus = api_resource_lib:is_authorized(UserId),
  {AuthorizeStatus, RD, Ctx#ctx{uid = UserId}}.

to_html(RD, #ctx{uid = UserId} = Ctx) ->
  lager:debug("user: ~p trying to fetch sm versions", [UserId]),
  {fetch_sm_vs(RD), RD, Ctx}.

%%%_* Internal Functions =======================================================
fetch_sm_vs(RD) ->
  Key = api_resource_lib:get_key(RD),
  case study_material_history:get_version_summaries(Key) of
    {ok,    Json}    -> Json;
    {error, _Reason} -> "not found"
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
