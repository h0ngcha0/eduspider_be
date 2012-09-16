%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc web machine resource for study materials
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(api_sm_resource).

%%%_* Exports ==================================================================
-export( [ allowed_methods/2
         , content_types_provided/2
         , init/1
         , is_authorized/2
         , process_post/2
         , to_json/2
         ]).

%%%_* Includes =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Macros ===================================================================
%%%_* Records ==================================================================
-record(ctx, { uid :: string()
             }).

%%%_* Code =====================================================================
init([]) -> {ok, #ctx{}}.

%% TODO: DELELTE and PUT?
allowed_methods(RD, Ctx) ->
  {['GET', 'POST'], RD, Ctx}.

is_authorized(RD, Ctx) ->
  UserId          = api_resource_lib:get_user(RD),
  AuthorizeStatus = api_resource_lib:is_authorized(UserId),
  {AuthorizeStatus, RD, Ctx#ctx{uid = UserId}}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

process_post(RD, #ctx{uid = UserId} = Ctx) ->
  SmJsonObj = wrq:req_body(RD),
  lager:debug("user: ~p~ntrying to store:~p", [UserId, SmJsonObj]),
  case study_material:store(UserId, SmJsonObj) of
    ok ->
      NewRD = api_resource_lib:make_ok_rd(RD),
      {true, NewRD, Ctx};
    {error, ErrJsonObj} ->
      NewRD = api_resource_lib:make_error_rd(ErrJsonObj, RD),
      {false, NewRD, Ctx}
  end.

to_json(RD, #ctx{uid = UserId} = Ctx) ->
  lager:debug("user: ~p trying to fetch a sm", [UserId]),
  {fetch_sm(RD), RD, Ctx}.

%%%_* Internal Functions =======================================================
fetch_sm(RD) ->
  Key = api_resource_lib:get_key(RD),
  Ver = get_version(RD),
  lager:debug("sm key:p~nversion:~p", [Key, Ver]),
  case do_fetch_sm(Key, Ver) of
    {ok,    Json}    -> Json;
    {error, _Reason} -> "not found"
  end.

do_fetch_sm(Key, undefined)   ->
  study_material:fetch(Key);
do_fetch_sm(Key, Version)     ->
  study_material:fetch_archive(Key, Version).

get_version(RD) ->
  api_resource_lib:get_value("version", RD).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
