%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc web machine resource for users
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(api_user_resource).

%%%_* Exports ==================================================================
-export( [ allowed_methods/2
         , init/1
         , is_authorized/2
         , process_post/2
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

%% TODO: DELELTE and PUT?
allowed_methods(RD, Ctx) ->
  {['GET', 'POST'], RD, Ctx}.

%% This is always authorized atm.
is_authorized(RD, Ctx) ->
  UserId          = api_resource_lib:get_user(RD),
  AuthorizeStatus = api_resource_lib:is_authorized(UserId),
  {AuthorizeStatus, RD, Ctx#ctx{uid = UserId}}.

process_post(RD, #ctx{} = Ctx) ->
  UserJsonDoc = wrq:req_body(RD),
  lager:debug("tring to store user:~p", [UserJsonDoc]),
  case wuser:store(UserJsonDoc) of
    ok ->
      NewRD = api_resource_lib:make_ok_rd(RD),
      {true, NewRD, Ctx};
    {error, ErrJsonObj} ->
      NewRD = api_resource_lib:make_error_rd(ErrJsonObj, RD),
      {false, NewRD, Ctx}
  end.

to_html(RD, #ctx{} = Ctx) ->
  {fetch_user(RD), RD, Ctx}.

%%%_* Internal Functions =======================================================
fetch_user(RD) ->
  Key = api_resource_lib:get_key(RD),
  lager:debug("fetch user id:p", [Key]),
  case wuser:fetch(Key) of
    {ok,    Json}    -> Json;
    {error, _Reason} -> "not found"
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
