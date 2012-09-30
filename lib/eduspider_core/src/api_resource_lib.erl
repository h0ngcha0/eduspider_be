%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Frontend module to manipulate the study material
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(api_resource_lib).

%%%_* Exports ==================================================================
-export([ get_user/1
        , get_key/1
        , get_value/2
        , is_authorized/1
        , make_error_rd/2
        , make_header/1
        , make_ok_rd/1
        ]).

%%%_* Code =====================================================================
get_user(RD) ->
  wrq:get_qs_value("uid", RD).

get_key(RD) ->
  get_value("id", RD).

get_value(Key, RD) ->
  Val = wrq:get_qs_value(Key, RD),
  case Val of
    undefined   -> undefined;
    Val         -> UnQuotedVal = mochiweb_util:unquote(Val),
                   list_to_binary(UnQuotedVal)
  end.

%% TODO: at least check if the user id exist in db.
is_authorized(_UserId) ->
  true.

make_ok_rd(RD) ->
  make_header(wrq:set_resp_body("ok", RD)).

make_error_rd(ErrorJson, RD) ->
  make_header(wrq:set_resp_body(ErrorJson, RD)).

make_header(RD) ->
  wrq:set_resp_header("Content-type", "application/json", RD).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
