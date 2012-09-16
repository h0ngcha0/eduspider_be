%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module that stores/fetches and manipulates the study material
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(study_material).

%%%_* Exports ==========================================================
-export([ archive_key/1
        , create/5
        , create_archive/1
        , fetch/1
        , fetch_archive/2
        , get_editor/1
        , get_message/1
        , get_text/1
        , get_timestamp/1
        , get_version/1
        , search/1
        , set_editor/2
        , set_message/2
        , set_text/2
        , store/2
        , url/1
        ]).

%%%_* Includes =========================================================
-include_lib("eduspider_core/include/eduspider_core.hrl").

%%%_* Macros ===========================================================
-define(L_EDITOR,  <<"editor">>).
-define(F_TEXT,    <<"text">>).
-define(F_MSG,     <<"message">>).
-define(F_VERSION, <<"version">>).
-define(F_TS,      <<"timestamp">>).

-define(SEARCH_OPERATORS, [ $:
                          , $(
                          , $)
                          , ${
                          , $}
                          , $[
                          , $]
                          , $+
                          , $-
                          , $!
                          , $&
                          , $|
                          , $^
                          , $~
                          , $*
                          , $?
                          , $
                          , 34  % double quote "
                          ]).

%%%_* Code =============================================================

%% @doc fetch a study material with its latest version @end
-spec fetch(ArticleKey :: binary()) ->
               {ok, Archive :: binary()} | {error, term()}.
fetch(ArticleKey) ->
  Fun = fun(Client) ->
            fetch(Client, ArticleKey)
        end,
  wrc:run(Fun).

fetch(Client, ArticleKey) ->
  case wrc:get(Client, ?B_ARTICLE, ArticleKey) of
    {ok, Object} ->
      Wobjs = case wobj:has_siblings(Object) of
                true  ->
                  wobj:get_siblings(Object);
                false ->
                  [Object]
              end,
      JsonResult = wobj_to_json(Wobjs),
      lager:debug("fetch_study material:~p", [JsonResult]),
      {ok, JsonResult};
    Error        ->
      lager:debug("error when fetching_study material:~p", [Error]),
      Error
  end.

%% @doc fetch an article of a particular version @end
-spec fetch_archive(ArticleKey :: binary(), Version :: binary()) ->
                       {ok, Archive :: binary()} | {error, term()}.
fetch_archive(ArticleKey, Version) ->
  Fun = fun(Client) ->
            fetch_archive(Client, ArticleKey, Version)
        end,
  wrc:run(Fun).

fetch_archive(Client, ArticleKey, Version)  ->
  case wrc:get(Client, ?B_ARCHIVE, archive_key(ArticleKey, Version)) of
    {ok, Wobj} ->
      JsonResult = wobj:to_json(<<"article">>, [Wobj]),
      lager:debug("fetch_archive: ~p", [JsonResult]),
      {ok, JsonResult};
    Error      ->
      lager:debug("error when fetching_archive: ~p", [Error]),
      Error
  end.

create(Key, Text, Message, Vclock, Editor)
  when is_binary(Key), is_binary(Text), is_binary(Message),
       (is_binary(Vclock) orelse Vclock==undefined), is_binary(Editor) ->
  update_version(
    set_text(
      set_message(
        set_editor(
          wobj:set_vclock(
            wobj:create(?B_ARTICLE, Key, {struct, []}),
            if Vclock == <<>> -> undefined;
               true           -> Vclock
            end),
          Editor),
        Message),
      Text));
create(Key, Text, Message, Vclock, Editor) when is_list(Vclock) ->
  create(Key, Text, Message, list_to_binary(Vclock), Editor).

create_archive(Article) ->
  set_editor(
    wobj:create(?B_ARCHIVE,
                archive_key(Article),
                wobj:get_value(Article)),
    get_editor(Article)).

archive_key(Article)                                     ->
  archive_key(wobj:key(Article), get_version(Article)).
archive_key(ArticleKey, Version)                         ->
  iolist_to_binary([Version,<<".">>,ArticleKey]).


url(Article) ->
  case wobj:bucket(Article) of
    ?B_ARTICLE ->
      ["/wiki/",mochiweb_util:quote_plus(
                  base64url:decode_to_string(wobj:key(Article)))];
    ?B_ARCHIVE ->
      ["/wiki/",mochiweb_util:quote_plus(
                  base64url:decode_to_string(
                    article_fe:article_key_from_archive_key(
                      wobj:key(Article))))]
  end.

get_editor(Article)                                             ->
  Links = wobj:get_links(Article),
  [Editor] = [ E || {{_, E}, T} <- Links, T =:= ?L_EDITOR],
  Editor.

set_editor(Article, Editor)                                     ->
  wobj:add_link(
    wobj:remove_links(Article, ?B_USER, ?L_EDITOR),
    {{?B_USER, Editor}, ?L_EDITOR}).

get_text(Article)                                               ->
  wobj:get_json_field(Article, ?F_TEXT).
set_text(Article, Text) when is_binary(Text)                    ->
  update_version(wobj:set_json_field(Article, ?F_TEXT, Text)).

get_message(Article)                                            ->
  wobj:get_json_field(Article, ?F_MSG).
set_message(Article, Message) when is_binary(Message)           ->
  update_version(wobj:set_json_field(Article, ?F_MSG, Message)).

get_version(Article)                                            ->
  wobj:get_json_field(Article, ?F_VERSION).

get_timestamp(Article)                                          ->
  wobj:get_json_field(Article, ?F_TS).

update_version(Article)                                         ->
  {MS, S, _US} = now(),
  TS = 1000000*MS+S,
  wobj:set_json_field(
    wobj:set_json_field(Article, ?F_TS, TS),
    ?F_VERSION, list_to_binary(
                  mochihex:to_hex(erlang:phash2({get_text(Article),
                                                 get_message(Article),
                                                 TS})))).

%% @doc store a study material @end
-spec store(UserId :: binary(), ArticleJson :: binary()) ->
               ok | {ok, tuple()} | {ok, binary()} | {error, term()}.
store(UserId, ArticleJson) ->
  Fun = fun(Client) ->
            store(Client, UserId, ArticleJson)
        end,
  wrc:run(Fun).

-spec store( Client0 :: pid()
           , UserId :: binary()
           , ArticleJson :: binary()) ->
               ok | {ok, tuple()} | {ok, binary()} | {error, term()}.
store(Client0, UserId, ArticleJson)  ->
  {ok, Client} = wrc:set_client_id(Client0, UserId),
  Article = to_wobj(ArticleJson),
  ok = wrc:put(Client, create_archive(Article)),
  lager:debug("store study material:~p", [Article]),
  %% update history
  ok = study_material_history:add_version(Client, Article),
  %% store archive version
  wrc:put(Client, Article).

%% @doc perform a search on the study material text @end
-spec search(RawSearch :: string()) -> list().
search(RawSearch) ->
  Fun = fun(Client) ->
            search(Client, RawSearch)
        end,
  wrc:run(Fun).

search(Client, RawSearch)               ->
  case eduspider_core:search_enabled() of
    true ->
      Search = split_search(sanitize_search(RawSearch)),
      {ok, RawResults} =
        wrc:mapred(Client,
                   {modfun, riak_search, mapred_search,
                    [<<"article">>, iolist_to_binary(Search)]},
                   [{map, {jsanon, search_fun()}, <<>>, true}]),
      case RawResults of
        [{0, Results}] ->
          [ [{title, base64url:decode(
                       proplists:get_value(<<"key">>, R))},
             {ranges, proplists:get_value(<<"ranges">>, R)}]
            || {struct, R} <- Results ];
        _ ->
          []
      end;
    false ->
      []
  end.

%%%_* Internal functions ===============================================

%% code for search map phase is in priv/mapred/search_map.js
search_fun()                            ->
  eduspider_core:get_app_env( search_map
                            , <<"function(v) { return [{key:v.key}]; }">>).

split_search(Search)                    ->
  Tokens = string:tokens(Search, " "),
  string:join([ [<<"text:">>, T] || T <- Tokens ], " OR ").

sanitize_search(RawSearch)              ->
  [ whitespace_search_operator(C) || C <- RawSearch ].

whitespace_search_operator(C)           ->
  case is_search_operator(C) of
    true  -> 32; % space
    false -> C
  end.

is_search_operator(C)                   ->
  lists:member(C, ?SEARCH_OPERATORS).


to_wobj(ArticleJson) ->
  {struct, [Articles]} = mochijson2:decode(ArticleJson),
  {<<"article">>, {struct, ArticleFields}} = Articles,
  Key = proplists:get_value(<<"key">>, ArticleFields),
  Editor = proplists:get_value(<<"editor">>, ArticleFields),
  Text = proplists:get_value(<<"text">>, ArticleFields),
  Msg  = proplists:get_value(<<"msg">>, ArticleFields),
  Vclock = get_vclock(ArticleFields),
  create(Key, Text, Msg, Vclock, Editor).

get_vclock(ArticleFields) ->
  case proplists:get_value(<<"vclock">>, ArticleFields) of
    <<"undefined">> -> undefined;
    Other           -> Other
  end.


wobj_to_json(Wobjs)             ->
  PropLFun = fun(Wobj) ->
                 Editor = get_editor(Wobj),
                 [{<<"editor">>, Editor}]
             end,
  wobj:to_json(<<"article">>, Wobjs, PropLFun).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
