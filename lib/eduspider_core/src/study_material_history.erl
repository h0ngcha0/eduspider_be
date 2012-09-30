%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Helper functions around study material history.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(study_material_history).

%%%_* Exports ==========================================================
-export([ add_version/2
        , get_version_summaries/1
        ]).

%%%_* Includes =========================================================
-include_lib("eduspider_core/include/eduspider_core.hrl").

%%%_* Code =============================================================
%% @spec add_version(eduspider_core:article()) -> ok
%% @doc Update the history object for Article with an
%%      entry for the revision contained in Article.
add_version(Client, Article)              ->
  {ok, Hist} = fetch_or_new(Client, wobj:key(Article)),
  Timestamp  = date_string(study_material:get_timestamp(Article)),
  ok = wrc:put( Client
              , wobj:add_link( Hist
                             , { { ?B_ARCHIVE
                               , study_material:archive_key(Article)}
                                 , Timestamp})),

  lager:info("store study material history:~p", [Hist]),

  ok.

%% @doc fetch the history of an article, return notfound if not found @end
fetch(Client, Key)                        ->
  case wrc:get(Client, ?B_HISTORY, Key) of
    {ok, H} ->
      case wobj:has_siblings(H) of
        true ->
          {ok, merge_siblings(wobj:get_siblings(H))};
        false ->
          {ok, H}
      end;
    {error, notfound} ->
      notfound
  end.

date_string(TS)                           ->
  integer_to_list(TS).

fetch_or_new(Client, Key)                 ->
  case fetch(Client, Key) of
    {ok, H}  -> {ok, H};
    notfound -> {ok, wobj:create(?B_HISTORY, Key, <<>>)}
  end.

merge_siblings(Siblings)                  ->
  lists:foldl(fun merge_links/2, hd(Siblings), tl(Siblings)).

merge_links(Obj, Acc)                     ->
  lists:foldl(fun(L, A) -> wobj:add_link(A, L) end,
              Acc,
              wobj:get_links(Obj)).

%% @doc get the version summaries of an article @end
-spec get_version_summaries(ArticleKey :: binary()) ->
                               {ok, list()}.
get_version_summaries(ArticleKey) ->
  Fun = fun(Client) ->
            get_version_summaries(Client, ArticleKey)
        end,
  wrc:run(Fun).

get_version_summaries(Client, ArticleKey) ->
  {ok, [{1, Results}]} =
    wrc:mapred(Client,
               [{?B_HISTORY, ArticleKey}],
               [{link, <<"archive">>, '_', false},
%%%{reduce, {jsanon, time_order_fun()}, <<>>, false}, %TODO: paging
                {map, {jsanon, summary_fun()}, <<>>, true}]),
  JsonVHists = vhist_to_json(Results),

  lager:debug("fetch version summeries:~p", [JsonVHists]),

  {ok, JsonVHists}.

vhist_to_json(VHists)            ->
  to_json({struct, [{<<"version_history">>, VHists}]}).

to_json(JsonStruct)                     ->
  list_to_binary(mochijson2:encode(JsonStruct)).

%% code for summary map phase is in priv/mapred/summary_map.js
summary_fun()                             ->
  eduspider_core:get_app_env( summary_map
                              , <<"function() { return []; }">>).

%% code for time order reduce phase is in priv/mapred/time_order_reduce.js
%% time_order_fun() ->
%%     eduspider_core:get_app_env( time_order_reduce
%%                               , <<"function(v) { return v; }">>).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
