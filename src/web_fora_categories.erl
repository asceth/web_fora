-module(web_fora_categories).

%-export([index/1, create/1]).
%-export([new/1]).
%-export([show/2, update/2, destroy/2]).
-export([edit/1, edit_view/1]).


edit(Session) ->
  CategoryId = web_router:id(lists:reverse(web_session:flash_lookup(Session, "path_tokens")), 2),
  Category = find_category(CategoryId),
  web_session:flash_merge_now(Session, [{"web_fora_category", Category}, {"status", 200}, {"headers", []}]).

edit_view(Session) ->
  Template = herml_parser:file("/srv/web/gamesyn.com/views/fora/categories/edit.herml"),
  herml_htmlizer:render(Template, [{"Session", Session}, {"WebForaCategory", web_session:flash_lookup(Session, "web_fora_category")}]).


update(CategoryId, Session) ->
  Category = find_category(CategoryId),
  {redirect, "/community/categories/" ++ CategoryId ++ "/edit", session, Session}.



find_category(CategoryId) ->
  eda:apply_in_tx(fun web_fora_category:first/3, [CategoryId]).
