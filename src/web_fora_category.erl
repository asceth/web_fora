-module(web_fora_category).

-export([id/1, name/1, position/1, access_role/1]).
-export([first/3]).
-export([save/3]).

-record(web_fora_category, {id,
                            name,
                            position,
                            access_role}).

id(WebForaCategory) ->
  WebForaCategory#web_fora_category.id.
name(WebForaCategory) ->
  case WebForaCategory#web_fora_category.name of
    null ->
      "";
    Name ->
      Name
  end.
position(WebForaCategory) ->
  WebForaCategory#web_fora_category.position.
access_role(WebForaCategory) ->
  case WebForaCategory#web_fora_category.access_role of
    null ->
      "";
    AccessRole ->
      AccessRole
  end.

first(Driver, Connection, Key) ->
  [Cols|Rows] = Driver:q(Connection, "SELECT * FROM fora_categories WHERE id = $1 LIMIT 1", [Key]),
  [Category] = build(Cols, Rows),
  Category.

save(Driver, Connection, Record) ->
  Sql = lists:flatten(["UPDATE forum_categories SET name = $1, position = $2, access_role = $3 WHERE id = $4"]),
  case Driver:execute(Connection, Sql, [Record#web_fora_category.name, Record#web_fora_category.position, Record#web_fora_category.access_role, Record#web_fora_category.id]) of
    ok ->
      Record;
    {error, _Reason} ->
      %% TODO: Should log the error
      Record
  end.


build(Cols, Rows) ->
  build(Cols, Rows, []).
build(_Cols, [], Acc) ->
  Acc;
build(Cols, [Row|Tail], Acc) ->
  build(Cols, Tail, [build_record(Cols, Row)|Acc]).



build_record(Cols, Row) ->
  build_record(Cols, Row, #web_fora_category{}).
build_record([], [], Record) ->
  Record;
build_record([], _Row, Record) ->
  Record;
build_record(_Cols, [], Record) ->
  Record;

build_record([<<"id">>|Cols], [Value|Row], Record) ->
  build_record(Cols, Row, Record#web_fora_category{id=Value});
build_record([<<"name">>|Cols], [Value|Row], Record) ->
  build_record(Cols, Row, Record#web_fora_category{name=Value});
build_record([<<"position">>|Cols], [Value|Row], Record) ->
  build_record(Cols, Row, Record#web_fora_category{position=Value});
build_record([<<"access_role">>|Cols], [Value|Row], Record) ->
  build_record(Cols, Row, Record#web_fora_category{access_role=Value});

build_record([_Key|Cols], [_Value|Row], Record) ->
  build_record(Cols, Row, Record).

