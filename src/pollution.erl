-module(pollution).
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_daily_mean/3, get_station_min/3, get_station_mean/3, get_minimum_pollution_station/2]).

create_monitor() -> #{stations => #{}, measurements => #{}}.

add_station(Name, Coords, #{stations := Station} = Pollution) ->
  case maps:find(Name, Station) of
    error ->
      case lists:any(fun({_, S}) -> S =:= Coords end, maps:to_list(Station)) of
        true -> {error, duplicate_coordinates};
        false -> Pollution#{stations => Station#{Name => Coords}}
      end;
    _ -> {error, duplicate_name}
  end.

find_station({X, Y}, #{stations := Stations}) ->
  [{Name, Coords} || {Name, Coords} <- maps:to_list(Stations), Coords =:= {X, Y}];
find_station(StationName, #{stations := Stations}) ->
  [{Name, Coords} || {Name, Coords} <- maps:to_list(Stations), Name =:= StationName].

add_value(Station, Date, Type, Value, #{measurements := Measurements} = Pollution) ->
  case find_station(Station, Pollution) of
    [] -> {error, no_station};
    [{Name, Coords}] ->
      case maps:find({Name, Date, Type}, Measurements) of
        error -> case maps:find({Coords, Date, Type}, Measurements) of
                   error -> Pollution#{measurements => Measurements#{{Name, Date, Type} => Value}};
                   _ -> {error, duplicate_value}
                 end;
        _ -> {error, duplicate_value}
      end
  end.


remove_value(Station, Date, Type, #{measurements := Measurements} = Pollution) ->
  Key = {Station, Date, Type},
  case maps:find(Key, Measurements) of
    error -> {error, not_found};
    _ -> Pollution#{measurements => maps:remove(Key, Measurements)}
  end.

get_one_value(Station, Date, Type, #{measurements := Measurements} = Pollution) ->
  FoundStations = find_station(Station, Pollution),
  case FoundStations of
    [{Name, Coords}] ->
      Key1 = {Name, Date, Type},
      Key2 = {Coords, Date, Type},
      case maps:find(Key1, Measurements) of
        {ok, Value} -> Value;
        error -> case maps:find(Key2, Measurements) of
                   {ok, Value} -> Value;
                   error -> {error, not_found}
                 end
      end;
    [] -> {error, not_found}
  end.

get_station_min(Station, Type, #{measurements := Measurements} = Pollution) ->
  FoundStation = find_station(Station, Pollution),
  case FoundStation of
    [{Name, Coords}] ->
      case [Value || {{Stat, _, TypePM}, Value} <- maps:to_list(Measurements), Stat =:= Name orelse Stat =:= Coords, TypePM =:= Type] of
        [] -> {error, no_measurements_found};
        Values -> lists:min(Values)
      end;
    [] -> {error, not_found}
  end.

get_station_mean(Station, Type, #{measurements := Measurements} = Pollution) ->
  FoundStation = find_station(Station, Pollution),
  case FoundStation of
    [{Name, Coords}] ->
      case [Value || {{Stat, _, TypePM}, Value} <- maps:to_list(Measurements), Stat =:= Name orelse Stat =:= Coords, TypePM =:= Type] of
        [] -> {error, no_measurements_found};
        Values -> lists:sum(Values) / length(Values)
      end;
    [] -> {error, not_found}
  end.

get_daily_mean(TypeChecked, DateChecked, #{measurements := Measurements}) ->
  case [Value || {{_, {Date, _}, Type}, Value} <- maps:to_list(Measurements), Date =:= DateChecked, Type =:= TypeChecked] of
    [] -> {error, no_measurements_found};
    Values -> lists:sum(Values) / length(Values)
  end.

get_minimum_pollution_station(Type, #{measurements := Measurements} = Pollution) ->
  case [{find_station(Station, Pollution), Value} || {{Station, _, TypePM}, Value} <- maps:to_list(Measurements), TypePM =:= Type] of
    [] -> {error, no_measurements_found};
    Values ->
      {Station, _} = lists:foldl(fun
                    (X, {_, -1}) -> X;
                    ({_, Val1}, {Station2, Val2}) when Val1 > Val2 -> {Station2, Val2};
                    (X, _) -> X
      end,{station, -1}, Values),
      Station
  end.
