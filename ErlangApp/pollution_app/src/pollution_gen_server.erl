-module(pollution_gen_server).
-behaviour(gen_server).

%% API
-export([start_link/0, add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_mean/2, get_station_min/2, get_daily_mean/2, get_minimum_pollution_station/1,
  crash/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_station(Name, Coords) ->
  gen_server:call(?MODULE, {add_station, Name, Coords}).

add_value(Station, Date, Type, Value) ->
  gen_server:cast(?MODULE, {add_value, Station, Date, Type, Value}).

remove_value(Station, Date, Type) ->
  gen_server:call(?MODULE, {remove_value, Station, Date, Type}).

get_one_value(Station, Date, Type) ->
  gen_server:call(?MODULE, {get_one_value, Station, Date, Type}).

get_station_min(Station, Type) ->
  gen_server:call(?MODULE, {get_station_min, Station, Type}).

get_station_mean(Station, Type) ->
  gen_server:call(?MODULE, {get_station_mean, Station, Type}).

get_daily_mean(Type, Date) ->
  gen_server:call(?MODULE, {get_daily_mean, Type, Date}).

get_minimum_pollution_station(Type) ->
  gen_server:call(?MODULE, {get_minimum_pollution_station, Type}).

crash() ->
  Fail = 1/0.

%% Callbacks
init([]) ->
  {ok, #{stations => #{}, measurements => #{}}}.

find_station({X, Y}, #{stations := Stations}) ->
  [{Name, Coords} || {Name, Coords} <- maps:to_list(Stations), Coords =:= {X, Y}];
find_station(StationName, #{stations := Stations}) ->
  [{Name, Coords} || {Name, Coords} <- maps:to_list(Stations), Name =:= StationName].

handle_call({add_station, Name, Coords}, _From, #{stations := Station} = Pollution) ->
  case maps:find(Name, Station) of
    error ->
      case lists:any(fun({_, S}) -> S =:= Coords end, maps:to_list(Station)) of
        true -> {reply, {error, duplicate_coordinates}, Pollution};
        false ->
          NewState = Pollution#{stations => Station#{Name => Coords}},
          {reply, ok, NewState}
      end;
    _ -> {reply, {error, duplicate_name}, Pollution}
  end;
handle_call({remove_value, Station, Date, Type}, _From, #{measurements := Measurements} = Pollution) ->
  Key = {Station, Date, Type},
  case maps:find(Key, Measurements) of
    error -> {reply, {error, not_found}, Pollution};
    _ -> {reply, ok, Pollution#{measurements => maps:remove(Key, Measurements)}}
  end;
handle_call({get_one_value, Station, Date, Type}, _From, #{measurements := Measurements} = Pollution) ->
  FoundStations = find_station(Station, Pollution),
  case FoundStations of
    [{Name, Coords}] ->
      Key1 = {Name, Date, Type},
      Key2 = {Coords, Date, Type},
      case maps:find(Key1, Measurements) of
        {ok, Value} -> {reply, Value, Pollution};
        error -> case maps:find(Key2, Measurements) of
                   {ok, Value} -> {reply, Value, Pollution};
                   error -> {reply, {error, not_found}, Pollution}
                 end
      end;
    [] -> {reply, {error, not_found}, Pollution}
  end;
handle_call({get_station_min, Station, Type}, _From, #{measurements := Measurements} = Pollution) ->
  FoundStation = find_station(Station, Pollution),
  case FoundStation of
    [{Name, Coords}] ->
      case [Value || {{Stat, _, TypePM}, Value} <- maps:to_list(Measurements), Stat =:= Name orelse Stat =:= Coords, TypePM =:= Type] of
        [] -> {reply, {error, no_measurements_found}, Pollution};
        Values -> {reply, lists:min(Values), Pollution}
      end;
    [] -> {reply, {error, not_found}, Pollution}
  end;
handle_call({get_station_mean, Station, Type}, _From, #{measurements := Measurements} = Pollution) ->
  FoundStation = find_station(Station, Pollution),
  case FoundStation of
    [{Name, Coords}] ->
      case [Value || {{Stat, _, TypePM}, Value} <- maps:to_list(Measurements), Stat =:= Name orelse Stat =:= Coords, TypePM =:= Type] of
        [] -> {reply, {error, no_measurements_found}, Pollution};
        Values -> {reply, lists:sum(Values) / length(Values), Pollution}
      end;
    [] -> {reply, {error, not_found}, Pollution}
  end;
handle_call({get_daily_mean, TypeChecked, DateChecked}, _From, #{measurements := Measurements} = Pollution) ->
  case [Value || {{_, {Date, _}, Type}, Value} <- maps:to_list(Measurements), Date =:= DateChecked, Type =:= TypeChecked] of
    [] -> {reply, {error, no_measurements_found}, Pollution};
    Values -> {reply, lists:sum(Values) / length(Values), Pollution}
  end;
handle_call({get_minimum_pollution_station, Type}, _From, #{measurements := Measurements} = Pollution) ->
  case [{find_station(Station, Pollution), Value} || {{Station, _, TypePM}, Value} <- maps:to_list(Measurements), TypePM =:= Type] of
    [] -> {reply, {error, no_measurements_found}, Pollution};
    Values ->
      {Station, _} = lists:foldl(fun
                                   (X, {_, -1}) -> X;
                                   ({_, Val1}, {Station2, Val2}) when Val1 > Val2 -> {Station2, Val2};
                                   (X, _) -> X
                                 end,{station, -1}, Values),
      {reply, Station, Pollution}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({add_value, Station, Date, Type, Value}, #{measurements := Measurements} = Pollution) ->
  case find_station(Station, Pollution) of
    [] -> {noreply, Pollution};
    [{Name, Coords}] ->
      case maps:find({Name, Date, Type}, Measurements) of
        error -> case maps:find({Coords, Date, Type}, Measurements) of
                   error ->
                     NewMeasurements = Measurements#{{Name, Date, Type} => Value},
                     {noreply, Pollution#{measurements => NewMeasurements}};
                   _ -> {noreply, Pollution}
                 end;
        _ -> {noreply, Pollution}
      end
  end;
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
