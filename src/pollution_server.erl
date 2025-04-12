-module(pollution_server).
-export([start/0, stop/0]).
-export([
  add_station/2, add_value/4, remove_value/3, get_one_value/3,
  get_daily_mean/2, get_station_min/2, get_minimum_pollution_station/1
]).
-export([init/0, loop/1]).

start() ->
  register(pollution_server, spawn(?MODULE, init, [])),
  ok.

stop() ->
  pollution_server ! stop,
  ok.

init() ->
  State = pollution:create_monitor(),
  loop(State).

loop(State) ->
  receive
    {From, {add_station, Name, Coords}} ->
      NewState = case pollution:add_station(Name, Coords, State) of
                   {error, _} = Error -> From ! Error, State;
                   S -> From ! ok, S
                 end,
      loop(NewState);
    {From, {add_value, Station, Date, Type, Value}} ->
      NewState = case pollution:add_value(Station, Date, Type, Value, State) of
                   {error, _} = Error -> From ! Error, State;
                   S -> From ! ok, S
                 end,
      loop(NewState);
    {From, {remove_value, Station, Date, Type}} ->
      NewState = case pollution:remove_value(Station, Date, Type, State) of
                   {error, _} = Error -> From ! Error, State;
                   S -> From ! ok, S
                 end,
      loop(NewState);
    {From, {get_one_value, Station, Date, Type}} ->
      From ! pollution:get_one_value(Station, Date, Type, State),
      loop(State);
    {From, {get_daily_mean, Type, Day}} ->
      From ! pollution:get_daily_mean(Type, Day, State),
      loop(State);
    {From, {get_station_min, Station, Type}} ->
      From ! pollution:get_station_min(Station, Type, State),
      loop(State);
    {From, {get_minimum_pollution_station, Type}} ->
      From ! pollution:get_minimum_pollution_station(Type, State),
      loop(State);

    stop  -> ok;
    _     -> loop(State)
  end.

add_station(Name, Coords) ->
  pollution_server ! {self(), {add_station, Name, Coords}},
  receive R -> R end.

add_value(Station, Date, Type, Value) ->
  pollution_server ! {self(), {add_value, Station, Date, Type, Value}},
  receive R -> R end.

remove_value(Station, Date, Type) ->
  pollution_server ! {self(), {remove_value, Station, Date, Type}},
  receive R -> R end.

get_one_value(Station, Date, Type) ->
  pollution_server ! {self(), {get_one_value, Station, Date, Type}},
  receive R -> R end.

get_daily_mean(Type, Day) ->
  pollution_server ! {self(), {get_daily_mean, Type, Day}},
  receive R -> R end.

get_station_min(Station, Type) ->
  pollution_server ! {self(), {get_station_min, Station, Type}},
  receive R -> R end.

get_minimum_pollution_station(Type) ->
  pollution_server ! {self(), {get_minimum_pollution_station, Type}},
  receive R -> R end.