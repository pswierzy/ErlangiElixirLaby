-module(pollution_value_collector_gen_statem).
-behavior(gen_statem).

-export([start_link/0, stop/0, callback_mode/0,
  set_station/1, add_value/3, store_data/0, init/1,
  terminate/3, code_change/4, collecting/3, idle/3]).

%% API -----------------------------------------

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_statem:cast(?MODULE, stop).

set_station(Station) ->
  gen_statem:cast(?MODULE, {set_station, Station}).

add_value(Date, Type, Value) ->
  gen_statem:cast(?MODULE, {add_value, Date, Type, Value}).

store_data() ->
  gen_statem:cast(?MODULE, {store_data}).

%% Init ---------------------------------------

init([]) ->
  {ok, idle, #{station => undefined, values => []}}.

%% States -------------------------------------

idle(cast, {set_station, Station}, State) ->
  NewState = State#{station => Station},
  {next_state, collecting, NewState};
idle(cast, stop, State) ->
  {stop, normal, State}.

collecting(cast, {add_value, Date, Type, Value}, State = #{values := Values}) ->
  Val = {Date, Type, Value},
  NewState = State#{values => [Val | Values]},
  {keep_state, NewState};
collecting(cast, {store_data}, #{station := Station, values := Values}) ->
  io:format("aAAAA"),
  [_ = pollution_gen_server:add_value(Station, Date, Type, Value) || {Date, Type, Value} <- Values],
  io:format("Zapisano dane stacji ~p: ~p~n", [Station, Values]),
  NewState = #{station => undefined, values => []},
  {next_state, idle, NewState};
collecting(cast, stop, State) ->
  {stop, normal, State}.


callback_mode() ->
  state_functions.

terminate(_Reason, _State, _Data) ->
  ok.

code_change(_OldVsn, _State, _Data, _Extra) ->
  {ok, _State, _Data}.


