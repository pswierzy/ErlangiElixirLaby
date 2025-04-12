-module(pollution_server_test).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  M = pollution_server:add_station("Stacja 1", {1, 1}),

  ?assertNotMatch({error, _}, M),
  ?assertMatch({error, _}, pollution_server:add_station("Stacja 1", {1,1})),
  ?assertMatch({error, _}, pollution_server:add_station("Stacja 1", {2,2})),
  ?assertMatch({error, _}, pollution_server:add_station("Stacja 2", {1,1})),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),
  pollution_server:stop(),
  timer:sleep(1100),
  pollution_server:start(),

  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  ?assertNotMatch({error, _},pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),

  timer:sleep(1100),
  Time2 = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3)),

  pollution_server:add_value({1,1}, Time2, "PM10", 46.3),
  pollution_server:add_value({1,1}, Time2, "PM1", 46.3),
  ?assertNotMatch({error, _}, pollution_server:add_value({1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3)),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  Time = calendar:local_time(),

  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 36.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 36.3)),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  Time = calendar:local_time(),

  ?assertMatch({error, _}, pollution_server:add_value("Stacja 2", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,2}, Time, "PM10", 46.3)),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  Time = calendar:local_time(),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM1", 46.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),
  ?assertEqual(ok, pollution_server:remove_value("Stacja 1", Time, "PM10")),
  ?assertEqual(ok, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  Time = calendar:local_time(),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM1", 46.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:remove_value({1,2}, Time, "PM10")),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 2", Time, "PM10")),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  Time = calendar:local_time(),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM1", 36.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3)),
  ?assertMatch(46.3, pollution_server:get_one_value("Stacja 1", Time, "PM10")),
  ?assertMatch(36.3, pollution_server:get_one_value("Stacja 1", Time, "PM1")),
  ?assertMatch(46.3, pollution_server:get_one_value({1,1}, Time, "PM10")),
  ?assertMatch(26.3, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  Time = calendar:local_time(),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", Time, "PM1", 36.3)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3)),

  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:get_one_value({1,1}, Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 2", Time, "PM1")),
  ?assertMatch({error, _}, pollution_server:get_one_value({1,2}, Time, "PM10")),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 20)),

  ?assertMatch(15.0, pollution_server:get_station_mean("Stacja 1", "PM10")),
  ?assertMatch(15.0, pollution_server:get_station_mean({1,1}, "PM10")),
  ?assertEqual(40 / 3, pollution_server:get_station_mean("Stacja 1", "PM10")),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_fail_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),

  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 1", "PM10")),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10)),

  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 2", "PM10")),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),

  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  ?assertEqual(ok, pollution_server:add_station("Stacja 2", {2,2})),
  ?assertEqual(ok, pollution_server:add_station("Stacja 3", {3,3})),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 3", {{2023,3,28},{11,16,18}}, "PM10", 1234)),

  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),
  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),
  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_server_test() ->
  timer:sleep(1100),
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:add_station("Stacja 1", {1,1})),
  ?assertEqual(ok, pollution_server:add_station("Stacja 2", {2,2})),

  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,27})),

  ?assertEqual(ok, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20)),

  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM25",{2023,3,27})),
  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,29})),

  pollution_server:stop().
