-module(pollutionCalc).
-export([example/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).


example() ->
  [{reading, "station 1", {date, 13, 3, 2024}, {time, 14, 23, 32}, [{"pm10", 0.32}, {"pm2.5", 0.12}, {"pm1", 3.1}]},
    {reading, "station 2", {date, 13, 3, 2024}, {time, 17, 21, 39}, [{"pm10", 0.34}, {"pm2.5", 0.23}, {"pm1", 2.5}]},
    {reading, "station 3", {date, 14, 3, 2024}, {time, 12, 34, 56}, [{"pm10", 1.23}, {"pm2.5", 0.45}, {"pm1", 0.67}]},
    {reading, "station 1", {date, 15, 3, 2024}, {time, 19, 20, 32}, [{"pm10", 0.24}, {"pm2.5", 0.34}, {"pm1", 0.98}]}
  ].

number_of_readings(Readings, Date) -> number_of_readings(Readings, Date, 0).

number_of_readings([], _, Acc) -> Acc;
number_of_readings([{reading, _, Date, _, _} | T], Date, Acc) -> number_of_readings(T, Date, Acc+1);
number_of_readings([_ | T], Date, Acc) -> number_of_readings(T, Date, Acc).

find_pm_in_list([], _) -> -1;
find_pm_in_list([{Type, Result} | _], Type) -> Result;
find_pm_in_list([_ | T], Type) -> find_pm_in_list(T, Type).

calculate_max(Readings, Type) -> calculate_max(Readings, Type, -1).

calculate_max([], _, Max) -> Max;
calculate_max([{reading, _, _, _, Results} | T], Type, Max) ->
  HeadResult = find_pm_in_list(Results, Type),
  if
    HeadResult > Max -> calculate_max(T, Type, HeadResult);
    true -> calculate_max(T, Type, Max)
  end.

calculate_mean(Readings, Type) -> calculate_mean(Readings, Type, 0, 0).

calculate_mean([], _, _, 0) -> -1;
calculate_mean([], _, Sum, Amount) -> round(Sum/Amount * 10000) /10000;
calculate_mean([{reading, _, _, _, Results} | T], Type, Sum, Amount) ->
  HeadResult = find_pm_in_list(Results, Type),
  if
    HeadResult < 0 -> calculate_mean(T, Type, Sum, Amount);
    true -> calculate_mean(T, Type, Sum+HeadResult, Amount+1)
  end.