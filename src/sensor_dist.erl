-module(sensor_dist).
-export([get_rand_locations/1, find_for_person/2, find_for_person/3, find_closest/2, find_closest/3]).

get_rand_locations(N) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, N)].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

find_for_person(PersonLoc, SensorsLocs) ->
  lists:min([{{dist(PersonLoc, SensorLoc)}, {PersonLoc, SensorLoc}} || SensorLoc <- SensorsLocs]).

find_for_person(PersonLoc, SensorsLocs, ParentPID) ->
  ParentPID ! find_for_person(PersonLoc, SensorsLocs).

find_closest(PeopleLocs, SensorsLocs) ->
  [find_for_person(PL, SensorsLocs) || PL <- PeopleLocs].

find_closest(PeopleLocs, SensorsLocs, ParentPID) ->
  [spawn(?MODULE, find_for_person, [PersonLoc, SensorsLocs, self()])
    || PersonLoc <- PeopleLocs],

  Results = [receive
               D -> D
             end || _ <- PeopleLocs],

  ParentPID ! Results.