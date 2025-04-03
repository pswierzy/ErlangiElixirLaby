-module(pingpong).
-export([start/0, stop/0, play/1, pong_loop/0, ping_loop/1]).

ping_loop(Sum) ->
  receive
    0 ->
      io:format("Koniec Ping ~nSuma: ~w~n", [Sum]),
      ping_loop(Sum);
    fin ->
      io:format("Koniec Ping ~nSuma: ~w~n", [Sum]),
      ok;
    N ->
      timer:sleep(200),
      io:format("Ping: ~w ~n", [N]),
      pong ! N-1,
      ping_loop(Sum + N)
  after 20000 ->
    io:format("Koniec Ping ~nSuma: ~w~n", [Sum]),
    ok
  end.

pong_loop() ->
  receive
    0 ->
      io:format("Koniec Pong ~n"),
      pong_loop();
    fin ->
      io:format("Koniec Pong ~n"),
      ok;
    N ->
      timer:sleep(200),
      io:format("Pong: ~w ~n", [N]),
      ping ! N-1,
      pong_loop()
  after 20000 ->
    io:format("Koniec Pong ~n"),
    ok
  end.

start() ->
  PongPID = spawn(?MODULE, pong_loop, []),
  PingPID = spawn(?MODULE, ping_loop, [0]),
  register(pong, PongPID),
  register(ping, PingPID),
  ok.

stop() ->
  ping ! fin,
  pong ! fin.

play(N) ->
  ping ! N,
  ok.