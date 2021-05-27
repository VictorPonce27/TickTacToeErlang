-module(server).

-export([game_controller/1, get_players/0,
	 start_server/0]).

% The server must be started first
% It will initialize the state and will spawn a process that will be listening to messages from the clients.
% In the messages we will need to handle registration, plays, etc.
% The clients will know the name of the server and even the registered name so they can send messages directly.
% -- e.g. In a client, we call {central_server NodeName} ! MSG

handle_player_reg(PlayerId, GameStatus) ->
    CurrentPlayers = maps:get(players, GameStatus),
    NumOfPlayers = length(CurrentPlayers),
    case NumOfPlayers of
      0 ->
	  RegistrationResult = {success, symbol, "X"},
	  NewPlayersList = [PlayerId],
	  NewGameStatus = maps:put(players, NewPlayersList,
				   GameStatus),
	  {RegistrationResult, NewGameStatus};
      1 ->
	  RegistrationResult = {success, symbol, "O"},
	  NewPlayersList = CurrentPlayers ++ PlayerId,
	  NewGameStatus = maps:put(players, NewPlayersList,
				   GameStatus),
	  {RegistrationResult, NewGameStatus};
      2 ->
	  RegistrationResult = {error, "No more players allowed"},
	  {RegistrationResult,
	   GameStatus} % We dont modify the status if something is not allowed.
    end.

game_controller(GameStatus) ->
    receive
      {register, PlayerId} ->
	  io:format("Handling player registration ~n"),
	  RegistrationResult = handle_player_reg(PlayerId,
						 GameStatus),
	  % We extract the result of the function
	  ResponseForUser = element(1, RegistrationResult),
	  NewGameStatus = element(2, RegistrationResult),
	  PlayerId ! ResponseForUser,
	  game_controller(NewGameStatus);

      {get_players, From} ->
	  From ! maps:get(players, GameStatus),
	  game_controller(GameStatus);
      % {play, From, Move} ->
      %     PlayResult = play(Move,GameStatus),
      %     RespondForUser = element(1,PlauResult),
      %     NewStatus = element(2,PlayResult),
      %     Done = element(3,PlayResult),
      %     game_controller(NewStatus);
      {print, PlayerId} ->
	  Board = {gameboard, maps:get(board, GameStatus)},
	  PlayerId ! Board,
	  game_controller(GameStatus);

      {exit} -> io:fwrite("See you!")
    end.

start_server() ->
    % Here, we initialize the map and status
    io:format("Starting the server~n"),
    io:format("Handling game ~n"),
    Gameboard = {{" - ", " - ", " - "}, 
                 {" - ", " - ", " - "},
		         {" - ", " - ", " - "}},
    %!Game board goes here
    InitialStatus = #{players => [], board => Gameboard,
		      score => [0, 0, 0]},
    Controller_Pid = spawn(?MODULE, game_controller,
			   [InitialStatus]),
    register(central_server, Controller_Pid).

% functions like this one can be implemented to get status info in the server.
% They might be even be called from clients using something like:
% erpc:call('server@DESKTOP-V6V2QAT', server, get_players,[]).
get_players() ->
    central_server ! {get_players, self()},
    receive Players -> Players end,
    Players.

% % The externally seen and used function
register_player(PlayerId) ->
    case whereis(register_pid) of
      undefined ->
	  Register_Pid = spawn(?MODULE, internal_register_player,
			       []),
	  register(register_pid, Register_Pid)
    end,
    register_pid ! PlayerId.

internal_register_player(PlayersList) ->
    receive
      PlayerId ->
	  CurrentPlayers = length(PlayersList),
	  case CurrentPlayers of
	    0 ->
		PlayerId ! {symbol, "X"},
		NewPlayersList = [PlayerId],
		internal_register_player(NewPlayersList);
	    1 ->
		PlayerId ! {symbol, "O"},
		NewPlayersList = PlayersList ++ PlayerId,
		internal_register_player(NewPlayersList);
	    2 ->
		PlayerId ! {error, "No more players allowed"},
		internal_register_player(PlayersList)
	  end;
      exit ->
	  io:fwrite("Good bye from internal register player")
    end.

% play(Map) ->
%     receive
%         {play, Row, Col, Symbol} ->



% start_game() ->
%     spawn(play(#{}))

