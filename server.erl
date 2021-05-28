-module(server).

-export([game_controller/1, get_players/0, start_server/0]).

% The server must be started first
% It will initialize the state and will spawn a process that will be listening to messages from the clients.
% In the messages we will need to handle registration, plays, etc.
% The clients will know the name of the server and even the registered name so they can send messages directly.
% -- e.g. In a client, we call {central_server NodeName} ! MSG
% maps:put(turn, 1, GameStatus)
%maps:get(turn, GameStatus)

handle_player_reg(PlayerId, GameStatus) ->
    CurrentPlayers = maps:get(players, GameStatus),
    NumOfPlayers = length(CurrentPlayers),
    case NumOfPlayers of
      0 ->
	  RegistrationResult = {success, symbol, " X "},
	  NewPlayersList = [PlayerId],
	  NewGameStatus = maps:put(players, NewPlayersList,
				   GameStatus),
	  {RegistrationResult, NewGameStatus};
      1 ->
	  RegistrationResult = {success, symbol, " O "},
	  NewPlayersList = CurrentPlayers ++ PlayerId,
	  NewGameStatus = maps:put(players, NewPlayersList,
				   GameStatus),
		maps:fold(
			fun(K, V, ok) ->
				io:format("~p: ~p~n", [K, V])
			end, ok, GameStatus),
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

      {move, PlayerId, Move} ->
	  Play = play(maps:get(board, GameStatus), PlayerId,Move),
	  NewBoard = {confirm, Play},
	  PlayerId ! NewBoard,
	  NewGameStatus = maps:put(board, Play, GameStatus),
	  Vert = check_vertical(NewGameStatus, element(1, Move), element(3, Move)),
	  Hori = check_horizontal(NewGameStatus, element(2, Move), element(3, Move)),
	  Diag = check_diagonal(NewGameStatus, element(3, Move)),

	  Answer = Vert + Hori + Diag,

	  if 
		  Answer > 0 ->
			  	PlayerId ! "Winner!";
			true ->
				PlayerId ! "Keep playing!"
	end.

	  game_controller(NewGameStatus);

      %   PlayResult = play(Move,GameStatus),
      %   RespondForUser = element(1,PlauResult),
      %   NewStatus = element(2,PlayResult),
      %   Done = element(3,PlayResult),
      %   game_controller(NewStatus);
      {print, PlayerId} ->
	  Board = {gameboard, maps:get(board, GameStatus)},
	  PlayerId ! Board,
	  game_controller(GameStatus);
      {exit} -> io:fwrite("See you!")
    end.

check_vertical(GameStatus, X, Sign) ->
	io:fwrite("Checking move ~n"),
	Board = maps:get(board, GameStatus),
	X1 = element(X, element(1,Board)),
	X2 = element(X, element(2,Board)),
	X3 = element(X, element(3,Board)),
	if X1 == Sign andalso X2 == Sign andalso X3 == Sign ->
		1;
		true ->
			0
		end.

check_horizontal(GameStatus, Y, Sign) ->
	io:fwrite("Checking move ~n"),
	Board = maps:get(board, GameStatus),
	Y1 = element(1, element(Y, Board)),
	Y2 = element(2, element(Y, Board)),
	Y3 = element(3, element(Y, Board)),
	if Y1 == Sign andalso Y2 == Sign andalso Y3 == Sign ->
		1;
		true ->
			0
		end.

check_diagonal(GameStatus, Sign) ->
	io:fwrite("Checking move ~n"),
	Board = maps:get(board, GameStatus),
	UpL = element(1, element(1, Board)),
	Mid = element(2, element(2, Board)),
	LowL = element(1, element(3, Board)),
	UpR = element(3, element(1, Board)),
	LowR = element(3, element(3, Board)),
	if 
		UpL == Sign andalso Mid == Sign andalso LowR == Sign -> 1;
		UpR == Sign andalso Mid == Sign andalso LowL == Sign -> 1;
		true ->
			0
		end.

start_server() ->
    % Here, we initialize the map and status
    io:format("Starting the server~n"),
    io:format("Handling game ~n"),
    Gameboard = {{" - ", " - ", " - "},
		 {" - ", " - ", " - "}, {" - ", " - ", " - "}},
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
holder(Move,Board) -> 
	X = element(1, Move),
	Y = element(2, Move),
	S = element(3, Move),
	%NewBoard = setelement(X,element(Y,Board),S), 
	%setelement(Y,Board,NewBoard)
	if 
        Compare == " - "  -> 
            NewBoard = setelement(X,element(Y,Board),S), 
            setelement(X,Board,NewBoard);
        true -> 
            io:fwrite("Invalid position try again"),
            Board
	end.

play(GameBoard, PlayerID, Move) ->
    % NewBoard = holder(Move,GameBoard).
	holder(Move,GameBoard).


board(Board, X) ->
    if X < 3 ->
	   io:fwrite("~s,~n", [tuple_to_list(element(X, Board))]),
	   board(Board, X + 1);
       true ->
	   io:fwrite("~s,~n", [tuple_to_list(element(X, Board))])
    end.

% TODO: Set symbol on the board. 
% start_game() ->
%     spawn(play(#{}))

