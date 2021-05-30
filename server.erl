-module(server).

-export([game_controller/1, get_players/0,
	 start_server/0]).

% The server must be started first
% It will initialize the state and will spawn a process that will be listening to messages from the clients.
% In the messages we will need to handle registration, plays, etc.
% The clients will know the name of the server and even the registered name so they can send messages directly.
% -- e.g. In a client, we call {central_server NodeName} ! MSG
% maps:put(turn, 1, GameStatus)
% maps:get(turn, GameStatus)

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
		RegistrationResult = handle_player_reg(PlayerId, GameStatus),
		% We extract the result of the function
		ResponseForUser = element(1, RegistrationResult),
		NewGameStatus = element(2, RegistrationResult),
		PlayerId ! ResponseForUser,
		game_controller(NewGameStatus);

      {get_players, From} ->
		From ! maps:get(players, GameStatus),
		game_controller(GameStatus);

      {move, PlayerId, Move} ->
		io:fwrite("Recieved the Move of the player~n"),
		Turn=maps:get(turn, GameStatus),
		ConditionTurn1 = Turn rem 2 /= 0,
		ConditionTurn2 = Turn rem 2 == 0,
		MoveSymbol = element(3,Move),
		State = validMove(maps:get(board,GameStatus),Move),
		io:fwrite("~w",[MoveSymbol]),
		  if
			ConditionTurn1 andalso MoveSymbol == " X "   ->
				if
					State == 1 ->
						Newturn = Turn + 1,
						io:fwrite("~w",[Newturn]),
						NewGameStatus = maps:put(turn,Newturn,GameStatus),
						FinalGameStatus = play(maps:get(board, NewGameStatus), PlayerId,Move,NewGameStatus),
						PlayerId ! {confirm, maps:get(board,FinalGameStatus)},
						game_controller(FinalGameStatus);
				true ->
					PlayerId ! {valid,"That spot is already being used, try another spot"},
					game_controller(GameStatus)
				end;
			ConditionTurn2 andalso MoveSymbol == " O "  ->
				if
					State == 1 ->
						Newturn = Turn + 1,
						io:fwrite("~w",[Newturn]),
						NewGameStatus = maps:put(turn,Newturn,GameStatus),
						FinalGameStatus = play(maps:get(board, NewGameStatus), PlayerId,Move,NewGameStatus),
						PlayerId ! {confirm, maps:get(board,FinalGameStatus)},
						game_controller(FinalGameStatus);
				true ->
					PlayerId ! {valid,"That spot is already being used, try another spot"},
					game_controller(GameStatus)
				end;
			true ->
				PlayerId ! {turn,"not your turn"},
				game_controller(GameStatus)
			end,
			io:frite("sucessfull~n");

      {print, PlayerId} ->
		Board = {gameboard, maps:get(board, GameStatus)},
		PlayerId ! Board,
		game_controller(GameStatus);

	  {getboard, PlayerID} ->
		  PlayerID ! {client_server,maps:get(board,GameStatus)},
		  game_controller(GameStatus);

      {exit} -> io:fwrite("See you!~n")
    end.

check_vertical(GameStatus, X, Sign) ->
    Board = maps:get(board, GameStatus),
    X1 = element(X, element(1, Board)),
    X2 = element(X, element(2, Board)),
    X3 = element(X, element(3, Board)),
    if X1 == Sign andalso X2 == Sign andalso X3 == Sign ->
	   1;
       true -> 0
    end.

check_horizontal(GameStatus, Y, Sign) ->
    Board = maps:get(board, GameStatus),
    Y1 = element(1, element(Y, Board)),
    Y2 = element(2, element(Y, Board)),
    Y3 = element(3, element(Y, Board)),
    if Y1 == Sign andalso Y2 == Sign andalso Y3 == Sign ->
	   1;
       true -> 0
    end.

check_diagonal(GameStatus, Sign) ->
    Board = maps:get(board, GameStatus),
    UpL = element(1, element(1, Board)),
    Mid = element(2, element(2, Board)),
    LowL = element(1, element(3, Board)),
    UpR = element(3, element(1, Board)),
    LowR = element(3, element(3, Board)),
    if UpL == Sign andalso
	 Mid == Sign andalso LowR == Sign ->
	   1;
       UpR == Sign andalso Mid == Sign andalso LowL == Sign ->
	   1;
       true -> 0
    end.

start_server() ->
    % Here, we initialize the map and status
    io:format("Starting the server~n"),
    io:format("Handling game ~n"),
    Gameboard = {{" - ", " - ", " - "},
		 {" - ", " - ", " - "}, {" - ", " - ", " - "}},
    %!Game board goes here
    InitialStatus = #{players => [], board => Gameboard,
		      score => {0, 0, 0}, turn => 1},
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

play(Board, PlayerId, Move, GameStatus) ->
    X = element(1, Move),
    Y = element(2, Move),
    S = element(3, Move),
    NewBoard = setelement(X, element(Y, Board), S),
    Play = setelement(Y, Board, NewBoard),
    FinalGameStatus = maps:put(board, Play, GameStatus),
    Vert = check_vertical(FinalGameStatus, element(1, Move),
			  element(3, Move)),
    Hori = check_horizontal(FinalGameStatus,
			    element(2, Move), element(3, Move)),
    Diag = check_diagonal(FinalGameStatus,
			  element(3, Move)),
    Answer = Vert + Hori + Diag,
    if Answer > 0 ->
	   PlayerId ! {confirm, "Winner!"},
	   io:fwrite("message of winner is sent ~n"),
	   ScoreX = element(1,maps:get(score, GameStatus)),
	   ScoreTie =  element(2,maps:get(score, GameStatus)),
	   ScoreO = element(3,maps:get(score, GameStatus)),
	   if
		   S == " X " ->
			   Score = {ScoreX+1, ScoreTie, ScoreO};
			   true ->
				   Score = {ScoreX, ScoreTie, ScoreO+1}
				   
		end,
	    Gameboard = {{" - ", " - ", " - "},
		 {" - ", " - ", " - "}, {" - ", " - ", " - "}},
    	%!Game board goes here
		io:fwrite("~w ~n", [Score]),
    	TempGameStatus = maps:put(score, Score, GameStatus),
		Temp2GameStatus = maps:put(board, Gameboard, TempGameStatus),
		Turn = maps:get(turn, Temp2GameStatus),
		maps:put(turn, Turn+1, Temp2GameStatus);
       true ->
	   FinalGameStatus                        % NewBoard = {confirm, NewGameStatus}
    end.

validMove(Board, Move) ->
    X = element(1, Move),
    Y = element(2, Move),
    Compare = element(X, element(Y, Board)),
    if Compare == " - " -> 1;
       true -> 0
    end.
