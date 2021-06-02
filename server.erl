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
	  NewPlayersList = lists:append(CurrentPlayers, [PlayerId]),
	  NewGameStatus = maps:put(players, NewPlayersList,
				   GameStatus),
	  {RegistrationResult, NewGameStatus};
      2 ->
	  RegistrationResult = {error, "No more players allowed"},
	  {RegistrationResult,
	   GameStatus} % We dont modify the status if something is not allowed.
    end.

% Funcion que mantendra el control de juego en todo momento

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

	% En caso de recibir un movimiento
      {move, PlayerId, Move} ->
		io:fwrite("Recieved the Move of the player \n"),
		Turn=maps:get(turn, GameStatus),
		ConditionTurn1 = Turn rem 2 /= 0,
		ConditionTurn2 = Turn rem 2 == 0,
		PlayerList = maps:get(players,GameStatus), 
		Player1 = lists:nth(1,PlayerList), 
		Player2 = lists:nth(2,PlayerList), 
		io:format("~w,~n",[PlayerId]),
		io:format("~w,~n",[Player1]),
		io:format("~w,~n",[Player2]),
		MoveSymbol = element(3,Move),
		State = validMove(maps:get(board,GameStatus),Move),
		io:fwrite("~w",[MoveSymbol]),
		% Condicion que verifica que el turno sea del jugador
		  if
			ConditionTurn1 andalso MoveSymbol == " X "   ->
				% Condicion que verifica que la casilla no este ocupada
				if
					State == 1 ->
						Newturn = Turn + 1,
						io:fwrite("~w",[Newturn]),
						NewGameStatus = maps:put(turn,Newturn,GameStatus),
						FinalGameStatus = play(maps:get(board, NewGameStatus), PlayerId,Move,NewGameStatus),
						Player2 ! {client, maps:get(board,FinalGameStatus)},
						Player1 ! {client, maps:get(board,FinalGameStatus)},
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
						Player2 ! {client, maps:get(board,FinalGameStatus)},
						Player1 ! {client,maps:get(board,FinalGameStatus)},
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

% Funcion que checa si ya se gano en vertical
check_vertical(GameStatus, X, Sign) ->
    Board = maps:get(board, GameStatus),
    X1 = element(X, element(1, Board)),
    X2 = element(X, element(2, Board)),
    X3 = element(X, element(3, Board)),
    if X1 == Sign andalso X2 == Sign andalso X3 == Sign ->
	   1;
       true -> 0
    end.

% Funcion que checa si ya se gano en horizontal
check_horizontal(GameStatus, Y, Sign) ->
    Board = maps:get(board, GameStatus),
    Y1 = element(1, element(Y, Board)),
    Y2 = element(2, element(Y, Board)),
    Y3 = element(3, element(Y, Board)),
    if Y1 == Sign andalso Y2 == Sign andalso Y3 == Sign ->
	   1;
       true -> 0
    end.

% Funcion que checa si ya se gano en diagonal
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

start(Players, Score, Turn) ->
	io:format("Handling game ~n"),
    Gameboard = {{" - ", " - ", " - "},
		 {" - ", " - ", " - "}, {" - ", " - ", " - "}},
    %!Game board goes here
    InitialStatus = #{players => Players, board => Gameboard,
		      score => Score, turn => Turn},
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

% Funcioni que registra un movimiento en el tablero del servidor
play(Board, PlayerId, Move, GameStatus) ->
    X = element(1, Move),
    Y = element(2, Move),
    S = element(3, Move),
	PlayerList = maps:get(players,GameStatus), 
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

	Player1 = lists:nth(1,PlayerList),
	Player2 = lists:nth(2,PlayerList),

	% Condicion que verifica si ya hubo un ganador
    if Answer > 0 ->
	   ScoreX = element(1,maps:get(score, GameStatus)),
	   ScoreTie =  element(2,maps:get(score, GameStatus)),
	   ScoreO = element(3,maps:get(score, GameStatus)),
	   if
		   S == " X " ->
			   	Player1 ! {done, "Winner!"},
				Player2 !  {done,"Player1 is the winner"},
				io:fwrite("message of winner is sent"),
			   	Score = {ScoreX+1, ScoreTie, ScoreO},
				Player1 ! {score, Score},
				Player2 ! {score, Score};
			   	true ->
					Player2 ! {done, "winner!"}, 
					Player1 !  {done,"Player2 is the winner"},
					io:fwrite("message of winner is sent"),
				   	Score = {ScoreX, ScoreTie, ScoreO+1}
		end,
	    Gameboard = {{" - ", " - ", " - "},
		 {" - ", " - ", " - "}, {" - ", " - ", " - "}},
    	%!Game board goes here
		io:fwrite("~w ~n", [Score]),
    	TempGameStatus = maps:put(score, Score, GameStatus),
		Temp2GameStatus = maps:put(board, Gameboard, TempGameStatus),
		Turn = maps:get(turn, Temp2GameStatus),
		game_controller(maps:put(turn, Turn+1, Temp2GameStatus));
       true ->
		Tie = checkTie(FinalGameStatus),
		% Condicion que verifica si hubo un empate
		if
			Tie == "Tie" ->
				ScoreX = element(1,maps:get(score, GameStatus)),
	   			ScoreTie =  element(2,maps:get(score, GameStatus)),
	   			ScoreO = element(3,maps:get(score, GameStatus)),
				Score = {ScoreX, ScoreTie+1, ScoreO},
				TempGameStatus = maps:put(score, Score, GameStatus),
				Gameboard = {{" - ", " - ", " - "},
		 		{" - ", " - ", " - "}, {" - ", " - ", " - "}},
				Temp2GameStatus = maps:put(board, Gameboard, TempGameStatus),
				Turn = maps:get(turn, Temp2GameStatus),
				io:fwrite("~w ~n", [Score]),
				Player2 ! {done, "Tie!"}, 
				Player1 !  {done,"Tie!"},
				io:fwrite("message of winner is sent"),
				game_controller(maps:put(turn, Turn+1, Temp2GameStatus));
			true ->
				FinalGameStatus             	
		end
    end.

% Funcion que verifica que sea un movimiento valido
validMove(Board, Move) ->
    X = element(1, Move),
    Y = element(2, Move),
    Compare = element(X, element(Y, Board)),
    if Compare == " - " -> 1;
       true -> 0
    end.

% Funcion que busca como minimo un '-' para saber si empataron
checkTie(GameStatus) ->
	Board = maps:get(board, GameStatus),
	Row1 = tuple_to_list(element(1, Board)),
	Row2 = tuple_to_list(element(2, Board)),
	Row3 = tuple_to_list(element(3, Board)),
	Pred = fun(L) -> L == " - " end,
	Status1 = lists:any(Pred, Row1),
	Status2 = lists:any(Pred, Row2),
	Status3 = lists:any(Pred, Row3),
	if 
		Status1 orelse Status2 orelse Status3 ->
			"Keeps going";
		true ->
			"Tie"
	end.
