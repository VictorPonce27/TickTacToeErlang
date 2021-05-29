-module(player).

-export([print/1, register_with_server/1, move/1]).

% Call like: player:register_with_server('server@DESKTOP-V6V2QAT').
register_with_server(ServerName) ->
    % central_server is a name known at the server side.
    {central_server, ServerName} ! {register, self()},
    receive
      {success, symbol, AssignedSymbol} -> AssignedSymbol
    end,
    AssignedSymbol.

print(ServerName) ->
    {central_server, ServerName} ! {print, self()},
    receive 
        {gameboard, GameBoard} -> GameBoard,
    board(GameBoard,1)
end. 

move(ServerName) -> 
    getBoard(ServerName),
    {ok,X} = io:read("Enter your position for X: "), 
    {ok,Y} = io:read("Enter your position for Y: "), 
    {ok,S} = io:read("Enter your symbol: "),
    Move = {X,Y,S}, 
    {central_server, ServerName} ! {move,self(),Move}, 
    receive 
        {turn, Confirm} -> Confirm;

        {badmove, BadMove} -> BadMove,
        io:fwrite("~s",[BadMove]);

        {confirm, Answer} -> Answer,
        
        Condition=is_tuple(Answer),
        if 
            Condition -> 
                board(Answer,1);
                
            true ->
            io:fwrite("~s",[Answer])
        end
    end.

getBoard(ServerName) -> 
    {central_server,ServerName} ! {getboard,self()}, 
    receive 
        {client_server, Board} -> Board, 
        board(Board,1)
    end.   


board(Board, X) ->
    if X < 3 ->
	   io:fwrite("~s,~n", [tuple_to_list(element(X, Board))]),
	   board(Board, X + 1);
       true ->
	   io:fwrite("~s,~n", [tuple_to_list(element(X, Board))])
    end.
    