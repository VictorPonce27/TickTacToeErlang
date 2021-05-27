-module(player).

-export([print/1, register_with_server/1]).

% Call like: player:register_with_server('server@DESKTOP-V6V2QAT').
register_with_server(ServerName) ->
    % central_server is a name known at the server side.
    {central_server, ServerName} ! {register, self()},
    receive
      {success, symbol, AssignedSymbol} -> AssignedSymbol
    end,
    AssignedSymbol.

% signOut(ServerName) ->
%     {central_server, ServerName} ! {exit, self()},
%     receive
%         {done,} ->
%     end,
%     _.

print(ServerName) ->
    {central_server, ServerName} ! {print, self()},
    receive 
        {gameboard, GameBoard} -> GameBoard,
    board(GameBoard,1)
end. 

board(Board, X) ->
    if X < 3 ->
	   io:fwrite("~p,~n", [tuple_to_list(element(X, Board))]),
	   board(Board, X + 1);
       true ->
	   io:fwrite("~p,~n", [tuple_to_list(element(X, Board))])
    end.

% move(ServerName, Move,Symbol) ->
%     {central_server, ServerName} ! {play, self(), Move},

%     receive
%         {}

% Check that we can also do erpc:call('server@DESKTOP-V6V2QAT', server, get_players,[]).

