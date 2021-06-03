% created By Victor Ponce A00827302
% Created By Sergio Lopez A00827462

-module(player).

-export([init/1, player_messages/0,
	 register_with_server/1,move/1]).

register_with_server(ServerName) ->
    % central_server is a name known at the server side.
    % whereis(player) will return the name Pid of the process registered as player
    % so the server will know where to reply
    {central_server, ServerName} ! {register, whereis(player)}.

% Call like: player:init('server@DESKTOP-V6V2QAT').
init(ServerName) ->
    Pid = spawn(?MODULE, player_messages, []),
    % We register the new process as player
    register(player, Pid),
    register_with_server(ServerName).

% This function will be for the process exclusively to print messages received from the server.
% You can personalize, use pattern matching to properly label the output, etc.
player_messages() ->
    receive
    %   A -> io:fwrite("~p~n", [A]), A, player_messages();
      exit -> io:fwrite("Bye ~n");
    
        {client,Answer} -> Answer, 
        board(Answer,1),
        player_messages();

        {valid,Answer} -> Answer,
        io:format("~s,~n",[Answer]),
        player_messages(); 

        {done, Answer} -> Answer,
        io:format("~s,~n",[Answer]),
        player_messages()
    end.

print(ServerName) ->
    {central_server, ServerName} ! {print, whereis(player)},
    receive 
        {gameboard, GameBoard} -> GameBoard,
    board(GameBoard,1)
end. 

move(ServerName) -> 
    {ok,X} = io:read("Enter your position for X: "), 
    {ok,Y} = io:read("Enter your position for Y: "), 
    {ok,S} = io:read("Enter your symbol: "),
    Move = {X,Y,S}, 
    {central_server, ServerName} ! {move,whereis(player),Move}.

getBoard(ServerName) -> 
    {central_server,ServerName} ! {getboard,whereis(player)}.


board(Board, X) ->
    if X < 3 ->
	   io:fwrite("~s,~n", [tuple_to_list(element(X, Board))]),
	   board(Board, X + 1);
       true ->
	   io:fwrite("~s,~n", [tuple_to_list(element(X, Board))])
    end.
% Check that we can also do erpc:call('server@DESKTOP-V6V2QAT', server, get_players,[]).

