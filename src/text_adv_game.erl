%%%-------------------------------------------------------------------
%%% @doc gen_server-based process implementing rules of our adventure
%%% game and storage for single game state. User interaction should be
%%% handled in some other place.
%%%
%%% Among other things process state contains:
%%% - current set of room with their descriptions
%%% - information about connectivity between rooms
%%% - set of commands currently available to user
%%%
%%% This we can dynamically change any aspect during a course of the
%%% game.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(text_adv_game).

-behaviour(gen_server).

%% API
-export([start_link/0, command/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% User commands, exported only for internal use, so we can reference
%% them by name later. Command dispatch table is a part of our process
%% state, so we couldn't just use short function references (fun
%% name/X) here, or we'll lose ability to reload changed code.
-export([cmd_look/4, cmd_go/4, cmd_get/4]).

%% We are storing our gen_server state in map.
-type state() :: #{}.

%% And we are also using maps for information that should be presented
%% to player.
-type reply() :: #{}.

%%%===================================================================
%%% Game metadata
%%%===================================================================

%% Room where the game starts.
-define(INITIAL_ROOM, a).

%% Initial room descriptions.
-define(ROOMS, #{ e => <<"A nice garden">>
                , a => <<"A cold room">>
                , b => <<"A dusky room">>
                , c => <<"A hot room">>
                , d => <<"A bright room">>
                }).

%% Initial room connectivity. If a pathway is accessible from both
%% rooms, there should be 2 edges going in opposite directions. Format
%% is like this:
%% #{ room_of_origin => #{ name_of_direction => destination_room, ... }, ...}
-define(EDGES, #{ a => #{ n => e, e => b, s => d }
                , b => #{ w => a, s => c }
                , c => #{ n => b, w => d }
                , d => #{ n => a, e => c }
                }).

%% Initial mapping of command names that user can issue to corresponding
%% functions. This way we can have aliases, like the get/take in the
%% following code.
-define(KNOWN_COMMANDS, #{<<"look">> => cmd_look,
                          <<"go">> => cmd_go,
                          <<"get">> => cmd_get,
                          <<"take">> => cmd_get}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Sends an user command to game server and awaits for reply that
%% should be presented to the user.
-spec command(Cmd :: binary(), Args :: [binary()], GenServerRef :: term()) -> term().
command(Cmd, Args, ServerRef) ->
    gen_server:call(ServerRef, {command, Cmd, Args}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> state().
init([]) ->
    {ok, make_game_state()}.

handle_call({command, Cmd, Args}, _From, State) ->
    {State1, Reply} = run_cmd(Cmd, Args, State),
    case is_final_state(State1) of
        true ->
            {stop, normal, Reply, State1};
        false ->
            {reply, Reply, State1}
    end;
handle_call(_Request, _From, State) ->
    Reply = make_empty_reply(),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Handling of user commands
%%%===================================================================

%% @doc Main driving force of our game. In response to the user
%% command we should decide what new world state would be and what we
%% should we reply to the user.
-spec run_cmd(Cmd :: binary(), Args :: [binary()], State :: state()) -> {state(), reply()}.
run_cmd(Cmd, Args, #{commands := Commands} = State) ->
    Reply = make_empty_reply(),
    case maps:find(Cmd, Commands) of
        {ok, Fun} ->
            ?MODULE:Fun(Cmd, Args, State, Reply);
        error ->
            {State, add_reply_error("Unknown command '~s'", [Cmd], Reply)}
    end.

%% @doc Describe world around the player, and the state of the player
%% himself.
-spec cmd_look(Cmd :: binary(), Args :: [binary()], State :: state(), Reply :: reply()) -> {state(), reply()}.
cmd_look(_Cmd, [], State, Reply) ->
    AddMessage = fun (Message, Acc) -> add_reply_text(Message, [], Acc) end,
    SkipEmptyString = fun (Str) -> Str =/= undefined end,
    {State, lists:foldl(AddMessage, Reply,
                        lists:filter(SkipEmptyString,
                                     [ describe_room(State)
                                     , describe_objects(State)
                                     ]))};
cmd_look(_Cmd, _Args, State, Reply) ->
    {State, add_reply_error("Usage: look (without any arguments)", [], Reply)}.

%% @doc Try to move in the direction specified by the player.
-spec cmd_go(Cmd :: binary(), Args :: [binary()], State :: state(), Reply :: reply()) -> {state(), reply()}.
cmd_go(_Cmd, _Direction, State, Reply) ->
    {State, Reply}.

%% @doc Try to move the named object from the world to the player's inventory.
-spec cmd_get(Cmd :: binary(), Args :: [binary()], State :: state(), Reply :: reply()) -> {state(), reply()}.
cmd_get(_Cmd, _Object, State, Reply) ->
    {State, Reply}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_final_state(_State) ->
    false.

make_empty_reply() ->
    #{ errors => [], messages => []}.

add_reply_error(FormatStr, Data, Reply) ->
    Reply#{ errors => [ list_to_binary(io_lib:format(FormatStr, Data)) | maps:get(errors, Reply) ]}.

add_reply_text(FormatStr, Data, Reply) ->
    Reply#{ messages => [ list_to_binary(io_lib:format(FormatStr, Data)) | maps:get(messages, Reply) ]}.

describe_room(#{location := Location, rooms := Rooms}) ->
    maps:get(Location, Rooms).

describe_objects(_State) ->
    undefined.

make_game_state() ->
    #{ location => ?INITIAL_ROOM
     , rooms => ?ROOMS
     , edges => ?EDGES
     , commands => ?KNOWN_COMMANDS
     }.
