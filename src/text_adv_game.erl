%%%-------------------------------------------------------------------
%%% @doc Process storing game state and processing it's logic.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(text_adv_game).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start_link/0, command/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% User commands, exported for internal use - so we can reference this
%% functions by name.
-export([cmd_look/4, cmd_go/4, cmd_get/4]).

%% We are storing our gen_server state in map.
-type state() :: #{}.

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
%% is like this: #{ room_of_origin => #{ name_of_direction => destination_room, ... }, ...}
-define(EDGES, #{ a => #{ n => e, e => b, s => d }
                , b => #{ w => a, s => c }
                , c => #{ n => b, w => d }
                , d => #{ n => a, e => c }
                }).

%% Mapping of command names that user can issue to corresponding
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

%% @doc Sends user command to game server and waits until it is handled.
-spec command(Cmd :: binary(), Args :: [binary()], GenServerRef :: term()) -> term().
command(Cmd, Args, ServerRef) ->
    gen_server:call(ServerRef, {command, Cmd, Args}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> state().
init([]) ->
    {ok, #{ location => ?INITIAL_ROOM
          , rooms => ?ROOMS
          , edges => ?EDGES
          , commands => ?KNOWN_COMMANDS
          }}.

handle_call({command, Cmd, Args}, _From, State) ->
    lager:info("Got command ~p with args ~p", [Cmd, Args]),
    {State1, Reply} = run_cmd(Cmd, Args, State),
    case is_final_state(State1) of
        true ->
            {stop, normal, Reply, State1};
        false ->
            {reply, Reply, State1}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
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
%%% Internal functions
%%%===================================================================

run_cmd(Cmd, Args, #{commands := Commands} = State) ->
    Reply = make_empty_reply(),
    case maps:find(Cmd, Commands) of
        {ok, Fun} ->
            ?MODULE:Fun(Cmd, Args, State, Reply);
        error ->
            {State, add_reply_error("Unknown command '~s'", [Cmd], Reply)}
    end.

cmd_look(_Cmd, [], State, Reply) ->
    AddMessage = fun (Message, Acc) -> add_reply_text(Message, [], Acc) end,
    SkipEmptyString = fun (Str) -> Str =/= undefined end,
    {State, lists:foldl(AddMessage, Reply,
                        lists:filter(SkipEmptyString,
                                     [ describe_room(State)
                                     , describe_objects(State)
                                     ]))};
cmd_look(_Cmd, _Args, State, Reply) ->
    {State, Reply}.

cmd_go(_Cmd, _Direction, State, Reply) ->
    {State, Reply}.

cmd_get(_Cmd, _Object, State, Reply) ->
    {State, Reply}.

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
