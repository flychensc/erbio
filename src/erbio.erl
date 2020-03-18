%%% erpbio encapsulates OpenSSL mem BIO APIs

-module(erpbio).

-behaviour(gen_server).

%% API
-export([start/0, start/1, start/2, start/3, stop/0]).
-export([create/1, cleanup/1, handshake/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {port}).

%% Start erpbio
-spec start(string(), string(), integer())  -> {ok, pid()}.
start(CertFile, KeyFile, BufferLen) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [CertFile, KeyFile, BufferLen], []).

-spec start(string(), string())  -> {ok, pid()}.
start(CertFile, KeyFile) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [CertFile, KeyFile, undef], []).

-spec start(integer())  -> {ok, pid()}.
start(BufferLen) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [undef, undef, BufferLen], []).

-spec start()  -> {ok, pid()}.
start() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [undef, undef, undef], []).

%% Stop erpbio
-spec stop() -> ok.
stop() ->
   gen_server:call(?MODULE, stop).

%% create client/server
-spec create(client | server) -> {ok, integer()} | fail.
create(Type) ->
    gen_server:call(?MODULE, {create, Type}).

%% destroy client/server
-spec cleanup(integer()) -> ok.
cleanup(Id) ->
    gen_server:call(?MODULE, {cleanup, Id}).

%% do handshake
-spec handshake(integer()) -> ok | fail | wouldblock.
handshake(Id) ->
    gen_server:call(?MODULE, {handshake, Id}).

init(Args) ->
    Command = genCommand(Args),
    % io:format("Command:~s~n", [Command]),
    Port = open_port({spawn, Command}, [{packet, 2}]),
   {ok, #state{port=Port}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({reg, Handler}, State) ->
    #state{handlers=Handlers} = State,
   {noreply, #state{handlers=lists:append(Handlers, [Handler])}};

handle_cast({send, Packet}, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, Packet}},
   {noreply, State};

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info({_Port, {data, Data}}, State) ->
    #state{handlers=Handlers} = State,
    Packet = list_to_binary(Data),
    % io:format("recv ~w bytes~n", [byte_size(Packet)]),
    lists:foreach(fun(Handler)-> Handler(Packet) end, Handlers),
   {noreply, State};

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, State) ->
    #state{port=Port} = State,
    Port ! {self(), close},
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


genCommand([undef, undef, undef]) ->
    unicode:characters_to_list(["erpbio "]);
genCommand([undef, undef, BufferLen]) ->
    unicode:characters_to_list(["erpbio -b ", BufferLen]);
genCommand([CertFile, KeyFile, undef]) ->
    unicode:characters_to_list(["erpbio --cert ", CertFile, " --key ", KeyFile]);
genCommand([CertFile, KeyFile, BufferLen]) ->
    unicode:characters_to_list(["erpbio --cert ", CertFile, " --key ", KeyFile, " -b ", BufferLen]).
