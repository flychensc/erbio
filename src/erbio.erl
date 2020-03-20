%%% erbio encapsulates OpenSSL mem BIO APIs

-module(erbio).

-behaviour(gen_server).

-include("erbio.hrl").

%% API
-export([start/0, start/1, start/2, start/3, stop/0]).
-export([create/1, cleanup/1]).
-export([handshake/1, is_init_finished/1]).
-export([ssl_write/2, ssl_read/1]).
-export([bio_write/2, bio_read/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {port}).

%% Start erbio
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

%% Stop erbio
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

%% is init finished
-spec is_init_finished(integer()) -> {ok, true | false} | fail.
is_init_finished(Id) ->
    gen_server:call(?MODULE, {is_init_finished, Id}).

%% do ssl_write
-spec ssl_write(integer(), binary()) -> {ok, integer()} | wouldblock | fail.
ssl_write(Id, Data) ->
    gen_server:call(?MODULE, {ssl_write, Id, Data}).

%% do ssl_read
-spec ssl_read(integer()) -> {ok, binary()} | wouldblock | fail.
ssl_read(Id) ->
    gen_server:call(?MODULE, {ssl_read, Id}).

%% do bio_write
-spec bio_write(integer(), binary()) -> {ok, integer()} | fail.
bio_write(Id, Data) ->
    gen_server:call(?MODULE, {bio_write, Id, Data}).

%% do bio_read
-spec bio_read(integer()) -> {ok, binary()} | fail.
bio_read(Id) ->
    gen_server:call(?MODULE, {bio_read, Id}).

init(Args) ->
    Command = genCommand(Args),
    % io:format("Command:~s~n", [Command]),
    Port = open_port({spawn, Command}, [{packet, 2}]),
   {ok, #state{port=Port}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({create, client}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_CREATE_CLIENT>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, Id:16/big-integer>> ->
               {reply, {ok, Id}, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({create, server}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_CREATE_SERVER>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, Id:16/big-integer>> ->
               {reply, {ok, Id}, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({cleanup, Id}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_CLEANUP, Id:16/big-integer>>}},
    receive
      {Port, {data, _Data}} ->
         {reply, ok, State}
    end;

handle_call({handshake, Id}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_HANDSHAKE, Id:16/big-integer>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK>> ->
               {reply, ok, State};

            <<?RET_WOULD_BLOCK>> ->
               {reply, wouldblock, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({is_init_finished, Id}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_IS_INIT_FINISHED, Id:16/big-integer>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, 1>> ->
               {reply, {ok, true}, State};

            <<?RET_OK, 0>> ->
               {reply, {ok, false}, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({ssl_write, Id, Data}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_SSL_WRITE, Id:16/big-integer, Data/binary>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, Written>> ->
               {reply, {ok, Written}, State};

            <<?RET_WOULD_BLOCK, 0>> ->
               {reply, wouldblock, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({ssl_read, Id}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_SSL_READ, Id:16/big-integer>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, ReadData/binary>> ->
               {reply, {ok, ReadData}, State};

            <<?RET_WOULD_BLOCK, 0>> ->
               {reply, wouldblock, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({bio_write, Id, Data}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_BIO_WRITE, Id:16/big-integer, Data/binary>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, Written>> ->
               {reply, {ok, Written}, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call({bio_read, Id}, _From, State) ->
    #state{port=Port} = State,
    Port ! {self(), {command, <<?CMD_BIO_READ, Id:16/big-integer>>}},
    receive
      {Port, {data, Data}} ->
         case list_to_binary(Data) of
            <<?RET_OK, ReadData/binary>> ->
               {reply, {ok, ReadData}, State};

            <<?RET_FAIL>> ->
               {reply, fail, State}
         end
    end;

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, State) ->
    #state{port=Port} = State,
    Port ! {self(), close},
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


genCommand([undef, undef, undef]) ->
    unicode:characters_to_list(["erbio "]);
genCommand([undef, undef, BufferLen]) ->
    unicode:characters_to_list(["erbio -b ", BufferLen]);
genCommand([CertFile, KeyFile, undef]) ->
    unicode:characters_to_list(["erbio --cert ", CertFile, " --key ", KeyFile]);
genCommand([CertFile, KeyFile, BufferLen]) ->
    unicode:characters_to_list(["erbio --cert ", CertFile, " --key ", KeyFile, " -b ", BufferLen]).
