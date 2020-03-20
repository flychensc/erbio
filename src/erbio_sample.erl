%%% A sample to use erpbio

-module(erbio_sample).


-export([start/0]).

start() ->
    erbio:start(),
    {Server, Client} = createPair(),
    % io:format("Server:~w Client:~w ~n", [Server, Client]),
    doHandShake(Client, Server),
    Data = say(Server, "Welcome to 2020"),
    echo(Client, Data),
    Data1 = say(Client, "Unforgettable 2020"),
    echo(Server, Data1),
    freePair(Server, Client).

createPair() ->
    {erbio:create(server),
    erbio:create(client)}.

freePair(Server, Client) ->
    erbio:cleanup(Client),
    erbio:cleanup(Server).

doHandShake(Peer1, Peer2) ->
    case erbio:is_init_finished(Peer1) of
        true -> done;

        false ->
            erbio:handshake(Peer1),
            Data = erbio:bio_read(Peer1),
            erbio:bio_write(Peer2, Data),
            doHandShake(Peer2, Peer1);

        _ -> error
    end.

say(Id, Msg) ->
    erbio:ssl_write(Id, Msg),
    % get encryted data
    erbio:bio_read(Id).

echo(Id, Data) ->
    % put encryted data
    erbio:bio_write(Id, Data),
    % get plaintext
    Msg = erbio:ssl_read(Id),
    io:format("receiev message:~w~n", [Msg]).