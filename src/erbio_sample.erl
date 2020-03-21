%%% A sample to use erpbio

-module(erbio_sample).


-export([start/0]).

start() ->
    erbio:start("cert/cacert.pem", "cert/cakey.pem"),
    {Server, Client} = createPair(),

    doHandShake(Client, Server),
    io:format("handshake done"),

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
        true ->
            case erbio:is_init_finished(Peer2) of
                true -> done;
                false -> doHandShake(Peer2, Peer1)
            end;

        false ->
            erbio:handshake(Peer1),
            Data = erbio:bio_read(Peer1),
            erbio:bio_write(Peer2, Data),
            doHandShake(Peer2, Peer1)
    end.

say(Id, Msg) ->
    erbio:ssl_write(Id, list_to_binary(Msg)),
    % get encryted data
    erbio:bio_read(Id).

echo(Id, Data) ->
    % put encryted data
    erbio:bio_write(Id, Data),
    % get plaintext
    Msg = binary_to_list(erbio:ssl_read(Id)),
    io:format("echo: ~s~n", [Msg]).
