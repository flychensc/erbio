%%% A sample to use erpbio

-module(erbio_sample).


-export([start/0]).

start() ->
    erbio:start("cert/cacert.pem", "cert/cakey.pem"),
    {Server, Client} = createPair(),

    erbio:handshake(Client),
    Data = erbio:bio_read(Client),
    erbio:bio_write(Server, Data),

    erbio:handshake(Server),
    Data1 = erbio:bio_read(Server),
    erbio:bio_write(Client, Data1),

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