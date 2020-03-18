% commands
-define(CMD_CLEANUP,            1).
-define(CMD_CREATE_CLIENT,      2).
-define(CMD_CREATE_SERVER,      3).
-define(CMD_HANDSHAKE,          4).
-define(CMD_IS_INIT_FINISHED,   5).
-define(CMD_SSL_WRITE,          6).
-define(CMD_SSL_READ,           7).
-define(CMD_BIO_WRITE,          8).
-define(CMD_BIO_READ,           9).

% result
-define(RET_OK,             1).
-define(RET_WOULD_BLOCK,    2).
-define(RET_FAIL,           3).
