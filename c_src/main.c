#include <stdio.h>
#include "bio.h"
#include "comm.h"
#include "db.h"

#define DEFAULG_BUFFER_LEN  2048    // 2KB


int _buffer_len = DEFAULG_BUFFER_LEN;
static byte *_recv_buffer = NULL;
static byte *_send_buffer = NULL;

#ifdef WIN32
#include <windows.h>
BOOL WINAPI ConsoleHandler(DWORD dwCtrlType) {
	db_release();
    ssl_release();

    if(_recv_buffer) {
        free(_recv_buffer);
        _recv_buffer = NULL;
    }
    if(_send_buffer) {
        free(_send_buffer);
        _send_buffer = NULL;
    }

	return TRUE;
}
#endif

enum port_cmd_e {
    CMD_CLEANUP             = 1,
    CMD_CREATE_CLIENT       = 2,
    CMD_CREATE_SERVER       = 3,
    CMD_HANDSHAKE           = 4,
    CMD_IS_INIT_FINISHED    = 5,
    CMD_SSL_WRITE           = 6,
    CMD_SSL_READ            = 7,
    CMD_BIO_WRITE           = 8,
    CMD_BIO_READ            = 9,

    CMD_LAST
};

enum port_result_e {
    RET_OK          = 1,
    RET_WOULD_BLOCK = 2,
    RET_FAIL        = 3,

    RET_LAST
};

#define CONV_IN16(_bytes)   ((_bytes)[0] << 8 | (_bytes)[1])

static void put16(byte *dst, short src) {
    dst[0] = (src >> 8) & 0xFF;
    dst[1] = src & 0xFF;
}

// ret < 0, error occur
typedef int (*command_handle)(byte *buf);

int handle_cleanup(byte *data) {
    struct ssl_client *client;
    int id;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        ssl_client_cleanup(client);
    }

    _send_buffer[0] = RET_OK;
    write_cmd(_send_buffer, 1);

    return 0;
}

int handle_create_client(byte *data) {
    struct ssl_client *client;
    int id;

    id = db_allocId();
    if (DB_INVALID_ID == id) {

        _send_buffer[0] = RET_FAIL;
        write_cmd(_send_buffer, 1);

        return 0;
    }

    client = db_get(id);
    if(client) {
        ssl_client_init(client, SSLMODE_CLIENT);

        _send_buffer[0] = RET_OK;
        put16(&_send_buffer[1], id);
        write_cmd(_send_buffer, 1+2);
    } else {
        return -1;
    }
}

int handle_create_server(byte *data) {
    struct ssl_client *client;
    int id;

    id = db_allocId();
    if (DB_INVALID_ID == id) {

        _send_buffer[0] = RET_FAIL;
        write_cmd(_send_buffer, 1);

        return 0;
    }

    client = db_get(id);
    if(client) {
        ssl_client_init(client, SSLMODE_SERVER);

        _send_buffer[0] = RET_OK;
        put16(&_send_buffer[1], id);
        write_cmd(_send_buffer, 1+2);

        return 0;
    } else {
        return -1;
    }
}

int handle_handshake(byte *data) {
    struct ssl_client *client;
    int id;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        switch(do_ssl_handshake(client)) {
            case SSLSTATUS_OK:
                _send_buffer[0] = RET_OK;
                break;
            case SSLSTATUS_WANT_IO:
                _send_buffer[0] = RET_WOULD_BLOCK;
                break;
            case SSLSTATUS_FAIL:
                _send_buffer[0] = RET_FAIL;
                break;
            default:
                return -1;
        }

        write_cmd(_send_buffer, 1);
        return 0;
    }

    _send_buffer[0] = RET_FAIL;
    write_cmd(_send_buffer, 1);

    return 0;
}

int handle_is_init_finished(byte *data) {
    struct ssl_client *client;
    int id;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        _send_buffer[0] = RET_OK;
        if(is_init_finished(client)) {
            _send_buffer[1] = 1;
        } else {
            _send_buffer[1] = 0;
        }
        write_cmd(_send_buffer, 1+1);
        return 0;
    }

    _send_buffer[0] = RET_FAIL;
    write_cmd(_send_buffer, 1);

    return 0;
}

int handle_ssl_write(byte *data) {
    struct ssl_client *client;
    int id;
    byte *buf;
    short dlen;
    int written;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        dlen = CONV_IN16(&data[2]);
        buf = &data[4];

        switch(do_ssl_write(client, buf, dlen, &written)) {
            case SSLSTATUS_OK:
                _send_buffer[0] = RET_OK;
                put16(&_send_buffer[1], written);
                write_cmd(_send_buffer, 1+2);
                return 0;
            case SSLSTATUS_WANT_IO:
                _send_buffer[0] = RET_WOULD_BLOCK;
                write_cmd(_send_buffer, 1);
                return 0;
            case SSLSTATUS_FAIL:
                _send_buffer[0] = RET_FAIL;
                write_cmd(_send_buffer, 1);
                return 0;
            default:
                return -1;
        }
    }

    _send_buffer[0] = RET_FAIL;
    write_cmd(_send_buffer, 1);

    return 0;
}

int handle_ssl_read(byte *data) {
    struct ssl_client *client;
    int id;
    byte *buf;
    short dlen;
    int readbytes;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        dlen = _buffer_len - 1;
        buf = &_send_buffer[1];

        switch(do_ssl_read(client, buf, dlen, &readbytes)) {
            case SSLSTATUS_OK:
                _send_buffer[0] = RET_OK;
                write_cmd(_send_buffer, 1+readbytes);
                return 0;
            case SSLSTATUS_WANT_IO:
                _send_buffer[0] = RET_WOULD_BLOCK;
                write_cmd(_send_buffer, 1);
                return 0;
            case SSLSTATUS_FAIL:
                _send_buffer[0] = RET_FAIL;
                write_cmd(_send_buffer, 1);
                return 0;
            default:
                return -1;
        }
    }

    _send_buffer[0] = RET_FAIL;
    write_cmd(_send_buffer, 1);

    return 0;
}

int handle_bio_write(byte *data) {
    struct ssl_client *client;
    int id;
    byte *buf;
    short dlen;
    int written;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        dlen = CONV_IN16(&data[2]);
        buf = &data[4];

        written = do_bio_write(client, buf, dlen);
        if (written > 0) {
            _send_buffer[0] = RET_OK;
            put16(&_send_buffer[1], written);
            write_cmd(_send_buffer, 1+2);
            return 0;
        } else {
            _send_buffer[0] = RET_FAIL;
            write_cmd(_send_buffer, 1);
            return 0;
        }
    }

    _send_buffer[0] = RET_FAIL;
    write_cmd(_send_buffer, 1);

    return 0;
}

int handle_bio_read(byte *data) {
    struct ssl_client *client;
    int id;
    byte *buf;
    short dlen;
    int readbytes;

    id = CONV_IN16(data);

    client = db_get(id);
    if(client) {
        dlen = _buffer_len - 1;
        buf = &_send_buffer[1];

        readbytes = do_bio_read(client, buf, dlen);
        if (readbytes > 0) {
            _send_buffer[0] = RET_OK;
            write_cmd(_send_buffer, 1+readbytes);
            return 0;
        } else {
            _send_buffer[0] = RET_FAIL;
            write_cmd(_send_buffer, 1);
            return 0;
        }
    }

    _send_buffer[0] = RET_FAIL;
    write_cmd(_send_buffer, 1);

    return 0;
}

static command_handle command_handles[] = {
    handle_cleanup,             // CMD_CLEANUP
    handle_create_client,       // CMD_CREATE_CLIENT
    handle_create_server,       // CMD_CREATE_SERVER
    handle_handshake,           // CMD_HANDSHAKE
    handle_is_init_finished,    // CMD_IS_INIT_FINISHED
    handle_ssl_write,           // CMD_SSL_WRITE
    handle_ssl_read,            // CMD_SSL_READ
    handle_bio_write,           // CMD_BIO_WRITE
    handle_bio_read,            // CMD_BIO_READ
};

static void printhelp(void) {
    printf("\n--cert NAME          -- Specifies certfile");
    printf("\n--key NAME           -- Specifies keyfile");
    printf("\n-b INT               -- Specifies buffer length (default:2KB)");
    printf("\n\n");
}

int main(int argc, char** argv) {
    char *certfile = NULL;
    char *keyfile = NULL;
    byte cmd;
    int ret = 0;

#ifdef WIN32
    SetConsoleCtrlHandler(ConsoleHandler, TRUE);
#endif

    for(int i=1; i<argc; i++) {
        if (!strcmp(argv[i], "--cert")) {
            i++;
			certfile = argv[i];
		} else if (!strcmp(argv[i], "--key")) {
            i++;
			keyfile = argv[i];
		} else if (!strcmp(argv[i], "-b")) {
            i++;
			_buffer_len = atoi(argv[i]);
		} else {
            printhelp();
            return(-1);
        }
    }

    _recv_buffer = malloc(_buffer_len);
    if(!_recv_buffer) {
        fprintf(stderr,"\r\n No memory to create recv buffer");
        return (-1);
    }
    _send_buffer = malloc(_buffer_len);
    if(!_send_buffer) {
        fprintf(stderr,"\r\n No memory to create send buffer");
        free(_recv_buffer);
        return (-1);
    }

    ssl_init(certfile, keyfile);
    db_init();

    while(read_cmd(_recv_buffer) > 0) {
        cmd = _recv_buffer[0];
        if ((cmd < CMD_LAST) && command_handles[cmd]) {
            ret = command_handles[cmd](&_recv_buffer[1]);
            if (ret < 0) {
                break;
            }
        } else {
            // recv invalid command
            fprintf(stderr,"\r\n invalid command=%d", cmd);
            ret = -1;
            break;
        }
    }

    db_release();
    ssl_release();

    if(_recv_buffer) {
        free(_recv_buffer);
        _recv_buffer = NULL;
    }
    if(_send_buffer) {
        free(_send_buffer);
        _send_buffer = NULL;
    }

    return ret;
}
