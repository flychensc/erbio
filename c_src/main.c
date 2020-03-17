#include <stdio.h>
#include "bio.h"
#include "comm.h"
#include "db.h"

#ifdef WIN32
#include <windows.h>
BOOL WINAPI ConsoleHandler(DWORD dwCtrlType) {
	db_release();
    ssl_release();

	return TRUE;
}
#endif

enum port_cmd_e {
    CMD_CLEANUP,
    CMD_CREATE_CLIENT,
    CMD_CREATE_SERVER,
    CMD_HANDSHAKE,
    CMD_IS_INIT_FINISHED,
    CMD_SSL_WRITE,
    CMD_SSL_READ,
    CMD_BIO_WRITE,
    CMD_BIO_READ,

    CMD_LAST
};

int main(int argc, char** argv) {
    int ret = 0;
    struct port_memory_s   port_mem;
    byte    cmd;

#ifdef WIN32
    SetConsoleCtrlHandler(ConsoleHandler, TRUE);
#endif

    ssl_init(NULL, NULL);
    db_init();

    memset(&port_mem, 0, sizeof(struct port_memory_s));

    port_mem.size = 0;
    port_mem.memory = malloc(1);
    if(!port_mem.memory) {
        fprintf(stderr,"\r\n No memory");
        ret = -1;
        goto end;
    }
    while(read_cmd(&port_mem) > 0) {
        if(port_mem.size < 1) {
            fprintf(stderr,"\r\n short command");
            ret = -1;
            break;
        }
        cmd = port_mem.memory[0];
        switch (cmd) {
            case CMD_CLEANUP:
                break;
            case CMD_CREATE_CLIENT:
                break;
            case CMD_CREATE_SERVER:
                break;
            case CMD_HANDSHAKE:
                break;
            case CMD_IS_INIT_FINISHED:
                break;
            case CMD_SSL_WRITE:
                break;
            case CMD_SSL_READ:
                break;
            case CMD_BIO_WRITE:
                break;
            case CMD_BIO_READ:
                break;
            default:
                // recv invalid command
                fprintf(stderr,"\r\n invalid command=%d", cmd);
                ret = -1;
                goto end;
        }
    }

end:
    db_release();
    ssl_release();

    return ret;
}
