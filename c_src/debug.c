#include <stdarg.h>
#include<stdio.h>
#include <windows.h>

static HANDLE _dbg_file = INVALID_HANDLE_VALUE;

static char _debug_buf[1024+4] = {0};

int init_debug(void)
{
    _dbg_file = CreateFile("erbio.log", GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, 0, NULL);
    if(INVALID_HANDLE_VALUE == _dbg_file) {
        return -1;
    }

    return 0;
}

void free_debug(void)
{
    if(INVALID_HANDLE_VALUE != _dbg_file) {
        CloseHandle(_dbg_file);
    }
}

void log_debug(char *fmt, ...)
{
    DWORD written;
    va_list argptr;

    va_start(argptr, fmt);
    vsprintf_s(_debug_buf, 1024, fmt, argptr);
    va_end(argptr);

    WriteFile(_dbg_file, _debug_buf, (DWORD)strlen(_debug_buf), &written, 0);
}

void dump_pkt(byte *pkt, int len)
{
    DWORD written;
    char str_buf[16+4];
    int i;

    for (i=0; i<len; i++) {
        if (0 == (i%16)) {
            WriteFile(_dbg_file, "\r\n", (DWORD)strlen("\r\n"), &written, 0);
        }
        sprintf_s(str_buf, 16, " %02X", pkt[i]);
        WriteFile(_dbg_file, str_buf, (DWORD)strlen(str_buf), &written, 0);
    }
}
