#include <stdio.h>
#include "bio.h"
#include "db.h"

#ifdef WIN32
#include <windows.h>
BOOL WINAPI ConsoleHandler(DWORD dwCtrlType) {
	db_release();
    ssl_release();

	return TRUE;
}
#endif

int main(int argc, char** argv) {

#ifdef WIN32
    SetConsoleCtrlHandler(ConsoleHandler, TRUE);
#endif

    ssl_init(NULL, NULL);
    db_init();


    while(1) {
        // todo:
    }
    
    db_release();

    return 0;
}
