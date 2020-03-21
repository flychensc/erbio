#ifndef _DB_H_
#define _DB_H_

#include "bio.h"

#define DB_INVALID_ID   0

void db_init(void);
void db_release(void);

// free *ssl, indicate no use
int db_allocId(void);
// will check valid
struct ssl_client *db_get(int id);
// not check valid or not
struct ssl_client *db_locate(int id);
#endif
