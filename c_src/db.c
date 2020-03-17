#include <stdlib.h>
#include <string.h>
#include "db.h"

#define DB_CLIENT_SIZE    32

#define IDX2ID(index)   (index+1)
#define ID2IDX(id)      (id-1)

struct db_mgt_s
{
    int size;
    int count;

    struct ssl_client   *clients;   // array;
};

static struct db_mgt_s db_manager = {
    .size = 0,
    .count = 0,
    .clients = NULL,
};

void db_init(void) {
    db_manager.clients = calloc(DB_CLIENT_SIZE, sizeof(struct ssl_client));
    if(NULL == db_manager.clients) {
        // no memory
        fprintf(stderr,"\r\n No memory to create clients");
        return;
    }
    db_manager.size = DB_CLIENT_SIZE;
}

void db_release(void) {
    if(NULL != db_manager.clients) {
        free(db_manager.clients);
        db_manager.clients = NULL;
        db_manager.size = 0;
    }
}

int db_allocId(void) {
    int index;
    void *new_memory;

    for(index = 0; index < db_manager.size; index++) {
        // no use?
        if(NULL == db_manager.clients[index].ssl) {
            return IDX2ID(index);
        }
    }

    // no space, grow
    new_memory = realloc(db_manager.clients, db_manager.size*2);
    if(NULL == new_memory) {
        fprintf(stderr,"\r\n No memory to grow");
        return DB_INVALID_ID;    
    }

    db_manager.clients = new_memory;

    // init rest part
    memset(&db_manager.clients[index], 0, index*sizeof(struct ssl_client));

    return IDX2ID(index);
}

struct ssl_client *db_get(int id)
{
    if(ID2IDX(id) < db_manager.size) {
        return &db_manager.clients[ID2IDX(id)];        
    }
    fprintf(stderr,"\r\n Not found client id=%d", id);
    return NULL;
}
