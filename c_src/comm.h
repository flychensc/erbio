#ifndef _COMM_H_
#define _COMM_H_

typedef unsigned char byte;

struct port_memory_s {
    int size;
    byte *memory;
};

/* Read the 2 length bytes (MSB first), then the data. */
int read_cmd(byte *buf);

/* Pack the 2 bytes length (MSB first) and send it */
int write_cmd(byte *buf, int len);

#endif
