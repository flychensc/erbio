#ifndef _BIO_H_
#define _BIO_H_

#include <openssl/bio.h>
#include <openssl/ssl.h>

struct ssl_client
{
  SSL *ssl;

  BIO *rbio; /* SSL reads from, we write to. */
  BIO *wbio; /* SSL writes to, we read from. */
};

/* This enum contols whether the SSL connection needs to initiate the SSL
 * handshake. */
enum ssl_mode { SSLMODE_SERVER, SSLMODE_CLIENT };

/* Obtain the return value of an SSL operation and convert into a simplified
 * error code, which is easier to examine for failure. */
enum sslstatus { SSLSTATUS_OK, SSLSTATUS_WANT_IO, SSLSTATUS_FAIL};

// > 0 is ok
int ssl_init(const char * certfile, const char* keyfile);
void ssl_release(void);

void ssl_client_init(struct ssl_client *p,
                     enum ssl_mode mode);
void ssl_client_cleanup(struct ssl_client *p);

enum sslstatus do_ssl_handshake(struct ssl_client *p);
// 1: finish, 0: not
int is_init_finished(struct ssl_client *p);

enum sslstatus do_ssl_write(struct ssl_client *p, const void *buf, int num, int *written);
enum sslstatus do_ssl_read(struct ssl_client *p, void *buf, int num, int *readbytes);

int do_bio_write(struct ssl_client *p, const void *data, int dlen);
int do_bio_read(struct ssl_client *p, void *data, int dlen);

#endif
