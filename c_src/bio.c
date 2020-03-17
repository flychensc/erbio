#include <string.h>
#include "bio.h"
#include <openssl/err.h>

/* Global SSL context */
static SSL_CTX *ctx = NULL;

void ssl_client_init(struct ssl_client *p,
                     enum ssl_mode mode)
{
  memset(p, 0, sizeof(struct ssl_client));

  p->rbio = BIO_new(BIO_s_mem());
  p->wbio = BIO_new(BIO_s_mem());
  p->ssl = SSL_new(ctx);

  if (mode == SSLMODE_SERVER)
  {
    SSL_set_accept_state(p->ssl);  /* ssl server mode */
  }
  else if (mode == SSLMODE_CLIENT)
  {
    SSL_set_connect_state(p->ssl); /* ssl client mode */
  }

  SSL_set_bio(p->ssl, p->rbio, p->wbio);
}

void ssl_client_cleanup(struct ssl_client *p)
{
  SSL_free(p->ssl);   /* free the SSL object and its BIO's */
  p->ssl = NULL;
  // BIO_free(p->rbio);
  p->rbio = NULL;
  // BIO_free(p->wbio);
  p->wbio = NULL;
}

static enum sslstatus get_sslstatus(SSL* ssl, int n)
{
  switch (SSL_get_error(ssl, n))
  {
    case SSL_ERROR_NONE:
      return SSLSTATUS_OK;
    case SSL_ERROR_WANT_WRITE:
    case SSL_ERROR_WANT_READ:
      return SSLSTATUS_WANT_IO;
    case SSL_ERROR_ZERO_RETURN:
    case SSL_ERROR_SYSCALL:
    default:
      return SSLSTATUS_FAIL;
  }
}

enum sslstatus do_ssl_handshake(struct ssl_client *p)
{
  int n = SSL_do_handshake(p->ssl);

  return get_sslstatus(p->ssl, n);
}

int is_init_finished(struct ssl_client *p)
{
    return SSL_is_init_finished(p->ssl);
}

enum sslstatus do_ssl_write(struct ssl_client *p, const void *buf, int num, int *written)
{
  int n = SSL_write(p->ssl, buf, num);

  if(n > 0) {

    *written = n;
    return SSLSTATUS_OK;
  } else {
    return get_sslstatus(p->ssl, n);
  }
}

enum sslstatus do_ssl_read(struct ssl_client *p, void *buf, int num, int *readbytes)
{
  int n = SSL_read(p->ssl, buf, num);

  if (n > 0) {

    *readbytes = n;
    return SSLSTATUS_OK;
  } else {
    return get_sslstatus(p->ssl, n);
  }
}

int do_bio_write(struct ssl_client *p, const void *data, int dlen)
{
  int n = BIO_write(p->rbio, data, dlen);

  if (n > 0) {
    return n;
  } else if (BIO_should_retry(p->rbio)) {
    return 0;
  } else {
    return  -1;
  }
}

int do_bio_read(struct ssl_client *p, void *data, int dlen)
{
  int n = BIO_read(p->wbio, data, dlen);

  if (n > 0) {
    return n;
  } else if (BIO_should_retry(p->wbio)) {
    return 0;
  } else {
    return -1;
  }
}

int ssl_init(const char * certfile, const char* keyfile)
{
  // printf("\r\n initialising SSL");

  /* SSL library initialisation */
  SSL_library_init();
  OpenSSL_add_all_algorithms();
  SSL_load_error_strings();
  ERR_load_BIO_strings();
  ERR_load_crypto_strings();

  /* create the SSL server context */
  ctx = SSL_CTX_new(SSLv23_method());
  if (!ctx) {
    fprintf(stderr,"\r\n SSL_CTX_new fail");
    return -1;
  }

  /* Load certificate and private key files, and check consistency */
  if (certfile && keyfile) {
    if (SSL_CTX_use_certificate_file(ctx, certfile,  SSL_FILETYPE_PEM) != 1) {
      fprintf(stderr,"\r\n SSL_CTX_use_certificate_file failed");
      SSL_CTX_free(ctx);
      return -1;
    }

    if (SSL_CTX_use_PrivateKey_file(ctx, keyfile, SSL_FILETYPE_PEM) != 1) {
      fprintf(stderr,"\r\n SSL_CTX_use_PrivateKey_file failed");
      SSL_CTX_free(ctx);
      return -1;
    }

    /* Make sure the key and certificate file match. */
    if (SSL_CTX_check_private_key(ctx) != 1) {
      fprintf(stderr,"\r\n SSL_CTX_check_private_key failed");
      SSL_CTX_free(ctx);
      return -1;
    } else {
      //printf("\r\n certificate and private key loaded and verified");
    }
  }

  /* Recommended to avoid SSLv2 & SSLv3 */
  SSL_CTX_set_options(ctx, SSL_OP_ALL|SSL_OP_NO_SSLv2|SSL_OP_NO_SSLv3);

  return 1;
}

void ssl_release(void)
{
  if (ctx != NULL) {
    SSL_CTX_free(ctx);
    ctx = NULL;
  }
}