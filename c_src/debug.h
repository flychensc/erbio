#ifndef _DEBUG_H_
#define _DEBUG_H_

//#define DEBUG

int init_debug(void);
void free_debug(void);
void log_debug(char *fmt, ...);
void dump_buf(unsigned char *buf, int len);

#ifdef DEBUG
#define INIT_DEBUG()  init_debug()
#define RELEASE_DEBUG()  free_debug()
#define LOG_DEBUG(fmt, ...)  log_debug(fmt, __VA_ARGS__)
#define DUMP_BUF(buf, len)  dump_buf(buf, len)
#else
#define INIT_DEBUG()
#define RELEASE_DEBUG()
#define LOG_DEBUG(fmt, ...)
#define DUMP_BUF(buf, len)
#endif

#endif
