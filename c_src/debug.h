#ifndef _DEBUG_H_
#define _DEBUG_H_

#define DEBUG

int init_debug(void);
void free_debug(void);
void log_debug(char *fmt, ...);

#ifdef DEBUG
#define INIT_DEBUG()  init_debug()
#define RELEASE_DEBUG()  free_debug()
#define LOG_DEBUG(fmt, ...)  log_debug(fmt, __VA_ARGS__)
#else
#define INIT_DEBUG()
#define RELEASE_DEBUG()
#define LOG_DEBUG(fmt, ...)
#endif

#endif
