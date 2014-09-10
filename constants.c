/* print various constant values used in the library */


#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <time.h>


struct sigaction sa;


int main()
{
  printf("(define-syntax %%SIGINT %d)\n", SIGINT);
  printf("(define-syntax %%SIG_IGN %ld)\n", (long)SIG_IGN);
  printf("(define-syntax %%SIG_DFL %ld)\n", (long)SIG_DFL);
  printf("(define-syntax %%O_RDONLY %d)\n", O_RDONLY);
  printf("(define-syntax %%O_WRONLY %d)\n", O_WRONLY);
  printf("(define-syntax %%O_CREAT %d)\n", O_CREAT);
  printf("(define-syntax %%O_TRUNC %d)\n", O_TRUNC);
  printf("(define-syntax %%O_APPEND %d)\n", O_APPEND);
#ifdef _WIN32
  printf("(define-syntax %%O_BINARY %d)\n", O_BINARY);
  printf("(define-syntax %%_S_IREAD %d)\n", _S_IREAD);
  printf("(define-syntax %%_S_IWRITE %d)\n", _S_IWRITE);
#else
  printf("(define-syntax %%S_IRUSR %d)\n", S_IRUSR);
  printf("(define-syntax %%S_IWUSR %d)\n", S_IWUSR);
  printf("(define-syntax %%S_IRGRP %d)\n", S_IRGRP);
  printf("(define-syntax %%S_IWGRP %d)\n", S_IWGRP);
  printf("(define-syntax %%S_IROTH %d)\n", S_IROTH);
#endif
#ifdef __linux__
  printf("(define-syntax %%WEXITED %d)\n", WEXITED);
  printf("(define-syntax %%CLOCK_MONOTONIC %d)\n", CLOCK_MONOTONIC);
#elif defined(__unix__)
  printf("(define-syntax %%CLOCK_REALTIME %d)\n", CLOCK_REALTIME);
#endif
  printf(";; sigaction-size: %ld\n", sizeof(struct sigaction));
  printf(";; sigaction-handler-offset: %ld\n", (char *)&sa.sa_handler - (char *)&sa);
  return 0;
}
