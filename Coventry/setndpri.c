#include <sys/param.h>
#include <sys/resource.h>
#include <sys/schedctl.h>
#include <errno.h>
#include <stdio.h>

int main(int argc, char *argv[])
{

  if (argc >= 2) {
    int ndpri = atoi(argv[1]);
    int i, status, pid;

    if ((ndpri < NDPNORMMAX || ndpri > NDPLOMIN) && ndpri != 0) {
      printf ("ndpri should be in range %d..%d\n", NDPNORMMAX, NDPLOMIN);
      return 1;
    }
    for (i = 2; i < argc; i++) {
      pid = atoi(argv[i]);
      if (pid <= 0 || pid > MAXPID) {
	printf("processid %d out of range 1..%i\n", pid, MAXPID);
	return 1;
      }
      printf ("Setting ndpri to %d for process %d ... ", ndpri, pid);
      errno = 0;
    
      status = schedctl(NDPRI, pid, ndpri);

      if (errno != 0) {
	printf ("failed: %s\n", strerror(errno));
	return 1;
      }
      printf ("done\n");
    }
    return 0;
  }
  printf ("Usage: setndpri ndpri pid [pid ...]\n");
  return 2;
}
