#include <sys/param.h>
#include <sys/resource.h>
#include <sys/schedctl.h>
#include <errno.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
  if (argc >= 1) {
    int i, pid, ndpri;

    for (i = 1; i < argc; i++) {
      pid = atoi(argv[i]);
      if (pid <= 0 || pid > MAXPID) {
	printf("processid %d out of range 1..%i\n", pid, MAXPID);
	return 1;
      }
      ndpri = schedctl(GETNDPRI, pid);
      printf ("ndpri(%d) = %d\n", pid, ndpri);
    }
    return 0;
  }
  printf ("Usage: getndpri pid [pid ...]\n");
  return 2;
}
