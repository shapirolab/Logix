#include <stdio.h>

#define DEFAULT 0

/******************************  Examples *******************************/
#define SQUARE 100
#define POLY   101
/************************************************************************/

struct weighter {
  char *name;
  int index;
};

/* Add the name and index of your computation to the following table. */

struct weighter weighttab[] = {
  /* Add weighter entries here in the form:
    { name, index },
  */

  /*****************************  Examples ******************************/
  { "square", SQUARE },
  { "poly", POLY },
  /**********************************************************************/

  { "default", DEFAULT }
};

int psi_weight_index(char *name) {

  int n = sizeof(weighttab)/sizeof(struct weighter);

  while (n-- > 0) {
    if (strcmp(name, weighttab[n].name) == 0) {
      return weighttab[n].index;
    }
  }
  fprintf(stderr, "unrecognized weight computation name: '%s'.\n", name);
  return(0);
}

double psi_compute_bimolecular_weight(int method,
				      double rate, int sends, int receives,
				      int argn, double argv[])
{
  double result;

  switch (method) {

    case DEFAULT: {
      result = rate*sends*receives;
      break;
    }

  /* Add your own computation here in the form:

    case <index of named computation>: {
      result = <biomolecular computation>;
      break;
    }
  */

    /*****************************  Examples ******************************/

    case SQUARE: {
      result = rate*(sends*sends * receives*receives);
      break;
    }

    case POLY: {
      double send = sends, receive = receives, sum = sends + receives; 
      int i;
      for (i = 0; i < argn; i++) {
	send *= sends; receive *=receives;
	sum += argv[i]*(send + receive);
      }
      result = rate*sum;
      break;
    }
    /**********************************************************************/

    default : {
      fprintf(stderr, "invalid weight computation index: '%i'.\n", method);
      result = 0;
    }
  }
  return result;
}

double psi_compute_homodimerized_weight(int method, double rate, int dimers,
					int argn, double argv[])
{
  double result;

  switch (method) {

    case DEFAULT: {
      result = (rate*dimers*(dimers-1))/2;
      break;
    }
  /* Add your own computation here in the form:

    case <index of named computation>: {
      result = <homodimerized computation>;
      break;
    }
  */

    default : {
      fprintf(stderr, "invalid weight computation index: '%i'.\n", method);
      result = 0;
    }
  }
  return result;
}



