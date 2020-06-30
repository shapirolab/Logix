/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman, Aviv Regev
     Weizmann Institute of Science, Rehovot, Israel

** EFCP is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** EFCP is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with EFCP; if not, see:

       http://www.gnu.org/licenses

** or write to:



       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

#include <stdio.h>

#define DEFAULT 0

#define MICHAELIS 1
/******************************  Examples *******************************/
#define SQUARE 100
#define POLY   101
/***************************** my defines *******************************/
#define DELTA_DELAY 300
#define GAUSSIAN_DELAY 301
struct weighter {
  char *name;
  int index;
};

double exp(double exp);

/* Add the name and index of your computation to the following table. */

struct weighter weighttab[] = {
  /* Add weighter entries here in the form:
    { name, index },
  */

  {"michaelis", MICHAELIS},

  /*****************************  Examples ******************************/
  { "square", SQUARE },
  { "poly", POLY },
  /**********************************************************************/	
  { "default", DEFAULT },
   {"delta_delay",DELTA_DELAY},	
   {"gaussian_delay" , GAUSSIAN_DELAY}
};

int spi_weight_name(int index, char **name) {

  int n = sizeof(weighttab)/sizeof(struct weighter);

  while (n-- > 0) {
    if (weighttab[n].index == index) {
      *name = (char *)&weighttab[n].name;
      return 1;
    }
  }
  fprintf(stderr, "unrecognized weight computation index: %d.\n", index);
  return 0;
}
  

int spi_weight_index(char *name) {

  int n = sizeof(weighttab)/sizeof(struct weighter);

  while (n-- > 0) {
    if (strcmp(name, weighttab[n].name) == 0) {
      return weighttab[n].index;
    }
  }
  fprintf(stderr, "unrecognized weight computation name: '%s'.\n", name);
  return(0);
}

double spi_compute_bimolecular_weight(int method,
				      double rate, int sends, int receives,
				      int argn, double argv[])
{
  double result;
  double exp(double expression);

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

  /*
   * modeling a delay:
   * using a gaussian with a given width(gaussbreite)
   * plus an offset delay time
   * thus needing three arguments:
   * double currentTime
   * double delayTime
   * double sigma_square(variance)
   * function: rate*exp(-(currentTime - delayTime)^2/2* sigma_square)
   *
   * (note that the function is independant of the sends and receives.)
   */

    case GAUSSIAN_DELAY: {
      if(argn >= 3){
	double currentTime = argv[0];
	double delayTime   = argv[1];
	double variance    = argv[2];
	double delta = currentTime - delayTime;

	result = rate*exp(-((delta*delta/2)*variance));
	/* if result is 0, this channel cannot be chosen. */
	if(result <= 0) {
	  /* This is a kluge, which may be removed, if you don't mind
	     indefinite delays in choosing this channel */
	  result = rate;
	}
      }
      else
	spi_compute_bimolecular_weight(DEFAULT,rate,sends,receives,argn,argv);
      break;
    }

    /*
     * modeling an offset delay:
     * thus needing two arguments:
     * double currentTime
     * double delayTime
     * function:
     *   currentTime > delayTime ? result = rate*sends*receives:result= 0;
     */

    case DELTA_DELAY: {
      if(argn >= 2){
	double currentTime = argv[0];
	double delayTime   = argv[1];
	result = (currentTime > delayTime) ? rate*sends*receives : 0;
	}
      else
	spi_compute_bimolecular_weight(DEFAULT,rate,sends,receives,argn,argv);
      break;
    }	
    case MICHAELIS: {
      double div = receives + argv[0];
      result = (argn >= 1 && div > 0) ?
	(rate*(sends*receives))/(receives + argv[0]) :
	0;
      break;
    }
    /*****************************  Examples ******************************/

    case SQUARE: {
      result = rate*(sends*sends * receives*receives);
      break;
    }

    case POLY: {
      double send = sends, receive = receives, sum = sends + receives; 
      int i;
      for (i = 0; i < argn; i++) {
	send *= sends; receive *= receives;
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

double spi_compute_homodimerized_weight(int method, double rate, int dimers,
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
