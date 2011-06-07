/*********************/
/* errmsg.h          */
/* for Par 1.52      */
/* Copyright 2001 by */
/* Adam M. Costello  */
/*********************/

/* This is ANSI C code (C89). */


#ifndef ERRMSG_H
#define ERRMSG_H


#define errmsg_size 163

/* This is the maximum number of characters that will  */
/* fit in an errmsg_t, including the terminating '\0'. */
/* It will never decrease, but may increase in future  */
/* versions of this header file.                       */


typedef char errmsg_t[errmsg_size];

/* Any function which takes the argument errmsg_t errmsg must, before */
/* returning, either set errmsg[0] to '\0' (indicating success), or   */
/* write an error message string into errmsg, (indicating failure),   */
/* being careful not to overrun the space.                            */


extern const char * const outofmem;
  /* "Out of memory.\n" */

extern const char * const impossibility;
  /* "Impossibility #%d has occurred.  Please report it.\n" */


#endif
