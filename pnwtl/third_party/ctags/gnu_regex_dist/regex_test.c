/* name: test_regex.c
   purpose: Simple regex test harness. Also serves
    as a main console mode project into which a
	DLL subproject may be inserted when using the
	Microsoft Developer Studio VC++ compiler.
   usage: test_regex [optional-input-file]
   http://people.delphi.com/gjc/winregex.html
   Author: George J. Carrette, March 1997.
   Copyright 1997 by George J. Carrette.
   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "regex.h"

static FILE *infile = NULL;

static int prompt(char *prompt,char *data,size_t len,char *help)
{char *p;
 while(1) 
 {if (!infile) printf("%s: ",prompt);
  if (!fgets(data,len,(infile) ? infile : stdin)) return(0);
  if ((p = strchr(data,'\n'))) *p = 0;
  if ((*data == 0) || (*data == '#')) continue;
  if (infile) printf("%s: %s\n",prompt,data);
  if (strcmp(data,"quit") == 0) return(0);
  if (strcmp(data,"?") == 0)
   printf("%s\n",help);
  else
   return(1);}}

static double myclock(void)
{return(((double)clock()) / ((double) CLOCKS_PER_SEC));}

int main(int argc,char **argv)
{char pattern[128],line[128],errbuff[128];
 int error,n,j,k,nloop,acc_loop = 0;
 double before_t,after_t,diff_t,acc_t = 0;
 regex_t *r; regmatch_t *m;
 printf("Copyright 1997 by George J. Carrette.\n");
 printf("Regex test driver. For more info see:\n");
 printf("http://people.delphi.com/gjc/winregex.html\n");
 if ((argc > 1) && (argv[1][0]))
  {if (!(infile = fopen(argv[1],"r")))
    {perror(argv[1]);
     return(1);}}
 r = (regex_t *) malloc(sizeof(regex_t));
 if (prompt("nloop",pattern,sizeof(pattern),"default 1"))
  nloop = atol(pattern);
 if (nloop <= 0) nloop = 1;
 while(prompt("Pattern",pattern,sizeof(pattern),
	          "quit, or try ^([0-9]+)(\\-| |$)(.*)$"))
 {memset(r,0,sizeof(regex_t));
  if ((error = regcomp(r,pattern,REG_EXTENDED)))
   {regerror(error,r,errbuff,sizeof(errbuff));
    printf("regcomp: %s\n",errbuff);}
  else
   {printf("Compiled with %d nsub\n",r->re_nsub);
    n = r->re_nsub + 1;
    m = (regmatch_t *) malloc(sizeof(regmatch_t) * n);
	while(prompt("Data",line,sizeof(line),pattern))
	{before_t = myclock();
	 for(k=0;k<nloop;++k) error = regexec(r,line,n,m,0);
	 after_t = myclock();
	 diff_t = after_t - before_t;
	 acc_loop += nloop;
	 acc_t += diff_t;
	 printf("%d loops, %.3f seconds, %.1f micro-seconds per loop\n",
		    nloop,diff_t,
			diff_t * 1000000 / nloop);
     if (error)
	   {regerror(error,r,errbuff,sizeof(errbuff));
	    printf("regexec: %s\n",errbuff);}
	 else
      for(j=0;j<n;++j)
	   printf("%d[%d,%d] = %.*s\n",
		      j,m[j].rm_so,m[j].rm_eo,
			  (m[j].rm_so >= 0)
			  ? (m[j].rm_eo - m[j].rm_so) : 0,
			  (m[j].rm_so >= 0)
			  ? &line[m[j].rm_so] : "");}
    free(m);
    regfree(r);}}
 free(r);
 if (infile) fclose(infile);
 if (acc_loop)
  printf("%d total loops. %.1f seconds, %.1f micro-seconds per loop\n",
	     acc_loop,acc_t, acc_t * 100000 / acc_loop);
 return(0);}


