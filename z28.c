/*@z28.c:Error Service:ErrorInit(), ErrorSeen()@******************************/
/*                                                                           */
/*  LOUT: A HIGH-LEVEL LANGUAGE FOR DOCUMENT FORMATTING (VERSION 2.05)       */
/*  COPYRIGHT (C) 1993 Jeffrey H. Kingston                                   */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.su.oz.au)                                   */
/*  Basser Department of Computer Science                                    */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either version 1, or (at your option)      */
/*  any later version.                                                       */
/*                                                                           */
/*  This program is distributed in the hope that it will be useful,          */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*  GNU General Public License for more details.                             */
/*                                                                           */
/*  You should have received a copy of the GNU General Public License        */
/*  along with this program; if not, write to the Free Software              */
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                */
/*                                                                           */
/*  FILE:         z28.c                                                      */
/*  MODULE:       Error Service                                              */
/*  EXTERNS:      ErrorInit(), Error(), ErrorSeen()                          */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#define	MAX_BLOCKS	 20		/* max number of error blocks        */
#define	MAX_ERRORS	 20		/* max number of held error messages */

static BOOLEAN	print_block[MAX_BLOCKS];	/* TRUE if print this block  */
static int	start_block[MAX_BLOCKS];	/* first message of block    */
static char	message[MAX_ERRORS][MAX_LINE];	/* the error messages    */
static FILE	*fp = NULL;			/* file pointer of log file  */
static BOOLEAN	error_seen = FALSE;		/* TRUE after first error    */
static int	block_top = 0;			/* first free error block    */
static int	mess_top = 0;			/* first free error message  */


/*****************************************************************************/
/*                                                                           */
/*  ErrorInit(str)                                                           */
/*                                                                           */
/*  Open log file str and initialise this module.                            */
/*                                                                           */
/*****************************************************************************/

ErrorInit(str)
FULL_CHAR *str;
{ if( fp != NULL )
    Error(FATAL, no_fpos, "-e argument appears twice in command line");
  fp = StringFOpen(str, "w");
  if( fp == NULL )
    Error(FATAL, no_fpos, "cannot open error file \"%s\"", str);
} /* end ErrorInit */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN ErrorSeen()                                                      */
/*                                                                           */
/*  TRUE once an error has been found.                                       */
/*                                                                           */
/*****************************************************************************/

BOOLEAN ErrorSeen()
{ return error_seen;
} /* end ErrorSeen */


/*@::EnterErrorBlock(), LeaveErrorBlock()@************************************/
/*                                                                           */
/*  EnterErrorBlock(ok_to_print)                                             */
/*                                                                           */
/*  Start off a new block of error messages.  If ok_to_print, they do not    */
/*  need to be held for a later commit.                                      */
/*                                                                           */
/*****************************************************************************/

EnterErrorBlock(ok_to_print)
BOOLEAN ok_to_print;
{ if( block_top < MAX_BLOCKS )
  { print_block[block_top] = ok_to_print;
    start_block[block_top] = mess_top;
    block_top++;
  }
  else Error(FATAL, no_fpos, "too many levels of error messages");
} /* end EnterErrorBlock */


/*****************************************************************************/
/*                                                                           */
/*  LeaveErrorBlock(commit)                                                  */
/*                                                                           */
/*  Finish off a block or error messages.  If commit is true, print them,    */
/*  otherwise discard them.                                                  */
/*                                                                           */
/*****************************************************************************/

LeaveErrorBlock(commit)
BOOLEAN commit;
{ int i;
  assert( block_top > 0, "LeaveErrorBlock: no matching EnterErrorBlock!" );
  assert( commit || !print_block[block_top - 1], "LeaveErrorBlock: commit!" );
  if( fp == NULL )  fp = stderr;
  if( commit )
  { for( i = start_block[block_top - 1];  i < mess_top;  i++ )
      fputs(message[i], fp);
  }
  block_top--;
  mess_top = start_block[block_top];
} /* end LeaveErrorBlock */


/*@::Error()@*****************************************************************/
/*                                                                           */
/*  Error(etype, pos, str, p1, p2, p3, p4, p5, p6)                           */
/*                                                                           */
/*  Report error of type etype at position *pos in input.                    */
/*  The error message is str with parameters p1 - p6.                        */
/*                                                                           */
/*****************************************************************************/

/*VARARGS3*/
Error(etype, pos, str, p1, p2, p3, p4, p5, p6)
int etype;  FILE_POS *pos;  char *str, *p1, *p2, *p3, *p4, *p5, *p6;
{ char val[MAX_LINE];
  sprintf(val, str, p1, p2, p3, p4, p5, p6);
  if( fp == NULL )  fp = stderr;
  switch( etype )
  {

    case INTERN:
    
      while( block_top > 0 )  LeaveErrorBlock(TRUE);
      fprintf(fp, "lout%s internal error: %s\n", EchoFilePos(pos), val);
#if DEBUG_ON
      abort();
#else
      exit(1);
#endif
      break;


    case FATAL:
    
      while( block_top > 0 )  LeaveErrorBlock(TRUE);
      fprintf(fp, "lout%s fatal error: %s\n", EchoFilePos(pos), val);
      exit(1);
      break;


    case WARN:
    
      if( block_top == 0 || print_block[block_top - 1] )
	fprintf(fp, "lout%s: %s\n", EchoFilePos(pos), val);
      else if( mess_top < MAX_ERRORS )
	sprintf(message[mess_top++], "lout%s: %s\n", EchoFilePos(pos), val);
      else Error(FATAL, pos, "too many error messages");
      error_seen = TRUE;
      break;


    default:
    
      Error(INTERN, no_fpos, "invalid error type");
      exit(1);
      break;

  }
} /* end Error */
