/*@z31.c:Memory Allocator:DebugMemory()@**************************************/
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
/*  FILE:         z31.c                                                      */
/*  MODULE:       Memory Allocator                                           */
/*  EXTERNS:      DebugMemory(), zz_free[], MemInit(), GetMemory()           */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#define	MEM_CHUNK	1020		/* how many ALIGNs to get from sys   */


#if DEBUG_ON
static	int	no_of_calls	= 0;	/* number of calls to calloc()       */
	int	zz_newcount	= 0;	/* number of calls to New()          */
	int	zz_disposecount	= 0;	/* number of calls to Dispose()      */

/*****************************************************************************/
/*                                                                           */
/*  DebugMemory()                                                            */
/*                                                                           */
/*  Print memory usage.                                                      */
/*                                                                           */
/*****************************************************************************/

DebugMemory()
{ int i, j;  OBJECT p;
  debug2(DMA, D, "calloc called %d times (%d bytes total)",
    no_of_calls, no_of_calls * MEM_CHUNK * sizeof(ALIGN));
  debug2(DMA, D, "New() called %d times;  Dispose() called %d times",
    zz_newcount, zz_disposecount);
  for( i = 0;  i < MAX_OBJECT_REC;  i++ )
  { if( zz_free[i] != nil )
    { j = 0;
      for( p = zz_free[i];  p != nil;  p = pred(p, CHILD) )  j++;
      debug2(DMA, DD, "zz_free[%2d]: %3d", i, j);
    }
  }
} /* end DebugMemory */
#endif


/*@::zz_free[], zz_lengths[], MemInit()@**************************************/
/*                                                                           */
/*  OBJECT         zz_free[], zz_hold, zz_tmp, zz_res                        */
/*  int            zz_size                                                   */
/*  unsigned char  zz_lengths[]                                              */
/*                                                                           */
/*  zz_free[i]:    free records of size i*sizeof(ALIGN).                     */
/*  zz_lengths[i]: the number of ALIGNs in a record of type i.               */
/*  These variables are used only within the New() and Dispose() macros,     */
/*  and the list handling macros.                                            */
/*                                                                           */
/*****************************************************************************/

OBJECT		zz_free[MAX_OBJECT_REC], zz_hold, zz_tmp, zz_res;
int		zz_size;
unsigned char	zz_lengths[DISPOSED];		/* DISPOSED is 1 + max type */
OBJECT 		xx_link, xx_tmp, xx_res, xx_hold;


/*****************************************************************************/
/*                                                                           */
/*  MemInit()                                                                */
/*                                                                           */
/*  Initialise memory allocator.                                             */
/*                                                                           */
/*****************************************************************************/

MemInit()
{
  zz_lengths[ WORD        ] = 0;
  zz_lengths[ QWORD       ] = 0;
  zz_lengths[ LINK        ] = ceiling( sizeof(struct link_type), sizeof(ALIGN));

  /* object types, except closure NB have actual() field in token phase! */
  zz_lengths[ SPLIT       ] =
  zz_lengths[ HEAD        ] =
  zz_lengths[ PAR         ] =
  zz_lengths[ ROW_THR     ] =
  zz_lengths[ COL_THR     ] =
  zz_lengths[ CLOSURE     ] =
  zz_lengths[ NULL_CLOS   ] =
  zz_lengths[ CROSS       ] =
  zz_lengths[ ONE_COL     ] =
  zz_lengths[ ONE_ROW     ] =
  zz_lengths[ WIDE        ] =
  zz_lengths[ HIGH        ] =
  zz_lengths[ HSCALE      ] =
  zz_lengths[ VSCALE      ] =
  zz_lengths[ HCONTRACT   ] =
  zz_lengths[ VCONTRACT   ] =
  zz_lengths[ HEXPAND     ] =
  zz_lengths[ VEXPAND     ] =
  zz_lengths[ PADJUST     ] =
  zz_lengths[ HADJUST     ] =
  zz_lengths[ VADJUST     ] =
  zz_lengths[ ROTATE      ] =
  zz_lengths[ SCALE       ] =
  zz_lengths[ CASE        ] =
  zz_lengths[ YIELD       ] =
  zz_lengths[ XCHAR       ] =
  zz_lengths[ FONT        ] =
  zz_lengths[ SPACE       ] =
  zz_lengths[ BREAK       ] =
  zz_lengths[ NEXT        ] =
  zz_lengths[ ENV         ] =
  zz_lengths[ CLOS        ] =
  zz_lengths[ LVIS        ] =
  zz_lengths[ OPEN        ] =
  zz_lengths[ TAGGED      ] =
  zz_lengths[ INCGRAPHIC  ] =
  zz_lengths[ SINCGRAPHIC ] =
  zz_lengths[ GRAPHIC     ] =
  zz_lengths[ ACAT        ] =
  zz_lengths[ HCAT        ] =
  zz_lengths[ VCAT        ] =
  zz_lengths[ LBR         ] =
  zz_lengths[ RBR         ] =
  zz_lengths[ BEGIN       ] =
  zz_lengths[ END         ] =
  zz_lengths[ USE         ] =
  zz_lengths[ PREPEND     ] =
  zz_lengths[ SYS_PREPEND ] =
  zz_lengths[ DATABASE    ] =
  zz_lengths[ SYS_DATABASE] =
  zz_lengths[ GSTUB_NONE  ] =
  zz_lengths[ GSTUB_INT   ] =
  zz_lengths[ GSTUB_EXT   ] =
  zz_lengths[ DEAD        ] =
  zz_lengths[ UNATTACHED  ] =
  zz_lengths[ RECEPTIVE   ] =
  zz_lengths[ RECEIVING   ] =
  zz_lengths[ RECURSIVE   ] =
  zz_lengths[ PRECEDES    ] =
  zz_lengths[ FOLLOWS     ] =
  zz_lengths[ CROSS_FOLL  ] =
  zz_lengths[ GALL_FOLL   ] =
  zz_lengths[ CROSS_TARG  ] =
  zz_lengths[ GALL_TARG   ] =
  zz_lengths[ GALL_PREC   ] =
  zz_lengths[ CROSS_PREC  ] =
  zz_lengths[ EXPAND_IND  ] =
  zz_lengths[ THREAD      ] =
  zz_lengths[ CR_LIST     ] =
	ceiling(sizeof(struct closure_type), sizeof(ALIGN));

  /* symbol types */
  zz_lengths[ MACRO       ] =
  zz_lengths[ LOCAL       ] =
  zz_lengths[ LPAR        ] =
  zz_lengths[ RPAR        ] =
  zz_lengths[ NPAR        ] =
	ceiling(sizeof(struct symbol_type), sizeof(ALIGN));

  /* gap objects */
  zz_lengths[ TSPACE      ] =
  zz_lengths[ TJUXTA      ] =
  zz_lengths[ GAP_OBJ     ] =
	ceiling(sizeof(struct gapobj_type), sizeof(ALIGN));

  /* cross-reference and data base types */
  zz_lengths[ CROSS_SYM   ] =
  zz_lengths[ CR_ROOT     ] = ceiling(sizeof(struct cr_type) , sizeof(ALIGN));

  /* external galley record */
  zz_lengths[ EXT_GALL  ] = ceiling(sizeof(struct ext_gall_type),sizeof(ALIGN));

} /* end MemInit() */


/*@::GetMemory()@*************************************************************/
/*                                                                           */
/*  OBJECT GetMemory(siz, pos)                                               */
/*                                                                           */
/*  Return a pointer to siz ALIGNs of memory (0 < siz < MAX_OBJECT_REC).     */
/*                                                                           */
/*****************************************************************************/

OBJECT GetMemory(siz, pos)
int siz;  FILE_POS *pos;
{ static ALIGN *next_free = (ALIGN *) nil;
  static ALIGN *top_free  = (ALIGN *) nil;
  OBJECT res;
  char *calloc();

  debug1(DMA, DDD, "GetMemory( %d )", siz);

  /* get memory from operating system, if not enough left here */
  if( &next_free[siz] > top_free )
  { next_free = (ALIGN *) calloc(MEM_CHUNK, sizeof(ALIGN));
    ifdebug(DMA, D, no_of_calls++; )
    if( next_free == NULL ) Error(FATAL,pos,"run out of memory - exiting now");
    top_free = &next_free[MEM_CHUNK];
    debug2(DMA, D, "GetMemory: calloc returned %d - %d",
      (int) next_free, (int) top_free);
  }

  res = (OBJECT) next_free;
  next_free = &next_free[siz];
  debug3(DMA, DDD, "GetMemory returning @%d (next_free = @%d, top_free = @%d",
    (int) res, (int) next_free, (int) top_free);
  return res;
} /* end GetMemory */
