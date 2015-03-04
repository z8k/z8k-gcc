/* Configuration for GNU C-compiler for Z8000
   Copyright (C) 1987 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR 8
#define HOST_BITS_PER_SHORT 16
#define HOST_BITS_PER_INT 16
#define HOST_BITS_PER_LONG 32
#define HOST_BITS_PER_LONGLONG 64

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#define USE_C_ALLOCA
#endif

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"


