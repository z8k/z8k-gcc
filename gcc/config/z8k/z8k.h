/* Definitions of target machine for GNU compiler, for Zilog Z8000
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com)

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

#ifndef GCC_Z8K_H
#define GCC_Z8K_H

#define TARGET_Z8000
#define TARGET_SEGMENTED 1

/* Names to predefine in the preprocessor for this target machine.  */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("z8000");		\
    }						\
  while (0)

/* Write out defines which depend on the compilation options */

#define SIZE_TYPE    (TARGET_BIG ? "long unsigned int" : "unsigned int")
#define PTRDIFF_TYPE (TARGET_BIG ? "long int" : "int")

#define CPP_SPEC \
  "%{mz8001:-D__Z8001__  -D__PTRDIFF_TYPE__=long\\ int} 	\
   %{!mz8001:-D__Z8002__ -D__PTRDIFF_TYPE__=int}		\
   %{mz8001:-D__SIZE_TYPE__=long\\ unsigned\\ int} 		\
   %{!mz8001:-D__SIZE_TYPE__=unsigned\\ int} 			\
   %{!mint32:-D__INT_MAX__=32767} %{mstd:-D__STD_CALL__} %{mint32:-D__INT_MAX__=2147483647}"

#define LINK_SPEC "%{mz8001:-m z8001}"

/* Fetch the libraries from the right place */
#define LIB_SPEC "-lc"

/* Turn on listing when asked */

#define ASM_SPEC "%{mlist:-ahld}"



#define TARGET_DEFAULT \
    (TARGET_REGPARMS|TARGET_STRUCT_BYTE_ALIGN|TARGET_TYPE64|TARGET_TYPED64)
    
/* -m options which take arguments */
/*
char *call_used_option;
char *args_in_option;
char *args_mlist;
char *fakes_option;
*/
/* #define TARGET_OPTIONS { { "call-used-",&call_used_option},\
			 { "args-in", &args_in_option}, \
			 { "fakes-", &fakes_option}, \
			 { "list", &args_mlist}}
*/


/* Do any checking or such that is needed after processing the -m
   switches.  */
/* #define OVERRIDE_OPTIONS override_options() */

/* Define this to change the optimizations performed by default.  */

/* #define OPTIMIZATION_OPTIONS(LEVEL)	\
{					\
  if ((LEVEL) >= 1)			\
    {					\
      flag_force_mem = 1;		\
      flag_omit_frame_pointer = 1;	\
    }					\
}
*/


/* Target machine storage layout */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
/* #define REAL_ARITHMETIC */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.

   There are no such instructions on the z8000, but the documentation
   is little endian for bits.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.
   This is true on the z8000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.

   For the z8000 this is true */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 16

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 2

/* Define the size of `short' */
#define SHORT_TYPE_SIZE 16

/* Define the size of `int'. */
#define INT_TYPE_SIZE (TARGET_INT32 ? 32 : 16)

/* Define the size of `long'. */
#define LONG_TYPE_SIZE 32

/* Define the size of `long long' */ 
#define LONG_LONG_TYPE_SIZE 	 (TARGET_TYPE64 ? 64 : 32)

/* We only support one size of floating-point number, both `float' and
   `double' are 32 bits long */

#define FLOAT_TYPE_SIZE 	 32
#define DOUBLE_TYPE_SIZE 	 (TARGET_TYPED64 ? 64 : 32)
#define LONG_DOUBLE_TYPE_SIZE    64 

#define WCHAR_TYPE "short int"
#define WCHAR_TYPE_SIZE 16

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE (TARGET_BIG ? 32: 16)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 16

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY (TARGET_STRUCT_BYTE_ALIGN ? 8 : 16)

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS (TARGET_PCCBITF)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 16

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.
 */

#define STRICT_ALIGNMENT 1


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   We define 16 integer registers, ignoring the addressable high bytes
   of r0..r7. 

   The 17th register is the fake arg pointer which has gone by code
   emission time, and the 18th register is the fake return address
   pointer which has to be gone by code emission time.
*/

#define FIRST_PSEUDO_REGISTER 19

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   In small mode we pretend that the stack pointer is really in r14,
   and renumber the r14 and r15 on output.  This allows us to leave it
   alone when we switch between z8001 and z8002 modes, but still gets
   the alignment right when in small mode */

#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* This is filled in in z8k.c since it changes depending upon various
   -t flags */
#define CALL_USED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (((REGNO)==ARG_POINTER_REGNUM) ? 1 :((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE. */

extern int hard_regno_mode_ok[FIRST_PSEUDO_REGISTER];
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
   ((hard_regno_mode_ok[REGNO] & (1<<(int)(MODE))) != 0)


/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2)  1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* z8000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 10

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM  16

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM 18

/* Register in which static-chain is passed to a function.

   For the z8000 this hasn't been tested */
#define STATIC_CHAIN_REGNUM 0

/* Register in which address to store a structure value
   arrives in the function.  On the z8000, the address is passed
   as a hidden argument.  */
#define STRUCT_VALUE 0

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

/* ??? If we need two DFmode/DImode reloads, then they must go into
   2/3/4/5 and 6/7/8/9.  If the first goes into 4/5/6/7 we lose.
   We put 4 early, so that it will be more likely to be used for
   register allocation, and thus less likely to be used for reloads.  */
#define REG_ALLOC_ORDER { 4,5,2,3,0,1,6,7,8,9,10,11,12,13,14,15,16,17}

#define GENERAL_REGS ALL_REGS
enum reg_class { NO_REGS, SQI_REGS, QI_REGS,NOTQI_REGS, SP_REGS, PTR_REGS, SP_QI_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES				\
 {"NO_REGS", "SQI_REGS", "QI_REGS", "NOTQI_REGS", "SP_REGS", "PTR_REGS", "SP_QI_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS		\
{	{ 0x00000 }, /* NO_REGS  */ 	\
	{ 0x000fe }, /* SQI_REGS  */ 	\
	{ 0x000ff }, /* QI_REGS  */ 	\
      	{ 0x0c000 }, /* SP_REGS  */ 	\
	{ 0x0ff00 }, /* NOTQI_REGS  */ 	\
      	{ 0x1fffe }, /* PTR_REGS */	 	\
	{ 0x0c0ff }, /* SP_QI_REGS */	\
        { 0x1ffff }}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
    (((REGNO) == 14 || (REGNO) == 15) ? SP_REGS :  \
     (REGNO >= 8 && REGNO <= 13) ? NOTQI_REGS :  \
     (REGNO > 15) ? PTR_REGS :  \
     (REGNO == 0) ? QI_REGS :  \
     SQI_REGS) 

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  PTR_REGS
#define BASE_REG_CLASS   PTR_REGS

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers. */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P hook_bool_mode_true


#define POWER_OF_2(I) ((I) && POWER_OF_2_or_0(I))
#define POWER_OF_2_or_0(I) (((I) & ((unsigned)(I) - 1)) == 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   On the z8k, it's better to have characters in the QI_REGS. */

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
   ((GET_MODE(X) == QImode) ? QI_REGS : CLASS)


/* Place additional restrictions on the register class to use when it
   is necessary to be able to hold a value of mode MODE in a reload
   register for which class CLASS would ordinarily be used. */

#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  ((MODE) == QImode && ((CLASS) == GENERAL_REGS) \
   ? QI_REGS : (CLASS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)				\
	 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Define the cost of moving between registers of various classes. 
   
   moving between QI_REGS and anything else may be expensive since the
   other regs don't do QI mode too well */

#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)  ((CLASS1) == (CLASS2) ? 2: 5) 

/* A C expressions returning the cost of moving data of MODE from a register to
   or from memory. */

/*#define MEMORY_MOVE_COST(MODE)  6*/

/* Provide the cost of a branch.  Exact meaning under development.  */
/*#define BRANCH_COST 5*/


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */

#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.

   On the z8k, we always push words */

#define PUSH_ROUNDING(X) (((X) + 1) & ~1)

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */

/*#define ACCUMULATE_OUTGOING_ARGS*/


/* Offset of first parameter from the argument pointer register value.
   */



#define ELIMINABLE_REGS \
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},			\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { RETURN_ADDRESS_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}


#define TARGET_CAN_ELIMINATE z8k_can_eliminate


/* On the z8k, the first parm is offset by the saved return address
   and the frame pointer - but we use eliminable regs so it should be 0*/

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */


#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) OFFSET=io(FROM,TO);



/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On z8k, this is a single integer, which is a number of words
   of arguments scanned so far. */


#define CUMULATIVE_ARGS  int 


/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.

*/

#define NPARM_REGS 6

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (CUM) = 7 ;


/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

*/

#define TARGET_FUNCTION_ARG z8k_function_arg




extern char arg_regs[FIRST_PSEUDO_REGISTER];
#define FUNCTION_ARG_REGNO_P(N) arg_regs[N]

#define TARGET_FUNCTION_ARG_ADVANCE z8k_function_arg_advance

/* Round a register number down to a proper boundary for an arg of mode 
   MODE. 
   
   We round down to an even reg for things larger than a word */

#define ROUND_REG(X, MODE) 					\
  ((TARGET_ALIGN_DOUBLE 					\
   && GET_MODE_UNIT_SIZE ((MODE)) > UNITS_PER_WORD) 		\
   ? ((X) - ((X) & 1)) : (X))

#define ROUND_ADVANCE(SIZE)	\
  ((SIZE + UNITS_PER_WORD - 1) / UNITS_PER_WORD)


/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define TARGET_FUNCTION_PROLOGUE(FILE, SIZE) ; 


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)


/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define TARGET_FUNCTION_EPILOGUE(FILE, SIZE)	;


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   big model

   1              		.z8001
   2 0000 14001234 		ldl	rr0,#0x12345678
   2      5678
   3 0006 5E008000 		jp	t,foo
   3      0000
   4 000c 0000     		

  small model

   1              		.z8002
   2 0000 21001234 		ld	r0,#0x1234
   3 0004 5E000000 		jp	t,foo
   4 0008 0000     		


 */

#define TARGET_ASM_TRAMPOLINE_TEMPLATE z8k_asm_trampoline_template

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE    (TARGET_BIG ? 12 : 8)


/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  We assume
   here that a function will be called many more times than its address
   is taken (e.g., it might be passed to qsort), so we take the trouble
   to initialize the "hint" field in the JMP insn.  */

#define TARGET_TRAMPOLINE_INIT z8k_trampoline_init

#define RETURN_ADDR_RTX(count, frame)						\
  ((count == 0)									\
   ? gen_rtx_MEM (Pmode, gen_rtx_REG (Pmode, RETURN_ADDRESS_POINTER_REGNUM))	\
   : (rtx) 0)

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) z8k_regno_ok_for_index_p (REGNO, true)


/* It's ok for a base reg if it's in a hard reg which is not 0 or it
   will be renumbered on the way out and its not -1 or 0 */

#define REGNO_OK_FOR_BASE_P(REGNO) z8k_regno_ok_for_base_p (REGNO, true)
 

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1


/* Recognize any constant value that is a valid address.

   For the z8k, any constant is a valid address */

#define CONSTANT_ADDRESS_P(X)   \
    (CONSTANT_P (X) && GET_CODE (X) != CONST_DOUBLE)

/* Include all constant integers and constant doubles, but not
   floating-point, except for floating-point zero.  */

#define TARGET_LEGITIMATE_CONSTANT_P z8k_legitimate_constant_p



/* true if the value may be used as a displacement (-32768<=x<=32767)

   (can also be a displacement if it's a symbol and we're in small
   mode */

#define DISP_P(X) disp_p(X)


/* Define this if some processing needs to be done immediately before
   emitting code for an insn.  */

/* We use when asked to print out how big the compiler thought the
   instructions were */

#define FINAL_PRESCAN_INSN(INSN,OPERANDS,NOPERANDS)            \
{							       \
/*  static int old = 0;					       \
  extern int *insn_addresses;				       \
  int uid = INSN_UID (INSN);				       \
  if (TARGET_ISIZE && insn_addresses)			       \
  {							       \
    fprintf (asm_out_file, "! %d %d\n", insn_addresses[uid],   \
	     insn_addresses[uid] - old);		       \
    old = insn_addresses[uid];				       \
  }							       \
*/}							       \
							        


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode 

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE  1

/* Specify the tree operation to be used to convert reals to integers.  */
/* #define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR */

/* This is the kind of divide that is easiest to do in the general case.  */
/* #define EASY_DIV_EXPR TRUNC_DIV_EXPR */

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.

   We actually lie a bit here as overflow conditions are different.  But
   they aren't being checked anyway.  */

/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* Max number of bytes we can move to or from memory
   in one reasonably fast instruction.  */

#define MOVE_MAX 4

/* Largest number of bytes of an object that can be placed in a register.
 */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (SImode)

/* Nonzero if access to memory by bytes is no faster than for words.
   Also non-zero if doing byte operations (specifically shifts) in registers
   is undesirable.

*/

#define SLOW_BYTE_ACCESS	1

/* Define if normal loads of shorter-than-word items from memory clears
   the rest of the bits in the register.  */
/*#define BYTE_LOADS_ZERO_EXTEND  */

/* Define if normal loads of shorter-than-word items from memory sign-extends
   the rest of the bits in the register.  */
/*#define BYTE_LOADS_SIGN_EXTEND*/

#define SDB_DEBUGGING_INFO 

/* Output DBX (stabs) debugging information if using -gstabs.  */

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Don't try to use the `x' type-cross-reference character in DBX data.
   Also has the consequence of putting each struct, union or enum
   into a separate .stabs, containing only cross-refs to the others.  */
#define DBX_NO_XREFS

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Define the value returned by a floating-point comparison instruction.  */

/*#define FLOAT_STORE_FLAG_VALUE 0.5*/

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */


#define Pmode (TARGET_BIG ? PSImode : HImode)
#define Imode HImode
/* Mode of a function address in a call instruction (for indexing purposes). */

#define FUNCTION_MODE QImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap. */
#define NO_FUNCTION_CSE

/* Define this if shift instructions ignore all but the low-order
   few bits. */
/*#define SHIFT_COUNT_TRUNCATED*/

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.

   We only care about the cost if it is valid in an insn, so all constants
   are cheap.  */
/*
#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
       if (CONST_OK_FOR_LETTER_P(INTVAL(RTX), 'I')) return 0;   \
       if (CONST_OK_FOR_LETTER_P(INTVAL(RTX), 'L')) return 1;   \
   return 2;                                                    \
  case CONST_DOUBLE:						\
  return 8;							\
  case SYMBOL_REF:						\
  case LABEL_REF:						\
    return 30;							\
  case CONST:  							\
    return 2;
*/

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */
/*
#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case PLUS:						\
  case MINUS:						\
    break;						\
  case MULT:						\
  return COSTS_N_INSNS (10);				\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
      return COSTS_N_INSNS (TARGET_FAST ? 17 : 2);	
*/

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

/* Z8000 CPU Section Name */
#define SECTION_NAME (z8k_sect.sect_name != 0 && strlen(z8k_sect.sect_name) > 0	\
                      ? z8k_sect.sect_name : "not")

  
/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

#define TEXT_SECTION_ASM_OP 		"\t.text"
#define CTORS_SECTION_ASM_OP 		"\t.section\t.ctors"
#define DTORS_SECTION_ASM_OP 		"\t.section\t.dtors"
#define READONLY_DATA_SECTION_ASM_OP  	"\tsect .rdata"
#define INIT_SECTION_ASM_OP 		"\t.init"
/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

#define TARGET_HAVE_NAMED_SECTIONS false

/* Define an extra section for read-only data, a routine to enter it, and
   indicate that it is for read-only data.  */
/*
#define EXTRA_SECTIONS	readonly_data, in_ctors, in_dtors

#define EXTRA_SECTION_FUNCTIONS					\
void							     \
ctors_section() 					     \
{							     \
  if (in_section != in_ctors)				     \
    {							     \
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);  \
      in_section = in_ctors;				     \
    }							     \
}							     \
void							     \
dtors_section() 					     \
{							     \
  if (in_section != in_dtors)				     \
    {							     \
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);  \
      in_section = in_dtors;				     \
    }							     \
}								\
void								\
literal_section ()						\
{								\
  if (in_section != readonly_data)				\
    {								\
      fprintf (asm_out_file, "%s\n", READONLY_DATA_SECTION_ASM_OP); \
      in_section = readonly_data;				\
    }								\
}								\

#define READONLY_DATA_SECTION	literal_section
*/

#define ASM_OUTPUT_PTR(FILE,NAME) \
 do { fprintf (FILE, "\t%s\t_%s\n", TARGET_BIG ? "lval" : "wval", NAME);} while(0)
/*
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
   do { ctors_section();  ASM_OUTPUT_PTR(FILE, NAME); } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)	\
   do {  dtors_section(); ASM_OUTPUT_PTR(FILE, NAME); } while (0)
*/
/*
#undef DO_GLOBAL_CTORS_BODY                     
#define DO_GLOBAL_CTORS_BODY			\
{						\
  typedef (*pfunc)();				\
  extern pfunc __ctors[];			\
  extern pfunc __ctors_end[];			\
  pfunc *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}						

#undef DO_GLOBAL_DTORS_BODY			 
#define DO_GLOBAL_DTORS_BODY                    \
{						\
  typedef (*pfunc)();				\
  extern pfunc __dtors[];			\
  extern pfunc __dtors_end[];			\
  pfunc *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}						 
*/


/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES						\
{ "r0","r1","r2","r3","r4","r5","r6","r7", 			\
   "r8","r9","r10","r11","r12","r13","r14","r15","apl","aph","rap"}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define TARGET_ASM_GLOBALIZE_LABEL z8k_asm_globalize_label

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */
/*
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
    fprintf (FILE, "E%s%d:\n", PREFIX, NUM)
*/
/* This is how to output a label for a jump table.  Arguments are the same as
   for ASM_OUTPUT_INTERNAL_LABEL, except the insn for the jump table is
   passed. */

/*#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 2); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }*/

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */


#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*E%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf (FILE, "\t.double %s\n", dstr);		\
   } while (0)


/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf (FILE, "\t.float %s\n", dstr);		\
   } while (0)


/* This is how to output an assembler line defining an 4 byte constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\tlval "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\twval "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)		\
( fprintf (FILE, "\tbval "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* We use the default ASCII-output routine, except that we don't write more
   than 50 characters since the assembler doesn't support very long lines.  */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to assemble a string constant containing the LEN bytes at PTR. */
#define ASM_OUTPUT_ASCII(FILE, P, SIZE)				\
  asm_output_ascii (FILE, P, SIZE)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */


#define ASM_OUTPUT_REG_PUSH(FILE,REGNO) \
 fprintf (FILE, "\tpushl %s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO) \
 fprintf (FILE, "\tmovl (sp)+,%s\n", reg_names[REGNO])


/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\tbval %d\n", (VALUE))

/* This is how to output an element of a case-vector that is absolute.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t%s EL%d\n", TARGET_BIG ? "lval":"wval", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (Alpha does not use such vectors, but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\twval EL%d-EL%d\n", VALUE, REL)

#define ASM_OUTPUT_OPCODE(f,s) s = z8k_asm_output_opcode(f,s)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */


#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  if (LOG==1) fprintf(FILE,"\teven\n"); \
    else fprintf (FILE, "\t.align %d\n", (LOG))

/* This is how to advance the location counter by SIZE bytes.  */
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tblock %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
   asm_output_common (FILE, NAME, SIZE, ROUNDED)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
   asm_output_local (FILE, NAME, SIZE, ROUNDED)

/* This says how to output an assembler line to start an
   object definition.  */

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)	\
   asm_output_name (FILE, NAME)

/* This says how to output an assembler line to start a
   function definition.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
   z8k_declare_function_name (FILE, NAME, DECL);

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s__%d", (NAME), (LABELNO)))


/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

/* Determine which codes are valid without a following integer.  These must
   not be alphabetic.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '^')

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address(FILE,	ADDR)

#define NOTICE_UPDATE_CC(exp, insn) z8k_notice_update_cc (exp, insn)


/* A C statement to output DBX or SDB debugging information before code for line
   number LINE of the current source file to the stdio stream STREAM. */

/* ??? stabs-in-coff will not work until this conflict is resolved.  */
/*#undef DBX_DEBUGGING_INFO
#undef ASM_OUTPUT_SOURCE_LINE

#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE)  				\
  (fprintf (file, "\t.ln\t%d\n", ((sdb_begin_function_line > -1) 	\
				  ? (LINE) - sdb_begin_function_line : 1)));
*/

  
/* A C statement to output DBX or SDB debugging information which indicates
   that filename NAME is the current source file to the stdio stream STREAM. */
/*
#define ASM_OUTPUT_SOURCE_FILENAME(STREAM,NAME)			\
  { char *quote = "\"";			\
    fprintf (STREAM, "	name	%s%s%s\n", quote, NAME, quote);	\
  }
*/


/* STUFF NEEDED TO DISABLE DEBUG OUTPUT */

#define DP if (!TARGET_DEFS) 

#define PUT_SDB_SCL(a)     DP fprintf(asm_out_file, "\t.scl\t%d%s", (a), SDB_DELIM)
#define PUT_SDB_INT_VAL(a) DP fprintf (asm_out_file, "\t.val\t%d%s", (a), SDB_DELIM)

#define PUT_SDB_VAL(a)	DP			\
( fputs ("\t.val\t", asm_out_file),		\
  output_addr_const (asm_out_file, (a)),	\
  fprintf (asm_out_file, SDB_DELIM))

#define PUT_SDB_DEF(a)	\
do { DP	 { fprintf (asm_out_file, "\t.def\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, a); 	\
     fprintf (asm_out_file, SDB_DELIM); } } while (0)

#define PUT_SDB_PLAIN_DEF(a) DP fprintf(asm_out_file,"\t.def\t.%s%s",a, SDB_DELIM)
#define PUT_SDB_ENDEF        DP fputs("\t.endef\n", asm_out_file)
#define PUT_SDB_TYPE(a)      DP fprintf(asm_out_file, "\t.type\t0%o%s", a, SDB_DELIM)
#define PUT_SDB_SIZE(a)      DP fprintf(asm_out_file, "\t.size\t%d%s", a, SDB_DELIM)
#define PUT_SDB_START_DIM    DP fprintf(asm_out_file, "\t.dim\t")
#define PUT_SDB_NEXT_DIM(a)  DP fprintf(asm_out_file, "%d,", a)
#define PUT_SDB_LAST_DIM(a)  DP fprintf(asm_out_file, "%d%s", a, SDB_DELIM)
#define PUT_SDB_TAG(a)	     DP			\
  do { fprintf (asm_out_file, "\t.tag\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, a);	\
     fprintf (asm_out_file, SDB_DELIM); } while (0)

#define PUT_SDB_BLOCK_START(LINE) DP		\
  fprintf (asm_out_file,			\
	   "\t.def\t.bb%s\t.val\t.%s\t.scl\t100%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_BLOCK_END(LINE)	 DP		\
  fprintf (asm_out_file,			\
	   "\t.def\t.eb%s\t.val\t.%s\t.scl\t100%s\t.line\t%d%s\t.endef\n",  \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_START(LINE) DP		\
  fprintf (asm_out_file,			\
	   "\t.def\t.bf%s\t.val\t.%s\t.scl\t101%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_END(LINE)	DP	\
  fprintf (asm_out_file,			\
	   "\t.def\t.ef%s\t.val\t.%s\t.scl\t101%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_EPILOGUE_END(NAME) 			\
do { DP { fprintf (asm_out_file, "\t.def\t");		\
     ASM_OUTPUT_LABELREF (asm_out_file, NAME);		\
     fprintf (asm_out_file,				\
	      "%s\t.val\t.%s\t.scl\t-1%s\t.endef\n",	\
	      SDB_DELIM, SDB_DELIM, SDB_DELIM); } } while (0)

#endif /* GCC_Z8K_H */
