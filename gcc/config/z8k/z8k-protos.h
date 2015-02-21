/* Function declarations for the Z8000 for GNU C compiler

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
   <http://www.gnu.org/licenses/>.
*/

extern int saved_reg_on_stack_hack;



int moveok (rtx *, enum machine_mode);
int null_epilogue ();
int COM_POWER_OF_2 (int);
int emit_move (rtx [], enum machine_mode, int);
void z8k_expand_prologue ();
void z8k_expand_epilogue ();

const char * output_move64 (rtx, rtx);

void maybe_need_resflg (rtx);

void asm_output_ascii (FILE *, const char *, int);
const char * z8k_asm_output_opcode(FILE *, const char *);

int data_ref_p (rtx);
bool bx_p (rtx, bool);
bool ba_p (rtx, bool);
bool x_p (rtx, bool);

bool inside_ba_p (rtx, bool);
bool inside_bx_p (rtx, bool);
bool inside_x_p (rtx, bool);
int io (int, int);

void z8k_output_external (const char *);
void z8k_declare_function_name (FILE *, const char *, tree);

void print_operand (FILE *, rtx, int);
void print_operand_address (FILE *, rtx);

enum reg_class secondary_reload_class (enum reg_class, enum machine_mode, rtx);



void asm_output_local (FILE *, const char *, int, int);
void asm_output_common (FILE *, const char *, int, int);
void asm_output_name (FILE *, const char *);

int load_source_file (const char *);



int BADSUBREG (rtx);
