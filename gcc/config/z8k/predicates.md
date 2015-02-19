;; Predicate definitions for Z8000
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


(define_predicate "smallint_operand" 
  (and (match_code "const_int")
       (ior (match_test "satisfies_constraint_J (op)")
            (match_test "satisfies_constraint_K (op)"))))

(define_predicate "immediate15_operand"
  (and (match_code "const_int")
       (match_test "! (INTVAL (op) & 0x8000)")))

(define_predicate "power_two_operand"
  (and (match_code "const_int")
       (match_test "POWER_OF_2 (INTVAL (op))")))

(define_predicate "com_power_two_operand"
  (and (match_code "const_int")
       (match_test "COM_POWER_OF_2 (INTVAL (op))")))

(define_predicate "symbol_ref"
  (match_code "symbol_ref"))

(define_predicate "not_subreg_register_operand"
  (match_code "reg"))



(define_predicate "r_operand"
  (and (match_operand 0 "register_operand")
       (match_test "! BADSUBREG (op)")))

(define_predicate "im_operand"
  (match_operand 0 "immediate_operand"))

(define_predicate "ir_operand"
  (and (match_code "mem")
       (match_test "! BADSUBREG (op)")
       (match_test "IR_P (op)")))

(define_predicate "da_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return DA_P (op);
})

;; 16 bit Reg  + pointer sized bit index
(define_predicate "x_operand"
  (and (match_operand 0 "register_operand")
       (match_test "! BADSUBREG (op)"))
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return X_P (op);
})

(define_predicate "ba_operand"
  (and (match_operand 0 "register_operand")
       (match_test "! BADSUBREG (op)"))
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (ba_p (op, 0))
    return 1;
  return 0;
})

(define_predicate "bx_operand"
  (and (match_operand 0 "register_operand")
       (match_test "! BADSUBREG (op)"))
{
  if (bx_p (op, 0))
    return 1;
  return 0;
})

(define_predicate "poi_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);
  if (GET_CODE (op) != POST_INC)
    return 0;
  return register_operand (XEXP (op, 0), Pmode);
})

(define_predicate "prd_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);
  if (GET_CODE (op) != PRE_DEC)
    return 0;
  return register_operand (XEXP (op, 0), Pmode);
})

(define_predicate "ir_da_x_ba_bx_operand"
  (ior (match_operand 0 "ir_operand")
       (match_operand 0 "da_operand")
       (match_operand 0 "x_operand")
       (match_operand 0 "ba_operand")
       (match_operand 0 "bx_operand")))

(define_predicate "r_da_operand"
  (ior (match_operand 0 "r_operand")
       (match_operand 0 "da_operand")))

(define_predicate "r_da_x_operand"
  (ior (match_operand 0 "r_da_operand")
       (match_operand 0 "x_operand")))

(define_predicate "ir_da_x_operand"
  (ior (match_operand 0 "ir_operand")
       (match_operand 0 "da_operand")
       (match_operand 0 "x_operand")))

(define_predicate "r_ir_da_operand"
  (ior (match_operand 0 "r_da_operand")
       (match_operand 0 "ir_operand")))

(define_predicate "r_ir_da_x_operand"
  (ior (match_operand 0 "r_operand")
       (match_operand 0 "ir_da_x_operand")))

(define_predicate "r_ir_da_x_ba_bx_operand"
  (ior (match_operand 0 "r_ir_da_x_operand")
       (match_operand 0 "ba_operand")
       (match_operand 0 "bx_operand")))

(define_predicate "r_ir_da_x_ba_bx_poi_operand"
  (ior (match_operand 0 "r_ir_da_x_ba_bx_operand")
       (match_operand 0 "poi_operand")))

(define_predicate "r_ir_da_x_ba_bx_prd_operand"
  (ior (match_operand 0 "r_ir_da_x_ba_bx_operand")
       (match_operand 0 "prd_operand")))

(define_predicate "r_im_operand"
  (ior (match_operand 0 "r_operand")
       (match_operand 0 "im_operand")))

(define_predicate "r_im_ir_operand"
  (ior (match_operand 0 "r_im_operand")
       (match_operand 0 "ir_operand")))

(define_predicate "r_im_ir_da_operand"
  (ior (match_operand 0 "r_im_ir_operand")
       (match_operand 0 "da_operand")))

(define_predicate "r_im_ir_da_x_operand"
  (ior (match_operand 0 "r_im_ir_da_operand")
       (match_operand 0 "x_operand")))

(define_predicate "r_im_ir_da_x_ba_operand"
  (ior (match_operand 0 "r_im_ir_da_x_operand")
       (match_operand 0 "ba_operand")))

(define_predicate "r_im_ir_da_x_ba_bx_operand"
  (ior (match_operand 0 "r_im_ir_da_x_ba_operand")
       (match_operand 0 "bx_operand")))

(define_predicate "r_im_ir_da_x_ba_bx_poi_operand"
  (ior (match_operand 0 "r_im_ir_da_x_ba_bx_operand")
       (match_operand 0 "poi_operand")))



;;  When operating on a DI or DF we'll always
;;  need to get to another double word.  This makes
;;  the ir mode not work (since 4(rn) is invalid unless
;;  bx can also be done).  This works on the Z8002 since
;;  x mode will suffice
(define_predicate "r_ir_da_x_operand_for_di"
  (ior (match_operand 0 "r_operand")
       (match_operand 0 "da_operand")
       (and (match_test "TARGET_SMALL")
            (ior (match_operand 0 "ir_operand")
                 (match_operand 0 "x_operand")))))

(define_predicate "r_im_ir_da_x_operand_for_di"
  (ior (match_operand 0 "r_ir_da_x_operand_for_di")
       (match_operand 0 "im_operand")))

;;  Note we can ir in this one because ba is included
(define_predicate "r_ir_da_x_ba_operand_for_di"
  (ior (match_operand 0 "r_ir_da_x_operand")
       (match_operand 0 "ba_operand")))

