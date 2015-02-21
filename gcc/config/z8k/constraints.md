;; Constraint definitions for the Z8000 for GNU C compiler

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; For z8k
;; `I' is the constant 0.
;; `J' is a constant for an inc (1..16)
;; `K' is a constant for a dec  (-1..-16)
;; `L' is 0..15, used in shifts and ldk
;; `M' is the constant 1, used in shifts
;; `N' is the constant 2, used in shifts
;; `O' is a power of two
;; `P' is a comp power of two

(define_constraint "I"
  "Zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "J"
  "Integer constant in the range 1 @dots{} 16."
  (and (match_code "const_int")
       (match_test "ival >= 1 && ival <= 16")))

(define_constraint "K"
  "Integer constant in the range -1 @dots{} -16."
  (and (match_code "const_int")
       (match_test "ival <= -1 && ival >= -16")))

(define_constraint "L"
  "Integer constant in the range 0 @dots{} 15."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 15")))

(define_constraint "M"
  "One."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "N"
  "Two."
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "O"
  "Power of two."
  (and (match_code "const_int")
       (match_test "POWER_OF_2(ival)")))

(define_constraint "P"
  "Comp power of two."
  (and (match_code "const_int")
       (match_test "COM_POWER_OF_2(ival)")))



;; 'Q' ir or da     PTR reg or direct address
;; 'R' x            address + HI reg - never valid in huge mode
;; 'S' ba or bx     disp + PTR reg   (or PTR reg + reg never in huge)
;; 'T' ba


;; #define EXTRA_CONSTRAINT(op, c)                                 \
;; 	(((c) == 'Q')  ? (IR_P(op) || DA_P(op)) :               \
;;         (((c) == 'R')  ? (X_P(op))  :                           \
;;         (((c) == 'T')  ? (BA_P(op))  :                          \
;;         (((c) == 'S')  ? (BA_P(op) || BX_P(op)) : abort() ))))

(define_constraint "Q"
  ""
  (match_test "ir_p (op) || da_p (op)"))

(define_constraint "R"
  ""
  (match_test "x_p (op, false)"))

(define_constraint "T"
  ""
  (match_test "ba_p (op, false)"))

(define_constraint "S"
  ""
  (ior (match_test "ba_p (op, false)")
       (match_test "bx_p (op, false)")))

;;  #define REG_CLASS_FROM_LETTER(C)		\
;;    ((C) == 'u' ? QI_REGS  : 			\
;;    ((C) == 'v' ? PTR_REGS :			\
;;    ((C) == 'q' ? SP_REGS  :			\
;;    ((C) == 'r' ? GENERAL_REGS  :			\
;;    (NO_REGS)))))

(define_register_constraint "u" "QI_REGS"
  "")

(define_register_constraint "v" "PTR_REGS"
  "")

(define_register_constraint "q" "SP_REGS"
  "")





