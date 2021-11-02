/*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland
    Copyright 2018 Christophe Troestler

    This file is part of the ocaml interval library.

    The ocaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ocaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the ocaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*/

#include <fenv.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#ifdef INTEL_ARCH
/* Intel architecture ------------------------------------------------ */

#include "interval_intel.h"

CAMLexport void ocaml_set_nearest() {
  asm __volatile__(SET_NEAREST(%0)
                   :"=m"(cw));
}

CAMLexport void ocaml_set_low() {
  asm __volatile__(SET_LOW(%0)
                   :"=m"(cw));
}

CAMLexport void ocaml_set_high() {
  asm __volatile__(SET_HIGH(%0)
                   :"=m"(cw));
}

/* Int -> float conversions */

static double low_float(long int a) {
  double res;
  tmp = a;
  asm __volatile__(SET_LOW(%0)
                   :"=m"(cw)
                   :"m"(tmp)
                   :"memory");
  asm __volatile__(FILDQ(%1)
                   :"=t"(res)
                   :"m"(tmp),"m"(cw)
                   :"memory");
  asm __volatile__(SET_NEAREST(%0)
                   :"=m"(cw)
                   :"m"(res),"m"(tmp)
                   :"memory");
  return(res);
}

CAMLexport double ocaml_low_float(intnat a)
{
  return(low_float(a));
}


static double high_float(long int a) {
  double res;

  tmp = a;
  asm __volatile__(SET_HIGH(%0)
                   :"=m"(cw)
                   :"m"(tmp)
                   :"memory");
  asm __volatile__(FILDQ(%1)
                   :"=t"(res)
                   :"m"(tmp),"m"(cw)
                   :"memory");
  asm __volatile__(SET_NEAREST(%0)
                   :"=m"(cw)
                   :"m"(res),"m"(tmp)
                   :"memory");
  return(res);
}

CAMLexport double ocaml_high_float(intnat a)
{
  return(high_float(a));
}

/* Arithmetic operations */

#define ARITH_OP(op, rounding)                                          \
  CAMLexport double ocaml_##rounding##_##op(double a, double b) {       \
    volatile double res;                                                \
                                                                        \
    asm __volatile__(SET_##rounding(%3)                                 \
                     "f" #op " %%st(1),%%st(0)\n\t"                     \
                     :"=t"(res)                                         \
                     :"0"(a),"u"(b),"m"(cw)                             \
                     :"memory");                                        \
                                                                        \
    asm __volatile__(SET_NEAREST(%0)                                    \
                     :"=m"(cw)                                          \
                     :"m"(cw)                                           \
                     :"memory");                                        \
    return(res);                                                        \
  }

ARITH_OP(add, LOW)
ARITH_OP(add, HIGH)
ARITH_OP(sub, LOW)
ARITH_OP(sub, HIGH)
ARITH_OP(mul, LOW)
ARITH_OP(mul, HIGH)
ARITH_OP(div, LOW)
ARITH_OP(div, HIGH)

/* sqrt */

#define SQRT(rounding) \
  CAMLexport double ocaml_##rounding##_sqrt(double x) { \
    volatile double res;                                \
    asm __volatile__(SET_##rounding(%2)                 \
                     "fsqrt\n\t"                        \
                     :"=t"(res)                         \
                     :"0"(x),"m"(cw)                    \
                     :"memory");                        \
    asm __volatile__(SET_NEAREST(%0)                    \
                     :"=m"(cw)                          \
                     :"m"(cw)                           \
                     :"memory");                        \
    return(res);                                        \
  }

SQRT(LOW)
SQRT(HIGH)

#elif __STDC_VERSION__ >= 199901L
/* Not INTEL_ARCH, use C99 ------------------------------------------- */

CAMLexport void ocaml_set_nearest() {
  fesetround(FE_TONEAREST);
}

CAMLexport void ocaml_set_low() {
  fesetround(FE_DOWNWARD);
}

CAMLexport void ocaml_set_high() {
  fesetround(FE_UPWARD);
}

CAMLexport double ocaml_low_float(intnat a)
{
  volatile double r;
  fesetround(FE_DOWNWARD);
  r = a;
  fesetround(FE_TONEAREST);
  return(r);
}

CAMLexport double ocaml_high_float(intnat a)
{
  volatile double r;
  fesetround(FE_UPWARD);
  r = a;
  fesetround(FE_TONEAREST);
  return(r);
}

/* Use "x" and "y" as the operation arguments. */
#define BIN_OP(name, round, op)                         \
  CAMLexport double ocaml_##name(double x, double y) {  \
    volatile double r;                                  \
    fesetround(round);                                  \
    r = (op);                                           \
    fesetround(FE_TONEAREST);                           \
    return(r);                                          \
  }

BIN_OP(LOW_add,  FE_DOWNWARD, x + y)
BIN_OP(HIGH_add, FE_UPWARD,   x + y)
BIN_OP(LOW_sub,  FE_DOWNWARD, x - y)
BIN_OP(HIGH_sub, FE_UPWARD,   x - y)
BIN_OP(LOW_mul,  FE_DOWNWARD, x * y)
BIN_OP(HIGH_mul, FE_UPWARD,   x * y)
BIN_OP(LOW_div,  FE_DOWNWARD, x / y)
BIN_OP(HIGH_div, FE_UPWARD,   x / y)

#define SQRT(name, round)                                   \
  CAMLexport double ocaml_##name(double x) {                \
    volatile double r;                                      \
    fesetround(round);                                      \
    r = sqrt(x);                                            \
    fesetround(FE_TONEAREST);                               \
    return(r);                                              \
  }

SQRT(LOW_sqrt, FE_DOWNWARD)
SQRT(HIGH_sqrt, FE_UPWARD)

#else  /* Not INTEL_ARCH, nor C99 */
#error "An Intel architecture or a C99 standard library is required"
/* FIXME: for basic arithmetic operations, one could add/substract 1
   ulp as a last resort. */
#endif  /* INTEL_ARCH */


/* Bytecode ---------------------------------------------------------- */

#define UNARY_BYTE(name, of_val)                           \
  CAMLexport value ocaml_##name##_byte(value a) {          \
    return(caml_copy_double(ocaml_##name(of_val(a))));   \
  }

UNARY_BYTE(low_float,  Long_val)
UNARY_BYTE(high_float, Long_val)
UNARY_BYTE(LOW_sqrt,  Double_val)
UNARY_BYTE(HIGH_sqrt, Double_val)

#define BIN_BYTE(name) \
  CAMLexport value ocaml_##name##_byte(value a, value b) {              \
    return caml_copy_double(ocaml_##name(Double_val(a), Double_val(b))); \
  }

BIN_BYTE(LOW_add)
BIN_BYTE(HIGH_add)
BIN_BYTE(LOW_sub)
BIN_BYTE(HIGH_sub)
BIN_BYTE(LOW_mul)
BIN_BYTE(HIGH_mul)
BIN_BYTE(LOW_div)
BIN_BYTE(HIGH_div)

/* Printing ---------------------------------------------------------- */

/* uint64_t */
