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

#ifndef _INTERVAL_BASE_H
#define _INTERVAL_BASE_H

/* We work with _RD (round down) and _RU (round up) version of
   arithmetic functions.  Extreme care must be taken regarding inline
   assembly.  As setting the mode of the processor has to be done
   before any computation, we have to prevent instructions reordering.
   This is also true for setting back the mode to nearest.  The result
   has to be stored in the stack before setting the mode to nearest:
   computation is done in 80 bits mode and casting the result to 64
   bits has to be done before changing rounding mode because the
   casting itself introduces errors.  This is why some (artificial)
   variables dependencies have to be used, along with the "volatile"
   keyword and the "memory" keyword in the clobber list.  It is
   extremely wise to check the assembly code generated...
*/

#if defined _MSC_BUILD
#error "MSVC inline assembly is not supported at the moment.  Please contribute."
#elif !(defined __GNUC__)
#define asm __asm__
#define __volatile__
#endif

/* Set the processor to different rounding modes */
#define SET_RD(ref) "fstcw "#ref"\n\t andw $0xf3ff,"#ref"\n\t orw $0x0400,"#ref"\n\t fldcw "#ref"\n\t"
#define SET_RU(ref) "fstcw "#ref"\n\t andw $0xf3ff,"#ref"\n\t orw $0x0800,"#ref"\n\t fldcw "#ref"\n\t"
#define SET_NEAREST(ref) "fstcw "#ref"\n\t andw $0xf3ff,"#ref"\n\t fldcw "#ref"\n\t"

static short int cw;
static long long int tmp;

#define FILDQ(ref) "fildq "#ref"\n\t"

#endif  /* interval_base.h included */
