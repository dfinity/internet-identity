/*
 * Copyright (c) 2012-2020 MIRACL UK Ltd.
 *
 * This file is part of MIRACL Core
 * (see https://github.com/miracl/core).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @file big.h
 * @author Mike Scott
 * @brief BIG Header File
 *
 */

#ifndef BIG_384_58_H
#define BIG_384_58_H

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "arch.h"
#include "core.h"
#include "config_big_384_58.h"

/* could comment this out if code size is a major issue */
#define UNWOUND  /**< Default to unwound code */
/* Normally recommended, but may not be optimal for some architectures, for example 32-bit ARM M4 */
#define USE_KARATSUBA  /**< Default to use Karatsuba method */

#define BIGBITS_384_58 (8*MODBYTES_384_58) /**< Length in bits */
#define NLEN_384_58 (1+((8*MODBYTES_384_58-1)/BASEBITS_384_58)) /**< length in bytes */
#define DNLEN_384_58 2*NLEN_384_58 /**< Double length in bytes */
#define BMASK_384_58 (((chunk)1<<BASEBITS_384_58)-1) /**< Mask = 2^BASEBITS-1 */
#define NEXCESS_384_58 (1<<(CHUNK-BASEBITS_384_58-1))           /**< 2^(CHUNK-BASEBITS-1) - digit cannot be multiplied by more than this before normalisation */

#define HBITS_384_58 (BASEBITS_384_58/2)      /**< Number of bits in number base divided by 2 */
#define HMASK_384_58 (((chunk)1<<HBITS_384_58)-1)    /**< Mask = 2^HBITS-1 */

//#define DEBUG_NORM

#ifdef DEBUG_NORM  /* Add an extra location to track chunk extension */
#define MPV_384_58 NLEN_384_58
#define MNV_384_58 (NLEN_384_58+1)
typedef chunk BIG_384_58[NLEN_384_58 + 2]; /**< Define type BIG as array of chunks */
#define DMPV_384_58 DNLEN_384_58
#define DMNV_384_58 (DNLEN_384_58+1)
typedef chunk DBIG_384_58[DNLEN_384_58 + 2]; /**< Define type DBIG as array of chunks */
#else
typedef chunk BIG_384_58[NLEN_384_58];     /**< Define type BIG as array of chunks */
typedef chunk DBIG_384_58[DNLEN_384_58];   /**< Define type DBIG as array of chunks */
#endif

/* BIG number prototypes */

/**	@brief Tests for BIG equal to zero (Constant Time)
 *
	@param x a BIG number
	@return 1 if zero, else returns 0
 */
extern int BIG_384_58_iszilch(BIG_384_58 x);
/**	@brief Tests for BIG equal to one (Constant Time)
 *
	@param x a BIG number
	@return 1 if one, else returns 0
 */
extern int BIG_384_58_isunity(BIG_384_58 x);
/**	@brief Tests for DBIG equal to zero (Constant Time)
 *
	@param x a DBIG number
	@return 1 if zero, else returns 0
 */
extern int BIG_384_58_diszilch(DBIG_384_58 x);
/**	@brief Outputs a BIG number to the console (Variable Time)
 *
	@param x a BIG number
 */
extern void BIG_384_58_output(BIG_384_58 x);
/**	@brief Outputs a BIG number to the console in raw form (Variable Time for debugging)
 *
	@param x a BIG number
 */
extern void BIG_384_58_rawoutput(BIG_384_58 x);
/**	@brief Conditional constant time swap of two BIG numbers
 *
	Conditionally swaps parameters in constant time (Constant Time without branching)
	@param x a BIG number
	@param y another BIG number
	@param s swap takes place if not equal to 0
 */
extern void BIG_384_58_cswap(BIG_384_58 x, BIG_384_58 y, int s);
/**	@brief Conditional copy of BIG number
 *
	Conditionally copies second parameter to the first (Constant Time without branching)
	@param x a BIG number
	@param y another BIG number
	@param s copy takes place if not equal to 0
 */
extern void BIG_384_58_cmove(BIG_384_58 x, BIG_384_58 y, int s);
/**	@brief Conditional copy of DBIG number
 *
	Conditionally copies second parameter to the first (Constant Time without branching)
	@param x a DBIG number
	@param y another DBIG number
	@param s copy takes place if not equal to 0
 */
extern void BIG_384_58_dcmove(BIG_384_58 x, BIG_384_58 y, int s);
/**	@brief Convert from BIG number to byte array (Constant Time)
 *
	@param a byte array
	@param x BIG number
 */
extern void BIG_384_58_toBytes(char *a, BIG_384_58 x);
/**	@brief Convert to BIG number from byte array (Constant Time)
 *
	@param x BIG number
	@param a byte array
 */
extern void BIG_384_58_fromBytes(BIG_384_58 x, char *a);
/**	@brief Convert to BIG number from byte array of given length (Variable Time)
 *
	@param x BIG number
	@param a byte array
	@param s byte array length
 */
extern void BIG_384_58_fromBytesLen(BIG_384_58 x, char *a, int s);
/**@brief Convert to DBIG number from byte array of given length (Variable Time) 
 *
   @param x DBIG number
   @param a byte array
   @param s byte array length
 */
extern void BIG_384_58_dfromBytesLen(DBIG_384_58 x, char *a, int s);
/**	@brief Outputs a DBIG number to the console (Variable Time)
 *
	@param x a DBIG number
 */
extern void BIG_384_58_doutput(DBIG_384_58 x);

/**	@brief Outputs a DBIG number to the console (Variable Time)
 *
	@param x a DBIG number
 */
extern void BIG_384_58_drawoutput(DBIG_384_58 x);

/**	@brief Copy BIG from Read-Only Memory to a BIG (Constant Time)
 *
	@param x BIG number
	@param y BIG number in ROM
 */
extern void BIG_384_58_rcopy(BIG_384_58 x, const BIG_384_58 y);
/**	@brief Copy BIG to another BIG (Constant Time)
 *
	@param x BIG number
	@param y BIG number to be copied
 */
extern void BIG_384_58_copy(BIG_384_58 x, BIG_384_58 y);
/**	@brief Copy DBIG to another DBIG (Constant Time)
 *
	@param x DBIG number
	@param y DBIG number to be copied
 */
extern void BIG_384_58_dcopy(DBIG_384_58 x, DBIG_384_58 y);
/**	@brief Copy BIG to upper half of DBIG (Constant Time)
 *
	@param x DBIG number
	@param y BIG number to be copied
 */
extern void BIG_384_58_dsucopy(DBIG_384_58 x, BIG_384_58 y);
/**	@brief Copy BIG to lower half of DBIG (Constant Time)
 *
	@param x DBIG number
	@param y BIG number to be copied
 */
extern void BIG_384_58_dscopy(DBIG_384_58 x, BIG_384_58 y);
/**	@brief Copy lower half of DBIG to a BIG (Constant Time)
 *
	@param x BIG number
	@param y DBIG number to be copied
 */
extern void BIG_384_58_sdcopy(BIG_384_58 x, DBIG_384_58 y);
/**	@brief Copy upper half of DBIG to a BIG (Constant Time)
 *
	@param x BIG number
	@param y DBIG number to be copied
 */
extern void BIG_384_58_sducopy(BIG_384_58 x, DBIG_384_58 y);
/**	@brief Set BIG to zero (Constant Time)
 *
	@param x BIG number to be set to zero
 */
extern void BIG_384_58_zero(BIG_384_58 x);
/**	@brief Set DBIG to zero (Constant Time)
 *
	@param x DBIG number to be set to zero
 */
extern void BIG_384_58_dzero(DBIG_384_58 x);
/**	@brief Set BIG to one (unity) (Constant Time)
 *
	@param x BIG number to be set to one.
 */
extern void BIG_384_58_one(BIG_384_58 x);
/**	@brief Set BIG to inverse mod 2^256 (Constant Time)
 *
	@param x BIG number to be inverted
 */
extern void BIG_384_58_invmod2m(BIG_384_58 x);
/**	@brief Set BIG to sum of two BIGs - output not normalised (Constant Time)
 *
	@param x BIG number, sum of other two
	@param y BIG number
	@param z BIG number
 */
extern void BIG_384_58_add(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z);

/**	@brief Set BIG to logical or of two BIGs - output normalised (Constant Time)
 *
	@param x BIG number, or of other two
	@param y BIG number
	@param z BIG number
 */
extern void BIG_384_58_or(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z);

/**	@brief Increment BIG by a small integer - output not normalised (Constant Time)
 *
	@param x BIG number to be incremented
	@param i integer
 */
extern void BIG_384_58_inc(BIG_384_58 x, int i);
/**	@brief Set BIG to difference of two BIGs (Constant Time)
 *
	@param x BIG number, difference of other two - output not normalised
	@param y BIG number
	@param z BIG number
 */
extern void BIG_384_58_sub(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z);
/**	@brief Decrement BIG by a small integer - output not normalised (Constant Time)
 *
	@param x BIG number to be decremented
	@param i integer
 */
extern void BIG_384_58_dec(BIG_384_58 x, int i);
/**	@brief Set DBIG to sum of two DBIGs (Constant Time)
 *
	@param x DBIG number, sum of other two - output not normalised
	@param y DBIG number
	@param z DBIG number
 */
extern void BIG_384_58_dadd(DBIG_384_58 x, DBIG_384_58 y, DBIG_384_58 z);
/**	@brief Set DBIG to difference of two DBIGs (Constant Time)
 *
	@param x DBIG number, difference of other two - output not normalised
	@param y DBIG number
	@param z DBIG number
 */
extern void BIG_384_58_dsub(DBIG_384_58 x, DBIG_384_58 y, DBIG_384_58 z);
/**	@brief Multiply BIG by a small integer - output not normalised (Constant Time)
 *
	@param x BIG number, product of other two
	@param y BIG number
	@param i small integer
 */
extern void BIG_384_58_imul(BIG_384_58 x, BIG_384_58 y, int i);
/**	@brief Multiply BIG by not-so-small small integer - output normalised (Constant Time)
 *
	@param x BIG number, product of other two
	@param y BIG number
	@param i small integer
	@return Overflowing bits
 */
extern chunk BIG_384_58_pmul(BIG_384_58 x, BIG_384_58 y, int i);
/**	@brief Divide BIG by 3 - output normalised (Constant Time)
 *
	@param x BIG number
	@return Remainder
 */
extern int BIG_384_58_div3(BIG_384_58 x);
/**	@brief Multiply BIG by even bigger small integer resulting in a DBIG - output normalised (Constant Time)
 *
	@param x DBIG number, product of other two
	@param y BIG number
	@param i small integer
 */
extern void BIG_384_58_pxmul(DBIG_384_58 x, BIG_384_58 y, int i);
/**	@brief Multiply BIG by another BIG resulting in DBIG - inputs normalised and output normalised (Constant Time)
 *
	@param x DBIG number, product of other two
	@param y BIG number
	@param z BIG number
 */
extern void BIG_384_58_mul(DBIG_384_58 x, BIG_384_58 y, BIG_384_58 z);
/**	@brief Multiply BIG by another BIG resulting in another BIG - inputs normalised and output normalised (Constant Time)
 *
	Note that the product must fit into a BIG, and x must be distinct from y and z
	@param x BIG number, product of other two
	@param y BIG number
	@param z BIG number
 */
extern void BIG_384_58_smul(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z);
/**	@brief Square BIG resulting in a DBIG - input normalised and output normalised (Constant Time)
 *
	@param x DBIG number, square of a BIG
	@param y BIG number to be squared
 */
extern void BIG_384_58_sqr(DBIG_384_58 x, BIG_384_58 y);

/**	@brief Montgomery reduction of a DBIG to a BIG  - input normalised and output normalised (Constant Time)
 *
	@param a BIG number, reduction of a BIG
	@param md BIG number, the modulus
	@param MC the Montgomery Constant
	@param d DBIG number to be reduced
 */
extern void BIG_384_58_monty(BIG_384_58 a, BIG_384_58 md, chunk MC, DBIG_384_58 d);

/**	@brief Shifts a BIG left by any number of bits - input must be normalised, output normalised (Constant Time)
 *
	@param x BIG number to be shifted
	@param s Number of bits to shift
 */
extern void BIG_384_58_shl(BIG_384_58 x, int s);
/**	@brief Fast shifts a BIG left by a small number of bits - input must be normalised, output will be normalised (Constant Time)
 *
	The number of bits to be shifted must be less than BASEBITS
	@param x BIG number to be shifted
	@param s Number of bits to shift
	@return Overflow bits
 */
extern int BIG_384_58_fshl(BIG_384_58 x, int s);
/**	@brief Shifts a DBIG left by any number of bits - input must be normalised, output normalised (Constant Time)
 *
	@param x DBIG number to be shifted
	@param s Number of bits to shift
 */
extern void BIG_384_58_dshl(DBIG_384_58 x, int s);
/**	@brief Shifts a BIG right by any number of bits - input must be normalised, output normalised (Constant Time)
 *
	@param x BIG number to be shifted
	@param s Number of bits to shift
 */
extern void BIG_384_58_shr(BIG_384_58 x, int s);

/**	@brief Fast time-critical combined shift by 1 bit, subtract and normalise (Constant Time)
 *
	@param r BIG number normalised output
	@param a BIG number to be subtracted from
	@param m BIG number to be shifted and subtracted
	@return sign of r
 */
extern int BIG_384_58_ssn(BIG_384_58 r, BIG_384_58 a, BIG_384_58 m);

/**	@brief Fast shifts a BIG right by a small number of bits - input must be normalised, output will be normalised (Constant Time)
 *
	The number of bits to be shifted must be less than BASEBITS
	@param x BIG number to be shifted
	@param s Number of bits to shift
	@return Shifted out bits
 */
extern int BIG_384_58_fshr(BIG_384_58 x, int s);
/**	@brief Shifts a DBIG right by any number of bits - input must be normalised, output normalised (Constant Time)
 *
	@param x DBIG number to be shifted
	@param s Number of bits to shift
 */
extern void BIG_384_58_dshr(DBIG_384_58 x, int s);
/**	@brief Splits a DBIG into two BIGs - input must be normalised, outputs normalised (Constant Time as used)
 *
	Internal function. The value of s must be approximately in the middle of the DBIG.
	Typically used to extract z mod 2^MODBITS and z/2^MODBITS
	@param x BIG number, top half of z
	@param y BIG number, bottom half of z
	@param z DBIG number to be split in two.
	@param s Bit position at which to split
	@return carry-out from top half
 */
extern chunk BIG_384_58_split(BIG_384_58 x, BIG_384_58 y, DBIG_384_58 z, int s);
/**	@brief Normalizes a BIG number - output normalised (Constant Time)
 *
	All digits of the input BIG are reduced mod 2^BASEBITS
	@param x BIG number to be normalised
 */
extern chunk BIG_384_58_norm(BIG_384_58 x);
/**	@brief Normalizes a DBIG number - output normalised (Constant Time)
 *
	All digits of the input DBIG are reduced mod 2^BASEBITS
	@param x DBIG number to be normalised
 */
extern void BIG_384_58_dnorm(DBIG_384_58 x);
/**	@brief Compares two BIG numbers. Inputs must be normalised externally (Constant Time)
 *
	@param x first BIG number to be compared
	@param y second BIG number to be compared
	@return -1 is x<y, 0 if x=y, 1 if x>y
 */
extern int BIG_384_58_comp(BIG_384_58 x, BIG_384_58 y);
/**	@brief Compares two DBIG numbers. Inputs must be normalised externally (Constant Time)
 *
	@param x first DBIG number to be compared
	@param y second DBIG number to be compared
	@return -1 is x<y, 0 if x=y, 1 if x>y
 */
extern int BIG_384_58_dcomp(DBIG_384_58 x, DBIG_384_58 y);
/**	@brief Calculate number of bits in a BIG - output normalised (Variable Time)
 *
	@param x BIG number
	@return Number of bits in x
 */
extern int BIG_384_58_nbits(BIG_384_58 x);
/**	@brief Calculate number of bits in a DBIG - output normalised (Variable Time)
 *
	@param x DBIG number
	@return Number of bits in x
 */
extern int BIG_384_58_dnbits(DBIG_384_58 x);
/**	@brief Reduce x mod n - input and output normalised (Variable Time)
 *
	Slow but rarely used
	@param x BIG number to be reduced mod n
	@param n The modulus
 */
extern void BIG_384_58_mod(BIG_384_58 x, BIG_384_58 n); 
/**	@brief Divide x by n - output normalised (Variable Time)
 *
	Slow but rarely used
	@param x BIG number to be divided by n
	@param n The Divisor
 */
extern void BIG_384_58_sdiv(BIG_384_58 x, BIG_384_58 n);
/**	@brief  x=y mod n - output normalised (Variable Time)
 *
	Slow but rarely used. y is destroyed.
	@param x BIG number, on exit = y mod n
	@param y DBIG number
	@param n Modulus
 */
extern void BIG_384_58_dmod(BIG_384_58 x, DBIG_384_58 y, BIG_384_58 n);
/**	@brief  x=y/n - output normalised (Variable Time)
 *
	Slow but rarely used. y is destroyed.
	@param x BIG number, on exit = y/n
	@param y DBIG number
	@param n Modulus
 */
extern void BIG_384_58_ddiv(BIG_384_58 x, DBIG_384_58 y, BIG_384_58 n);
/**	@brief  return parity of BIG, that is the least significant bit (Constant Time)
 *
	@param x BIG number
	@return 0 or 1
 */
extern int BIG_384_58_parity(BIG_384_58 x);
/**	@brief  return i-th of BIG (Constant Time)
 *
	@param x BIG number
	@param i the bit of x to be returned
	@return 0 or 1
 */
extern int BIG_384_58_bit(BIG_384_58 x, int i);
/**	@brief  return least significant bits of a BIG (Constant Time)
 *
	@param x BIG number
	@param n number of bits to return. Assumed to be less than BASEBITS.
	@return least significant n bits as an integer
 */
extern int BIG_384_58_lastbits(BIG_384_58 x, int n);
/**	@brief  Create a random BIG from a random number generator (Constant Time)
 *
	Assumes that the random number generator has been suitably initialised
	@param x BIG number, on exit a random number
	@param r A pointer to a Cryptographically Secure Random Number Generator
 */
extern void BIG_384_58_random(BIG_384_58 x, csprng *r);
/**	@brief  Create an unbiased random BIG from a random number generator, reduced with respect to a modulus (Constant Time as used)
 *
	Assumes that the random number generator has been suitably initialised
	@param x BIG number, on exit a random number
	@param n The modulus
	@param r A pointer to a Cryptographically Secure Random Number Generator
 */
extern void BIG_384_58_randomnum(BIG_384_58 x, BIG_384_58 n, csprng *r);

/**	@brief  Create an unbiased random BIG from a random number generator, reduced with respect to a modulus and truncated to max bit length (Constant Time as used)
 *
	Assumes that the random number generator has been suitably initialised
	@param x BIG number, on exit a random number
	@param n The modulus
	@param t Maximum bit length
	@param r A pointer to a Cryptographically Secure Random Number Generator
 */
extern void BIG_384_58_randtrunc(BIG_384_58 x, BIG_384_58 n, int t, csprng *r);

/**	brief  return NAF (Non-Adjacent-Form) value as +/- 1, 3 or 5, inputs must be normalised
 *
	Given x and 3*x extracts NAF value from given bit position, and returns number of bits processed, and number of trailing zeros detected if any
	param x BIG number
	param x3 BIG number, three times x
	param i bit position
	param nbs pointer to integer returning number of bits processed
	param nzs pointer to integer returning number of trailing 0s
	return + or - 1, 3 or 5
*/

/**	@brief  Calculate x=y*z mod n (Variable Time)
 *
	Slow method for modular multiplication 
	@param x BIG number, on exit = y*z mod n
	@param y BIG number
	@param z BIG number
	@param n The BIG Modulus
 */
extern void BIG_384_58_modmul(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z, BIG_384_58 n);
/**	@brief  Calculate x=y/z mod n (Variable Time)
 *
	Slow method for modular division
	@param x BIG number, on exit = y/z mod n
	@param y BIG number
	@param z BIG number
	@param n The BIG Modulus
 */
extern void BIG_384_58_moddiv(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z, BIG_384_58 n);
/**	@brief  Calculate x=y^2 mod n (Variable Time)
 *
	Slow method for modular squaring
	@param x BIG number, on exit = y^2 mod n
	@param y BIG number
	@param n The BIG Modulus
 */
extern void BIG_384_58_modsqr(BIG_384_58 x, BIG_384_58 y, BIG_384_58 n);
/**	@brief  Calculate x=-y mod n (Variable Time)
 *
	Modular negation
	@param x BIG number, on exit = -y mod n
	@param y BIG number
	@param n The BIG Modulus
 */
extern void BIG_384_58_modneg(BIG_384_58 x, BIG_384_58 y, BIG_384_58 n);

/**	@brief  Calculate x=y+z mod n (Variable Time)
 *
	Slow method for modular addition
	@param x BIG number, on exit = y+z mod n
	@param y BIG number
	@param z BIG number
	@param n The BIG Modulus
 */
extern void BIG_384_58_modadd(BIG_384_58 x, BIG_384_58 y, BIG_384_58 z, BIG_384_58 n);

/**	@brief  Calculate jacobi Symbol (x/y) (Variable Time)
 *
	@param x BIG number
	@param y BIG number
	@return Jacobi symbol, -1,0 or 1
 */
extern int BIG_384_58_jacobi(BIG_384_58 x, BIG_384_58 y);
/**	@brief  Calculate x=1/y mod n (Variable Time)
 *
	Modular Inversion - This is slow. Uses binary method.
	@param x BIG number, on exit = 1/y mod n
	@param y BIG number
	@param n The BIG Modulus
 */
extern void BIG_384_58_invmodp(BIG_384_58 x, BIG_384_58 y, BIG_384_58 n);
/** @brief Calculate x=x mod 2^m (Variable Time)
 *
	Truncation
	@param x BIG number, on reduced mod 2^m
	@param m new truncated size
*/
extern void BIG_384_58_mod2m(BIG_384_58 x, int m);

/**	@brief Calculates a*b+c+*d (Constant Time)
 *
	Calculate partial product of a.b, add in carry c, and add total to d
	@param x multiplier
	@param y multiplicand
	@param c carry
	@param r pointer to accumulated bottom half of result
	@return top half of result
 */

#ifdef dchunk

/* Method required to calculate x*y+c+r, bottom half in r, top half returned */
static inline chunk muladd_384_58(chunk x, chunk y, chunk c, chunk *r)
{
    dchunk prod = (dchunk)x * y + c + *r;
    *r = (chunk)prod & BMASK_384_58;
    return (chunk)(prod >> BASEBITS_384_58);
}

#else

/* No integer type available that can store double the wordlength */
/* accumulate partial products */

static inline chunk muladd_384_58(chunk x, chunk y, chunk c, chunk *r)
{
    chunk x0, x1, y0, y1;
    chunk bot, top, mid, carry;
    x0 = x & HMASK_384_58;
    x1 = (x >> HBITS_384_58);
    y0 = y & HMASK_384_58;
    y1 = (y >> HBITS_384_58);
    bot = x0 * y0;
    top = x1 * y1;
    mid = x0 * y1 + x1 * y0;
    x0 = mid & HMASK_384_58;
    x1 = (mid >> HBITS_384_58);
    bot += x0 << HBITS_384_58;
    bot += *r;
    bot += c;

    top += x1;
    carry = bot >> BASEBITS_384_58;
    bot &= BMASK_384_58;
    top += carry;

    *r = bot;
    return top;
}

#endif


#endif
