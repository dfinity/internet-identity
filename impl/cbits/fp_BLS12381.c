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

/* CORE mod p functions */
/* Small Finite Field arithmetic */
/* SU=m, SU is Stack Usage (NOT_SPECIAL Modulus) */

#include "fp_BLS12381.h"

/* Fast Modular Reduction Methods */

/* r=d mod m */
/* d MUST be normalised */
/* Products must be less than pR in all cases !!! */
/* So when multiplying two numbers, their product *must* be less than MODBITS+BASEBITS*NLEN */
/* Results *may* be one bit bigger than MODBITS */

#if MODTYPE_BLS12381 == PSEUDO_MERSENNE
/* r=d mod m */

/* Converts from BIG integer to residue form mod Modulus */
void FP_BLS12381_nres(FP_BLS12381 *y, BIG_384_58 x)
{
    BIG_384_58 mdls;
    BIG_384_58_rcopy(mdls, Modulus_BLS12381);
    BIG_384_58_copy(y->g, x);
    BIG_384_58_mod(y->g,mdls);
    y->XES = 1;
}

/* Converts from residue form back to BIG integer form */
void FP_BLS12381_redc(BIG_384_58 x, FP_BLS12381 *y)
{
    BIG_384_58_copy(x, y->g);
}

/* reduce a DBIG to a BIG exploiting the special form of the modulus */
void FP_BLS12381_mod(BIG_384_58 r, DBIG_384_58 d)
{
    BIG_384_58 t, b;
    chunk v, tw;
    BIG_384_58_split(t, b, d, MODBITS_BLS12381);

    /* Note that all of the excess gets pushed into t. So if squaring a value with a 4-bit excess, this results in
       t getting all 8 bits of the excess product! So products must be less than pR which is Montgomery compatible */

    if (MConst_BLS12381 < NEXCESS_384_58)
    {
        BIG_384_58_imul(t, t, MConst_BLS12381);
        BIG_384_58_norm(t);
        BIG_384_58_add(r, t, b);
        BIG_384_58_norm(r);
        tw = r[NLEN_384_58 - 1];
        r[NLEN_384_58 - 1] &= TMASK_BLS12381;
        r[0] += MConst_BLS12381 * ((tw >> TBITS_BLS12381));
    }
    else
    {
        v = BIG_384_58_pmul(t, t, MConst_BLS12381);
        BIG_384_58_add(r, t, b);
        BIG_384_58_norm(r);
        tw = r[NLEN_384_58 - 1];
        r[NLEN_384_58 - 1] &= TMASK_BLS12381;
#if CHUNK == 16
        r[1] += muladd_384_58(MConst_BLS12381, ((tw >> TBITS_BLS12381) + (v << (BASEBITS_384_58 - TBITS_BLS12381))), 0, &r[0]);
#else
        r[0] += MConst_BLS12381 * ((tw >> TBITS_BLS12381) + (v << (BASEBITS_384_58 - TBITS_BLS12381)));
#endif
    }
    BIG_384_58_norm(r);
}
#endif

/* This only applies to Curve C448, so specialised (for now) */
#if MODTYPE_BLS12381 == GENERALISED_MERSENNE

void FP_BLS12381_nres(FP_BLS12381 *y, BIG_384_58 x)
{
    BIG_384_58 mdls;
    BIG_384_58_rcopy(mdls, Modulus_BLS12381);
    BIG_384_58_copy(y->g, x);
    BIG_384_58_mod(y->g,mdls);
    y->XES = 1;
}

/* Converts from residue form back to BIG integer form */
void FP_BLS12381_redc(BIG_384_58 x, FP_BLS12381 *y)
{
    BIG_384_58_copy(x, y->g);
}

/* reduce a DBIG to a BIG exploiting the special form of the modulus */
void FP_BLS12381_mod(BIG_384_58 r, DBIG_384_58 d)
{
    BIG_384_58 t, b;
    chunk carry;
    BIG_384_58_split(t, b, d, MBITS_BLS12381);

    BIG_384_58_add(r, t, b);

    BIG_384_58_dscopy(d, t);
    BIG_384_58_dshl(d, MBITS_BLS12381 / 2);

    BIG_384_58_split(t, b, d, MBITS_BLS12381);

    BIG_384_58_add(r, r, t);
    BIG_384_58_add(r, r, b);
    BIG_384_58_norm(r);
    BIG_384_58_shl(t, MBITS_BLS12381 / 2);

    BIG_384_58_add(r, r, t);

    carry = r[NLEN_384_58 - 1] >> TBITS_BLS12381;

    r[NLEN_384_58 - 1] &= TMASK_BLS12381;
    r[0] += carry;

    r[224 / BASEBITS_384_58] += carry << (224 % BASEBITS_384_58); /* need to check that this falls mid-word */
    BIG_384_58_norm(r);
}

#endif

#if MODTYPE_BLS12381 == MONTGOMERY_FRIENDLY

/* convert to Montgomery n-residue form */
void FP_BLS12381_nres(FP_BLS12381 *y, BIG_384_58 x)
{
    DBIG_384_58 d;
    BIG_384_58 r;
    BIG_384_58_rcopy(r, R2modp_BLS12381);
    BIG_384_58_mul(d, x, r);
    FP_BLS12381_mod(y->g, d);
    y->XES = 2;
}

/* convert back to regular form */
void FP_BLS12381_redc(BIG_384_58 x, FP_BLS12381 *y)
{
    DBIG_384_58 d;
    BIG_384_58_dzero(d);
    BIG_384_58_dscopy(d, y->g);
    FP_BLS12381_mod(x, d);
}

/* fast modular reduction from DBIG to BIG exploiting special form of the modulus */
void FP_BLS12381_mod(BIG_384_58 a, DBIG_384_58 d)
{
    int i;

    for (i = 0; i < NLEN_384_58; i++)
        d[NLEN_384_58 + i] += muladd_384_58(d[i], MConst_BLS12381 - 1, d[i], &d[NLEN_384_58 + i - 1]);

    BIG_384_58_sducopy(a, d);
    BIG_384_58_norm(a);
}

#endif

#if MODTYPE_BLS12381 == NOT_SPECIAL

/* convert to Montgomery n-residue form */
void FP_BLS12381_nres(FP_BLS12381 *y, BIG_384_58 x)
{
    DBIG_384_58 d;
    BIG_384_58 r;
    BIG_384_58_rcopy(r, R2modp_BLS12381);
    BIG_384_58_mul(d, x, r);
    FP_BLS12381_mod(y->g, d);
    y->XES = 2;
}

/* convert back to regular form */
void FP_BLS12381_redc(BIG_384_58 x, FP_BLS12381 *y)
{
    DBIG_384_58 d;
    BIG_384_58_dzero(d);
    BIG_384_58_dscopy(d, y->g);
    FP_BLS12381_mod(x, d);
}


/* reduce a DBIG to a BIG using Montgomery's no trial division method */
/* d is expected to be dnormed before entry */
/* SU= 112 */
void FP_BLS12381_mod(BIG_384_58 a, DBIG_384_58 d)
{
    BIG_384_58 mdls;
    BIG_384_58_rcopy(mdls, Modulus_BLS12381);
    BIG_384_58_monty(a, mdls, MConst_BLS12381, d);
}

#endif

void FP_BLS12381_from_int(FP_BLS12381 *x,int a)
{
    BIG_384_58 w;
    if (a<0) BIG_384_58_rcopy(w, Modulus_BLS12381);
    else BIG_384_58_zero(w); 
    BIG_384_58_inc(w,a); BIG_384_58_norm(w); 
    FP_BLS12381_nres(x,w);
}

/* test x==0 ? */
/* SU= 48 */
int FP_BLS12381_iszilch(FP_BLS12381 *x)
{
    BIG_384_58 m;
    FP_BLS12381 y;
    FP_BLS12381_copy(&y,x);
    FP_BLS12381_reduce(&y);
    FP_BLS12381_redc(m,&y);
    return BIG_384_58_iszilch(m);
}

int FP_BLS12381_isunity(FP_BLS12381 *x)
{
    BIG_384_58 m;
    FP_BLS12381 y;
    FP_BLS12381_copy(&y,x);
    FP_BLS12381_reduce(&y);
    FP_BLS12381_redc(m,&y);
    return BIG_384_58_isunity(m);
}


void FP_BLS12381_copy(FP_BLS12381 *y, FP_BLS12381 *x)
{
    BIG_384_58_copy(y->g, x->g);
    y->XES = x->XES;
}

void FP_BLS12381_rcopy(FP_BLS12381 *y, const BIG_384_58 c)
{
    BIG_384_58 b;
    BIG_384_58_rcopy(b, c);
    FP_BLS12381_nres(y, b);
}

/* Swap a and b if d=1 */
void FP_BLS12381_cswap(FP_BLS12381 *a, FP_BLS12381 *b, int d)
{
    sign32 t, c = d;
    BIG_384_58_cswap(a->g, b->g, d);

    c = ~(c - 1);
    t = c & ((a->XES) ^ (b->XES));
    a->XES ^= t;
    b->XES ^= t;

}

/* Move b to a if d=1 */
void FP_BLS12381_cmove(FP_BLS12381 *a, FP_BLS12381 *b, int d)
{
    sign32 c = -d;

    BIG_384_58_cmove(a->g, b->g, d);
    a->XES ^= (a->XES ^ b->XES)&c;
}

void FP_BLS12381_zero(FP_BLS12381 *x)
{
    BIG_384_58_zero(x->g);
    x->XES = 1;
}

int FP_BLS12381_equals(FP_BLS12381 *x, FP_BLS12381 *y)
{
    FP_BLS12381 xg, yg;
    FP_BLS12381_copy(&xg, x);
    FP_BLS12381_copy(&yg, y);
    FP_BLS12381_reduce(&xg);
    FP_BLS12381_reduce(&yg);
    if (BIG_384_58_comp(xg.g, yg.g) == 0) return 1;
    return 0;
}

/* output FP */
/* SU= 48 */
void FP_BLS12381_output(FP_BLS12381 *r)
{
    BIG_384_58 c;
    FP_BLS12381_reduce(r);
    FP_BLS12381_redc(c, r);
    BIG_384_58_output(c);
}

void FP_BLS12381_rawoutput(FP_BLS12381 *r)
{
    BIG_384_58_rawoutput(r->g);
}

#ifdef GET_STATS
int tsqr = 0, rsqr = 0, tmul = 0, rmul = 0;
int tadd = 0, radd = 0, tneg = 0, rneg = 0;
int tdadd = 0, rdadd = 0, tdneg = 0, rdneg = 0;
#endif

#ifdef FUSED_MODMUL

/* Insert fastest code here */

#endif

/* r=a*b mod Modulus */
/* product must be less that p.R - and we need to know this in advance! */
/* SU= 88 */
void FP_BLS12381_mul(FP_BLS12381 *r, FP_BLS12381 *a, FP_BLS12381 *b)
{
    DBIG_384_58 d;

    if ((sign64)a->XES * b->XES > (sign64)FEXCESS_BLS12381)
    {
#ifdef DEBUG_REDUCE
        printf("Product too large - reducing it\n");
#endif
        FP_BLS12381_reduce(a);  /* it is sufficient to fully reduce just one of them < p */
    }

#ifdef FUSED_MODMUL
    FP_BLS12381_modmul(r->g, a->g, b->g);
#else
    BIG_384_58_mul(d, a->g, b->g);
    FP_BLS12381_mod(r->g, d);
#endif
    r->XES = 2;
}


/* multiplication by an integer, r=a*c */
/* SU= 136 */
void FP_BLS12381_imul(FP_BLS12381 *r, FP_BLS12381 *a, int c)
{
    int s = 0;

    if (c < 0)
    {
        c = -c;
        s = 1;
    }

#if MODTYPE_BLS12381==PSEUDO_MERSENNE || MODTYPE_BLS12381==GENERALISED_MERSENNE
    DBIG_384_58 d;
    BIG_384_58_pxmul(d, a->g, c);
    FP_BLS12381_mod(r->g, d);
    r->XES = 2;

#else
    //Montgomery
    BIG_384_58 k;
    FP_BLS12381 f;
    if (a->XES * c <= FEXCESS_BLS12381)
    {
        BIG_384_58_pmul(r->g, a->g, c);
        r->XES = a->XES * c; // careful here - XES jumps!
    }
    else
    {
        // don't want to do this - only a problem for Montgomery modulus and larger constants
        BIG_384_58_zero(k);
        BIG_384_58_inc(k, c);
        BIG_384_58_norm(k);
        FP_BLS12381_nres(&f, k);
        FP_BLS12381_mul(r, a, &f);
    }
#endif

    if (s)
    {
        FP_BLS12381_neg(r, r);
        FP_BLS12381_norm(r);
    }
}

/* Set r=a^2 mod m */
/* SU= 88 */
void FP_BLS12381_sqr(FP_BLS12381 *r, FP_BLS12381 *a)
{
    DBIG_384_58 d;

    if ((sign64)a->XES * a->XES > (sign64)FEXCESS_BLS12381)
    {
#ifdef DEBUG_REDUCE
        printf("Product too large - reducing it\n");
#endif
        FP_BLS12381_reduce(a);
    }

    BIG_384_58_sqr(d, a->g);
    FP_BLS12381_mod(r->g, d);
    r->XES = 2;
}

/* SU= 16 */
/* Set r=a+b */
void FP_BLS12381_add(FP_BLS12381 *r, FP_BLS12381 *a, FP_BLS12381 *b)
{
    BIG_384_58_add(r->g, a->g, b->g);
    r->XES = a->XES + b->XES;
    if (r->XES > FEXCESS_BLS12381)
    {
#ifdef DEBUG_REDUCE
        printf("Sum too large - reducing it \n");
#endif
        FP_BLS12381_reduce(r);
    }
}

/* Set r=a-b mod m */
/* SU= 56 */
void FP_BLS12381_sub(FP_BLS12381 *r, FP_BLS12381 *a, FP_BLS12381 *b)
{
    FP_BLS12381 n;
    FP_BLS12381_neg(&n, b);
    FP_BLS12381_add(r, a, &n);
}

// https://graphics.stanford.edu/~seander/bithacks.html
// constant time log to base 2 (or number of bits in)

static int logb2(unsign32 v)
{
    int r;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;

    v = v - ((v >> 1) & 0x55555555);
    v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
    r = (((v + (v >> 4)) & 0xF0F0F0F) * 0x1010101) >> 24;
    return r;
}

// find appoximation to quotient of a/m
// Out by at most 2.
// Note that MAXXES is bounded to be 2-bits less than half a word
static int quo(BIG_384_58 n, BIG_384_58 m)
{
    int sh;
    chunk num, den;
    int hb = CHUNK / 2;
    if (TBITS_BLS12381 < hb)
    {
        sh = hb - TBITS_BLS12381;
        num = (n[NLEN_384_58 - 1] << sh) | (n[NLEN_384_58 - 2] >> (BASEBITS_384_58 - sh));
        den = (m[NLEN_384_58 - 1] << sh) | (m[NLEN_384_58 - 2] >> (BASEBITS_384_58 - sh));
    }
    else
    {
        num = n[NLEN_384_58 - 1];
        den = m[NLEN_384_58 - 1];
    }
    return (int)(num / (den + 1));
}

/* SU= 48 */
/* Fully reduce a mod Modulus */
void FP_BLS12381_reduce(FP_BLS12381 *a)
{
    BIG_384_58 m, r;
    int sr, sb, q;
    chunk carry;

    BIG_384_58_rcopy(m, Modulus_BLS12381);

    BIG_384_58_norm(a->g);

    if (a->XES > 16)
    {
        q = quo(a->g, m);
        carry = BIG_384_58_pmul(r, m, q);
        r[NLEN_384_58 - 1] += (carry << BASEBITS_384_58); // correction - put any carry out back in again
        BIG_384_58_sub(a->g, a->g, r);
        BIG_384_58_norm(a->g);
        sb = 2;
    }
    else sb = logb2(a->XES - 1); // sb does not depend on the actual data

    BIG_384_58_fshl(m, sb);

    while (sb > 0)
    {
// constant time...
        sr = BIG_384_58_ssn(r, a->g, m); // optimized combined shift, subtract and norm
        BIG_384_58_cmove(a->g, r, 1 - sr);
        sb--;
    }

    //BIG_384_58_mod(a->g,m);
    a->XES = 1;
}

void FP_BLS12381_norm(FP_BLS12381 *x)
{
    BIG_384_58_norm(x->g);
}

/* Set r=-a mod Modulus */
/* SU= 64 */
void FP_BLS12381_neg(FP_BLS12381 *r, FP_BLS12381 *a)
{
    int sb;
    BIG_384_58 m;

    BIG_384_58_rcopy(m, Modulus_BLS12381);

    sb = logb2(a->XES - 1);
    BIG_384_58_fshl(m, sb);
    BIG_384_58_sub(r->g, m, a->g);
    r->XES = ((sign32)1 << sb) + 1;

    if (r->XES > FEXCESS_BLS12381)
    {
#ifdef DEBUG_REDUCE
        printf("Negation too large -  reducing it \n");
#endif
        FP_BLS12381_reduce(r);
    }

}

/* Set r=a/2. */
/* SU= 56 */
void FP_BLS12381_div2(FP_BLS12381 *r, FP_BLS12381 *a)
{
    BIG_384_58 m;
    BIG_384_58 w;
    BIG_384_58_rcopy(m, Modulus_BLS12381);
    int pr=BIG_384_58_parity(a->g);

    FP_BLS12381_copy(r, a);
    BIG_384_58_copy(w,r->g);
    BIG_384_58_fshr(r->g,1);
    BIG_384_58_add(w, w, m);
    BIG_384_58_norm(w);
    BIG_384_58_fshr(w, 1);   
    
    BIG_384_58_cmove(r->g,w,pr);

}


void FP_BLS12381_pow(FP_BLS12381 *r, FP_BLS12381 *a, BIG_384_58 b)
{
    sign8 w[1 + (NLEN_384_58 * BASEBITS_384_58 + 3) / 4];
    FP_BLS12381 tb[16];
    BIG_384_58 t;
    int i, nb;

    FP_BLS12381_copy(r,a);
    FP_BLS12381_norm(r);
    BIG_384_58_copy(t, b);
    BIG_384_58_norm(t);
    nb = 1 + (BIG_384_58_nbits(t) + 3) / 4;
    /* convert exponent to 4-bit window */
    for (i = 0; i < nb; i++)
    {
        w[i] = BIG_384_58_lastbits(t, 4);
        BIG_384_58_dec(t, w[i]);
        BIG_384_58_norm(t);
        BIG_384_58_fshr(t, 4);
    }

    FP_BLS12381_one(&tb[0]);
    FP_BLS12381_copy(&tb[1], r);
    for (i = 2; i < 16; i++)
        FP_BLS12381_mul(&tb[i], &tb[i - 1], r);

    FP_BLS12381_copy(r, &tb[w[nb - 1]]);
    for (i = nb - 2; i >= 0; i--)
    {
        FP_BLS12381_sqr(r, r);
        FP_BLS12381_sqr(r, r);
        FP_BLS12381_sqr(r, r);
        FP_BLS12381_sqr(r, r);
        FP_BLS12381_mul(r, r, &tb[w[i]]);
    }
    FP_BLS12381_reduce(r);
}


#if MODTYPE_BLS12381 == PSEUDO_MERSENNE || MODTYPE_BLS12381==GENERALISED_MERSENNE

// See eprint paper https://eprint.iacr.org/2018/1038
// If p=3 mod 4 r= x^{(p-3)/4}, if p=5 mod 8 r=x^{(p-5)/8}

static void FP_BLS12381_fpow(FP_BLS12381 *r, FP_BLS12381 *x)
{
    int i, j, k, bw, w, c, nw, lo, m, n, nd, e=PM1D2_BLS12381;
    FP_BLS12381 xp[11], t, key;
    const int ac[] = {1, 2, 3, 6, 12, 15, 30, 60, 120, 240, 255};
// phase 1
    FP_BLS12381_copy(&xp[0], x); // 1
    FP_BLS12381_sqr(&xp[1], x); // 2
    FP_BLS12381_mul(&xp[2], &xp[1], x); //3
    FP_BLS12381_sqr(&xp[3], &xp[2]); // 6
    FP_BLS12381_sqr(&xp[4], &xp[3]); // 12
    FP_BLS12381_mul(&xp[5], &xp[4], &xp[2]); // 15
    FP_BLS12381_sqr(&xp[6], &xp[5]); // 30
    FP_BLS12381_sqr(&xp[7], &xp[6]); // 60
    FP_BLS12381_sqr(&xp[8], &xp[7]); // 120
    FP_BLS12381_sqr(&xp[9], &xp[8]); // 240
    FP_BLS12381_mul(&xp[10], &xp[9], &xp[5]); // 255

#if MODTYPE_BLS12381==PSEUDO_MERSENNE
    n = MODBITS_BLS12381;
#endif
#if MODTYPE_BLS12381==GENERALISED_MERSENNE  // Goldilocks ONLY
    n = MODBITS_BLS12381 / 2;
#endif

    n-=(e+1);
    c=(MConst_BLS12381+(1<<e)+1)/(1<<(e+1));

// need c to be odd
    nd=0;
    while (c%2==0)
    {
        c/=2;
        n-=1;
        nd++;
    }

    bw = 0; w = 1; while (w < c) {w *= 2; bw += 1;}
    k = w - c;

    if (k != 0)
    {
        i = 10; while (ac[i] > k) i--;
        FP_BLS12381_copy(&key, &xp[i]);
        k -= ac[i];
    }
    while (k != 0)
    {
        i--;
        if (ac[i] > k) continue;
        FP_BLS12381_mul(&key, &key, &xp[i]);
        k -= ac[i];
    }

// phase 2
    FP_BLS12381_copy(&xp[1], &xp[2]);
    FP_BLS12381_copy(&xp[2], &xp[5]);
    FP_BLS12381_copy(&xp[3], &xp[10]);

    j = 3; m = 8;
    nw = n - bw;
    while (2 * m < nw)
    {
        FP_BLS12381_copy(&t, &xp[j++]);
        for (i = 0; i < m; i++)
            FP_BLS12381_sqr(&t, &t);
        FP_BLS12381_mul(&xp[j], &xp[j - 1], &t);
        m *= 2;
    }

    lo = nw - m;
    FP_BLS12381_copy(r, &xp[j]);

    while (lo != 0)
    {
        m /= 2; j--;
        if (lo < m) continue;
        lo -= m;
        FP_BLS12381_copy(&t, r);
        for (i = 0; i < m; i++)
            FP_BLS12381_sqr(&t, &t);
        FP_BLS12381_mul(r, &t, &xp[j]);
    }
// phase 3

    if (bw != 0)
    {
        for (i = 0; i < bw; i++ )
            FP_BLS12381_sqr(r, r);
        FP_BLS12381_mul(r, r, &key);
    }
#if MODTYPE_BLS12381==GENERALISED_MERSENNE  // Goldilocks ONLY
    FP_BLS12381_copy(&key, r);
    FP_BLS12381_sqr(&t, &key);
    FP_BLS12381_mul(r, &t, &xp[0]);
    for (i = 0; i < n + 1; i++)
        FP_BLS12381_sqr(r, r);
    FP_BLS12381_mul(r, r, &key);
#endif

    for (i=0;i<nd;i++)
        FP_BLS12381_sqr(r,r);
}

#endif


// calculates r=x^(p-1-2^e)/2^{e+1) where 2^e|p-1
void FP_BLS12381_progen(FP_BLS12381 *r,FP_BLS12381 *x)
{
#if MODTYPE_BLS12381==PSEUDO_MERSENNE  || MODTYPE_BLS12381==GENERALISED_MERSENNE
    FP_BLS12381_fpow(r, x);  
#else
    int e=PM1D2_BLS12381;
    BIG_384_58 m;
    BIG_384_58_rcopy(m, Modulus_BLS12381);
    BIG_384_58_dec(m,1);
    BIG_384_58_shr(m,e);
    BIG_384_58_dec(m,1);
    BIG_384_58_fshr(m,1);
    FP_BLS12381_pow(r,x,m);
#endif
}

/* Is x a QR? return optional hint for fast follow-up square root */
int FP_BLS12381_qr(FP_BLS12381 *x,FP_BLS12381 *h)
{
    FP_BLS12381 r;
    int i,e=PM1D2_BLS12381;
    FP_BLS12381_progen(&r,x);
    if (h!=NULL)
        FP_BLS12381_copy(h,&r);

    FP_BLS12381_sqr(&r,&r);
    FP_BLS12381_mul(&r,x,&r);
    for (i=0;i<e-1;i++ )
        FP_BLS12381_sqr(&r,&r);


//    for (i=0;i<e;i++)
//        FP_BLS12381_sqr(&r,&r);
//    FP_BLS12381_copy(&s,x);
//    for (i=0;i<e-1;i++ )
//        FP_BLS12381_sqr(&s,&s);
//    FP_BLS12381_mul(&r,&r,&s);
    
    return FP_BLS12381_isunity(&r);
}

/* Modular inversion */
void FP_BLS12381_inv(FP_BLS12381 *r,FP_BLS12381 *x,FP_BLS12381 *h)
{
    int i,e=PM1D2_BLS12381;
    FP_BLS12381 s,t;
    FP_BLS12381_norm(x);
    FP_BLS12381_copy(&s,x);

    if (h==NULL)
        FP_BLS12381_progen(&t,x);
    else
        FP_BLS12381_copy(&t,h);

    for (i=0;i<e-1;i++)
    {  
        FP_BLS12381_sqr(&s,&s);
        FP_BLS12381_mul(&s,&s,x);
    }
  
    for (i=0;i<=e;i++)
        FP_BLS12381_sqr(&t,&t);
    
    FP_BLS12381_mul(r,&t,&s);
    FP_BLS12381_reduce(r);
}

// Tonelli-Shanks in constant time
void FP_BLS12381_sqrt(FP_BLS12381 *r, FP_BLS12381 *a, FP_BLS12381 *h)
{
    int i,j,k,u,e=PM1D2_BLS12381;
    FP_BLS12381 v,g,t,b;
    BIG_384_58 m;

    if (h==NULL)
        FP_BLS12381_progen(&g,a);
    else
        FP_BLS12381_copy(&g,h);

    BIG_384_58_rcopy(m,ROI_BLS12381);
    FP_BLS12381_nres(&v,m);

    FP_BLS12381_sqr(&t,&g);
    FP_BLS12381_mul(&t,&t,a);
   
    FP_BLS12381_mul(r,&g,a);
    FP_BLS12381_copy(&b,&t);
    for (k=e;k>1;k--)
    {
        for (j=1;j<k-1;j++)
            FP_BLS12381_sqr(&b,&b);
        u=1-FP_BLS12381_isunity(&b);
        FP_BLS12381_mul(&g,r,&v);
        FP_BLS12381_cmove(r,&g,u);
        FP_BLS12381_sqr(&v,&v);
        FP_BLS12381_mul(&g,&t,&v);
        FP_BLS12381_cmove(&t,&g,u);
        FP_BLS12381_copy(&b,&t);
    }
// always return +ve square root
    k=FP_BLS12381_sign(r);
    FP_BLS12381_neg(&v,r); FP_BLS12381_norm(&v);
    FP_BLS12381_cmove(r,&v,k);
}

// Calculate both inverse and square root of x, return QR
int FP_BLS12381_invsqrt(FP_BLS12381 *i, FP_BLS12381 *s, FP_BLS12381 *x)
{
    FP_BLS12381 h;
    int qr=FP_BLS12381_qr(x,&h);
    FP_BLS12381_sqrt(s,x,&h);
    FP_BLS12381_inv(i,x,&h);
    return qr;
}

// Two for Price of One - See Hamburg https://eprint.iacr.org/2012/309.pdf
// Calculate inverse of i and square root of s, return QR
int FP_BLS12381_tpo(FP_BLS12381 *i, FP_BLS12381 *s)
{
    int qr;
    FP_BLS12381 w,t;
    FP_BLS12381_mul(&w,s,i);
    FP_BLS12381_mul(&t,&w,i);
    qr=FP_BLS12381_invsqrt(i,s,&t);
    FP_BLS12381_mul(i,i,&w);
    FP_BLS12381_mul(s,s,i);
    return qr;
}

/* SU=8 */
/* set n=1 */
void FP_BLS12381_one(FP_BLS12381 *n)
{
    BIG_384_58 b;
    BIG_384_58_one(b);
    FP_BLS12381_nres(n, b);
}

int FP_BLS12381_sign(FP_BLS12381 *x)
{
#ifdef BIG_ENDIAN_SIGN_BLS12381
    int cp;
    BIG_384_58 m,pm1d2;
    FP_BLS12381 y;
    BIG_384_58_rcopy(pm1d2, Modulus_BLS12381);
    BIG_384_58_dec(pm1d2,1);
    BIG_384_58_fshr(pm1d2,1); //(p-1)/2
     
    FP_BLS12381_copy(&y,x);
    FP_BLS12381_reduce(&y);
    FP_BLS12381_redc(m,&y);
    cp=BIG_384_58_comp(m,pm1d2);
    return ((cp+1)&2)>>1;

#else
    BIG_384_58 m;
    FP_BLS12381 y;
    FP_BLS12381_copy(&y,x);
    FP_BLS12381_reduce(&y);
    FP_BLS12381_redc(m,&y);
    return BIG_384_58_parity(m);
#endif
}

void FP_BLS12381_rand(FP_BLS12381 *x,csprng *rng)
{
    BIG_384_58 w,m;
    BIG_384_58_rcopy(m,Modulus_BLS12381);
    BIG_384_58_randomnum(w,m,rng);
    FP_BLS12381_nres(x,w);
}


