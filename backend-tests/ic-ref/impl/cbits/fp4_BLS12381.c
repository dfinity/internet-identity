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

/* CORE Fp^4 functions */
/* SU=m, m is Stack Usage (no lazy )*/

/* FP4 elements are of the form a+ib, where i is sqrt(-1+sqrt(-1)) */

#include "fp4_BLS12381.h"

/* test x==0 ? */
/* SU= 8 */
int FP4_BLS12381_iszilch(FP4_BLS12381 *x)
{
    return (FP2_BLS12381_iszilch(&(x->a)) & FP2_BLS12381_iszilch(&(x->b)));
}

/* test x==1 ? */
/* SU= 8 */
int FP4_BLS12381_isunity(FP4_BLS12381 *x)
{
    return (FP2_BLS12381_isunity(&(x->a)) & FP2_BLS12381_iszilch(&(x->b)));
}

/* test is w real? That is in a+ib test b is zero */
int FP4_BLS12381_isreal(FP4_BLS12381 *w)
{
    return FP2_BLS12381_iszilch(&(w->b));
}

/* return 1 if x==y, else 0 */
/* SU= 16 */
int FP4_BLS12381_equals(FP4_BLS12381 *x, FP4_BLS12381 *y)
{
    return (FP2_BLS12381_equals(&(x->a), &(y->a)) & FP2_BLS12381_equals(&(x->b), &(y->b)));
}

/* set FP4 from two FP2s */
/* SU= 16 */
void FP4_BLS12381_from_FP2s(FP4_BLS12381 *w, FP2_BLS12381 * x, FP2_BLS12381* y)
{
    FP2_BLS12381_copy(&(w->a), x);
    FP2_BLS12381_copy(&(w->b), y);
}

/* set FP4 from FP2 */
/* SU= 8 */
void FP4_BLS12381_from_FP2(FP4_BLS12381 *w, FP2_BLS12381 *x)
{
    FP2_BLS12381_copy(&(w->a), x);
    FP2_BLS12381_zero(&(w->b));
}

/* set high part of FP4 from FP2 */
/* SU= 8 */
void FP4_BLS12381_from_FP2H(FP4_BLS12381 *w, FP2_BLS12381 *x)
{
    FP2_BLS12381_copy(&(w->b), x);
    FP2_BLS12381_zero(&(w->a));
}

/* set FP4 from FP */
void FP4_BLS12381_from_FP(FP4_BLS12381 *w, FP_BLS12381 *x)
{
    FP2_BLS12381 t;
    FP2_BLS12381_from_FP(&t, x);
    FP4_BLS12381_from_FP2(w, &t);
}

/* FP4 copy w=x */
/* SU= 16 */
void FP4_BLS12381_copy(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    if (w == x) return;
    FP2_BLS12381_copy(&(w->a), &(x->a));
    FP2_BLS12381_copy(&(w->b), &(x->b));
}

/* FP4 w=0 */
/* SU= 8 */
void FP4_BLS12381_zero(FP4_BLS12381 *w)
{
    FP2_BLS12381_zero(&(w->a));
    FP2_BLS12381_zero(&(w->b));
}

/* FP4 w=1 */
/* SU= 8 */
void FP4_BLS12381_one(FP4_BLS12381 *w)
{
    FP2_BLS12381_one(&(w->a));
    FP2_BLS12381_zero(&(w->b));
}

int FP4_BLS12381_sign(FP4_BLS12381 *w)
{
    int p1,p2;
    p1=FP2_BLS12381_sign(&(w->a));
    p2=FP2_BLS12381_sign(&(w->b));
#ifdef BIG_ENDIAN_SIGN_BLS12381
    p2 ^= (p1 ^ p2)&FP2_BLS12381_iszilch(&(w->b));
    return p2;
#else
    p1 ^= (p1 ^ p2)&FP2_BLS12381_iszilch(&(w->a));
    return p1;
#endif
}

/* Set w=-x */
/* SU= 160 */
void FP4_BLS12381_neg(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    /* Just one field neg */
    FP2_BLS12381 m, t;
    FP4_BLS12381_norm(x);
    FP2_BLS12381_add(&m, &(x->a), &(x->b));
    FP2_BLS12381_neg(&m, &m);
    FP2_BLS12381_add(&t, &m, &(x->b));
    FP2_BLS12381_add(&(w->b), &m, &(x->a));
    FP2_BLS12381_copy(&(w->a), &t);
    FP4_BLS12381_norm(w);
}

/* Set w=conj(x) */
/* SU= 16 */
void FP4_BLS12381_conj(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    FP2_BLS12381_copy(&(w->a), &(x->a));
    FP2_BLS12381_neg(&(w->b), &(x->b));
    FP4_BLS12381_norm(w);
}

/* Set w=-conj(x) */
/* SU= 16 */
void FP4_BLS12381_nconj(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    FP2_BLS12381_copy(&(w->b), &(x->b));
    FP2_BLS12381_neg(&(w->a), &(x->a));
    FP4_BLS12381_norm(w);
}

/* Set w=x+y */
/* SU= 16 */
void FP4_BLS12381_add(FP4_BLS12381 *w, FP4_BLS12381 *x, FP4_BLS12381 *y)
{
    FP2_BLS12381_add(&(w->a), &(x->a), &(y->a));
    FP2_BLS12381_add(&(w->b), &(x->b), &(y->b));
}

/* Set w=x-y */
/* Input y MUST be normed */
void FP4_BLS12381_sub(FP4_BLS12381 *w, FP4_BLS12381 *x, FP4_BLS12381 *y)
{
    FP4_BLS12381 my;
    FP4_BLS12381_neg(&my, y);
    FP4_BLS12381_add(w, x, &my);
}
/* SU= 8 */
/* reduce all components of w mod Modulus */
void FP4_BLS12381_reduce(FP4_BLS12381 *w)
{
    FP2_BLS12381_reduce(&(w->a));
    FP2_BLS12381_reduce(&(w->b));
}

/* SU= 8 */
/* normalise all elements of w */
void FP4_BLS12381_norm(FP4_BLS12381 *w)
{
    FP2_BLS12381_norm(&(w->a));
    FP2_BLS12381_norm(&(w->b));
}

/* Set w=s*x, where s is FP2 */
/* SU= 16 */
void FP4_BLS12381_pmul(FP4_BLS12381 *w, FP4_BLS12381 *x, FP2_BLS12381 *s)
{
    FP2_BLS12381_mul(&(w->a), &(x->a), s);
    FP2_BLS12381_mul(&(w->b), &(x->b), s);
}

/* Set w=s*x, where s is FP */
void FP4_BLS12381_qmul(FP4_BLS12381 *w, FP4_BLS12381 *x, FP_BLS12381 *s)
{
    FP2_BLS12381_pmul(&(w->a), &(x->a), s);
    FP2_BLS12381_pmul(&(w->b), &(x->b), s);
}

/* SU= 16 */
/* Set w=s*x, where s is int */
void FP4_BLS12381_imul(FP4_BLS12381 *w, FP4_BLS12381 *x, int s)
{
    FP2_BLS12381_imul(&(w->a), &(x->a), s);
    FP2_BLS12381_imul(&(w->b), &(x->b), s);
}

/* Set w=x^2 */
/* Input MUST be normed  */
void FP4_BLS12381_sqr(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    FP2_BLS12381 t1, t2, t3;

    FP2_BLS12381_mul(&t3, &(x->a), &(x->b)); /* norms x */
    FP2_BLS12381_copy(&t2, &(x->b));
    FP2_BLS12381_add(&t1, &(x->a), &(x->b));
    FP2_BLS12381_mul_ip(&t2);

    FP2_BLS12381_add(&t2, &(x->a), &t2);

    FP2_BLS12381_norm(&t1);  // 2
    FP2_BLS12381_norm(&t2);  // 2

    FP2_BLS12381_mul(&(w->a), &t1, &t2);

    FP2_BLS12381_copy(&t2, &t3);
    FP2_BLS12381_mul_ip(&t2);

    FP2_BLS12381_add(&t2, &t2, &t3);

    FP2_BLS12381_norm(&t2);  // 2
    FP2_BLS12381_neg(&t2, &t2);
    FP2_BLS12381_add(&(w->a), &(w->a), &t2); /* a=(a+b)(a+i^2.b)-i^2.ab-ab = a*a+ib*ib */
    FP2_BLS12381_add(&(w->b), &t3, &t3); /* b=2ab */

    FP4_BLS12381_norm(w);
}

/* Set w=x*y */
/* Inputs MUST be normed  */
void FP4_BLS12381_mul(FP4_BLS12381 *w, FP4_BLS12381 *x, FP4_BLS12381 *y)
{

    FP2_BLS12381 t1, t2, t3, t4;
    FP2_BLS12381_mul(&t1, &(x->a), &(y->a));
    FP2_BLS12381_mul(&t2, &(x->b), &(y->b));

    FP2_BLS12381_add(&t3, &(y->b), &(y->a));
    FP2_BLS12381_add(&t4, &(x->b), &(x->a));

    FP2_BLS12381_norm(&t4); // 2
    FP2_BLS12381_norm(&t3); // 2

    FP2_BLS12381_mul(&t4, &t4, &t3); /* (xa+xb)(ya+yb) */

    FP2_BLS12381_neg(&t3, &t1); // 1
    FP2_BLS12381_add(&t4, &t4, &t3); //t4E=3
    FP2_BLS12381_norm(&t4);

    FP2_BLS12381_neg(&t3, &t2); // 1
    FP2_BLS12381_add(&(w->b), &t4, &t3); //wbE=3

    FP2_BLS12381_mul_ip(&t2);
    FP2_BLS12381_add(&(w->a), &t2, &t1);

    FP4_BLS12381_norm(w);
}

/* output FP4 in format [a,b] */
/* SU= 8 */
void FP4_BLS12381_output(FP4_BLS12381 *w)
{
    printf("[");
    FP2_BLS12381_output(&(w->a));
    printf(",");
    FP2_BLS12381_output(&(w->b));
    printf("]");
}

/* SU= 8 */
void FP4_BLS12381_rawoutput(FP4_BLS12381 *w)
{
    printf("[");
    FP2_BLS12381_rawoutput(&(w->a));
    printf(",");
    FP2_BLS12381_rawoutput(&(w->b));
    printf("]");
}

/* Set w=1/x */
/* SU= 160 */
void FP4_BLS12381_inv(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    FP2_BLS12381 t1, t2;
    FP2_BLS12381_sqr(&t1, &(x->a));
    FP2_BLS12381_sqr(&t2, &(x->b));
    FP2_BLS12381_mul_ip(&t2);
    FP2_BLS12381_norm(&t2);
    FP2_BLS12381_sub(&t1, &t1, &t2);
    FP2_BLS12381_inv(&t1, &t1);
    FP2_BLS12381_mul(&(w->a), &t1, &(x->a));
    FP2_BLS12381_neg(&t1, &t1);
    FP2_BLS12381_norm(&t1);
    FP2_BLS12381_mul(&(w->b), &t1, &(x->b));
}

/* w*=i where i = sqrt(1+sqrt(-1)) */
/* SU= 200 */
void FP4_BLS12381_times_i(FP4_BLS12381 *w)
{
    FP2_BLS12381 t;
    FP2_BLS12381_copy(&t, &(w->b));
    FP2_BLS12381_copy(&(w->b), &(w->a));
    FP2_BLS12381_mul_ip(&t);
    FP2_BLS12381_copy(&(w->a), &t);
    FP4_BLS12381_norm(w);
#if TOWER_BLS12381 == POSITOWER
    FP4_BLS12381_neg(w, w);  // ***
    FP4_BLS12381_norm(w);
#endif
}

/* Set w=w^p using Frobenius */
/* SU= 16 */
void FP4_BLS12381_frob(FP4_BLS12381 *w, FP2_BLS12381 *f)
{
    FP2_BLS12381_conj(&(w->a), &(w->a));
    FP2_BLS12381_conj(&(w->b), &(w->b));
    FP2_BLS12381_mul( &(w->b), f, &(w->b));
}

/* Set r=a^b mod m */
/* SU= 240 */
/*
void FP4_BLS12381_pow(FP4_BLS12381 *r, FP4_BLS12381* a, BIG_384_58 b)
{
    FP4_BLS12381 w;
    BIG_384_58 z, zilch;
    int bt;

    BIG_384_58_zero(zilch);

    BIG_384_58_copy(z, b);
    BIG_384_58_norm(z);
    FP4_BLS12381_copy(&w, a);
    FP4_BLS12381_norm(&w);
    FP4_BLS12381_one(r);

    while (1)
    {
        bt = BIG_384_58_parity(z);
        BIG_384_58_shr(z, 1);
        if (bt) FP4_BLS12381_mul(r, r, &w);
        if (BIG_384_58_comp(z, zilch) == 0) break;
        FP4_BLS12381_sqr(&w, &w);
    }
    FP4_BLS12381_reduce(r);
} */

/* SU= 304 */
/* XTR xtr_a function */
void FP4_BLS12381_xtr_A(FP4_BLS12381 *r, FP4_BLS12381 *w, FP4_BLS12381 *x, FP4_BLS12381 *y, FP4_BLS12381 *z)
{
    FP4_BLS12381 t1, t2;
    FP4_BLS12381_copy(r, x);
    FP4_BLS12381_sub(&t1, w, y);
    FP4_BLS12381_norm(&t1);
    FP4_BLS12381_pmul(&t1, &t1, &(r->a));
    FP4_BLS12381_add(&t2, w, y);
    FP4_BLS12381_norm(&t2);
    FP4_BLS12381_pmul(&t2, &t2, &(r->b));
    FP4_BLS12381_times_i(&t2);

    FP4_BLS12381_add(r, &t1, &t2);
    FP4_BLS12381_add(r, r, z);

    FP4_BLS12381_reduce(r);
}

/* SU= 152 */
/* XTR xtr_d function */
void FP4_BLS12381_xtr_D(FP4_BLS12381 *r, FP4_BLS12381 *x)
{
    FP4_BLS12381 w;
    FP4_BLS12381_copy(r, x);
    FP4_BLS12381_conj(&w, r);
    FP4_BLS12381_add(&w, &w, &w);
    FP4_BLS12381_sqr(r, r);
    FP4_BLS12381_norm(&w);
    FP4_BLS12381_sub(r, r, &w);
    FP4_BLS12381_reduce(r);    /* reduce here as multiple calls trigger automatic reductions */
}

/* SU= 728 */
/* r=x^n using XTR method on traces of FP12s */
void FP4_BLS12381_xtr_pow(FP4_BLS12381 *r, FP4_BLS12381 *x, BIG_384_58 n)
{
    int i, par, nb;
    BIG_384_58 v;
    FP2_BLS12381 w;
    FP4_BLS12381 t, a, b, c, sf;

    BIG_384_58_zero(v);
    BIG_384_58_inc(v, 3);
    BIG_384_58_norm(v);
    FP2_BLS12381_from_BIG(&w, v);
    FP4_BLS12381_from_FP2(&a, &w);

    FP4_BLS12381_copy(&sf, x);
    FP4_BLS12381_norm(&sf);
    FP4_BLS12381_copy(&b, &sf);
    FP4_BLS12381_xtr_D(&c, &sf);

    par = BIG_384_58_parity(n);
    BIG_384_58_copy(v, n);
    BIG_384_58_norm(v);
    BIG_384_58_shr(v, 1);
    if (par == 0)
    {
        BIG_384_58_dec(v, 1);
        BIG_384_58_norm(v);
    }

    nb = BIG_384_58_nbits(v);
    for (i = nb - 1; i >= 0; i--)
    {
        if (!BIG_384_58_bit(v, i))
        {
            FP4_BLS12381_copy(&t, &b);
            FP4_BLS12381_conj(&sf, &sf);
            FP4_BLS12381_conj(&c, &c);
            FP4_BLS12381_xtr_A(&b, &a, &b, &sf, &c);
            FP4_BLS12381_conj(&sf, &sf);
            FP4_BLS12381_xtr_D(&c, &t);
            FP4_BLS12381_xtr_D(&a, &a);
        }
        else
        {
            FP4_BLS12381_conj(&t, &a);
            FP4_BLS12381_xtr_D(&a, &b);
            FP4_BLS12381_xtr_A(&b, &c, &b, &sf, &t);
            FP4_BLS12381_xtr_D(&c, &c);
        }
    }

    if (par == 0) FP4_BLS12381_copy(r, &c);
    else FP4_BLS12381_copy(r, &b);
    FP4_BLS12381_reduce(r);
}

/* SU= 872 */
/* r=ck^a.cl^n using XTR double exponentiation method on traces of FP12s. See Stam thesis. */
void FP4_BLS12381_xtr_pow2(FP4_BLS12381 *r, FP4_BLS12381 *ck, FP4_BLS12381 *cl, FP4_BLS12381 *ckml, FP4_BLS12381 *ckm2l, BIG_384_58 a, BIG_384_58 b)
{
    int i, f2;
    BIG_384_58 d, e, w;
    FP4_BLS12381 t, cu, cv, cumv, cum2v;


    BIG_384_58_copy(e, a);
    BIG_384_58_copy(d, b);
    BIG_384_58_norm(e);
    BIG_384_58_norm(d);
    FP4_BLS12381_copy(&cu, ck);
    FP4_BLS12381_copy(&cv, cl);
    FP4_BLS12381_copy(&cumv, ckml);
    FP4_BLS12381_copy(&cum2v, ckm2l);

    f2 = 0;
    while (BIG_384_58_parity(d) == 0 && BIG_384_58_parity(e) == 0)
    {
        BIG_384_58_shr(d, 1);
        BIG_384_58_shr(e, 1);
        f2++;
    }
    while (BIG_384_58_comp(d, e) != 0)
    {
        if (BIG_384_58_comp(d, e) > 0)
        {
            BIG_384_58_imul(w, e, 4);
            BIG_384_58_norm(w);
            if (BIG_384_58_comp(d, w) <= 0)
            {
                BIG_384_58_copy(w, d);
                BIG_384_58_copy(d, e);
                BIG_384_58_sub(e, w, e);
                BIG_384_58_norm(e);
                FP4_BLS12381_xtr_A(&t, &cu, &cv, &cumv, &cum2v);
                FP4_BLS12381_conj(&cum2v, &cumv);
                FP4_BLS12381_copy(&cumv, &cv);
                FP4_BLS12381_copy(&cv, &cu);
                FP4_BLS12381_copy(&cu, &t);
            }
            else if (BIG_384_58_parity(d) == 0)
            {
                BIG_384_58_shr(d, 1);
                FP4_BLS12381_conj(r, &cum2v);
                FP4_BLS12381_xtr_A(&t, &cu, &cumv, &cv, r);
                FP4_BLS12381_xtr_D(&cum2v, &cumv);
                FP4_BLS12381_copy(&cumv, &t);
                FP4_BLS12381_xtr_D(&cu, &cu);
            }
            else if (BIG_384_58_parity(e) == 1)
            {
                BIG_384_58_sub(d, d, e);
                BIG_384_58_norm(d);
                BIG_384_58_shr(d, 1);
                FP4_BLS12381_xtr_A(&t, &cu, &cv, &cumv, &cum2v);
                FP4_BLS12381_xtr_D(&cu, &cu);
                FP4_BLS12381_xtr_D(&cum2v, &cv);
                FP4_BLS12381_conj(&cum2v, &cum2v);
                FP4_BLS12381_copy(&cv, &t);
            }
            else
            {
                BIG_384_58_copy(w, d);
                BIG_384_58_copy(d, e);
                BIG_384_58_shr(d, 1);
                BIG_384_58_copy(e, w);
                FP4_BLS12381_xtr_D(&t, &cumv);
                FP4_BLS12381_conj(&cumv, &cum2v);
                FP4_BLS12381_conj(&cum2v, &t);
                FP4_BLS12381_xtr_D(&t, &cv);
                FP4_BLS12381_copy(&cv, &cu);
                FP4_BLS12381_copy(&cu, &t);
            }
        }
        if (BIG_384_58_comp(d, e) < 0)
        {
            BIG_384_58_imul(w, d, 4);
            BIG_384_58_norm(w);
            if (BIG_384_58_comp(e, w) <= 0)
            {
                BIG_384_58_sub(e, e, d);
                BIG_384_58_norm(e);
                FP4_BLS12381_xtr_A(&t, &cu, &cv, &cumv, &cum2v);
                FP4_BLS12381_copy(&cum2v, &cumv);
                FP4_BLS12381_copy(&cumv, &cu);
                FP4_BLS12381_copy(&cu, &t);
            }
            else if (BIG_384_58_parity(e) == 0)
            {
                BIG_384_58_copy(w, d);
                BIG_384_58_copy(d, e);
                BIG_384_58_shr(d, 1);
                BIG_384_58_copy(e, w);
                FP4_BLS12381_xtr_D(&t, &cumv);
                FP4_BLS12381_conj(&cumv, &cum2v);
                FP4_BLS12381_conj(&cum2v, &t);
                FP4_BLS12381_xtr_D(&t, &cv);
                FP4_BLS12381_copy(&cv, &cu);
                FP4_BLS12381_copy(&cu, &t);
            }
            else if (BIG_384_58_parity(d) == 1)
            {
                BIG_384_58_copy(w, e);
                BIG_384_58_copy(e, d);
                BIG_384_58_sub(w, w, d);
                BIG_384_58_norm(w);
                BIG_384_58_copy(d, w);
                BIG_384_58_shr(d, 1);
                FP4_BLS12381_xtr_A(&t, &cu, &cv, &cumv, &cum2v);
                FP4_BLS12381_conj(&cumv, &cumv);
                FP4_BLS12381_xtr_D(&cum2v, &cu);
                FP4_BLS12381_conj(&cum2v, &cum2v);
                FP4_BLS12381_xtr_D(&cu, &cv);
                FP4_BLS12381_copy(&cv, &t);
            }
            else
            {
                BIG_384_58_shr(d, 1);
                FP4_BLS12381_conj(r, &cum2v);
                FP4_BLS12381_xtr_A(&t, &cu, &cumv, &cv, r);
                FP4_BLS12381_xtr_D(&cum2v, &cumv);
                FP4_BLS12381_copy(&cumv, &t);
                FP4_BLS12381_xtr_D(&cu, &cu);
            }
        }
    }
    FP4_BLS12381_xtr_A(r, &cu, &cv, &cumv, &cum2v);
    for (i = 0; i < f2; i++)    FP4_BLS12381_xtr_D(r, r);
    FP4_BLS12381_xtr_pow(r, r, d);
}

/* Move b to a if d=1 */
void FP4_BLS12381_cmove(FP4_BLS12381 *f, FP4_BLS12381 *g, int d)
{
    FP2_BLS12381_cmove(&(f->a), &(g->a), d);
    FP2_BLS12381_cmove(&(f->b), &(g->b), d);
}

/* New stuff for ECp4 support */

/* Set w=x/2 */
void FP4_BLS12381_div2(FP4_BLS12381 *w, FP4_BLS12381 *x)
{
    FP2_BLS12381_div2(&(w->a), &(x->a));
    FP2_BLS12381_div2(&(w->b), &(x->b));
}

void FP4_BLS12381_rand(FP4_BLS12381 *x,csprng *rng)
{
    FP2_BLS12381_rand(&(x->a),rng);
    FP2_BLS12381_rand(&(x->b),rng);
}

#if PAIRING_FRIENDLY_BLS12381 >= BLS24_CURVE

/* test for x a QR */
int FP4_BLS12381_qr(FP4_BLS12381 *x)
{ /* test x^(p^4-1)/2 = 1 */
    FP4_BLS12381 c;
    FP4_BLS12381_conj(&c,x);
    FP4_BLS12381_mul(&c,&c,x);

    return FP2_BLS12381_qr(&(c.a));
}

/* sqrt(a+xb) = sqrt((a+sqrt(a*a-n*b*b))/2)+x.b/(2*sqrt((a+sqrt(a*a-n*b*b))/2)) */

void FP4_BLS12381_sqrt(FP4_BLS12381 *r, FP4_BLS12381* x)
{
    FP2_BLS12381 a, b, s, t;
    FP4_BLS12381 nr;
    int sgn;

    FP4_BLS12381_copy(r, x);
    if (FP4_BLS12381_iszilch(x)) return;

    FP2_BLS12381_copy(&a, &(x->a));
    FP2_BLS12381_copy(&s, &(x->b));

    FP2_BLS12381_sqr(&s, &s); // s*=s
    FP2_BLS12381_sqr(&a, &a); // a*=a
    FP2_BLS12381_mul_ip(&s);
    FP2_BLS12381_norm(&s);
    FP2_BLS12381_sub(&a, &a, &s); // a-=txx(s)
    FP2_BLS12381_norm(&a);
    FP2_BLS12381_sqrt(&s, &a);

    FP2_BLS12381_copy(&t, &(x->a));

    FP2_BLS12381_add(&a, &t, &s);
    FP2_BLS12381_norm(&a);
    FP2_BLS12381_div2(&a, &a);

    FP2_BLS12381_sub(&b, &t, &s);
    FP2_BLS12381_norm(&b);
    FP2_BLS12381_div2(&b, &b);

    FP2_BLS12381_cmove(&a,&b,FP2_BLS12381_qr(&b)); // one or the other will be a QR

    FP2_BLS12381_sqrt(&a, &a);
    FP2_BLS12381_copy(&t, &(x->b));
    FP2_BLS12381_add(&s, &a, &a);
    FP2_BLS12381_norm(&s);
    FP2_BLS12381_inv(&s, &s);

    FP2_BLS12381_mul(&t, &t, &s);
    FP4_BLS12381_from_FP2s(r, &a, &t);

    sgn=FP4_BLS12381_sign(r);
    FP4_BLS12381_neg(&nr,r); FP4_BLS12381_norm(&nr);
    FP4_BLS12381_cmove(r,&nr,sgn);
}

void FP4_BLS12381_div_i(FP4_BLS12381 *f)
{
    FP2_BLS12381 u, v;
    FP2_BLS12381_copy(&u, &(f->a));
    FP2_BLS12381_copy(&v, &(f->b));
    FP2_BLS12381_div_ip(&u);
    FP2_BLS12381_copy(&(f->a), &v);
    FP2_BLS12381_copy(&(f->b), &u);
#if TOWER_BLS12381 == POSITOWER
    FP4_BLS12381_neg(f, f);  // ***
    FP4_BLS12381_norm(f);
#endif
}
/*
void FP4_BLS12381_div_2i(FP4_BLS12381 *f)
{
    FP2_BLS12381 u,v;
    FP2_BLS12381_copy(&u,&(f->a));
    FP2_BLS12381_copy(&v,&(f->b));
    FP2_BLS12381_div_ip2(&u);
    FP2_BLS12381_add(&v,&v,&v);
    FP2_BLS12381_norm(&v);
    FP2_BLS12381_copy(&(f->a),&v);
    FP2_BLS12381_copy(&(f->b),&u);
}
*/
#endif
