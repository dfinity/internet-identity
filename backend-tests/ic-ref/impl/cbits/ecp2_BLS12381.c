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

/* CORE Weierstrass elliptic curve functions over FP2 */
/* SU=m, m is Stack Usage */

#include <string.h>
#include "ecp2_BLS12381.h"
#include "ecp_BLS12381.h"

int ECP2_BLS12381_isinf(ECP2_BLS12381 *P)
{
    return (FP2_BLS12381_iszilch(&(P->x)) & FP2_BLS12381_iszilch(&(P->z)));
}

/* Set P=Q */
/* SU= 16 */
void ECP2_BLS12381_copy(ECP2_BLS12381 *P, ECP2_BLS12381 *Q)
{
    FP2_BLS12381_copy(&(P->x), &(Q->x));
    FP2_BLS12381_copy(&(P->y), &(Q->y));
    FP2_BLS12381_copy(&(P->z), &(Q->z));
}

/* set P to Infinity */
/* SU= 8 */
void ECP2_BLS12381_inf(ECP2_BLS12381 *P)
{
    FP2_BLS12381_zero(&(P->x));
    FP2_BLS12381_one(&(P->y));
    FP2_BLS12381_zero(&(P->z));
}

/* Conditional move Q to P dependant on d */
static void ECP2_BLS12381_cmove(ECP2_BLS12381 *P, ECP2_BLS12381 *Q, int d)
{
    FP2_BLS12381_cmove(&(P->x), &(Q->x), d);
    FP2_BLS12381_cmove(&(P->y), &(Q->y), d);
    FP2_BLS12381_cmove(&(P->z), &(Q->z), d);
}

/* return 1 if b==c, no branching */
static int teq(sign32 b, sign32 c)
{
    sign32 x = b ^ c;
    x -= 1; // if x=0, x now -1
    return (int)((x >> 31) & 1);
}

/* Constant time select from pre-computed table */
static void ECP2_BLS12381_select(ECP2_BLS12381 *P, ECP2_BLS12381 W[], sign32 b)
{
    ECP2_BLS12381 MP;
    sign32 m = b >> 31;
    sign32 babs = (b ^ m) - m;

    babs = (babs - 1) / 2;

    ECP2_BLS12381_cmove(P, &W[0], teq(babs, 0)); // conditional move
    ECP2_BLS12381_cmove(P, &W[1], teq(babs, 1));
    ECP2_BLS12381_cmove(P, &W[2], teq(babs, 2));
    ECP2_BLS12381_cmove(P, &W[3], teq(babs, 3));
    ECP2_BLS12381_cmove(P, &W[4], teq(babs, 4));
    ECP2_BLS12381_cmove(P, &W[5], teq(babs, 5));
    ECP2_BLS12381_cmove(P, &W[6], teq(babs, 6));
    ECP2_BLS12381_cmove(P, &W[7], teq(babs, 7));

    ECP2_BLS12381_copy(&MP, P);
    ECP2_BLS12381_neg(&MP);  // minus P
    ECP2_BLS12381_cmove(P, &MP, (int)(m & 1));
}

/* return 1 if P==Q, else 0 */
/* SU= 312 */
int ECP2_BLS12381_equals(ECP2_BLS12381 *P, ECP2_BLS12381 *Q)
{
    FP2_BLS12381 a, b;

    FP2_BLS12381_mul(&a, &(P->x), &(Q->z));
    FP2_BLS12381_mul(&b, &(Q->x), &(P->z));
    if (!FP2_BLS12381_equals(&a, &b)) return 0;

    FP2_BLS12381_mul(&a, &(P->y), &(Q->z));
    FP2_BLS12381_mul(&b, &(Q->y), &(P->z));
    if (!FP2_BLS12381_equals(&a, &b)) return 0;
    return 1;
}

/* Make P affine (so z=1) */
/* SU= 232 */
void ECP2_BLS12381_affine(ECP2_BLS12381 *P)
{
    FP2_BLS12381 one, iz;
    if (ECP2_BLS12381_isinf(P)) return;

    FP2_BLS12381_one(&one);
    if (FP2_BLS12381_isunity(&(P->z)))
    {
        FP2_BLS12381_reduce(&(P->x));
        FP2_BLS12381_reduce(&(P->y));
        return;
    }

    FP2_BLS12381_inv(&iz, &(P->z));
    FP2_BLS12381_mul(&(P->x), &(P->x), &iz);
    FP2_BLS12381_mul(&(P->y), &(P->y), &iz);

    FP2_BLS12381_reduce(&(P->x));
    FP2_BLS12381_reduce(&(P->y));
    FP2_BLS12381_copy(&(P->z), &one);
}

/* extract x, y from point P */
/* SU= 16 */
int ECP2_BLS12381_get(FP2_BLS12381 *x, FP2_BLS12381 *y, ECP2_BLS12381 *P)
{
    ECP2_BLS12381 W;
    ECP2_BLS12381_copy(&W, P);
    ECP2_BLS12381_affine(&W);
    if (ECP2_BLS12381_isinf(&W)) return -1;
    FP2_BLS12381_copy(y, &(W.y));
    FP2_BLS12381_copy(x, &(W.x));
    return 0;
}

/* SU= 152 */
/* Output point P */
void ECP2_BLS12381_output(ECP2_BLS12381 *P)
{
    FP2_BLS12381 x, y;
    if (ECP2_BLS12381_isinf(P))
    {
        printf("Infinity\n");
        return;
    }
    ECP2_BLS12381_get(&x, &y, P);
    printf("(");
    FP2_BLS12381_output(&x);
    printf(",");
    FP2_BLS12381_output(&y);
    printf(")\n");
}

/* SU= 232 */
void ECP2_BLS12381_outputxyz(ECP2_BLS12381 *P)
{
    ECP2_BLS12381 Q;
    if (ECP2_BLS12381_isinf(P))
    {
        printf("Infinity\n");
        return;
    }
    ECP2_BLS12381_copy(&Q, P);
    printf("(");
    FP2_BLS12381_output(&(Q.x));
    printf(",");
    FP2_BLS12381_output(&(Q.y));
    printf(",");
    FP2_BLS12381_output(&(Q.z));
    printf(")\n");
}

/* SU= 168 */
/* Convert Q to octet string */
void ECP2_BLS12381_toOctet(octet *W, ECP2_BLS12381 *Q, bool compress)
{
    BIG_384_58 b;
    FP2_BLS12381 qx, qy;
    ECP2_BLS12381_get(&qx, &qy, Q);

    FP_BLS12381_redc(b, &(qx.a));
    BIG_384_58_toBytes(&(W->val[1]), b);
    FP_BLS12381_redc(b, &(qx.b));
    BIG_384_58_toBytes(&(W->val[MODBYTES_384_58+1]), b);
    if (!compress)
    {
        W->val[0] = 0x04;
        FP_BLS12381_redc(b, &(qy.a));
        BIG_384_58_toBytes(&(W->val[2 * MODBYTES_384_58+1]), b);
        FP_BLS12381_redc(b, &(qy.b));
        BIG_384_58_toBytes(&(W->val[3 * MODBYTES_384_58+1]), b);

        W->len = 4 * MODBYTES_384_58+1;
    } else {
        W->val[0]=0x02;
        if (FP2_BLS12381_sign(&qy)==1) W->val[0] = 0x03;
        W->len = 2 * MODBYTES_384_58 + 1;
    }

}
void ECP2_BLS12381_toOctet_ZCash(octet *W, ECP2_BLS12381 *Q)
{
    BIG_384_58 b;
    FP2_BLS12381 qx, qy, qy_neg;
    ECP2_BLS12381_get(&qx, &qy, Q);

    FP_BLS12381_redc(b, &(qx.b));
    BIG_384_58_toBytes(&(W->val[0]), b);
    FP_BLS12381_redc(b, &(qx.a));
    BIG_384_58_toBytes(&(W->val[MODBYTES_384_58]), b);
    W->len = 2 * MODBYTES_384_58;

    W->val[0] |= 128; // 0x1000 0000: Compressed form

    // is this lexicographic larger, or the negation?
    char t1[2*MODBYTES_384_58];
    octet T1 = {0,sizeof(t1),t1};
    FP_BLS12381_redc(b, &(qy.b));
    BIG_384_58_toBytes(&(T1.val[0]), b);
    FP_BLS12381_redc(b, &(qy.a));
    BIG_384_58_toBytes(&(T1.val[MODBYTES_384_58]), b);

    ECP2_BLS12381 Q_neg;
    ECP2_BLS12381_copy(&Q_neg, Q);
    ECP2_BLS12381_neg(&Q_neg);
    ECP2_BLS12381_get(&qx, &qy_neg, &Q_neg);

    char t2[2*MODBYTES_384_58];
    octet T2 = {0,sizeof(t2),t2};
    FP_BLS12381_redc(b, &(qy_neg.b));
    BIG_384_58_toBytes(&(T2.val[0]), b);
    FP_BLS12381_redc(b, &(qy_neg.a));
    BIG_384_58_toBytes(&(T2.val[MODBYTES_384_58]), b);

    if (memcmp(T1.val, T2.val, 2*MODBYTES_384_58) > 0) {
        W->val[0] |= 32; // 0b0010 0000: It is the larger of the two y
    } else {
    }
}

/* SU= 176 */
/* restore Q from octet string */
int ECP2_BLS12381_fromOctet(ECP2_BLS12381 *Q, octet *W)
{
    BIG_384_58 b;
    FP2_BLS12381 qx, qy;
    int typ = W->val[0];

    BIG_384_58_fromBytes(b, &(W->val[1]));
    FP_BLS12381_nres(&(qx.a), b);
    BIG_384_58_fromBytes(b, &(W->val[MODBYTES_384_58+1]));
    FP_BLS12381_nres(&(qx.b), b);
    if (typ == 0x04)
    {
        BIG_384_58_fromBytes(b, &(W->val[2 * MODBYTES_384_58+1]));
        FP_BLS12381_nres(&(qy.a), b);
        BIG_384_58_fromBytes(b, &(W->val[3 * MODBYTES_384_58+1]));
        FP_BLS12381_nres(&(qy.b), b);

        if (ECP2_BLS12381_set(Q, &qx, &qy)) return 1;
    } else {
        if (ECP2_BLS12381_setx(Q, &qx, typ&1)) return 1;
    }
    return 0;
}

int ECP2_BLS12381_fromOctet_ZCash(ECP2_BLS12381 *Q, octet *W)
{
    BIG_384_58 b;
    FP2_BLS12381 qx;
    if (!(W->val[0] & 128)) return 0; // only accept compressed
    int large = (W->val[0] & 32);
    W->val[0] &= 31; // mask high bits

    BIG_384_58_fromBytes(b, &(W->val[0]));
    FP_BLS12381_nres(&(qx.b), b);
    BIG_384_58_fromBytes(b, &(W->val[MODBYTES_384_58]));
    FP_BLS12381_nres(&(qx.a), b);
    if (!ECP2_BLS12381_setx(Q, &qx, 0)) return 0;

    // try to encode again to find out sign
    // (expensive but easy to implement)
    char t1[2*MODBYTES_384_58];
    octet T1 = {0,sizeof(t1),t1};
    ECP2_BLS12381_toOctet_ZCash(&T1, Q);

    if ((T1.val[0] & 32) != large) {
      // need to negate second component of P
      ECP2_BLS12381_neg(Q);
    }

    return 1;
}


/* SU= 128 */
/* Calculate RHS of twisted curve equation x^3+B/i or x^3+Bi*/
void ECP2_BLS12381_rhs(FP2_BLS12381 *rhs, FP2_BLS12381 *x)
{
    /* calculate RHS of elliptic curve equation */
    FP2_BLS12381 t;
    BIG_384_58 b;
    FP2_BLS12381_sqr(&t, x);

    FP2_BLS12381_mul(rhs, &t, x);

    /* Assuming CURVE_A=0 */

    BIG_384_58_rcopy(b, CURVE_B_BLS12381);

    FP2_BLS12381_from_BIG(&t, b);

#if SEXTIC_TWIST_BLS12381 == D_TYPE
    FP2_BLS12381_div_ip(&t);   /* IMPORTANT - here we use the correct SEXTIC twist of the curve */
#endif

#if SEXTIC_TWIST_BLS12381 == M_TYPE
    FP2_BLS12381_norm(&t);
    FP2_BLS12381_mul_ip(&t);   /* IMPORTANT - here we use the correct SEXTIC twist of the curve */
    FP2_BLS12381_norm(&t);

#endif


    FP2_BLS12381_add(rhs, &t, rhs);
    FP2_BLS12381_reduce(rhs);
}


/* Set P=(x,y). Return 1 if (x,y) is on the curve, else return 0*/
/* SU= 232 */
int ECP2_BLS12381_set(ECP2_BLS12381 *P, FP2_BLS12381 *x, FP2_BLS12381 *y)
{
    FP2_BLS12381 rhs, y2;

    FP2_BLS12381_sqr(&y2, y);
    ECP2_BLS12381_rhs(&rhs, x);

    if (!FP2_BLS12381_equals(&y2, &rhs))
    {
        ECP2_BLS12381_inf(P);
        return 0;
    }

    FP2_BLS12381_copy(&(P->x), x);
    FP2_BLS12381_copy(&(P->y), y);

    FP2_BLS12381_one(&(P->z));
    return 1;
}

/* Set P=(x,y). Return 1 if (x,.) is on the curve, else return 0 */
/* SU= 232 */
int ECP2_BLS12381_setx(ECP2_BLS12381 *P, FP2_BLS12381 *x, int s)
{
    FP2_BLS12381 y;
    ECP2_BLS12381_rhs(&y, x);

    if (!FP2_BLS12381_qr(&y))
    {
        ECP2_BLS12381_inf(P);
        return 0;
    }
    FP2_BLS12381_sqrt(&y, &y);

    FP2_BLS12381_copy(&(P->x), x);
    FP2_BLS12381_copy(&(P->y), &y);
    FP2_BLS12381_one(&(P->z));

    if (FP2_BLS12381_sign(&(P->y)) != s)
        FP2_BLS12381_neg(&(P->y),&(P->y));
    FP2_BLS12381_reduce(&(P->y));
    return 1;
}

/* Set P=-P */
/* SU= 8 */
void ECP2_BLS12381_neg(ECP2_BLS12381 *P)
{
    FP2_BLS12381_norm(&(P->y));
    FP2_BLS12381_neg(&(P->y), &(P->y));
    FP2_BLS12381_norm(&(P->y));
}

/* R+=R */
/* return -1 for Infinity, 0 for addition, 1 for doubling */
/* SU= 448 */
int ECP2_BLS12381_dbl(ECP2_BLS12381 *P)
{
    FP2_BLS12381 t0, t1, t2, iy, x3, y3;

    FP2_BLS12381_copy(&iy, &(P->y));     //FP2 iy=new FP2(y);
#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP2_BLS12381_mul_ip(&iy);            //iy.mul_ip();
    FP2_BLS12381_norm(&iy);              //iy.norm();
#endif
    FP2_BLS12381_sqr(&t0, &(P->y));          //t0.sqr();
#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP2_BLS12381_mul_ip(&t0);            //t0.mul_ip();
#endif
    FP2_BLS12381_mul(&t1, &iy, &(P->z)); //t1.mul(z);
    FP2_BLS12381_sqr(&t2, &(P->z));              //t2.sqr();

    FP2_BLS12381_add(&(P->z), &t0, &t0); //z.add(t0);
    FP2_BLS12381_norm(&(P->z));              //z.norm();
    FP2_BLS12381_add(&(P->z), &(P->z), &(P->z)); //z.add(z);
    FP2_BLS12381_add(&(P->z), &(P->z), &(P->z)); //z.add(z);
    FP2_BLS12381_norm(&(P->z));          //z.norm();

    FP2_BLS12381_imul(&t2, &t2, 3 * CURVE_B_I_BLS12381); //t2.imul(3*ROM.CURVE_B_I);
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_mul_ip(&t2);
    FP2_BLS12381_norm(&t2);
#endif

    FP2_BLS12381_mul(&x3, &t2, &(P->z)); //x3.mul(z);

    FP2_BLS12381_add(&y3, &t0, &t2);     //y3.add(t2);
    FP2_BLS12381_norm(&y3);              //y3.norm();
    FP2_BLS12381_mul(&(P->z), &(P->z), &t1); //z.mul(t1);

    FP2_BLS12381_add(&t1, &t2, &t2);     //t1.add(t2);
    FP2_BLS12381_add(&t2, &t2, &t1);     //t2.add(t1);
    FP2_BLS12381_norm(&t2);              //t2.norm();
    FP2_BLS12381_sub(&t0, &t0, &t2);     //t0.sub(t2);
    FP2_BLS12381_norm(&t0);              //t0.norm();                           //y^2-9bz^2
    FP2_BLS12381_mul(&y3, &y3, &t0);     //y3.mul(t0);
    FP2_BLS12381_add(&(P->y), &y3, &x3);     //y3.add(x3);                          //(y^2+3z*2)(y^2-9z^2)+3b.z^2.8y^2
    FP2_BLS12381_mul(&t1, &(P->x), &iy);     //t1.mul(iy);                       //
    FP2_BLS12381_norm(&t0);          //x.norm();
    FP2_BLS12381_mul(&(P->x), &t0, &t1); //x.mul(t1);
    FP2_BLS12381_add(&(P->x), &(P->x), &(P->x)); //x.add(x);       //(y^2-9bz^2)xy2

    FP2_BLS12381_norm(&(P->x));          //x.norm();
    FP2_BLS12381_norm(&(P->y));          //y.norm();

    return 1;
}

/* Set P+=Q */
/* SU= 400 */
int ECP2_BLS12381_add(ECP2_BLS12381 *P, ECP2_BLS12381 *Q)
{
    FP2_BLS12381 t0, t1, t2, t3, t4, x3, y3, z3;
    int b3 = 3 * CURVE_B_I_BLS12381;

    FP2_BLS12381_mul(&t0, &(P->x), &(Q->x)); //t0.mul(Q.x);         // x.Q.x
    FP2_BLS12381_mul(&t1, &(P->y), &(Q->y)); //t1.mul(Q.y);       // y.Q.y

    FP2_BLS12381_mul(&t2, &(P->z), &(Q->z)); //t2.mul(Q.z);
    FP2_BLS12381_add(&t3, &(P->x), &(P->y)); //t3.add(y);
    FP2_BLS12381_norm(&t3);              //t3.norm();          //t3=X1+Y1

    FP2_BLS12381_add(&t4, &(Q->x), &(Q->y)); //t4.add(Q.y);
    FP2_BLS12381_norm(&t4);              //t4.norm();            //t4=X2+Y2
    FP2_BLS12381_mul(&t3, &t3, &t4);     //t3.mul(t4);                       //t3=(X1+Y1)(X2+Y2)
    FP2_BLS12381_add(&t4, &t0, &t1);     //t4.add(t1);       //t4=X1.X2+Y1.Y2

    FP2_BLS12381_sub(&t3, &t3, &t4);     //t3.sub(t4);
    FP2_BLS12381_norm(&t3);              //t3.norm();
#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP2_BLS12381_mul_ip(&t3);            //t3.mul_ip();
    FP2_BLS12381_norm(&t3);              //t3.norm();         //t3=(X1+Y1)(X2+Y2)-(X1.X2+Y1.Y2) = X1.Y2+X2.Y1
#endif
    FP2_BLS12381_add(&t4, &(P->y), &(P->z)); //t4.add(z);
    FP2_BLS12381_norm(&t4);              //t4.norm();            //t4=Y1+Z1
    FP2_BLS12381_add(&x3, &(Q->y), &(Q->z)); //x3.add(Q.z);
    FP2_BLS12381_norm(&x3);              //x3.norm();            //x3=Y2+Z2

    FP2_BLS12381_mul(&t4, &t4, &x3);     //t4.mul(x3);                       //t4=(Y1+Z1)(Y2+Z2)
    FP2_BLS12381_add(&x3, &t1, &t2);     //x3.add(t2);                       //X3=Y1.Y2+Z1.Z2

    FP2_BLS12381_sub(&t4, &t4, &x3);     //t4.sub(x3);
    FP2_BLS12381_norm(&t4);              //t4.norm();
#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP2_BLS12381_mul_ip(&t4);            //t4.mul_ip();
    FP2_BLS12381_norm(&t4);              //t4.norm();          //t4=(Y1+Z1)(Y2+Z2) - (Y1.Y2+Z1.Z2) = Y1.Z2+Y2.Z1
#endif
    FP2_BLS12381_add(&x3, &(P->x), &(P->z)); //x3.add(z);
    FP2_BLS12381_norm(&x3);              //x3.norm();    // x3=X1+Z1
    FP2_BLS12381_add(&y3, &(Q->x), &(Q->z)); //y3.add(Q.z);
    FP2_BLS12381_norm(&y3);              //y3.norm();                // y3=X2+Z2
    FP2_BLS12381_mul(&x3, &x3, &y3);     //x3.mul(y3);                           // x3=(X1+Z1)(X2+Z2)
    FP2_BLS12381_add(&y3, &t0, &t2);     //y3.add(t2);                           // y3=X1.X2+Z1+Z2
    FP2_BLS12381_sub(&y3, &x3, &y3);     //y3.rsub(x3);
    FP2_BLS12381_norm(&y3);              //y3.norm();                // y3=(X1+Z1)(X2+Z2) - (X1.X2+Z1.Z2) = X1.Z2+X2.Z1
#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP2_BLS12381_mul_ip(&t0);            //t0.mul_ip();
    FP2_BLS12381_norm(&t0);              //t0.norm(); // x.Q.x
    FP2_BLS12381_mul_ip(&t1);            //t1.mul_ip();
    FP2_BLS12381_norm(&t1);              //t1.norm(); // y.Q.y
#endif
    FP2_BLS12381_add(&x3, &t0, &t0);     //x3.add(t0);
    FP2_BLS12381_add(&t0, &t0, &x3);     //t0.add(x3);
    FP2_BLS12381_norm(&t0);              //t0.norm();
    FP2_BLS12381_imul(&t2, &t2, b3);     //t2.imul(b);
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_mul_ip(&t2);
    FP2_BLS12381_norm(&t2);
#endif
    FP2_BLS12381_add(&z3, &t1, &t2);     //z3.add(t2);
    FP2_BLS12381_norm(&z3);              //z3.norm();
    FP2_BLS12381_sub(&t1, &t1, &t2);     //t1.sub(t2);
    FP2_BLS12381_norm(&t1);              //t1.norm();
    FP2_BLS12381_imul(&y3, &y3, b3);     //y3.imul(b);
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_mul_ip(&y3);
    FP2_BLS12381_norm(&y3);
#endif
    FP2_BLS12381_mul(&x3, &y3, &t4);     //x3.mul(t4);
    FP2_BLS12381_mul(&t2, &t3, &t1);     //t2.mul(t1);
    FP2_BLS12381_sub(&(P->x), &t2, &x3);     //x3.rsub(t2);
    FP2_BLS12381_mul(&y3, &y3, &t0);     //y3.mul(t0);
    FP2_BLS12381_mul(&t1, &t1, &z3);     //t1.mul(z3);
    FP2_BLS12381_add(&(P->y), &y3, &t1);     //y3.add(t1);
    FP2_BLS12381_mul(&t0, &t0, &t3);     //t0.mul(t3);
    FP2_BLS12381_mul(&z3, &z3, &t4);     //z3.mul(t4);
    FP2_BLS12381_add(&(P->z), &z3, &t0);     //z3.add(t0);

    FP2_BLS12381_norm(&(P->x));          //x.norm();
    FP2_BLS12381_norm(&(P->y));          //y.norm();
    FP2_BLS12381_norm(&(P->z));          //z.norm();

    return 0;
}

/* Set P-=Q */
/* SU= 16 */
void ECP2_BLS12381_sub(ECP2_BLS12381 *P, ECP2_BLS12381 *Q)
{
    ECP2_BLS12381 NQ;
    ECP2_BLS12381_copy(&NQ, Q);
    ECP2_BLS12381_neg(&NQ);
    ECP2_BLS12381_add(P, &NQ);
}

/* P*=e */
/* SU= 280 */
void ECP2_BLS12381_mul(ECP2_BLS12381 *P, BIG_384_58 e)
{
    /* fixed size windows */
    int i, nb, s, ns;
    BIG_384_58 mt, t;
    ECP2_BLS12381 Q, W[8], C;
    sign8 w[1 + (NLEN_384_58 * BASEBITS_384_58 + 3) / 4];

    if (ECP2_BLS12381_isinf(P)) return;

    /* precompute table */

    ECP2_BLS12381_copy(&Q, P);
    ECP2_BLS12381_dbl(&Q);
    ECP2_BLS12381_copy(&W[0], P);

    for (i = 1; i < 8; i++)
    {
        ECP2_BLS12381_copy(&W[i], &W[i - 1]);
        ECP2_BLS12381_add(&W[i], &Q);
    }

    /* make exponent odd - add 2P if even, P if odd */
    BIG_384_58_copy(t, e);
    s = BIG_384_58_parity(t);
    BIG_384_58_inc(t, 1);
    BIG_384_58_norm(t);
    ns = BIG_384_58_parity(t);
    BIG_384_58_copy(mt, t);
    BIG_384_58_inc(mt, 1);
    BIG_384_58_norm(mt);
    BIG_384_58_cmove(t, mt, s);
    ECP2_BLS12381_cmove(&Q, P, ns);
    ECP2_BLS12381_copy(&C, &Q);

    nb = 1 + (BIG_384_58_nbits(t) + 3) / 4;

    /* convert exponent to signed 4-bit window */
    for (i = 0; i < nb; i++)
    {
        w[i] = BIG_384_58_lastbits(t, 5) - 16;
        BIG_384_58_dec(t, w[i]);
        BIG_384_58_norm(t);
        BIG_384_58_fshr(t, 4);
    }
    w[nb] = BIG_384_58_lastbits(t, 5);

    ECP2_BLS12381_copy(P, &W[(w[nb] - 1) / 2]);
    for (i = nb - 1; i >= 0; i--)
    {
        ECP2_BLS12381_select(&Q, W, w[i]);
        ECP2_BLS12381_dbl(P);
        ECP2_BLS12381_dbl(P);
        ECP2_BLS12381_dbl(P);
        ECP2_BLS12381_dbl(P);
        ECP2_BLS12381_add(P, &Q);
    }
    ECP2_BLS12381_sub(P, &C); /* apply correction */
}

/* Calculates q.P using Frobenius constant X */
/* SU= 96 */
void ECP2_BLS12381_frob(ECP2_BLS12381 *P, FP2_BLS12381 *X)
{
    FP2_BLS12381 X2;

    FP2_BLS12381_sqr(&X2, X);
    FP2_BLS12381_conj(&(P->x), &(P->x));
    FP2_BLS12381_conj(&(P->y), &(P->y));
    FP2_BLS12381_conj(&(P->z), &(P->z));
    FP2_BLS12381_reduce(&(P->z));

    FP2_BLS12381_mul(&(P->x), &X2, &(P->x));
    FP2_BLS12381_mul(&(P->y), &X2, &(P->y));
    FP2_BLS12381_mul(&(P->y), X, &(P->y));

}


// Bos & Costello https://eprint.iacr.org/2013/458.pdf
// Faz-Hernandez & Longa & Sanchez  https://eprint.iacr.org/2013/158.pdf
// Side channel attack secure

void ECP2_BLS12381_mul4(ECP2_BLS12381 *P, ECP2_BLS12381 Q[4], BIG_384_58 u[4])
{
    int i, j, k, nb, pb, bt;
    ECP2_BLS12381 T[8], W;
    BIG_384_58 t[4], mt;
    sign8 w[NLEN_384_58 * BASEBITS_384_58 + 1];
    sign8 s[NLEN_384_58 * BASEBITS_384_58 + 1];

    for (i = 0; i < 4; i++)
    {
        BIG_384_58_copy(t[i], u[i]);
    }

// Precomputed table
    ECP2_BLS12381_copy(&T[0], &Q[0]); // Q[0]
    ECP2_BLS12381_copy(&T[1], &T[0]);
    ECP2_BLS12381_add(&T[1], &Q[1]); // Q[0]+Q[1]
    ECP2_BLS12381_copy(&T[2], &T[0]);
    ECP2_BLS12381_add(&T[2], &Q[2]); // Q[0]+Q[2]
    ECP2_BLS12381_copy(&T[3], &T[1]);
    ECP2_BLS12381_add(&T[3], &Q[2]); // Q[0]+Q[1]+Q[2]
    ECP2_BLS12381_copy(&T[4], &T[0]);
    ECP2_BLS12381_add(&T[4], &Q[3]); // Q[0]+Q[3]
    ECP2_BLS12381_copy(&T[5], &T[1]);
    ECP2_BLS12381_add(&T[5], &Q[3]); // Q[0]+Q[1]+Q[3]
    ECP2_BLS12381_copy(&T[6], &T[2]);
    ECP2_BLS12381_add(&T[6], &Q[3]); // Q[0]+Q[2]+Q[3]
    ECP2_BLS12381_copy(&T[7], &T[3]);
    ECP2_BLS12381_add(&T[7], &Q[3]); // Q[0]+Q[1]+Q[2]+Q[3]

// Make it odd
    pb = 1 - BIG_384_58_parity(t[0]);
    BIG_384_58_inc(t[0], pb);
    BIG_384_58_norm(t[0]);

// Number of bits
    BIG_384_58_zero(mt);
    for (i = 0; i < 4; i++)
    {
        BIG_384_58_or(mt, mt, t[i]);
    }
    nb = 1 + BIG_384_58_nbits(mt);

// Sign pivot
    s[nb - 1] = 1;
    for (i = 0; i < nb - 1; i++)
    {
        BIG_384_58_fshr(t[0], 1);
        s[i] = 2 * BIG_384_58_parity(t[0]) - 1;
    }

// Recoded exponent
    for (i = 0; i < nb; i++)
    {
        w[i] = 0;
        k = 1;
        for (j = 1; j < 4; j++)
        {
            bt = s[i] * BIG_384_58_parity(t[j]);
            BIG_384_58_fshr(t[j], 1);

            BIG_384_58_dec(t[j], (bt >> 1));
            BIG_384_58_norm(t[j]);
            w[i] += bt * k;
            k *= 2;
        }
    }

// Main loop
    ECP2_BLS12381_select(P, T, 2 * w[nb - 1] + 1);
    for (i = nb - 2; i >= 0; i--)
    {
        ECP2_BLS12381_select(&W, T, 2 * w[i] + s[i]);
        ECP2_BLS12381_dbl(P);
        ECP2_BLS12381_add(P, &W);
    }

// apply correction
    ECP2_BLS12381_copy(&W, P);
    ECP2_BLS12381_sub(&W, &Q[0]);
    ECP2_BLS12381_cmove(P, &W, pb);
}

/* Hunt and Peck a BIG to a curve point */
/*
void ECP2_BLS12381_hap2point(ECP2_BLS12381 *Q,BIG_384_58 h)
{
    BIG_384_58 one,hv;
    FP2_BLS12381 X;
    BIG_384_58_one(one);
    BIG_384_58_copy(hv,h);
    for (;;)
    {
        FP2_BLS12381_from_BIGs(&X,one,hv);
        if (ECP2_BLS12381_setx(Q,&X,0)) break;
        BIG_384_58_inc(hv,1);
        BIG_384_58_norm(hv);
    }
}
*/
/* Constant time Map to Point in G2 */
void ECP2_BLS12381_map2point(ECP2_BLS12381 *Q,FP2_BLS12381 *H)
{ // SSWU plus isogenies method

    int i,k,sgn,ne,isox,isoy,iso=HTC_ISO_G2_BLS12381;
    FP2_BLS12381 X1,X2,X3,W,Y,T,A,NY;
    FP_BLS12381 Z,s;
#if HTC_ISO_G2_BLS12381 != 0
    FP2_BLS12381 ZZ,Ad,Bd;
    FP2_BLS12381 xnum,xden,ynum,yden;
    FP2_BLS12381_from_ints(&ZZ,RIADZG2A_BLS12381,RIADZG2B_BLS12381);

    FP2_BLS12381_rcopy(&Ad,CURVE_Adr_BLS12381,CURVE_Adi_BLS12381);
    FP2_BLS12381_rcopy(&Bd,CURVE_Bdr_BLS12381,CURVE_Bdi_BLS12381);

    FP2_BLS12381_one(&NY);
    FP2_BLS12381_copy(&T,H);
    sgn=FP2_BLS12381_sign(&T);

    FP2_BLS12381_sqr(&T,&T);
    FP2_BLS12381_mul(&T,&T,&ZZ);
    FP2_BLS12381_add(&W,&T,&NY);
    FP2_BLS12381_norm(&W);

    FP2_BLS12381_mul(&W,&W,&T);
    FP2_BLS12381_mul(&A,&Ad,&W);
    FP2_BLS12381_inv(&A,&A);
    FP2_BLS12381_add(&W,&W,&NY);
    FP2_BLS12381_norm(&W);
    FP2_BLS12381_mul(&W,&W,&Bd);
    FP2_BLS12381_neg(&W,&W);
    FP2_BLS12381_norm(&W);

    FP2_BLS12381_mul(&X2,&W,&A);
    FP2_BLS12381_mul(&X3,&T,&X2);

    FP2_BLS12381_sqr(&W,&X3); FP2_BLS12381_add(&W,&W,&Ad); FP2_BLS12381_norm(&W); FP2_BLS12381_mul(&W,&W,&X3); FP2_BLS12381_add(&W,&W,&Bd); FP2_BLS12381_norm(&W);  // w=x^3+Ax+B
    FP2_BLS12381_cmove(&X2,&X3,FP2_BLS12381_qr(&W));                    
    FP2_BLS12381_sqr(&W,&X2); FP2_BLS12381_add(&W,&W,&Ad); FP2_BLS12381_norm(&W); FP2_BLS12381_mul(&W,&W,&X2); FP2_BLS12381_add(&W,&W,&Bd); FP2_BLS12381_norm(&W);
    FP2_BLS12381_sqrt(&Y,&W);  

    ne=FP2_BLS12381_sign(&Y)^sgn;
    FP2_BLS12381_neg(&NY,&Y); FP2_BLS12381_norm(&NY);
    FP2_BLS12381_cmove(&Y,&NY,ne);

// (X2,Y) is on isogenous curve

    k=0;
    isox=iso;
    isoy=3*(iso-1)/2;

// xnum
    FP2_BLS12381_rcopy(&xnum,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
    for (i=0;i<isox;i++)
    {
        FP2_BLS12381_mul(&xnum,&xnum,&X2); 
        FP2_BLS12381_rcopy(&W,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
        FP2_BLS12381_add(&xnum,&xnum,&W); FP2_BLS12381_norm(&xnum);
    }

// xden
    FP2_BLS12381_copy(&xden,&X2);
    FP2_BLS12381_rcopy(&W,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
    FP2_BLS12381_add(&xden,&xden,&W); FP2_BLS12381_norm(&xden);
    for (i=0;i<isox-2;i++)
    {
        FP2_BLS12381_mul(&xden,&xden,&X2);
        FP2_BLS12381_rcopy(&W,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
        FP2_BLS12381_add(&xden,&xden,&W); FP2_BLS12381_norm(&xden);
    }

// ynum
        FP2_BLS12381_rcopy(&ynum,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
        for (i=0;i<isoy;i++)
        {
            FP2_BLS12381_mul(&ynum,&ynum,&X2); 
            FP2_BLS12381_rcopy(&W,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
            FP2_BLS12381_add(&ynum,&ynum,&W); FP2_BLS12381_norm(&ynum);
        }

// yden 
        FP2_BLS12381_copy(&yden,&X2);
        FP2_BLS12381_rcopy(&W,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
        FP2_BLS12381_add(&yden,&yden,&W); FP2_BLS12381_norm(&yden);
      
        for (i=0;i<isoy-1;i++)
        {
            FP2_BLS12381_mul(&yden,&yden,&X2); 
            FP2_BLS12381_rcopy(&W,PCR_BLS12381[k],PCI_BLS12381[k]); k++;
            FP2_BLS12381_add(&yden,&yden,&W); FP2_BLS12381_norm(&yden);
        }

        FP2_BLS12381_mul(&ynum,&ynum,&Y);



        FP2_BLS12381_mul(&T,&xnum,&yden);
        FP2_BLS12381_copy(&(Q->x),&T);

        FP2_BLS12381_mul(&T,&ynum,&xden);
        FP2_BLS12381_copy(&(Q->y),&T);

        FP2_BLS12381_mul(&T,&xden,&yden);
        FP2_BLS12381_copy(&(Q->z),&T);

#else
    FP2_BLS12381_one(&NY);
    FP2_BLS12381_copy(&T,H);
    sgn=FP2_BLS12381_sign(&T);

    FP_BLS12381_from_int(&Z,RIADZG2A_BLS12381);
    FP2_BLS12381_from_FP(&A,&Z);
    ECP2_BLS12381_rhs(&A,&A);  // A=g(Z)

   if (CURVE_B_I_BLS12381==4 && SEXTIC_TWIST_BLS12381==M_TYPE && RIADZG2A_BLS12381==-1 && RIADZG2B_BLS12381==0)
    { // special case for BLS12381
        FP2_BLS12381_from_ints(&W,2,1);
    } else {
        FP2_BLS12381_sqrt(&W,&A);   // sqrt(g(Z))
    }
    FP2_BLS12381_sqrt(&W,&A);   // sqrt(g(Z))

    FP_BLS12381_rcopy(&s,SQRTm3_BLS12381);

    FP_BLS12381_mul(&Z,&Z,&s);     // Z.sqrt(-3)

    FP2_BLS12381_sqr(&T,&T);
    FP2_BLS12381_mul(&Y,&A,&T);   // tv1=u^2*g(Z)
    FP2_BLS12381_add(&T,&NY,&Y); FP2_BLS12381_norm(&T); // tv2=1+tv1
    FP2_BLS12381_sub(&Y,&NY,&Y); FP2_BLS12381_norm(&Y); // tv1=1-tv1
    FP2_BLS12381_mul(&NY,&T,&Y);

    FP2_BLS12381_pmul(&NY,&NY,&Z);

    FP2_BLS12381_inv(&NY,&NY);     // tv3=inv0(tv1*tv2)

    FP2_BLS12381_pmul(&W,&W,&Z); // tv4=Z*sqrt(-3).sqrt(g(Z))
    if (FP2_BLS12381_sign(&W)==1)
    {
        FP2_BLS12381_neg(&W,&W);
        FP2_BLS12381_norm(&W);
    }
    FP2_BLS12381_pmul(&W,&W,&Z);
    FP2_BLS12381_mul(&W,&W,H);
    FP2_BLS12381_mul(&W,&W,&Y);
    FP2_BLS12381_mul(&W,&W,&NY);     // tv5=u*tv1*tv3*tv4*Z*sqrt(-3)

    FP2_BLS12381_from_ints(&X1,RIADZG2A_BLS12381,RIADZG2B_BLS12381);
    FP2_BLS12381_copy(&X3,&X1);
    FP2_BLS12381_neg(&X1,&X1); FP2_BLS12381_norm(&X1); FP2_BLS12381_div2(&X1,&X1); // -Z/2
    FP2_BLS12381_copy(&X2,&X1);
    FP2_BLS12381_sub(&X1,&X1,&W); FP2_BLS12381_norm(&X1);
    FP2_BLS12381_add(&X2,&X2,&W); FP2_BLS12381_norm(&X2);
    FP2_BLS12381_add(&A,&A,&A);
    FP2_BLS12381_add(&A,&A,&A);
    FP2_BLS12381_norm(&A);      // 4*g(Z)
    FP2_BLS12381_sqr(&T,&T);
    FP2_BLS12381_mul(&T,&T,&NY);
    FP2_BLS12381_sqr(&T,&T);    // (tv2^2*tv3)^2
    FP2_BLS12381_mul(&A,&A,&T); // 4*g(Z)*(tv2^2*tv3)^2
    FP2_BLS12381_add(&X3,&X3,&A); FP2_BLS12381_norm(&X3);


    ECP2_BLS12381_rhs(&W,&X2);
    FP2_BLS12381_cmove(&X3,&X2,FP2_BLS12381_qr(&W));
    ECP2_BLS12381_rhs(&W,&X1);
    FP2_BLS12381_cmove(&X3,&X1,FP2_BLS12381_qr(&W));
    ECP2_BLS12381_rhs(&W,&X3);
    FP2_BLS12381_sqrt(&Y,&W);

    ne=FP2_BLS12381_sign(&Y)^sgn;
    FP2_BLS12381_neg(&W,&Y); FP2_BLS12381_norm(&W);
    FP2_BLS12381_cmove(&Y,&W,ne);

    ECP2_BLS12381_set(Q,&X3,&Y);
#endif
}

/* Map octet to point */
/*
void ECP2_BLS12381_mapit(ECP2_BLS12381 *Q, octet *W)
{
    BIG_384_58 q, x;
    DBIG_384_58 dx;
    BIG_384_58_rcopy(q, Modulus_BLS12381);

    BIG_384_58_dfromBytesLen(dx,W->val,W->len);
    BIG_384_58_dmod(x,dx,q);


    ECP2_BLS12381_hap2point(Q,x);
    ECP2_BLS12381_cfp(Q);
}
*/
/* cofactor product */
void ECP2_BLS12381_cfp(ECP2_BLS12381 *Q)
{
    FP_BLS12381 Fx, Fy;
    FP2_BLS12381 X;
    BIG_384_58 x;
#if (PAIRING_FRIENDLY_BLS12381 == BN_CURVE)
    ECP2_BLS12381 T, K;
#elif (PAIRING_FRIENDLY_BLS12381 > BN_CURVE)
    ECP2_BLS12381 xQ, x2Q;
#endif
    FP_BLS12381_rcopy(&Fx, Fra_BLS12381);
    FP_BLS12381_rcopy(&Fy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &Fx, &Fy);

#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_inv(&X, &X);
    FP2_BLS12381_norm(&X);
#endif

    BIG_384_58_rcopy(x, CURVE_Bnx_BLS12381);

#if (PAIRING_FRIENDLY_BLS12381 == BN_CURVE)

    // Faster Hashing to G2 - Fuentes-Castaneda, Knapp and Rodriguez-Henriquez
    // Q -> xQ + F(3xQ) + F(F(xQ)) + F(F(F(Q))).
    ECP2_BLS12381_copy(&T, Q);
    ECP2_BLS12381_mul(&T, x);
#if SIGN_OF_X_BLS12381==NEGATIVEX
    ECP2_BLS12381_neg(&T);   // our x is negative
#endif
    ECP2_BLS12381_copy(&K, &T);
    ECP2_BLS12381_dbl(&K);
    ECP2_BLS12381_add(&K, &T);

    ECP2_BLS12381_frob(&K, &X);
    ECP2_BLS12381_frob(Q, &X);
    ECP2_BLS12381_frob(Q, &X);
    ECP2_BLS12381_frob(Q, &X);
    ECP2_BLS12381_add(Q, &T);
    ECP2_BLS12381_add(Q, &K);
    ECP2_BLS12381_frob(&T, &X);
    ECP2_BLS12381_frob(&T, &X);
    ECP2_BLS12381_add(Q, &T);

#elif (PAIRING_FRIENDLY_BLS12381 > BN_CURVE)

    // Efficient hash maps to G2 on BLS curves - Budroni, Pintore
    // Q -> x2Q -xQ -Q +F(xQ -Q) +F(F(2Q))

    ECP2_BLS12381_copy(&xQ, Q);
    ECP2_BLS12381_mul(&xQ, x);
    ECP2_BLS12381_copy(&x2Q, &xQ);
    ECP2_BLS12381_mul(&x2Q, x);

#if SIGN_OF_X_BLS12381==NEGATIVEX
    ECP2_BLS12381_neg(&xQ);
#endif

    ECP2_BLS12381_sub(&x2Q, &xQ);
    ECP2_BLS12381_sub(&x2Q, Q);

    ECP2_BLS12381_sub(&xQ, Q);
    ECP2_BLS12381_frob(&xQ, &X);

    ECP2_BLS12381_dbl(Q);
    ECP2_BLS12381_frob(Q, &X);
    ECP2_BLS12381_frob(Q, &X);

    ECP2_BLS12381_add(Q, &x2Q);
    ECP2_BLS12381_add(Q, &xQ);

#endif
}

int ECP2_BLS12381_generator(ECP2_BLS12381 *G)
{
    FP2_BLS12381 wx, wy;

    FP2_BLS12381_rcopy(&wx,CURVE_Pxa_BLS12381,CURVE_Pxb_BLS12381);
    FP2_BLS12381_rcopy(&wy,CURVE_Pya_BLS12381,CURVE_Pyb_BLS12381);

    return ECP2_BLS12381_set(G, &wx, &wy);
}
