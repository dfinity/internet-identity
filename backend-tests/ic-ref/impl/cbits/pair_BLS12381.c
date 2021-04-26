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

/* CORE BN Curve pairing functions */

//#define HAS_MAIN

#include "pair_BLS12381.h"

// Point doubling for pairings
static void PAIR_BLS12381_double(ECP2_BLS12381 *A, FP2_BLS12381 *AA, FP2_BLS12381 *BB, FP2_BLS12381 *CC)
{
    FP2_BLS12381 YY;
    FP2_BLS12381_copy(CC, &(A->x));  //FP2 XX=new FP2(A.getx());  //X
    FP2_BLS12381_copy(&YY, &(A->y)); //FP2 YY=new FP2(A.gety());  //Y
    FP2_BLS12381_copy(BB, &(A->z));  //FP2 ZZ=new FP2(A.getz());  //Z

    FP2_BLS12381_copy(AA, &YY);      //FP2 YZ=new FP2(YY);        //Y
    FP2_BLS12381_mul(AA, AA, BB);    //YZ.mul(ZZ);                //YZ
    FP2_BLS12381_sqr(CC, CC);        //XX.sqr();                //X^2
    FP2_BLS12381_sqr(&YY, &YY);      //YY.sqr();                //Y^2
    FP2_BLS12381_sqr(BB, BB);        //ZZ.sqr();                //Z^2

    FP2_BLS12381_add(AA, AA, AA);
    FP2_BLS12381_neg(AA, AA);
    FP2_BLS12381_norm(AA);           // -2YZ
    FP2_BLS12381_mul_ip(AA);
    FP2_BLS12381_norm(AA);           // -2YZi

    FP2_BLS12381_imul(BB, BB, 3 * CURVE_B_I_BLS12381); //3Bz^2
    FP2_BLS12381_imul(CC, CC, 3);            //3X^2

#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP2_BLS12381_mul_ip(&YY);                    // Y^2.i
    FP2_BLS12381_norm(&YY);
    FP2_BLS12381_mul_ip(CC);                 // 3X^2.i
    FP2_BLS12381_norm(CC);
#endif

#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_mul_ip(BB);                 // 3Bz^2.i
    FP2_BLS12381_norm(BB);
#endif

    FP2_BLS12381_sub(BB, BB, &YY);
    FP2_BLS12381_norm(BB);

    ECP2_BLS12381_dbl(A);            //A.dbl();
}

// Point addition for pairings
static void PAIR_BLS12381_add(ECP2_BLS12381 *A, ECP2_BLS12381 *B, FP2_BLS12381 *AA, FP2_BLS12381 *BB, FP2_BLS12381 *CC)
{
    FP2_BLS12381 T1;
    FP2_BLS12381_copy(AA, &(A->x));      //FP2 X1=new FP2(A.getx());    // X1
    FP2_BLS12381_copy(CC, &(A->y));      //FP2 Y1=new FP2(A.gety());    // Y1
    FP2_BLS12381_copy(&T1, &(A->z));     //FP2 T1=new FP2(A.getz());    // Z1

    FP2_BLS12381_copy(BB, &T1);          //FP2 T2=new FP2(A.getz());    // Z1

    FP2_BLS12381_mul(&T1, &T1, &(B->y)); //T1.mul(B.gety());    // T1=Z1.Y2
    FP2_BLS12381_mul(BB, BB, &(B->x));   //T2.mul(B.getx());    // T2=Z1.X2

    FP2_BLS12381_sub(AA, AA, BB);        //X1.sub(T2);
    FP2_BLS12381_norm(AA);               //X1.norm();  // X1=X1-Z1.X2
    FP2_BLS12381_sub(CC, CC, &T1);       //Y1.sub(T1);
    FP2_BLS12381_norm(CC);               //Y1.norm();  // Y1=Y1-Z1.Y2

    FP2_BLS12381_copy(&T1, AA);          //T1.copy(X1);            // T1=X1-Z1.X2

#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_mul_ip(AA);
    FP2_BLS12381_norm(AA);
#endif

    FP2_BLS12381_mul(&T1, &T1, &(B->y)); //T1.mul(B.gety());       // T1=(X1-Z1.X2).Y2

    FP2_BLS12381_copy(BB, CC);           //T2.copy(Y1);            // T2=Y1-Z1.Y2
    FP2_BLS12381_mul(BB, BB, &(B->x));   //T2.mul(B.getx());       // T2=(Y1-Z1.Y2).X2
    FP2_BLS12381_sub(BB, BB, &T1);       //T2.sub(T1);
    FP2_BLS12381_norm(BB);               //T2.norm();          // T2=(Y1-Z1.Y2).X2 - (X1-Z1.X2).Y2

    FP2_BLS12381_neg(CC, CC);            //Y1.neg();
    FP2_BLS12381_norm(CC);               //Y1.norm(); // Y1=-(Y1-Z1.Y2).Xs                - ***

    ECP2_BLS12381_add(A, B);             //A.add(B);
}

/* Line function */
static void PAIR_BLS12381_line(FP12_BLS12381 *v, ECP2_BLS12381 *A, ECP2_BLS12381 *B, FP_BLS12381 *Qx, FP_BLS12381 *Qy)
{
    FP2_BLS12381 AA, BB, CC;
    FP4_BLS12381 a, b, c;

    if (A == B)
        PAIR_BLS12381_double(A, &AA, &BB, &CC);
    else
        PAIR_BLS12381_add(A, B, &AA, &BB, &CC);

    FP2_BLS12381_pmul(&CC, &CC, Qx);
    FP2_BLS12381_pmul(&AA, &AA, Qy);

    FP4_BLS12381_from_FP2s(&a, &AA, &BB);
#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP4_BLS12381_from_FP2(&b, &CC);
    FP4_BLS12381_zero(&c);
#endif
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP4_BLS12381_zero(&b);
    FP4_BLS12381_from_FP2H(&c, &CC);
#endif

    FP12_BLS12381_from_FP4s(v, &a, &b, &c);
    v->type = FP_SPARSER;
}

/* prepare ate parameter, n=6u+2 (BN) or n=u (BLS), n3=3*n */
int PAIR_BLS12381_nbits(BIG_384_58 n3, BIG_384_58 n)
{
    BIG_384_58 x;
    BIG_384_58_rcopy(x, CURVE_Bnx_BLS12381);

#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    BIG_384_58_pmul(n, x, 6);
#if SIGN_OF_X_BLS12381==POSITIVEX
    BIG_384_58_inc(n, 2);
#else
    BIG_384_58_dec(n, 2);
#endif

#else
    BIG_384_58_copy(n, x);
#endif

    BIG_384_58_norm(n);
    BIG_384_58_pmul(n3, n, 3);
    BIG_384_58_norm(n3);

    return BIG_384_58_nbits(n3);
}

/*
    For multi-pairing, product of n pairings
    1. Declare FP12 array of length number of bits in Ate parameter
    2. Initialise this array by calling PAIR_initmp()
    3. Accumulate each pairing by calling PAIR_another() n times
    4. Call PAIR_miller()
    5. Call final exponentiation PAIR_fexp()
*/

/* prepare for multi-pairing */
void PAIR_BLS12381_initmp(FP12_BLS12381 r[])
{
    int i;
    for (i = ATE_BITS_BLS12381 - 1; i >= 0; i--)
        FP12_BLS12381_one(&r[i]);
    return;
}

// Store precomputed line details in an FP4
static void PAIR_BLS12381_pack(FP4_BLS12381 *T, FP2_BLS12381* AA, FP2_BLS12381* BB, FP2_BLS12381 *CC)
{
    FP2_BLS12381 I, A, B;
    FP2_BLS12381_inv(&I, CC);
    FP2_BLS12381_mul(&A, AA, &I);
    FP2_BLS12381_mul(&B, BB, &I);
    FP4_BLS12381_from_FP2s(T, &A, &B);
}

// Unpack G2 line function details and include G1
static void PAIR_BLS12381_unpack(FP12_BLS12381 *v, FP4_BLS12381* T, FP_BLS12381 *Qx, FP_BLS12381 *Qy)
{
    FP4_BLS12381 a, b, c;
    FP2_BLS12381 t;
    FP4_BLS12381_copy(&a, T);
    FP2_BLS12381_pmul(&a.a, &a.a, Qy);
    FP2_BLS12381_from_FP(&t, Qx);

#if SEXTIC_TWIST_BLS12381==D_TYPE
    FP4_BLS12381_from_FP2(&b, &t);
    FP4_BLS12381_zero(&c);
#endif
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP4_BLS12381_zero(&b);
    FP4_BLS12381_from_FP2H(&c, &t);
#endif

    FP12_BLS12381_from_FP4s(v, &a, &b, &c);
    v->type = FP_SPARSEST;
}


/* basic Miller loop */
void PAIR_BLS12381_miller(FP12_BLS12381 *res, FP12_BLS12381 r[])
{
    int i;
    FP12_BLS12381_one(res);
    for (i = ATE_BITS_BLS12381 - 1; i >= 1; i--)
    {
        FP12_BLS12381_sqr(res, res);
        FP12_BLS12381_ssmul(res, &r[i]);
        FP12_BLS12381_zero(&r[i]);
    }

#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(res, res);
#endif
    FP12_BLS12381_ssmul(res, &r[0]);
    FP12_BLS12381_zero(&r[0]);
    return;
}


// Precompute table of line functions for fixed G2 value
void PAIR_BLS12381_precomp(FP4_BLS12381 T[], ECP2_BLS12381* GV)
{
    int i, j, nb, bt;
    BIG_384_58 n, n3;
    FP2_BLS12381 AA, BB, CC;
    ECP2_BLS12381 A, G, NG;
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    ECP2_BLS12381 K;
    FP2_BLS12381 X;
    FP_BLS12381 Qx, Qy;
    FP_BLS12381_rcopy(&Qx, Fra_BLS12381);
    FP_BLS12381_rcopy(&Qy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &Qx, &Qy);
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_inv(&X, &X);
    FP2_BLS12381_norm(&X);
#endif
#endif

    ECP2_BLS12381_copy(&A, GV);
    ECP2_BLS12381_copy(&G, GV);
    ECP2_BLS12381_copy(&NG, GV);
    ECP2_BLS12381_neg(&NG);

    nb = PAIR_BLS12381_nbits(n3, n);
    j = 0;

    for (i = nb - 2; i >= 1; i--)
    {
        PAIR_BLS12381_double(&A, &AA, &BB, &CC);
        PAIR_BLS12381_pack(&T[j++], &AA, &BB, &CC);

        bt = BIG_384_58_bit(n3, i) - BIG_384_58_bit(n, i); // bt=BIG_bit(n,i);
        if (bt == 1)
        {
            PAIR_BLS12381_add(&A, &G, &AA, &BB, &CC);
            PAIR_BLS12381_pack(&T[j++], &AA, &BB, &CC);
        }
        if (bt == -1)
        {
            PAIR_BLS12381_add(&A, &NG, &AA, &BB, &CC);
            PAIR_BLS12381_pack(&T[j++], &AA, &BB, &CC);
        }
    }
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE

#if SIGN_OF_X_BLS12381==NEGATIVEX
    ECP2_BLS12381_neg(&A);
#endif

    ECP2_BLS12381_copy(&K, &G);
    ECP2_BLS12381_frob(&K, &X);
    PAIR_BLS12381_add(&A, &K, &AA, &BB, &CC);
    PAIR_BLS12381_pack(&T[j++], &AA, &BB, &CC);
    ECP2_BLS12381_frob(&K, &X);
    ECP2_BLS12381_neg(&K);
    PAIR_BLS12381_add(&A, &K, &AA, &BB, &CC);
    PAIR_BLS12381_pack(&T[j++], &AA, &BB, &CC);

#endif
}

/* Accumulate another set of line functions for n-pairing, assuming precomputation on G2 */
void PAIR_BLS12381_another_pc(FP12_BLS12381 r[], FP4_BLS12381 T[], ECP_BLS12381 *QV)
{
    int i, j, nb, bt;
    BIG_384_58 x, n, n3;
    FP12_BLS12381 lv, lv2;
    ECP_BLS12381 Q;
    FP_BLS12381 Qx, Qy;

    if (ECP_BLS12381_isinf(QV)) return;

    nb = PAIR_BLS12381_nbits(n3, n);

    ECP_BLS12381_copy(&Q, QV);
    ECP_BLS12381_affine(&Q);

    FP_BLS12381_copy(&Qx, &(Q.x));
    FP_BLS12381_copy(&Qy, &(Q.y));

    j = 0;
    for (i = nb - 2; i >= 1; i--)
    {
        PAIR_BLS12381_unpack(&lv, &T[j++], &Qx, &Qy);

        bt = BIG_384_58_bit(n3, i) - BIG_384_58_bit(n, i); // bt=BIG_bit(n,i);
        if (bt == 1)
        {
            PAIR_BLS12381_unpack(&lv2, &T[j++], &Qx, &Qy);
            FP12_BLS12381_smul(&lv, &lv2);
        }
        if (bt == -1)
        {
            PAIR_BLS12381_unpack(&lv2, &T[j++], &Qx, &Qy);
            FP12_BLS12381_smul(&lv, &lv2);
        }
        FP12_BLS12381_ssmul(&r[i], &lv);
    }

#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    PAIR_BLS12381_unpack(&lv, &T[j++], &Qx, &Qy);
    PAIR_BLS12381_unpack(&lv2, &T[j++], &Qx, &Qy);
    FP12_BLS12381_smul(&lv, &lv2);
    FP12_BLS12381_ssmul(&r[0], &lv);
#endif
}

/* Accumulate another set of line functions for n-pairing */
void PAIR_BLS12381_another(FP12_BLS12381 r[], ECP2_BLS12381* PV, ECP_BLS12381* QV)
{
    int i, j, nb, bt;
    BIG_384_58 x, n, n3;
    FP12_BLS12381 lv, lv2;
    ECP2_BLS12381 A, NP, P;
    ECP_BLS12381 Q;
    FP_BLS12381 Qx, Qy;
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    ECP2_BLS12381 K;
    FP2_BLS12381 X;
    FP_BLS12381_rcopy(&Qx, Fra_BLS12381);
    FP_BLS12381_rcopy(&Qy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &Qx, &Qy);
#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_inv(&X, &X);
    FP2_BLS12381_norm(&X);
#endif
#endif

    if (ECP_BLS12381_isinf(QV)) return;

    nb = PAIR_BLS12381_nbits(n3, n);

    ECP2_BLS12381_copy(&P, PV);
    ECP_BLS12381_copy(&Q, QV);

    ECP2_BLS12381_affine(&P);
    ECP_BLS12381_affine(&Q);

    FP_BLS12381_copy(&Qx, &(Q.x));
    FP_BLS12381_copy(&Qy, &(Q.y));

    ECP2_BLS12381_copy(&A, &P);
    ECP2_BLS12381_copy(&NP, &P); ECP2_BLS12381_neg(&NP);

    for (i = nb - 2; i >= 1; i--)
    {
        PAIR_BLS12381_line(&lv, &A, &A, &Qx, &Qy);

        bt = BIG_384_58_bit(n3, i) - BIG_384_58_bit(n, i); // bt=BIG_bit(n,i);
        if (bt == 1)
        {
            PAIR_BLS12381_line(&lv2, &A, &P, &Qx, &Qy);
            FP12_BLS12381_smul(&lv, &lv2);
        }
        if (bt == -1)
        {
            PAIR_BLS12381_line(&lv2, &A, &NP, &Qx, &Qy);
            FP12_BLS12381_smul(&lv, &lv2);
        }
        FP12_BLS12381_ssmul(&r[i], &lv);
    }

#if PAIRING_FRIENDLY_BLS12381==BN_CURVE

#if SIGN_OF_X_BLS12381==NEGATIVEX
    ECP2_BLS12381_neg(&A);
#endif

    ECP2_BLS12381_copy(&K, &P);
    ECP2_BLS12381_frob(&K, &X);
    PAIR_BLS12381_line(&lv, &A, &K, &Qx, &Qy);
    ECP2_BLS12381_frob(&K, &X);
    ECP2_BLS12381_neg(&K);
    PAIR_BLS12381_line(&lv2, &A, &K, &Qx, &Qy);
    FP12_BLS12381_smul(&lv, &lv2);
    FP12_BLS12381_ssmul(&r[0], &lv);

#endif
}

/* Optimal R-ate pairing r=e(P,Q) */
void PAIR_BLS12381_ate(FP12_BLS12381 *r, ECP2_BLS12381 *P1, ECP_BLS12381 *Q1)
{

    BIG_384_58 x, n, n3;
    FP_BLS12381 Qx, Qy;
    int i, nb, bt;
    ECP2_BLS12381 A, NP, P;
    ECP_BLS12381 Q;
    FP12_BLS12381 lv, lv2;
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    ECP2_BLS12381 KA;
    FP2_BLS12381 X;

    FP_BLS12381_rcopy(&Qx, Fra_BLS12381);
    FP_BLS12381_rcopy(&Qy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &Qx, &Qy);

#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_inv(&X, &X);
    FP2_BLS12381_norm(&X);
#endif
#endif

    FP12_BLS12381_one(r);
    if (ECP_BLS12381_isinf(Q1)) return;

    nb = PAIR_BLS12381_nbits(n3, n);

    ECP2_BLS12381_copy(&P, P1);
    ECP_BLS12381_copy(&Q, Q1);

    ECP2_BLS12381_affine(&P);
    ECP_BLS12381_affine(&Q);

    FP_BLS12381_copy(&Qx, &(Q.x));
    FP_BLS12381_copy(&Qy, &(Q.y));

    ECP2_BLS12381_copy(&A, &P);
    ECP2_BLS12381_copy(&NP, &P); ECP2_BLS12381_neg(&NP);



    /* Main Miller Loop */
    for (i = nb - 2; i >= 1; i--) //0
    {
        FP12_BLS12381_sqr(r, r);
        PAIR_BLS12381_line(&lv, &A, &A, &Qx, &Qy);

        bt = BIG_384_58_bit(n3, i) - BIG_384_58_bit(n, i); // bt=BIG_bit(n,i);
        if (bt == 1)
        {
            PAIR_BLS12381_line(&lv2, &A, &P, &Qx, &Qy);
            FP12_BLS12381_smul(&lv, &lv2);
        }
        if (bt == -1)
        {
            PAIR_BLS12381_line(&lv2, &A, &NP, &Qx, &Qy);
            FP12_BLS12381_smul(&lv, &lv2);
        }
        FP12_BLS12381_ssmul(r, &lv);

    }


#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(r, r);
#endif

    /* R-ate fixup required for BN curves */
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE

#if SIGN_OF_X_BLS12381==NEGATIVEX
    ECP2_BLS12381_neg(&A);
#endif

    ECP2_BLS12381_copy(&KA, &P);
    ECP2_BLS12381_frob(&KA, &X);
    PAIR_BLS12381_line(&lv, &A, &KA, &Qx, &Qy);
    ECP2_BLS12381_frob(&KA, &X);
    ECP2_BLS12381_neg(&KA);
    PAIR_BLS12381_line(&lv2, &A, &KA, &Qx, &Qy);
    FP12_BLS12381_smul(&lv, &lv2);
    FP12_BLS12381_ssmul(r, &lv);
#endif
}

/* Optimal R-ate double pairing e(P,Q).e(R,S) */
void PAIR_BLS12381_double_ate(FP12_BLS12381 *r, ECP2_BLS12381 *P1, ECP_BLS12381 *Q1, ECP2_BLS12381 *R1, ECP_BLS12381 *S1)
{
    BIG_384_58 x, n, n3;
    FP_BLS12381 Qx, Qy, Sx, Sy;
    int i, nb, bt;
    ECP2_BLS12381 A, B, NP, NR, P, R;
    ECP_BLS12381 Q, S;
    FP12_BLS12381 lv, lv2;
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    FP2_BLS12381 X;
    ECP2_BLS12381 K;

    FP_BLS12381_rcopy(&Qx, Fra_BLS12381);
    FP_BLS12381_rcopy(&Qy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &Qx, &Qy);

#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_inv(&X, &X);
    FP2_BLS12381_norm(&X);
#endif
#endif

    if (ECP_BLS12381_isinf(Q1))
    {
        PAIR_BLS12381_ate(r, R1, S1);
        return;
    }
    if (ECP_BLS12381_isinf(S1))
    {
        PAIR_BLS12381_ate(r, P1, Q1);
        return;
    }
    nb = PAIR_BLS12381_nbits(n3, n);

    ECP2_BLS12381_copy(&P, P1);
    ECP_BLS12381_copy(&Q, Q1);

    ECP2_BLS12381_affine(&P);
    ECP_BLS12381_affine(&Q);

    ECP2_BLS12381_copy(&R, R1);
    ECP_BLS12381_copy(&S, S1);

    ECP2_BLS12381_affine(&R);
    ECP_BLS12381_affine(&S);

    FP_BLS12381_copy(&Qx, &(Q.x));
    FP_BLS12381_copy(&Qy, &(Q.y));

    FP_BLS12381_copy(&Sx, &(S.x));
    FP_BLS12381_copy(&Sy, &(S.y));

    ECP2_BLS12381_copy(&A, &P);
    ECP2_BLS12381_copy(&B, &R);

    ECP2_BLS12381_copy(&NP, &P); ECP2_BLS12381_neg(&NP);
    ECP2_BLS12381_copy(&NR, &R); ECP2_BLS12381_neg(&NR);

    FP12_BLS12381_one(r);

    /* Main Miller Loop */
    for (i = nb - 2; i >= 1; i--)
    {
        FP12_BLS12381_sqr(r, r);
        PAIR_BLS12381_line(&lv, &A, &A, &Qx, &Qy);
        PAIR_BLS12381_line(&lv2, &B, &B, &Sx, &Sy);
        FP12_BLS12381_smul(&lv, &lv2);
        FP12_BLS12381_ssmul(r, &lv);

        bt = BIG_384_58_bit(n3, i) - BIG_384_58_bit(n, i); // bt=BIG_bit(n,i);
        if (bt == 1)
        {
            PAIR_BLS12381_line(&lv, &A, &P, &Qx, &Qy);
            PAIR_BLS12381_line(&lv2, &B, &R, &Sx, &Sy);
            FP12_BLS12381_smul(&lv, &lv2);
            FP12_BLS12381_ssmul(r, &lv);
        }
        if (bt == -1)
        {
            PAIR_BLS12381_line(&lv, &A, &NP, &Qx, &Qy);
            PAIR_BLS12381_line(&lv2, &B, &NR, &Sx, &Sy);
            FP12_BLS12381_smul(&lv, &lv2);
            FP12_BLS12381_ssmul(r, &lv);
        }

    }


    /* R-ate fixup required for BN curves */

#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(r, r);
#endif

#if PAIRING_FRIENDLY_BLS12381==BN_CURVE

#if SIGN_OF_X_BLS12381==NEGATIVEX
    ECP2_BLS12381_neg(&A);
    ECP2_BLS12381_neg(&B);
#endif

    ECP2_BLS12381_copy(&K, &P);
    ECP2_BLS12381_frob(&K, &X);
    PAIR_BLS12381_line(&lv, &A, &K, &Qx, &Qy);
    ECP2_BLS12381_frob(&K, &X);
    ECP2_BLS12381_neg(&K);
    PAIR_BLS12381_line(&lv2, &A, &K, &Qx, &Qy);
    FP12_BLS12381_smul(&lv, &lv2);
    FP12_BLS12381_ssmul(r, &lv);

    ECP2_BLS12381_copy(&K, &R);
    ECP2_BLS12381_frob(&K, &X);
    PAIR_BLS12381_line(&lv, &B, &K, &Sx, &Sy);
    ECP2_BLS12381_frob(&K, &X);
    ECP2_BLS12381_neg(&K);
    PAIR_BLS12381_line(&lv2, &B, &K, &Sx, &Sy);
    FP12_BLS12381_smul(&lv, &lv2);
    FP12_BLS12381_ssmul(r, &lv);
#endif
}

/* final exponentiation - keep separate for multi-pairings and to avoid thrashing stack */
void PAIR_BLS12381_fexp(FP12_BLS12381 *r)
{
    FP2_BLS12381 X;
    BIG_384_58 x;
    FP_BLS12381 a, b;
    FP12_BLS12381 t0, y0, y1;
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    FP12_BLS12381 y2, y3;
#endif

    BIG_384_58_rcopy(x, CURVE_Bnx_BLS12381);
    FP_BLS12381_rcopy(&a, Fra_BLS12381);
    FP_BLS12381_rcopy(&b, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &a, &b);

    /* Easy part of final exp */

    FP12_BLS12381_inv(&t0, r);
    FP12_BLS12381_conj(r, r);

    FP12_BLS12381_mul(r, &t0);
    FP12_BLS12381_copy(&t0, r);

    FP12_BLS12381_frob(r, &X);
    FP12_BLS12381_frob(r, &X);
    FP12_BLS12381_mul(r, &t0);

    /* Hard part of final exp - see Duquesne & Ghamman eprint 2015/192.pdf */
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    FP12_BLS12381_pow(&t0, r, x); // t0=f^-u
#if SIGN_OF_X_BLS12381==POSITIVEX
    FP12_BLS12381_conj(&t0, &t0);
#endif
    FP12_BLS12381_usqr(&y3, &t0); // y3=t0^2
    FP12_BLS12381_copy(&y0, &t0);
    FP12_BLS12381_mul(&y0, &y3); // y0=t0*y3
    FP12_BLS12381_copy(&y2, &y3);
    FP12_BLS12381_frob(&y2, &X); // y2=y3^p
    FP12_BLS12381_mul(&y2, &y3); //y2=y2*y3
    FP12_BLS12381_usqr(&y2, &y2); //y2=y2^2
    FP12_BLS12381_mul(&y2, &y3); // y2=y2*y3

    FP12_BLS12381_pow(&t0, &y0, x); //t0=y0^-u
#if SIGN_OF_X_BLS12381==POSITIVEX
    FP12_BLS12381_conj(&t0, &t0);
#endif
    FP12_BLS12381_conj(&y0, r);    //y0=~r
    FP12_BLS12381_copy(&y1, &t0);
    FP12_BLS12381_frob(&y1, &X);
    FP12_BLS12381_frob(&y1, &X); //y1=t0^p^2
    FP12_BLS12381_mul(&y1, &y0); // y1=y0*y1
    FP12_BLS12381_conj(&t0, &t0); // t0=~t0
    FP12_BLS12381_copy(&y3, &t0);
    FP12_BLS12381_frob(&y3, &X); //y3=t0^p
    FP12_BLS12381_mul(&y3, &t0); // y3=t0*y3
    FP12_BLS12381_usqr(&t0, &t0); // t0=t0^2
    FP12_BLS12381_mul(&y1, &t0); // y1=t0*y1

    FP12_BLS12381_pow(&t0, &y3, x); // t0=y3^-u
#if SIGN_OF_X_BLS12381==POSITIVEX
    FP12_BLS12381_conj(&t0, &t0);
#endif
    FP12_BLS12381_usqr(&t0, &t0); //t0=t0^2
    FP12_BLS12381_conj(&t0, &t0); //t0=~t0
    FP12_BLS12381_mul(&y3, &t0); // y3=t0*y3

    FP12_BLS12381_frob(r, &X);
    FP12_BLS12381_copy(&y0, r);
    FP12_BLS12381_frob(r, &X);
    FP12_BLS12381_mul(&y0, r);
    FP12_BLS12381_frob(r, &X);
    FP12_BLS12381_mul(&y0, r);

    FP12_BLS12381_usqr(r, &y3); //r=y3^2
    FP12_BLS12381_mul(r, &y2);  //r=y2*r
    FP12_BLS12381_copy(&y3, r);
    FP12_BLS12381_mul(&y3, &y0); // y3=r*y0
    FP12_BLS12381_mul(r, &y1); // r=r*y1
    FP12_BLS12381_usqr(r, r); // r=r^2
    FP12_BLS12381_mul(r, &y3); // r=r*y3
    FP12_BLS12381_reduce(r);
#else

// See https://eprint.iacr.org/2020/875.pdf
    FP12_BLS12381_usqr(&y1,r);
    FP12_BLS12381_mul(&y1,r);     // y1=r^3

    FP12_BLS12381_pow(&y0,r,x);   // y0=r^x
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y0, &y0);
#endif
    FP12_BLS12381_conj(&t0,r);    // t0=r^-1
    FP12_BLS12381_copy(r,&y0);
    FP12_BLS12381_mul(r,&t0);    // r=r^(x-1)

    FP12_BLS12381_pow(&y0,r,x);   // y0=r^x
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y0, &y0);
#endif
    FP12_BLS12381_conj(&t0,r);    // t0=r^-1
    FP12_BLS12381_copy(r,&y0);
    FP12_BLS12381_mul(r,&t0);    // r=r^(x-1)

// ^(x+p)
    FP12_BLS12381_pow(&y0,r,x);  // y0=r^x
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y0, &y0);
#endif
    FP12_BLS12381_copy(&t0,r);   
    FP12_BLS12381_frob(&t0,&X); // t0=r^p
    FP12_BLS12381_copy(r,&y0);
    FP12_BLS12381_mul(r,&t0); // r=r^x.r^p

// ^(x^2+p^2-1)
    FP12_BLS12381_pow(&y0,r,x);  
    FP12_BLS12381_pow(&y0,&y0,x); // y0=r^x^2
    FP12_BLS12381_copy(&t0,r);    
    FP12_BLS12381_frob(&t0,&X);
    FP12_BLS12381_frob(&t0,&X);   // t0=r^p^2
    FP12_BLS12381_mul(&y0,&t0);   // y0=r^x^2.r^p^2
    FP12_BLS12381_conj(&t0,r);    // t0=r^-1
    FP12_BLS12381_copy(r,&y0);    // 
    FP12_BLS12381_mul(r,&t0);     // r=r^x^2.r^p^2.r^-1

    FP12_BLS12381_mul(r,&y1);    
    FP12_BLS12381_reduce(r);

// Ghamman & Fouotsa Method
/*
    FP12_BLS12381_usqr(&y0, r);
    FP12_BLS12381_pow(&y1, &y0, x);
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y1, &y1);
#endif

    BIG_384_58_fshr(x, 1);
    FP12_BLS12381_pow(&y2, &y1, x);
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y2, &y2);
#endif

    BIG_384_58_fshl(x, 1); // x must be even
    FP12_BLS12381_conj(&y3, r);
    FP12_BLS12381_mul(&y1, &y3);

    FP12_BLS12381_conj(&y1, &y1);
    FP12_BLS12381_mul(&y1, &y2);

    FP12_BLS12381_pow(&y2, &y1, x);
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y2, &y2);
#endif

    FP12_BLS12381_pow(&y3, &y2, x);
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y3, &y3);
#endif
    FP12_BLS12381_conj(&y1, &y1);
    FP12_BLS12381_mul(&y3, &y1);

    FP12_BLS12381_conj(&y1, &y1);
    FP12_BLS12381_frob(&y1, &X);
    FP12_BLS12381_frob(&y1, &X);
    FP12_BLS12381_frob(&y1, &X);
    FP12_BLS12381_frob(&y2, &X);
    FP12_BLS12381_frob(&y2, &X);
    FP12_BLS12381_mul(&y1, &y2);

    FP12_BLS12381_pow(&y2, &y3, x);
#if SIGN_OF_X_BLS12381==NEGATIVEX
    FP12_BLS12381_conj(&y2, &y2);
#endif
    FP12_BLS12381_mul(&y2, &y0);
    FP12_BLS12381_mul(&y2, r);

    FP12_BLS12381_mul(&y1, &y2);
    FP12_BLS12381_copy(&y2, &y3);
    FP12_BLS12381_frob(&y2, &X);
    FP12_BLS12381_mul(&y1, &y2);
    FP12_BLS12381_copy(r, &y1);
    FP12_BLS12381_reduce(r);
*/
#endif
}

#ifdef USE_GLV_BLS12381
/* GLV method */
static void glv(BIG_384_58 u[2], BIG_384_58 e)
{
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    int i, j;
    BIG_384_58 v[2], t, q;
    DBIG_384_58 d;
    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
    for (i = 0; i < 2; i++)
    {
        BIG_384_58_rcopy(t, CURVE_W_BLS12381[i]);
        BIG_384_58_mul(d, t, e);
        BIG_384_58_ddiv(v[i], d, q);
        BIG_384_58_zero(u[i]);
    }
    BIG_384_58_copy(u[0], e);
    for (i = 0; i < 2; i++)
        for (j = 0; j < 2; j++)
        {
            BIG_384_58_rcopy(t, CURVE_SB_BLS12381[j][i]);
            BIG_384_58_modmul(t, v[j], t, q);
            BIG_384_58_add(u[i], u[i], q);
            BIG_384_58_sub(u[i], u[i], t);
            BIG_384_58_mod(u[i], q);
        }

#else
// -(x^2).P = (Beta.x,y)

    BIG_384_58 x, x2, q;
    BIG_384_58_rcopy(x, CURVE_Bnx_BLS12381);
    BIG_384_58_smul(x2, x, x);
    BIG_384_58_copy(u[0], e);
    BIG_384_58_mod(u[0], x2);
    BIG_384_58_copy(u[1], e);
    BIG_384_58_sdiv(u[1], x2);

    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
    BIG_384_58_sub(u[1], q, u[1]);

#endif

    return;
}
#endif // USE_GLV

/* Galbraith & Scott Method */
static void gs(BIG_384_58 u[4], BIG_384_58 e)
{
    int i;
#if PAIRING_FRIENDLY_BLS12381==BN_CURVE
    int j;
    BIG_384_58 v[4], t, q;
    DBIG_384_58 d;
    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
    for (i = 0; i < 4; i++)
    {
        BIG_384_58_rcopy(t, CURVE_WB_BLS12381[i]);
        BIG_384_58_mul(d, t, e);
        BIG_384_58_ddiv(v[i], d, q);
        BIG_384_58_zero(u[i]);
    }

    BIG_384_58_copy(u[0], e);
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
        {
            BIG_384_58_rcopy(t, CURVE_BB_BLS12381[j][i]);
            BIG_384_58_modmul(t, v[j], t, q);
            BIG_384_58_add(u[i], u[i], q);
            BIG_384_58_sub(u[i], u[i], t);
            BIG_384_58_mod(u[i], q);
        }

#else

    BIG_384_58 x, w, q;
    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
    BIG_384_58_rcopy(x, CURVE_Bnx_BLS12381);
    BIG_384_58_copy(w, e);

    for (i = 0; i < 3; i++)
    {
        BIG_384_58_copy(u[i], w);
        BIG_384_58_mod(u[i], x);
        BIG_384_58_sdiv(w, x);
    }
    BIG_384_58_copy(u[3], w);

    /*  */
#if SIGN_OF_X_BLS12381==NEGATIVEX
    BIG_384_58_modneg(u[1], u[1], q);
    BIG_384_58_modneg(u[3], u[3], q);
#endif

#endif



    return;
}

/* Multiply P by e in group G1 */
void PAIR_BLS12381_G1mul(ECP_BLS12381 *P, BIG_384_58 e)
{
#ifdef USE_GLV_BLS12381   /* Note this method is patented */
    int np, nn;
    ECP_BLS12381 Q;
    FP_BLS12381 cru;
    BIG_384_58 t, q;
    BIG_384_58 u[2];

    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
    glv(u, e);

    ECP_BLS12381_copy(&Q, P); ECP_BLS12381_affine(&Q);
    FP_BLS12381_rcopy(&cru, CRu_BLS12381);
    FP_BLS12381_mul(&(Q.x), &(Q.x), &cru);

    /* note that -a.B = a.(-B). Use a or -a depending on which is smaller */

    np = BIG_384_58_nbits(u[0]);
    BIG_384_58_modneg(t, u[0], q);
    nn = BIG_384_58_nbits(t);
    if (nn < np)
    {
        BIG_384_58_copy(u[0], t);
        ECP_BLS12381_neg(P);
    }

    np = BIG_384_58_nbits(u[1]);
    BIG_384_58_modneg(t, u[1], q);
    nn = BIG_384_58_nbits(t);
    if (nn < np)
    {
        BIG_384_58_copy(u[1], t);
        ECP_BLS12381_neg(&Q);
    }
    BIG_384_58_norm(u[0]);
    BIG_384_58_norm(u[1]);
    ECP_BLS12381_mul2(P, &Q, u[0], u[1]);

#else
    ECP_BLS12381_mul(P, e);
#endif
}

/* Multiply P by e in group G2 */
void PAIR_BLS12381_G2mul(ECP2_BLS12381 *P, BIG_384_58 e)
{
#ifdef USE_GS_G2_BLS12381   /* Well I didn't patent it :) */
    int i, np, nn;
    ECP2_BLS12381 Q[4];
    FP2_BLS12381 X;
    FP_BLS12381 fx, fy;
    BIG_384_58 x, y, u[4];

    FP_BLS12381_rcopy(&fx, Fra_BLS12381);
    FP_BLS12381_rcopy(&fy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &fx, &fy);

#if SEXTIC_TWIST_BLS12381==M_TYPE
    FP2_BLS12381_inv(&X, &X);
    FP2_BLS12381_norm(&X);
#endif

    BIG_384_58_rcopy(y, CURVE_Order_BLS12381);
    gs(u, e);

    ECP2_BLS12381_copy(&Q[0], P);
    for (i = 1; i < 4; i++)
    {
        ECP2_BLS12381_copy(&Q[i], &Q[i - 1]);
        ECP2_BLS12381_frob(&Q[i], &X);
    }

    for (i = 0; i < 4; i++)
    {
        np = BIG_384_58_nbits(u[i]);
        BIG_384_58_modneg(x, u[i], y);
        nn = BIG_384_58_nbits(x);
        if (nn < np)
        {
            BIG_384_58_copy(u[i], x);
            ECP2_BLS12381_neg(&Q[i]);
        }
        BIG_384_58_norm(u[i]);
    }

    ECP2_BLS12381_mul4(P, Q, u);

#else
    ECP2_BLS12381_mul(P, e);
#endif
}

/* f=f^e */
void PAIR_BLS12381_GTpow(FP12_BLS12381 *f, BIG_384_58 e)
{
#ifdef USE_GS_GT_BLS12381   /* Note that this option requires a lot of RAM! Maybe better to use compressed XTR method, see fp4.c */
    int i, np, nn;
    FP12_BLS12381 g[4];
    FP2_BLS12381 X;
    BIG_384_58 t, q;
    FP_BLS12381 fx, fy;
    BIG_384_58 u[4];

    FP_BLS12381_rcopy(&fx, Fra_BLS12381);
    FP_BLS12381_rcopy(&fy, Frb_BLS12381);
    FP2_BLS12381_from_FPs(&X, &fx, &fy);

    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
    gs(u, e);

    FP12_BLS12381_copy(&g[0], f);
    for (i = 1; i < 4; i++)
    {
        FP12_BLS12381_copy(&g[i], &g[i - 1]);
        FP12_BLS12381_frob(&g[i], &X);
    }

    for (i = 0; i < 4; i++)
    {
        np = BIG_384_58_nbits(u[i]);
        BIG_384_58_modneg(t, u[i], q);
        nn = BIG_384_58_nbits(t);
        if (nn < np)
        {
            BIG_384_58_copy(u[i], t);
            FP12_BLS12381_conj(&g[i], &g[i]);
        }
        BIG_384_58_norm(u[i]);
    }
    FP12_BLS12381_pow4(f, g, u);

#else
    FP12_BLS12381_pow(f, f, e);
#endif
}


/* test G1 group membership */

int PAIR_BLS12381_G1member(ECP_BLS12381 *P)
{
	BIG_384_58 q;
	ECP_BLS12381 W;
    if (ECP_BLS12381_isinf(P)) return 0;
    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
	ECP_BLS12381_copy(&W,P);
	PAIR_BLS12381_G1mul(&W,q);
	if (!ECP_BLS12381_isinf(&W)) return 0;
	return 1;
}

/* test G2 group membership */

int PAIR_BLS12381_G2member(ECP2_BLS12381 *P)
{
	BIG_384_58 q;
	ECP2_BLS12381 W;
   if (ECP2_BLS12381_isinf(P)) return 0;
    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
	ECP2_BLS12381_copy(&W,P);
	PAIR_BLS12381_G2mul(&W,q);
	if (!ECP2_BLS12381_isinf(&W)) return 0;
	return 1;
}

/* test GT group membership */
/* First check that m!=1, conj(m)*m==1, and m.m^{p^4}=m^{p^2} */

int PAIR_BLS12381_GTmember(FP12_BLS12381 *m)
{
	BIG_384_58 q;
	FP_BLS12381 fx,fy;
	FP2_BLS12381 X;
	FP12_BLS12381 r,w;
	if (FP12_BLS12381_isunity(m)) return 0;
	FP12_BLS12381_conj(&r,m);
	FP12_BLS12381_mul(&r,m);
	if (!FP12_BLS12381_isunity(&r)) return 0;

	FP_BLS12381_rcopy(&fx,Fra_BLS12381);
	FP_BLS12381_rcopy(&fy,Frb_BLS12381);
	FP2_BLS12381_from_FPs(&X,&fx,&fy);

	FP12_BLS12381_copy(&r,m); FP12_BLS12381_frob(&r,&X); FP12_BLS12381_frob(&r,&X);
	FP12_BLS12381_copy(&w,&r); FP12_BLS12381_frob(&w,&X); FP12_BLS12381_frob(&w,&X);
	FP12_BLS12381_mul(&w,m);

	if (!FP12_BLS12381_equals(&w,&r)) return 0;

    BIG_384_58_rcopy(q, CURVE_Order_BLS12381);
	FP12_BLS12381_copy(&r,m);
	PAIR_BLS12381_GTpow(&r,q);
	if (!FP12_BLS12381_isunity(&r)) return 0;
	return 1;

}

#ifdef HAS_MAIN

int main()
{
    int i;
    char byt[32];
    csprng rng;
    BIG_384_58 xa, xb, ya, yb, w, a, b, t1, q, u[2], v[4], m, r;
    ECP2_BLS12381 P, G;
    ECP_BLS12381 Q, R;
    FP12_BLS12381 g, gp;
    FP4_BLS12381 t, c, cp, cpm1, cpm2;
    FP2_BLS12381 x, y, X;


    BIG_384_58_rcopy(a, CURVE_Fra);
    BIG_384_58_rcopy(b, CURVE_Frb);
    FP2_BLS12381_from_BIGs(&X, a, b);

    BIG_384_58_rcopy(xa, CURVE_Gx);
    BIG_384_58_rcopy(ya, CURVE_Gy);

    ECP_BLS12381_set(&Q, xa, ya);
    if (Q.inf) printf("Failed to set - point not on curve\n");
    else printf("G1 set success\n");

    printf("Q= ");
    ECP_BLS12381_output(&Q);
    printf("\n");

    BIG_384_58_rcopy(xa, CURVE_Pxa);
    BIG_384_58_rcopy(xb, CURVE_Pxb);
    BIG_384_58_rcopy(ya, CURVE_Pya);
    BIG_384_58_rcopy(yb, CURVE_Pyb);

    FP2_BLS12381_from_BIGs(&x, xa, xb);
    FP2_BLS12381_from_BIGs(&y, ya, yb);

    ECP2_BLS12381_set(&P, &x, &y);
    if (P.inf) printf("Failed to set - point not on curve\n");
    else printf("G2 set success\n");

    printf("P= ");
    ECP2_BLS12381_output(&P);
    printf("\n");

    for (i = 0; i < 1000; i++ )
    {
        PAIR_BLS12381_ate(&g, &P, &Q);
        PAIR_BLS12381_fexp(&g);
    }
    printf("g= ");
    FP12_BLS12381_output(&g);
    printf("\n");
}

#endif
