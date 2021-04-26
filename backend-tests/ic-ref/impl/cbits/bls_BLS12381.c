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

/* Boneh-Lynn-Shacham signature 128-bit API */

/* Loosely (for now) following https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-02 */

// Minimal-signature-size variant

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bls_BLS12381.h"


static FP4_BLS12381 G2_TAB[G2_TABLE_BLS12381];  // space for precomputation on fixed G2 parameter

#define CEIL(a,b) (((a)-1)/(b)+1)

/* output u[i] \in F_p */
/* https://datatracker.ietf.org/doc/draft-irtf-cfrg-hash-to-curve/ */
static void hash_to_field(int hash,int hlen,FP_BLS12381 *u,octet *DST,octet *M, int ctr)
{
    int i,j,L;
    BIG_384_58 q,w;
    DBIG_384_58 dx;
    char okm[256],fd[128];
    octet OKM = {0,sizeof(okm),okm};

    BIG_384_58_rcopy(q, Modulus_BLS12381);
    L=CEIL(BIG_384_58_nbits(q)+CURVE_SECURITY_BLS12381,8);

    XMD_Expand(hash,hlen,&OKM,L*ctr,DST,M);
    for (i=0;i<ctr;i++)
    {
        for (j=0;j<L;j++)
            fd[j]=OKM.val[i*L+j];
        
        BIG_384_58_dfromBytesLen(dx,fd,L);
        BIG_384_58_dmod(w,dx,q);
        FP_BLS12381_nres(&u[i],w);
    }
}


/* hash a message to an ECP point, using SHA2, random oracle method */
static void BLS_HASH_TO_POINT(ECP_BLS12381 *P, octet *M)
{
    FP_BLS12381 u[2];
    ECP_BLS12381 P1;
    char dst[50];
    octet DST = {0,sizeof(dst),dst};

    OCT_jstring(&DST,(char *)"BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_");
    hash_to_field(MC_SHA2,HASH_TYPE_BLS12381,u,&DST,M,2);

    ECP_BLS12381_map2point(P,&u[0]);
    ECP_BLS12381_map2point(&P1,&u[1]);
    ECP_BLS12381_add(P,&P1);
    ECP_BLS12381_cfp(P);
    ECP_BLS12381_affine(P);
}

int BLS_BLS12381_INIT()
{
    ECP2_BLS12381 G;
    if (!ECP2_BLS12381_generator(&G)) return BLS_FAIL;
    PAIR_BLS12381_precomp(G2_TAB, &G);  // should be done just once on start-up
    return BLS_OK;
}

/* generate key pair, private key S, public key W */
int BLS_BLS12381_KEY_PAIR_GENERATE(octet *IKM, octet* S, octet *W)
{
    int L;
    BIG_384_58 r,s;
    DBIG_384_58 dx;
    ECP2_BLS12381 G;
    char salt[20],prk[HASH_TYPE_BLS12381],okm[128],aikm[65],len[2];
    octet SALT = {0,sizeof(salt),salt};
    octet PRK = {0,sizeof(prk),prk};
    octet OKM = {0,sizeof(okm),okm};
    octet AIKM = {0,sizeof(aikm),aikm};
    octet LEN = {0,sizeof(len),len};

    OCT_copy(&AIKM,IKM);
    OCT_jbyte(&AIKM,0,1);

    BIG_384_58_rcopy(r, CURVE_Order_BLS12381);
    L=CEIL(3*CEIL(BIG_384_58_nbits(r),8),2);
    OCT_jint(&LEN,L,2);

    if (!ECP2_BLS12381_generator(&G)) return BLS_FAIL;

    OCT_jstring(&SALT,(char *)"BLS-SIG-KEYGEN-SALT-");
    HKDF_Extract(MC_SHA2,HASH_TYPE_BLS12381,&PRK,&SALT,&AIKM);
    HKDF_Expand(MC_SHA2,HASH_TYPE_BLS12381,&OKM,L,&PRK,&LEN);

    BIG_384_58_dfromBytesLen(dx,OKM.val,L);
    BIG_384_58_dmod(s,dx,r);
    BIG_384_58_toBytes(S->val, s);
    S->len = MODBYTES_384_58;

// SkToPk

    PAIR_BLS12381_G2mul(&G, s);
    //ECP2_BLS12381_toOctet(W, &G, true);
    ECP2_BLS12381_toOctet_ZCash(W, &G);
    return BLS_OK;
}

/* Sign message M using private key S to produce signature SIG */
int BLS_BLS12381_CORE_SIGN(octet *SIG, octet *M, octet *S)
{
    BIG_384_58 s;
    ECP_BLS12381 D;
    BLS_HASH_TO_POINT(&D, M);
    BIG_384_58_fromBytes(s, S->val);
    PAIR_BLS12381_G1mul(&D, s);
    //ECP_BLS12381_toOctet(SIG, &D, true); /* compress output */
    ECP_BLS12381_toOctet_ZCash(SIG, &D); /* compress output */
    return BLS_OK;
}

/* Verify signature of message M, the signature SIG, and the public key W */
int BLS_BLS12381_CORE_VERIFY(octet *SIG, octet *M, octet *W)
{
    FP12_BLS12381 v;
    ECP2_BLS12381 G, PK;
    ECP_BLS12381 D, HM;
    BLS_HASH_TO_POINT(&HM, M);

    //if (!ECP_BLS12381_fromOctet(&D, SIG)) return BLS_FAIL;
    if (!ECP_BLS12381_fromOctet_ZCash(&D, SIG)) return BLS_FAIL;
    if (!PAIR_BLS12381_G1member(&D)) return BLS_FAIL;
    ECP_BLS12381_neg(&D);

    //if (!ECP2_BLS12381_fromOctet(&PK, W)) return BLS_FAIL;
    if (!ECP2_BLS12381_fromOctet_ZCash(&PK, W)) return BLS_FAIL;
    if (!PAIR_BLS12381_G2member(&PK)) return BLS_FAIL;

// Use new multi-pairing mechanism

/*
    FP12_BLS12381 r[ATE_BITS_BLS12381];
    PAIR_BLS12381_initmp(r);
    PAIR_BLS12381_another_pc(r, G2_TAB, &D);
    PAIR_BLS12381_another(r, &PK, &HM);
    PAIR_BLS12381_miller(&v, r);
*/

//.. or alternatively
      if (!ECP2_BLS12381_generator(&G)) return BLS_FAIL;
      PAIR_BLS12381_double_ate(&v,&G,&D,&PK,&HM);

    PAIR_BLS12381_fexp(&v);
    if (FP12_BLS12381_isunity(&v)) return BLS_OK;
    return BLS_FAIL;
}

