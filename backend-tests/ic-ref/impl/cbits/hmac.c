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

/*
    HMAC functions
*/

#include "arch.h"
#include "core.h"

#define ROUNDUP(a,b) ((a)-1)/(b)+1
#define CEIL(a,b) (((a)-1)/(b)+1)

/* General Purpose hash function, padding with zeros, optional input octets p and x, optional integer n,hash to octet w of length olen */
/* hash is the Hash family, either MC_SHA2 or MC_SHA3 */
/* hlen should be 32,48 or 64 for MC_SHA2 (that is SHA256/384/512) */
/* hlen should be 24,32,48,64 for MC_SHA3 */
/* olen=0 -     output = hlen bytes */
/* olen<=hlen -  output = olen bytes */
/* olen>hlen  -  output is padded with leading zeros and then hlen bytes */


void GPhash(int hash,int hlen,octet *w,int olen,int pad,octet *p,int n,octet *x)
{
    hash256 sh256;
    hash384 sh384;
    hash512 sh512;
    sha3 sh3;
    int i,c[4];
    char hh[64];

    if (n>=0)
    {
        c[0] = (n >> 24) & 0xff;
        c[1] = (n >> 16) & 0xff;
        c[2] = (n >> 8) & 0xff;
        c[3] = (n) & 0xff;
    }

    switch (hash)
    {
    case MC_SHA2 :
        switch (hlen)
        {
        case SHA256 :
            HASH256_init(&sh256);
            for (i=0;i<pad;i++) HASH256_process(&sh256,0);
            if (p!=NULL)
                for (i=0;i<p->len;i++) HASH256_process(&sh256,p->val[i]);
            if (n>=0)
                for (i=0;i<4;i++) HASH256_process(&sh256,c[i]);
            if (x!=NULL)
                for (i=0;i<x->len;i++) HASH256_process(&sh256,x->val[i]);
            HASH256_hash(&sh256,hh);
            break;
        case SHA384 :
            HASH384_init(&sh384);
            for (i=0;i<pad;i++) HASH384_process(&sh384,0);
            if (p!=NULL)
                for (i=0;i<p->len;i++) HASH384_process(&sh384,p->val[i]);
            if (n>=0)
                for (i=0;i<4;i++) HASH384_process(&sh384,c[i]);
            if (x!=NULL)
                for (i=0;i<x->len;i++) HASH384_process(&sh384,x->val[i]);
            HASH384_hash(&sh384,hh);
            break;
        case SHA512 :
            HASH512_init(&sh512);
            for (i=0;i<pad;i++) HASH512_process(&sh512,0);
            if (p!=NULL)
                for (i=0;i<p->len;i++) HASH512_process(&sh512,p->val[i]);
            if (n>=0)
                for (i=0;i<4;i++) HASH512_process(&sh512,c[i]);
            if (x!=NULL)
                for (i=0;i<x->len;i++) HASH512_process(&sh512,x->val[i]);
            HASH512_hash(&sh512,hh);   
            break;
        }
        break;
    case MC_SHA3 :
        SHA3_init(&sh3,hlen);
        for (i=0;i<pad;i++) SHA3_process(&sh3,0);
        if (p!=NULL)
            for (i=0;p->len;i++) SHA3_process(&sh3,p->val[i]);
        if (n>=0)
            for (i=0;i<4;i++) SHA3_process(&sh3,c[i]);
        if (x!=NULL)
            for (i=0;x->len;i++) SHA3_process(&sh3,x->val[i]);
        SHA3_hash(&sh3,hh);  
        break;
    default: return;
    }
    OCT_empty(w);
    if (!olen)
        OCT_jbytes(w,hh,hlen);
    else
    {
        if (olen<=hlen)
        {
            OCT_jbytes(w,hh,olen);
        } else {
            OCT_jbyte(w, 0, olen - hlen);
            OCT_jbytes(w, hh, hlen);
        }
    }
}

/* Simple hash octet p to octet w of length hlen */
void SPhash(int hash, int hlen,octet *w, octet *p)
{
    GPhash(hash, hlen, w, 0, 0, p, -1, NULL);
}


static int blksize(int hash,int hlen)
{
    int blk=0;
    switch (hash)
    {
    case MC_SHA2 :
            blk=64;
            if (hlen>32) blk=128;
            break;
    case MC_SHA3 :
            blk=200-2*hlen;
            break;
    default: break;
    }
    return blk;
}

/* RFC 2104 */
void HMAC(int hash,int hlen,octet *TAG,int olen,octet *K,octet *M)
{
    int blk;
    char h[128],k0[200];   // assumes max block sizes
    octet K0 = {0, sizeof(k0), k0};
    octet H={0,sizeof(h),h};

    blk=blksize(hash,hlen);
    if (blk==0) return;

    if (K->len > blk) SPhash(hash,hlen,&K0,K);
    else              OCT_copy(&K0,K);
    OCT_jbyte(&K0,0,blk-K0.len); 
    OCT_xorbyte(&K0,0x36);

    GPhash(hash,hlen,&H,0,0,&K0,-1,M); 

    OCT_xorbyte(&K0,0x6a);   /* 0x6a = 0x36 ^ 0x5c */
    GPhash(hash,hlen,&H,0,0,&K0,-1,&H);

    OCT_empty(TAG);
    OCT_jbytes(TAG,H.val,olen);

    OCT_clear(&H);
    OCT_clear(&K0);
}

/* RFC 5869 */

void HKDF_Extract(int hash,int hlen,octet *PRK,octet *SALT,octet *IKM)
{
    char h[64];
    octet H={0,sizeof(h),h};
    if (SALT==NULL) {
        OCT_jbyte(&H,0,hlen);
        HMAC(hash,hlen,PRK,hlen,&H,IKM);
    } else {
        HMAC(hash,hlen,PRK,hlen,SALT,IKM);
    }
}

void HKDF_Expand(int hash,int hlen,octet *OKM,int olen,octet *PRK,octet *INFO)
{
    int i;
    char t[1024];    // >= info.length+hlen+1
    octet T={0,sizeof(t),t};
    int n=olen/hlen; 
    int flen=olen%hlen;

    OCT_empty(OKM);
    for (i=1;i<=n;i++)
    {
        OCT_joctet(&T,INFO);
        OCT_jbyte(&T,i,1);
        HMAC(hash,hlen,&T,hlen,PRK,&T);
        OCT_joctet(OKM,&T);
    }
    if (flen>0)
    {
        OCT_joctet(&T,INFO);
        OCT_jbyte(&T,n+1,1);
        HMAC(hash,hlen,&T,flen,PRK,&T);
        OCT_joctet(OKM,&T);
    }
}

/* https://datatracker.ietf.org/doc/draft-irtf-cfrg-hash-to-curve/ */

void XOF_Expand(int hlen,octet *OKM,int olen,octet *DST,octet *M)
{
    int i;
    sha3 SHA3;
    SHA3_init(&SHA3,hlen);
    for (i=0;i<M->len;i++) SHA3_process(&SHA3,M->val[i]);
    SHA3_process(&SHA3,olen/256);
    SHA3_process(&SHA3,olen%256);

    for (i=0;i<DST->len;i++)
        SHA3_process(&SHA3,DST->val[i]);
    SHA3_process(&SHA3,DST->len);


    SHA3_shake(&SHA3,OKM->val,olen);
    OKM->len=olen;
}

void XMD_Expand(int hash, int hlen,octet *OKM,int olen,octet *DST,octet *M)
{
    int i,blk;
    int ell=CEIL(olen,hlen);
    char tmp[260]; 
    octet TMP={0,sizeof(tmp),tmp};
    char h0[64];
    octet H0 = {0, sizeof(h0), h0};
    char h1[64];
    octet H1 = {0, sizeof(h1), h1};

    blk=blksize(hash,hlen);
    OCT_jint(&TMP,olen,2);
    OCT_jint(&TMP,0,1);
    OCT_joctet(&TMP,DST);
    OCT_jint(&TMP,DST->len,1);

    GPhash(hash,hlen,&H0,0,blk,M,-1,&TMP);
    OCT_empty(&TMP);
    OCT_jint(&TMP,1,1);
    OCT_joctet(&TMP,DST);
    OCT_jint(&TMP,DST->len,1);

    GPhash(hash,hlen,&H1,0,0,&H0,-1,&TMP);
    OCT_empty(OKM);
    OCT_joctet(OKM,&H1);
    for (i=2;i<=ell;i++)
    {
        OCT_xor(&H1,&H0);
        OCT_empty(&TMP);
        OCT_jint(&TMP,i,1);
        OCT_joctet(&TMP,DST);
        OCT_jint(&TMP,DST->len,1);
        GPhash(hash,hlen,&H1,0,0,&H1,-1,&TMP);
        OCT_joctet(OKM,&H1);
    }
    OKM->len=olen; 
}

/* Key Derivation Function */

void KDF2(int hash, int hlen, octet *key, int olen, octet *z, octet *p)
{
    /* NOTE: the parameter olen is the length of the output k in bytes */
    char h[64];
    octet H = {0, sizeof(h), h};
    int counter, cthreshold;

    OCT_empty(key);

    cthreshold = ROUNDUP(olen, hlen);

    for (counter = 1; counter <= cthreshold; counter++)
    {
        GPhash(hash,hlen, &H, 0, 0, z, counter, p);
        if (key->len + hlen > olen)  OCT_jbytes(key, H.val, olen % hlen);
        else                     OCT_joctet(key, &H);
    }

}

/* Password based Key Derivation Function */
/* Input password p, salt s, and repeat count */
/* Output key of length olen */
void PBKDF2(int hash, int hlen, octet *key, int olen, octet *p, octet *s, int rep)
{
    int i, j, len, d = ROUNDUP(olen, hlen);
    char f[64], u[64];
    octet F = {0, sizeof(f), f};
    octet U = {0, sizeof(u), u};
    OCT_empty(key);

    for (i = 1; i <= d; i++)
    {
        len = s->len;
        OCT_jint(s, i, 4);

        HMAC(hash, hlen, &F, hlen, s, p);

        s->len = len;
        OCT_copy(&U, &F);
        for (j = 2; j <= rep; j++)
        {
            HMAC(hash, hlen, &U, hlen, &U, p);
            OCT_xor(&F, &U);
        }

        OCT_joctet(key, &F);
    }

    OCT_chop(key, NULL, olen);
}

/* gcc -O2 hmac.c oct.c hash.c rand.c -o hmac  

int main()
{
    char ikm[22],salt[13],prk[32],info[10],okm[50];  
    octet IKM = {0, sizeof(ikm), ikm};
    octet SALT={0,sizeof(salt),salt};
    octet PRK={0,sizeof(prk),prk};
    octet OKM={0,sizeof(okm),okm};
    octet INFO={0,sizeof(info),info};
    int i;
    for (i=0;i<22;i++) IKM.val[i]=0x0b;
    for (i=0;i<13;i++) SALT.val[i]=i;
    for (i=0;i<10;i++) INFO.val[i]=0xf0+i;
    
    IKM.len=22; SALT.len=13; INFO.len=10;

    printf("IKM= "); OCT_output(&IKM); 
    printf("SALT= "); OCT_output(&SALT); 

    HKDF_Extract(MC_SHA2,32,&PRK,&SALT,&IKM);

    //HMAC(&PRK,32,&SALT,&IKM,SHA2,32);

    printf("PRK= "); OCT_output(&PRK); 

    HKDF_Expand(MC_SHA2,32,&OKM,42,&PRK,&INFO);

    printf("OKM= %d ",OKM.len); OCT_output(&OKM); 
}
*/

