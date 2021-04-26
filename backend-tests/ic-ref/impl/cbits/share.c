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

/* Shamir threshold secret sharing module */
/* Split any octet into number of shares <256 */
/* Specify number of shares required for recovery - nsr */

/* See testmpin.c for an example of use */

#include "arch.h"
#include "core.h"

/* Field GF(2^8) precalculated tables */

static const uchar ptab[] =
{
    1, 3, 5, 15, 17, 51, 85, 255, 26, 46, 114, 150, 161, 248, 19, 53,
    95, 225, 56, 72, 216, 115, 149, 164, 247, 2, 6, 10, 30, 34, 102, 170,
    229, 52, 92, 228, 55, 89, 235, 38, 106, 190, 217, 112, 144, 171, 230, 49,
    83, 245, 4, 12, 20, 60, 68, 204, 79, 209, 104, 184, 211, 110, 178, 205,
    76, 212, 103, 169, 224, 59, 77, 215, 98, 166, 241, 8, 24, 40, 120, 136,
    131, 158, 185, 208, 107, 189, 220, 127, 129, 152, 179, 206, 73, 219, 118, 154,
    181, 196, 87, 249, 16, 48, 80, 240, 11, 29, 39, 105, 187, 214, 97, 163,
    254, 25, 43, 125, 135, 146, 173, 236, 47, 113, 147, 174, 233, 32, 96, 160,
    251, 22, 58, 78, 210, 109, 183, 194, 93, 231, 50, 86, 250, 21, 63, 65,
    195, 94, 226, 61, 71, 201, 64, 192, 91, 237, 44, 116, 156, 191, 218, 117,
    159, 186, 213, 100, 172, 239, 42, 126, 130, 157, 188, 223, 122, 142, 137, 128,
    155, 182, 193, 88, 232, 35, 101, 175, 234, 37, 111, 177, 200, 67, 197, 84,
    252, 31, 33, 99, 165, 244, 7, 9, 27, 45, 119, 153, 176, 203, 70, 202,
    69, 207, 74, 222, 121, 139, 134, 145, 168, 227, 62, 66, 198, 81, 243, 14,
    18, 54, 90, 238, 41, 123, 141, 140, 143, 138, 133, 148, 167, 242, 13, 23,
    57, 75, 221, 124, 132, 151, 162, 253, 28, 36, 108, 180, 199, 82, 246, 1
};

static const uchar ltab[] =
{
    0, 255, 25, 1, 50, 2, 26, 198, 75, 199, 27, 104, 51, 238, 223, 3,
    100, 4, 224, 14, 52, 141, 129, 239, 76, 113, 8, 200, 248, 105, 28, 193,
    125, 194, 29, 181, 249, 185, 39, 106, 77, 228, 166, 114, 154, 201, 9, 120,
    101, 47, 138, 5, 33, 15, 225, 36, 18, 240, 130, 69, 53, 147, 218, 142,
    150, 143, 219, 189, 54, 208, 206, 148, 19, 92, 210, 241, 64, 70, 131, 56,
    102, 221, 253, 48, 191, 6, 139, 98, 179, 37, 226, 152, 34, 136, 145, 16,
    126, 110, 72, 195, 163, 182, 30, 66, 58, 107, 40, 84, 250, 133, 61, 186,
    43, 121, 10, 21, 155, 159, 94, 202, 78, 212, 172, 229, 243, 115, 167, 87,
    175, 88, 168, 80, 244, 234, 214, 116, 79, 174, 233, 213, 231, 230, 173, 232,
    44, 215, 117, 122, 235, 22, 11, 245, 89, 203, 95, 176, 156, 169, 81, 160,
    127, 12, 246, 111, 23, 196, 73, 236, 216, 67, 31, 45, 164, 118, 123, 183,
    204, 187, 62, 90, 251, 96, 177, 134, 59, 82, 161, 108, 170, 85, 41, 157,
    151, 178, 135, 144, 97, 190, 220, 252, 188, 149, 207, 205, 55, 63, 91, 209,
    83, 57, 132, 60, 65, 162, 109, 71, 20, 42, 158, 93, 86, 242, 211, 171,
    68, 17, 146, 217, 35, 32, 46, 137, 180, 124, 184, 38, 119, 153, 227, 165,
    103, 74, 237, 222, 197, 49, 254, 24, 13, 99, 140, 128, 192, 247, 112, 7
};

/* field addition */
static uchar add(uchar x,uchar y)
{
    return (x^y);
}

/* x.y= AntiLog(Log(x) + Log(y)) */
static uchar mul(uchar x,uchar y)
{ 
    if (x && y) return ptab[(ltab[x]+ltab[y])%255];
    else return 0;
}

/* multiplicative inverse */
static uchar inv(uchar x)
{
    return ptab[255-ltab[x]];  
}

/* Lagrange interpolation */
static int interpolate(int n, uchar x[], uchar y[])
{
    int i,j;
    uchar p,yp=0;
    for(i=0;i<n;i++)
    {
        p=1;
        for (j=0;j<n;j++)
            if (i!=j) p=mul(p,mul(x[j],inv(add(x[i],x[j]))));
        yp=add(yp,mul(p,y[i]));
    }
    return yp;
}

/* return a share of M */
/* input id - Unique share ID */
/* input nsr - Number of shares required for recovery */
/* output S - share as octet */
/* input Message M to be shared */
/* input Random seed R */
/* return share structure */

share getshare(int id,int nsr,octet *S,octet *M,octet *R)
{
    csprng rng;                // Crypto Strong RNG
    share Sh;
    int j,n,m;
    uchar x;
    if (id<1 || id>=256 || nsr<2 || nsr>=256)
    {
        Sh.id=0;
        Sh.nsr=0;
        Sh.B=NULL;
        return Sh;
    }
    RAND_clean(&rng);
    RAND_seed(&rng,R->len,R->val);
    Sh.id=id;
    Sh.nsr=nsr;
    Sh.B=S;
    m=M->len;
    S->len=m;
    for (j=0;j<m;j++)
    {
        x=(uchar)id;
        S->val[j]=M->val[j];
        for (n=1;n<nsr;n++ )
        {
            S->val[j]=add(S->val[j],mul(RAND_byte(&rng),x));
            x=mul(x,(uchar)id);
        }
    }
    return Sh;
}

/* Recover message from shares */

int recover(octet *M,share S[])
{
    uchar x[256],y[256];
    int i,j,res=0;
    int len=S[0].B->len;
    int nsr=S[0].nsr;
    for (i=1;i<nsr;i++)
    {
        if (S[i].nsr!=nsr || S[i].B->len!=len)
        {
            res=-1;
            break;
        }
    }
    if (res) return res;

    for (j=0;j<len;j++)
    {
        for (i=0;i<nsr;i++)
        {
            x[i]=S[i].id;
            y[i]=S[i].B->val[j];
        }
        M->val[j]=interpolate(nsr,x,y);
    }
    M->len=len;
    return res;
}

/*
#include <stdio.h>
#include <time.h>

int main()
{ // test driver 
    int ii,j,k,l,m,n,nsr;
    char mc[10],b1c[10],b2c[10],b3c[10],b4c[10],r[30];
    octet M={0,sizeof(mc),mc};
    octet B1={0,sizeof(b1c),b1c};
    octet B2={0,sizeof(b2c),b2c};
    octet B3={0,sizeof(b3c),b3c};
    octet B4={0,sizeof(b4c),b4c};
    octet R={0,sizeof(r),r};
    share S[3];

    nsr=3;

    srand(time(NULL));

    M.len=5;
    M.val[0]=rand()%128; M.val[1]=rand()%128; M.val[2]=rand()%128; M.val[3]=rand()%128; M.val[4]=rand()%128;

    printf("Message= "); OCT_output(&M);

    R.len=(nsr-1)*M.len;
    for (j=0;j<R.len;j++)
        R.val[j]=rand()%256;

// 3 out of 4
    share S0=getshare(1,nsr,&B1,&M,&R);  // R is random number seed
    share S1=getshare(2,nsr,&B2,&M,&R);
    share S2=getshare(3,nsr,&B3,&M,&R);
    share S3=getshare(4,nsr,&B4,&M,&R);

    printf("Share 1= "); OCT_output(&B1);
    printf("Share 2= "); OCT_output(&B2);
    printf("Share 3= "); OCT_output(&B3);
    printf("Share 4= "); OCT_output(&B4);

    S[0]=S0;  // pick any 3 shares
    S[1]=S1;
    S[2]=S3;

    OCT_clear(&M);

    recover(&M,S);

    printf("Message= "); OCT_output(&M);

    return 0;
}
*/

