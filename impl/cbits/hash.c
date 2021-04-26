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
 * Implementation of the Secure Hashing Algorithm (SHA-256/384/512 and SHA3)
 *
 * Generates a message digest. It should be impossible to come
 * come up with two messages that hash to the same value ("collision free").
 *
 * For use with byte-oriented messages only. Could/Should be speeded
 * up by unwinding loops in HASH_transform(), and assembly patches.
 */

#include "arch.h"
#include "core.h"


#define H0_256 0x6A09E667L
#define H1_256 0xBB67AE85L
#define H2_256 0x3C6EF372L
#define H3_256 0xA54FF53AL
#define H4_256 0x510E527FL
#define H5_256 0x9B05688CL
#define H6_256 0x1F83D9ABL
#define H7_256 0x5BE0CD19L

static const unsign32 K_256[64] =
{
    0x428a2f98L, 0x71374491L, 0xb5c0fbcfL, 0xe9b5dba5L, 0x3956c25bL, 0x59f111f1L, 0x923f82a4L, 0xab1c5ed5L,
    0xd807aa98L, 0x12835b01L, 0x243185beL, 0x550c7dc3L, 0x72be5d74L, 0x80deb1feL, 0x9bdc06a7L, 0xc19bf174L,
    0xe49b69c1L, 0xefbe4786L, 0x0fc19dc6L, 0x240ca1ccL, 0x2de92c6fL, 0x4a7484aaL, 0x5cb0a9dcL, 0x76f988daL,
    0x983e5152L, 0xa831c66dL, 0xb00327c8L, 0xbf597fc7L, 0xc6e00bf3L, 0xd5a79147L, 0x06ca6351L, 0x14292967L,
    0x27b70a85L, 0x2e1b2138L, 0x4d2c6dfcL, 0x53380d13L, 0x650a7354L, 0x766a0abbL, 0x81c2c92eL, 0x92722c85L,
    0xa2bfe8a1L, 0xa81a664bL, 0xc24b8b70L, 0xc76c51a3L, 0xd192e819L, 0xd6990624L, 0xf40e3585L, 0x106aa070L,
    0x19a4c116L, 0x1e376c08L, 0x2748774cL, 0x34b0bcb5L, 0x391c0cb3L, 0x4ed8aa4aL, 0x5b9cca4fL, 0x682e6ff3L,
    0x748f82eeL, 0x78a5636fL, 0x84c87814L, 0x8cc70208L, 0x90befffaL, 0xa4506cebL, 0xbef9a3f7L, 0xc67178f2L
};

#define PAD  0x80
#define ZERO 0

/* functions */

#define S(m,n,x) (((x)>>n) | ((x)<<(m-n)))
#define R(n,x) ((x)>>n)

#define Ch(x,y,z)  ((x&y)^(~(x)&z))
#define Maj(x,y,z) ((x&y)^(x&z)^(y&z))
#define Sig0_256(x)    (S(32,2,x)^S(32,13,x)^S(32,22,x))
#define Sig1_256(x)    (S(32,6,x)^S(32,11,x)^S(32,25,x))
#define theta0_256(x)  (S(32,7,x)^S(32,18,x)^R(3,x))
#define theta1_256(x)  (S(32,17,x)^S(32,19,x)^R(10,x))

#define Sig0_512(x)    (S(64,28,x)^S(64,34,x)^S(64,39,x))
#define Sig1_512(x)    (S(64,14,x)^S(64,18,x)^S(64,41,x))
#define theta0_512(x)  (S(64,1,x)^S(64,8,x)^R(7,x))
#define theta1_512(x)  (S(64,19,x)^S(64,61,x)^R(6,x))


/* SU= 72 */
static void HASH256_transform(hash256 *sh)
{
    /* basic transformation step */
    unsign32 a, b, c, d, e, f, g, h, t1, t2;
    int j;
    for (j = 16; j < 64; j++)
        sh->w[j] = theta1_256(sh->w[j - 2]) + sh->w[j - 7] + theta0_256(sh->w[j - 15]) + sh->w[j - 16];

    a = sh->h[0];
    b = sh->h[1];
    c = sh->h[2];
    d = sh->h[3];
    e = sh->h[4];
    f = sh->h[5];
    g = sh->h[6];
    h = sh->h[7];

    for (j = 0; j < 64; j++)
    {
        /* 64 times - mush it up */
        t1 = h + Sig1_256(e) + Ch(e, f, g) + K_256[j] + sh->w[j];
        t2 = Sig0_256(a) + Maj(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }

    sh->h[0] += a;
    sh->h[1] += b;
    sh->h[2] += c;
    sh->h[3] += d;
    sh->h[4] += e;
    sh->h[5] += f;
    sh->h[6] += g;
    sh->h[7] += h;
}

/* Initialise Hash function */
void HASH256_init(hash256 *sh)
{
    /* re-initialise */
    int i;
    for (i = 0; i < 64; i++) sh->w[i] = 0L;
    sh->length[0] = sh->length[1] = 0L;
    sh->h[0] = H0_256;
    sh->h[1] = H1_256;
    sh->h[2] = H2_256;
    sh->h[3] = H3_256;
    sh->h[4] = H4_256;
    sh->h[5] = H5_256;
    sh->h[6] = H6_256;
    sh->h[7] = H7_256;

    sh->hlen = 32;
}

/* process a single byte */
void HASH256_process(hash256 *sh, int byt)
{
    /* process the next message byte */
    int cnt;
    cnt = (int)((sh->length[0] / 32) % 16);

    sh->w[cnt] <<= 8;
    sh->w[cnt] |= (unsign32)(byt & 0xFF);

    sh->length[0] += 8;
    if (sh->length[0] == 0L)
    {
        sh->length[1]++;
        sh->length[0] = 0L;
    }
    if ((sh->length[0] % 512) == 0) HASH256_transform(sh);
}

/* SU= 24 */
/* Generate 32-byte Hash */
void HASH256_hash(hash256 *sh, char *digest)
{
    /* pad message and finish - supply digest */
    int i;
    unsign32 len0, len1;
    len0 = sh->length[0];
    len1 = sh->length[1];
    HASH256_process(sh, PAD);
    while ((sh->length[0] % 512) != 448) HASH256_process(sh, ZERO);
    sh->w[14] = len1;
    sh->w[15] = len0;
    HASH256_transform(sh);
    for (i = 0; i < sh->hlen; i++)
    {
        /* convert to bytes */
        digest[i] = (char)((sh->h[i / 4] >> (8 * (3 - i % 4))) & 0xffL);
    }
    HASH256_init(sh);
}


#define H0_512 0x6a09e667f3bcc908
#define H1_512 0xbb67ae8584caa73b
#define H2_512 0x3c6ef372fe94f82b
#define H3_512 0xa54ff53a5f1d36f1
#define H4_512 0x510e527fade682d1
#define H5_512 0x9b05688c2b3e6c1f
#define H6_512 0x1f83d9abfb41bd6b
#define H7_512 0x5be0cd19137e2179

#define H8_512 0xcbbb9d5dc1059ed8
#define H9_512 0x629a292a367cd507
#define HA_512 0x9159015a3070dd17
#define HB_512 0x152fecd8f70e5939
#define HC_512 0x67332667ffc00b31
#define HD_512 0x8eb44a8768581511
#define HE_512 0xdb0c2e0d64f98fa7
#define HF_512 0x47b5481dbefa4fa4

/* */

static const unsign64 K_512[80] =
{
    0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
    0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
    0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
    0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
    0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
    0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
    0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
    0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
    0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
    0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
    0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
    0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
    0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
    0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
    0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
    0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
    0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
    0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
    0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
    0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817
};


static void HASH512_transform(hash512 *sh)
{
    /* basic transformation step */
    unsign64 a, b, c, d, e, f, g, h, t1, t2;
    int j;
    for (j = 16; j < 80; j++)
        sh->w[j] = theta1_512(sh->w[j - 2]) + sh->w[j - 7] + theta0_512(sh->w[j - 15]) + sh->w[j - 16];

    a = sh->h[0];
    b = sh->h[1];
    c = sh->h[2];
    d = sh->h[3];
    e = sh->h[4];
    f = sh->h[5];
    g = sh->h[6];
    h = sh->h[7];

    for (j = 0; j < 80; j++)
    {
        /* 80 times - mush it up */
        t1 = h + Sig1_512(e) + Ch(e, f, g) + K_512[j] + sh->w[j];
        t2 = Sig0_512(a) + Maj(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }
    sh->h[0] += a;
    sh->h[1] += b;
    sh->h[2] += c;
    sh->h[3] += d;
    sh->h[4] += e;
    sh->h[5] += f;
    sh->h[6] += g;
    sh->h[7] += h;
}

void HASH384_init(hash384 *sh)
{
    /* re-initialise */
    int i;
    for (i = 0; i < 80; i++) sh->w[i] = 0;
    sh->length[0] = sh->length[1] = 0;
    sh->h[0] = H8_512;
    sh->h[1] = H9_512;
    sh->h[2] = HA_512;
    sh->h[3] = HB_512;
    sh->h[4] = HC_512;
    sh->h[5] = HD_512;
    sh->h[6] = HE_512;
    sh->h[7] = HF_512;

    sh->hlen = 48;

}

void HASH384_process(hash384 *sh, int byt)
{
    /* process the next message byte */
    HASH512_process(sh, byt);
}

void HASH384_hash(hash384 *sh, char *hash)
{
    /* pad message and finish - supply digest */
    HASH512_hash(sh, hash);
}

void HASH512_init(hash512 *sh)
{
    /* re-initialise */
    int i;

    for (i = 0; i < 80; i++) sh->w[i] = 0;
    sh->length[0] = sh->length[1] = 0;
    sh->h[0] = H0_512;
    sh->h[1] = H1_512;
    sh->h[2] = H2_512;
    sh->h[3] = H3_512;
    sh->h[4] = H4_512;
    sh->h[5] = H5_512;
    sh->h[6] = H6_512;
    sh->h[7] = H7_512;

    sh->hlen = 64;
}

void HASH512_process(hash512 *sh, int byt)
{
    /* process the next message byte */
    int cnt;

    cnt = (int)((sh->length[0] / 64) % 16);

    sh->w[cnt] <<= 8;
    sh->w[cnt] |= (unsign64)(byt & 0xFF);

    sh->length[0] += 8;
    if (sh->length[0] == 0L)
    {
        sh->length[1]++;
        sh->length[0] = 0L;
    }
    if ((sh->length[0] % 1024) == 0) HASH512_transform(sh);
}

void HASH512_hash(hash512 *sh, char *hash)
{
    /* pad message and finish - supply digest */
    int i;
    unsign64 len0, len1;
    len0 = sh->length[0];
    len1 = sh->length[1];
    HASH512_process(sh, PAD);
    while ((sh->length[0] % 1024) != 896) HASH512_process(sh, ZERO);
    sh->w[14] = len1;
    sh->w[15] = len0;
    HASH512_transform(sh);
    for (i = 0; i < sh->hlen; i++)
    {
        /* convert to bytes */
        hash[i] = (char)((sh->h[i / 8] >> (8 * (7 - i % 8))) & 0xffL);
    }
    HASH512_init(sh);
}



/* SHA3 */

#define SHA3_ROUNDS 24
#define rotl(x,n) (((x)<<n) | ((x)>>(64-n)))

/* round constants */

static const unsign64 RC[24] =
{
    0x0000000000000001UL, 0x0000000000008082UL, 0x800000000000808AUL, 0x8000000080008000UL,
    0x000000000000808BUL, 0x0000000080000001UL, 0x8000000080008081UL, 0x8000000000008009UL,
    0x000000000000008AUL, 0x0000000000000088UL, 0x0000000080008009UL, 0x000000008000000AUL,
    0x000000008000808BUL, 0x800000000000008BUL, 0x8000000000008089UL, 0x8000000000008003UL,
    0x8000000000008002UL, 0x8000000000000080UL, 0x000000000000800AUL, 0x800000008000000AUL,
    0x8000000080008081UL, 0x8000000000008080UL, 0x0000000080000001UL, 0x8000000080008008UL
};

/* permutation */

static void SHA3_transform(sha3 *sh)
{
    int i, j, k;
    unsign64 C[5], D[5], B[5][5];

    for (k = 0; k < SHA3_ROUNDS; k++)
    {
        C[0] = sh->S[0][0] ^ sh->S[0][1] ^ sh->S[0][2] ^ sh->S[0][3] ^ sh->S[0][4];
        C[1] = sh->S[1][0] ^ sh->S[1][1] ^ sh->S[1][2] ^ sh->S[1][3] ^ sh->S[1][4];
        C[2] = sh->S[2][0] ^ sh->S[2][1] ^ sh->S[2][2] ^ sh->S[2][3] ^ sh->S[2][4];
        C[3] = sh->S[3][0] ^ sh->S[3][1] ^ sh->S[3][2] ^ sh->S[3][3] ^ sh->S[3][4];
        C[4] = sh->S[4][0] ^ sh->S[4][1] ^ sh->S[4][2] ^ sh->S[4][3] ^ sh->S[4][4];

        D[0] = C[4] ^ rotl(C[1], 1);
        D[1] = C[0] ^ rotl(C[2], 1);
        D[2] = C[1] ^ rotl(C[3], 1);
        D[3] = C[2] ^ rotl(C[4], 1);
        D[4] = C[3] ^ rotl(C[0], 1);

        for (i = 0; i < 5; i++)
            for (j = 0; j < 5; j++)
                sh->S[i][j] ^= D[i]; /* let the compiler unroll it! */

        B[0][0] = sh->S[0][0];
        B[1][3] = rotl(sh->S[0][1], 36);
        B[2][1] = rotl(sh->S[0][2], 3);
        B[3][4] = rotl(sh->S[0][3], 41);
        B[4][2] = rotl(sh->S[0][4], 18);

        B[0][2] = rotl(sh->S[1][0], 1);
        B[1][0] = rotl(sh->S[1][1], 44);
        B[2][3] = rotl(sh->S[1][2], 10);
        B[3][1] = rotl(sh->S[1][3], 45);
        B[4][4] = rotl(sh->S[1][4], 2);

        B[0][4] = rotl(sh->S[2][0], 62);
        B[1][2] = rotl(sh->S[2][1], 6);
        B[2][0] = rotl(sh->S[2][2], 43);
        B[3][3] = rotl(sh->S[2][3], 15);
        B[4][1] = rotl(sh->S[2][4], 61);

        B[0][1] = rotl(sh->S[3][0], 28);
        B[1][4] = rotl(sh->S[3][1], 55);
        B[2][2] = rotl(sh->S[3][2], 25);
        B[3][0] = rotl(sh->S[3][3], 21);
        B[4][3] = rotl(sh->S[3][4], 56);

        B[0][3] = rotl(sh->S[4][0], 27);
        B[1][1] = rotl(sh->S[4][1], 20);
        B[2][4] = rotl(sh->S[4][2], 39);
        B[3][2] = rotl(sh->S[4][3], 8);
        B[4][0] = rotl(sh->S[4][4], 14);

        for (i = 0; i < 5; i++)
            for (j = 0; j < 5; j++)
                sh->S[i][j] = B[i][j] ^ (~B[(i + 1) % 5][j] & B[(i + 2) % 5][j]);

        sh->S[0][0] ^= RC[k];
    }
}

/* Re-Initialize. olen is output length in bytes -
   should be 28, 32, 48 or 64 (224, 256, 384, 512 bits resp.) */

void SHA3_init(sha3 *sh, int olen)
{
    int i, j;
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            sh->S[i][j] = 0;  /* 5x5x8 bytes = 200 bytes of state */
    sh->length = 0;
    sh->len = olen;
    sh->rate = 200 - 2 * olen; /* number of bytes consumed in one gulp. Note that some bytes in the
                            state ("capacity") are not touched. Gulps are smaller for larger digests.
                            Important that olen<rate */
}

/* process a single byte */
void SHA3_process(sha3 *sh, int byt)
{
    int cnt = (int)(sh->length % sh->rate);
    int i, j, b = cnt % 8;
    cnt /= 8;
    i = cnt % 5;
    j = cnt / 5; /* process by columns! */
    sh->S[i][j] ^= ((unsign64)byt << (8 * b));
    sh->length++;
    if (sh->length % sh->rate == 0) SHA3_transform(sh);
}

/* squeeze the sponge */
void SHA3_squeeze(sha3 *sh, char *buff, int len)
{
    int done, i, j, k, m = 0;
    unsign64 el;
    /* extract by columns */
    done = 0;
    for (;;)
    {
        for (j = 0; j < 5; j++)
        {
            for (i = 0; i < 5; i++)
            {
                el = sh->S[i][j];
                for (k = 0; k < 8; k++)
                {
                    buff[m++] = (el & 0xff);
                    if (m >= len || m % sh->rate == 0)
                    {
                        done = 1;
                        break;
                    }
                    el >>= 8;
                }
                if (done) break;
            }
            if (done) break;
        }
        if (m >= len) break;
        done = 0;
        SHA3_transform(sh);
    }
}

void SHA3_hash(sha3 *sh, char *hash)
{
    /* generate a SHA3 hash of appropriate size */
    int q = sh->rate - (sh->length % sh->rate);
    if (q == 1) SHA3_process(sh, 0x86);
    else
    {
        SHA3_process(sh, 0x06);  /* 0x06 for SHA-3 */
        while ((int)sh->length % sh->rate != sh->rate - 1) SHA3_process(sh, 0x00);
        SHA3_process(sh, 0x80); /* this will force a final transform */
    }
    SHA3_squeeze(sh, hash, sh->len);
}

void SHA3_shake(sha3 *sh, char *buff, int len)
{
    /* SHAKE out a buffer of variable length len */
    int q = sh->rate - (sh->length % sh->rate);
    if (q == 1) SHA3_process(sh, 0x9f);
    else
    {
        SHA3_process(sh, 0x1f);  // 0x06 for SHA-3 !!!!
        while ((int) sh->length % sh->rate != sh->rate - 1) SHA3_process(sh, 0x00);
        SHA3_process(sh, 0x80); /* this will force a final transform */
    }
    SHA3_squeeze(sh, buff, len);
}


/* test program: should produce digest

160 bit

84983e44 1c3bd26e baae4aa1 f95129e5 e54670f1

256 bit

248d6a61 d20638b8 e5c02693 0c3e6039 a33ce459 64ff2167 f6ecedd4 19db06c1

512 bit

8e959b75dae313da 8cf4f72814fc143f 8f7779c6eb9f7fa1 7299aeadb6889018
501d289e4900f7e4 331b99dec4b5433a c7d329eeb6dd2654 5e96e55b874be909

384 bit

09330c33f71147e8 3d192fc782cd1b47 53111b173b3b05d2 2fa08086e3b0f712
fcc7c71a557e2db9 66c3e9fa91746039
*/
/*
#include <stdio.h>

char test160[]="abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";
char test256[]="abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";
char test512[]="abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";

int main()
{
    char digest[100];
    int i;

    hash256 sh256;
    hash384 sh384;
    hash512 sh512;
    sha3 SHA3;

    HASH256_init(&sh256);
    for (i=0;test256[i]!=0;i++) HASH256_process(&sh256,test256[i]);
    HASH256_hash(&sh256,digest);
    for (i=0;i<32;i++) printf("%02x",(unsigned char)digest[i]);
    printf("\n");

    HASH384_init(&sh384);
    for (i=0;test512[i]!=0;i++) HASH384_process(&sh384,test512[i]);
    HASH384_hash(&sh384,digest);
    for (i=0;i<48;i++) printf("%02x",(unsigned char)digest[i]);
    printf("\n");

    HASH512_init(&sh512);
    for (i=0;test512[i]!=0;i++) HASH512_process(&sh512,test512[i]);
    HASH512_hash(&sh512,digest);
    for (i=0;i<64;i++) printf("%02x",(unsigned char)digest[i]);
    printf("\n");

    SHA3_init(&SHA3,SHA3_HASH256);
    for (i=0;test512[i]!=0;i++) SHA3_process(&SHA3,test512[i]);
    SHA3_hash(&SHA3,digest);
    for (i=0;i<32;i++) printf("%02x",(unsigned char)digest[i]);
    printf("\n");

    SHA3_init(&SHA3,SHA3_HASH512);
    for (i=0;test512[i]!=0;i++) SHA3_process(&SHA3,test512[i]);
    SHA3_hash(&SHA3,digest);
    for (i=0;i<64;i++) printf("%02x",(unsigned char)digest[i]);
    printf("\n");

    SHA3_init(&SHA3,SHAKE256);
    for (i=0;test512[i]!=0;i++) SHA3_process(&SHA3,test512[i]);
    SHA3_shake(&SHA3,digest,72);
    for (i=0;i<72;i++) printf("%02x",(unsigned char)digest[i]);
    printf("\n");


    return 0;
}

*/
