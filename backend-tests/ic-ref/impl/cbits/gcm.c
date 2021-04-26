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
 * Implementation of the AES-GCM Encryption/Authentication
 *
 * Some restrictions..
 * 1. Only for use with AES
 * 2. Returned tag is always 128-bits. Truncate at your own risk.
 * 3. The order of function calls must follow some rules
 *
 * Typical sequence of calls..
 * 1. call GCM_init
 * 2. call GCM_add_header any number of times, as long as length of header is multiple of 16 bytes (block size)
 * 3. call GCM_add_header one last time with any length of header
 * 4. call GCM_add_cipher any number of times, as long as length of cipher/plaintext is multiple of 16 bytes
 * 5. call GCM_add_cipher one last time with any length of cipher/plaintext
 * 6. call GCM_finish to extract the tag.
 *
 * See http://www.mindspring.com/~dmcgrew/gcm-nist-6.pdf
 */
/* SU=m, m is Stack Usage */

#include <stdlib.h>
#include <string.h>
#include "arch.h"
#include "core.h"

#define NB 4
#define MR_TOBYTE(x) ((uchar)((x)))

static unsign32 pack(const uchar *b)
{
    /* pack bytes into a 32-bit Word */
    return ((unsign32)b[0] << 24) | ((unsign32)b[1] << 16) | ((unsign32)b[2] << 8) | (unsign32)b[3];
}

static void unpack(unsign32 a, uchar *b)
{
    /* unpack bytes from a word */
    b[3] = MR_TOBYTE(a);
    b[2] = MR_TOBYTE(a >> 8);
    b[1] = MR_TOBYTE(a >> 16);
    b[0] = MR_TOBYTE(a >> 24);
}

static void precompute(gcm *g, uchar *H)
{
    /* precompute small 2k bytes gf2m table of x^n.H */
    int i, j;
    unsign32 *last, *next, b;

    for (i = j = 0; i < NB; i++, j += 4) g->table[0][i] = pack((uchar *)&H[j]);

    for (i = 1; i < 128; i++)
    {
        next = g->table[i];
        last = g->table[i - 1];
        b = 0;
        for (j = 0; j < NB; j++)
        {
            next[j] = b | (last[j]) >> 1;
            b = last[j] << 31;
        }
        if (b) next[0] ^= 0xE1000000; /* irreducible polynomial */
    }
}

/* SU= 32 */
static void gf2mul(gcm *g)
{
    /* gf2m mul - Z=H*X mod 2^128 */
    int i, j, m, k;
    unsign32 P[4];
    unsign32 b;

    P[0] = P[1] = P[2] = P[3] = 0;
    j = 8;
    m = 0;
    for (i = 0; i < 128; i++)
    {
        b = (unsign32)(g->stateX[m] >> (--j)) & 1;
        b = ~b + 1;
        for (k = 0; k < NB; k++) P[k] ^= (g->table[i][k] & b);
        if (j == 0)
        {
            j = 8;
            m++;
            if (m == 16) break;
        }
    }
    for (i = j = 0; i < NB; i++, j += 4) unpack(P[i], (uchar *)&g->stateX[j]);
}

/* SU= 32 */
static void GCM_wrap(gcm *g)
{
    /* Finish off GHASH */
    int i, j;
    unsign32 F[4];
    uchar L[16];

    /* convert lengths from bytes to bits */
    F[0] = (g->lenA[0] << 3) | (g->lenA[1] & 0xE0000000) >> 29;
    F[1] = g->lenA[1] << 3;
    F[2] = (g->lenC[0] << 3) | (g->lenC[1] & 0xE0000000) >> 29;
    F[3] = g->lenC[1] << 3;
    for (i = j = 0; i < NB; i++, j += 4) unpack(F[i], (uchar *)&L[j]);

    for (i = 0; i < 16; i++) g->stateX[i] ^= L[i];
    gf2mul(g);
}

static int GCM_ghash(gcm *g, char *plain, int len)
{
    int i, j = 0;
    if (g->status == GCM_ACCEPTING_HEADER) g->status = GCM_ACCEPTING_CIPHER;
    if (g->status != GCM_ACCEPTING_CIPHER) return 0;

    while (j < len)
    {
        for (i = 0; i < 16 && j < len; i++)
        {
            g->stateX[i] ^= plain[j++];
            g->lenC[1]++;
            if (g->lenC[1] == 0) g->lenC[0]++;
        }
        gf2mul(g);
    }
    if (len % 16 != 0) g->status = GCM_NOT_ACCEPTING_MORE;
    return 1;
}

/* SU= 48 */
/* Initialize GCM mode */
void GCM_init(gcm* g, int nk, char *key, int niv, char *iv)
{
    /* iv size niv is usually 12 bytes (96 bits). AES key size nk can be 16,24 or 32 bytes */
    int i;
    uchar H[16];
    for (i = 0; i < 16; i++)
    {
        H[i] = 0;
        g->stateX[i] = 0;
    }

    AES_init(&(g->a), ECB, nk, key, iv);
    AES_ecb_encrypt(&(g->a), H);    /* E(K,0) */
    precompute(g, H);

    g->lenA[0] = g->lenC[0] = g->lenA[1] = g->lenC[1] = 0;
    if (niv == 12)
    {
        for (i = 0; i < 12; i++) g->a.f[i] = iv[i];
        unpack((unsign32)1, (uchar *) & (g->a.f[12])); /* initialise IV */
        for (i = 0; i < 16; i++) g->Y_0[i] = g->a.f[i];
    }
    else
    {
        g->status = GCM_ACCEPTING_CIPHER;
        GCM_ghash(g, iv, niv); /* GHASH(H,0,IV) */
        GCM_wrap(g);
        for (i = 0; i < 16; i++)
        {
            g->a.f[i] = g->stateX[i];
            g->Y_0[i] = g->a.f[i];
            g->stateX[i] = 0;
        }
        g->lenA[0] = g->lenC[0] = g->lenA[1] = g->lenC[1] = 0;
    }
    g->status = GCM_ACCEPTING_HEADER;
}

/* SU= 24 */
/* Add Header data - included but not encrypted */
int GCM_add_header(gcm* g, char *header, int len)
{
    /* Add some header. Won't be encrypted, but will be authenticated. len is length of header */
    int i, j = 0;
    if (g->status != GCM_ACCEPTING_HEADER) return 0;

    while (j < len)
    {
        for (i = 0; i < 16 && j < len; i++)
        {
            g->stateX[i] ^= header[j++];
            g->lenA[1]++;
            if (g->lenA[1] == 0) g->lenA[0]++;
        }
        gf2mul(g);
    }
    if (len % 16 != 0) g->status = GCM_ACCEPTING_CIPHER;
    return 1;
}

/* SU= 48 */
/* Add Plaintext - included and encrypted */
int GCM_add_plain(gcm *g, char *cipher, char *plain, int len)
{
    /* Add plaintext to extract ciphertext, len is length of plaintext.  */
    int i, j = 0;
    unsign32 counter;
    uchar B[16];
    if (g->status == GCM_ACCEPTING_HEADER) g->status = GCM_ACCEPTING_CIPHER;
    if (g->status != GCM_ACCEPTING_CIPHER) return 0;

    while (j < len)
    {
        counter = pack((uchar *) & (g->a.f[12]));
        counter++;
        unpack(counter, (uchar *) & (g->a.f[12])); /* increment counter */
        for (i = 0; i < 16; i++) B[i] = g->a.f[i];
        AES_ecb_encrypt(&(g->a), B);       /* encrypt it  */

        for (i = 0; i < 16 && j < len; i++)
        {
            cipher[j] = plain[j] ^ B[i];
            g->stateX[i] ^= cipher[j++];
            g->lenC[1]++;
            if (g->lenC[1] == 0) g->lenC[0]++;
        }
        gf2mul(g);
    }
    if (len % 16 != 0) g->status = GCM_NOT_ACCEPTING_MORE;
    return 1;
}

/* SU= 48 */
/* Add Ciphertext - decrypts to plaintext */
int GCM_add_cipher(gcm *g, char *plain, char *cipher, int len)
{
    /* Add ciphertext to extract plaintext, len is length of ciphertext. */
    int i, j = 0;
    unsign32 counter;
    char oc;
    uchar B[16];
    if (g->status == GCM_ACCEPTING_HEADER) g->status = GCM_ACCEPTING_CIPHER;
    if (g->status != GCM_ACCEPTING_CIPHER) return 0;

    while (j < len)
    {
        counter = pack((uchar *) & (g->a.f[12]));
        counter++;
        unpack(counter, (uchar *) & (g->a.f[12])); /* increment counter */
        for (i = 0; i < 16; i++) B[i] = g->a.f[i];
        AES_ecb_encrypt(&(g->a), B);       /* encrypt it  */
        for (i = 0; i < 16 && j < len; i++)
        {
            oc = cipher[j];
            plain[j] = cipher[j] ^ B[i];
            g->stateX[i] ^= oc;
            j++;
            g->lenC[1]++;
            if (g->lenC[1] == 0) g->lenC[0]++;
        }
        gf2mul(g);
    }
    if (len % 16 != 0) g->status = GCM_NOT_ACCEPTING_MORE;
    return 1;
}

/* SU= 16 */
/* Finish and extract Tag */
void GCM_finish(gcm *g, char *tag)
{
    /* Finish off GHASH and extract tag (MAC) */
    int i;

    GCM_wrap(g);

    /* extract tag */
    if (tag != NULL)
    {
        AES_ecb_encrypt(&(g->a), g->Y_0);       /* E(K,Y0) */
        for (i = 0; i < 16; i++) g->Y_0[i] ^= g->stateX[i];
        for (i = 0; i < 16; i++)
        {
            tag[i] = g->Y_0[i];
            g->Y_0[i] = g->stateX[i] = 0;
        }
    }
    g->status = GCM_FINISHED;
    AES_end(&(g->a));
}

/* AES-GCM Encryption of octets, K is key, H is header,
   P is plaintext, C is ciphertext, T is authentication tag */
void AES_GCM_ENCRYPT(octet *K, octet *IV, octet *H, octet *P, octet *C, octet *T)
{
    gcm g;
    GCM_init(&g, K->len, K->val, IV->len, IV->val);
    GCM_add_header(&g, H->val, H->len);
    GCM_add_plain(&g, C->val, P->val, P->len);
    C->len = P->len;
    GCM_finish(&g, T->val);
    T->len = 16;
}

/* AES-GCM Decryption of octets, K is key, H is header,
   P is plaintext, C is ciphertext, T is authentication tag */
void AES_GCM_DECRYPT(octet *K, octet *IV, octet *H, octet *C, octet *P, octet *T)
{
    gcm g;
    GCM_init(&g, K->len, K->val, IV->len, IV->val);
    GCM_add_header(&g, H->val, H->len);
    GCM_add_cipher(&g, P->val, C->val, C->len);
    P->len = C->len;
    GCM_finish(&g, T->val);
    T->len = 16;
}

// Compile with
// gcc -O2 gcm.c aes.c -o gcm.exe
/* SU= 16
*/

/* static void hex2bytes(char *hex,char *bin) */
/* { */
/*  int i; */
/*  char v; */
/*  int len=strlen(hex); */
/*  for (i = 0; i < len/2; i++) { */
/*         char c = hex[2*i]; */
/*         if (c >= '0' && c <= '9') { */
/*             v = c - '0'; */
/*         } else if (c >= 'A' && c <= 'F') { */
/*             v = c - 'A' + 10; */
/*         } else if (c >= 'a' && c <= 'f') { */
/*             v = c - 'a' + 10; */
/*         } else { */
/*             v = 0; */
/*         } */
/*         v <<= 4; */
/*         c = hex[2*i + 1]; */
/*         if (c >= '0' && c <= '9') { */
/*             v += c - '0'; */
/*         } else if (c >= 'A' && c <= 'F') { */
/*             v += c - 'A' + 10; */
/*         } else if (c >= 'a' && c <= 'f') { */
/*             v += c - 'a' + 10; */
/*         } else { */
/*             v = 0; */
/*         } */
/*         bin[i] = v; */
/*     } */
/* } */

/*
int main()
{
    int i;

//  char* KT="feffe9928665731c6d6a8f9467308308";
//  char* MT="d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39";
//  char* HT="feedfacedeadbeeffeedfacedeadbeefabaddad2";
//  char* NT="cafebabefacedbaddecaf888";
// Tag should be 5bc94fbc3221a5db94fae95ae7121a47
//  char* NT="9313225df88406e555909c5aff5269aa6a7a9538534f7da1e4c303d2a318a728c3c0c95156809539fcf0e2429a6b525416aedbf5a0de6a57a637b39b";
// Tag should be 619cc5aefffe0bfa462af43c1699d050

  char* KT="6dfb5dc68af6ae2f3242e9184f100918";
  char* MT="47809d16c2c6ec685962c90e53fe1bba";
  char* HT="dd0fa6e494031139d71ee45f00d56fa4";
  char* NT="37d36f5c54d53479d4745dd1";


    int len=strlen(MT)/2;
    int lenH=strlen(HT)/2;
    int lenK=strlen(KT)/2;
    int lenIV=strlen(NT)/2;

    char T[16];   // Tag
    char K[16];   // AES Key
    char H[64];   // Header - to be included in Authentication, but not encrypted
    char N[100];   // IV - Initialisation vector
    char M[100];  // Plaintext to be encrypted/authenticated
    char C[100];  // Ciphertext
    char P[100];  // Recovered Plaintext

    gcm g;

    hex2bytes(MT, M);
    hex2bytes(HT, H);
    hex2bytes(NT, N);
    hex2bytes(KT, K);

    printf("lenK= %d\n",lenK);

    printf("Plaintext=\n");
    for (i=0;i<len;i++) printf("%02x",(unsigned char)M[i]);
    printf("\n");

    GCM_init(&g,16,K,lenIV,N);
    GCM_add_header(&g,H,lenH);
    GCM_add_plain(&g,C,M,len);
    GCM_finish(&g,T);

    printf("Ciphertext=\n");
    for (i=0;i<len;i++) printf("%02x",(unsigned char)C[i]);
    printf("\n");

    printf("Tag=\n");
    for (i=0;i<16;i++) printf("%02x",(unsigned char)T[i]);
    printf("\n");

    GCM_init(&g,16,K,lenIV,N);
    GCM_add_header(&g,H,lenH);
    GCM_add_cipher(&g,P,C,len);
    GCM_finish(&g,T);

    printf("Plaintext=\n");
    for (i=0;i<len;i++) printf("%02x",(unsigned char)P[i]);
    printf("\n");

    printf("Tag=\n");
    for (i=0;i<16;i++) printf("%02x",(unsigned char)T[i]);
    printf("\n");
}

*/
