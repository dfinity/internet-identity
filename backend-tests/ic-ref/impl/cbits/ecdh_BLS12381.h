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
 * @file ecdh.h
 * @author Mike Scott
 * @brief ECDH Header file for implementation of standard EC protocols
 *
 *
 */

#ifndef ECDH_BLS12381_H
#define ECDH_BLS12381_H

#include "ecp_BLS12381.h"
//#include "ecdh_support.h"


/*** START OF USER CONFIGURABLE SECTION -  ***/

//#define EAS_BLS12381 16 /**< Symmetric Key size - 128 bits */
//#define HASH_TYPE_ECC_BLS12381 SHA512  /**< Hash type */

/*** END OF USER CONFIGURABLE SECTION ***/

#define EGS_BLS12381 MODBYTES_384_58  /**< ECC Group Size in bytes */
#define EFS_BLS12381 MODBYTES_384_58  /**< ECC Field Size in bytes */

#define ECDH_OK                     0     /**< Function completed without error */
/*#define ECDH_DOMAIN_ERROR          -1*/
#define ECDH_INVALID_PUBLIC_KEY    -2	/**< Public Key is Invalid */
#define ECDH_ERROR                 -3	/**< ECDH Internal Error */
//#define ECDH_INVALID               -4	/**< ECDH Internal Error */
/*#define ECDH_DOMAIN_NOT_FOUND      -5
#define ECDH_OUT_OF_MEMORY         -6
#define ECDH_DIV_BY_ZERO           -7
#define ECDH_BAD_ASSUMPTION        -8*/

/* ECDH primitives */

/**	@brief Test if group element in correct range
 *
	@param s is a random number
    @return 1 if 0<s<r where r is group order, else 0
*/
extern int ECP_BLS12381_IN_RANGE(octet *s);
/**	@brief Generate an ECC public/private key pair
 *
	@param R is a pointer to a cryptographically secure random number generator
	@param s the private key, an output internally randomly generated if R!=NULL, otherwise must be provided as an input
	@param W the output public key, which is s.G, where G is a fixed generator
	@return 0 or an error code
 */
extern int  ECP_BLS12381_KEY_PAIR_GENERATE(csprng *R, octet *s, octet *W);
/**	@brief Validate an ECC public key
 *
	@param W the input public key to be validated
	@return 0 if public key is OK, or an error code
 */
extern int  ECP_BLS12381_PUBLIC_KEY_VALIDATE(octet *W);

/* ECDH primitives */

/**	@brief Generate Diffie-Hellman shared key
 *
	IEEE-1363 Diffie-Hellman shared secret calculation
	@param s is the input private key,
	@param W the input public key of the other party
	@param K the output shared key, in fact the x-coordinate of s.W
    @param type the output form = 0 for just x, 1 for compressed, 2 for uncompressed
	@return 0 or an error code
 */
extern int ECP_BLS12381_SVDP_DH(octet *s, octet *W, octet *K, int type);
/*extern int ECPSVDP_DHC(octet *,octet *,int,octet *);*/

/*#if CURVETYPE!=MONTGOMERY */
/* ECIES functions */
/*#if CURVETYPE!=MONTGOMERY */
/* ECIES functions */
/**	@brief ECIES Encryption
 *
	IEEE-1363 ECIES Encryption
	@param h is the hash type
	@param P1 input Key Derivation parameters
	@param P2 input Encoding parameters
	@param R is a pointer to a cryptographically secure random number generator
	@param W the input public key of the recieving party
	@param M is the plaintext message to be encrypted
	@param len the length of the HMAC tag
	@param V component of the output ciphertext
	@param C the output ciphertext
	@param T the output HMAC tag, part of the ciphertext
 */
extern void ECP_BLS12381_ECIES_ENCRYPT(int h, octet *P1, octet *P2, csprng *R, octet *W, octet *M, int len, octet *V, octet *C, octet *T);
/**	@brief ECIES Decryption
 *
	IEEE-1363 ECIES Decryption
	@param h is the hash type
	@param P1 input Key Derivation parameters
	@param P2 input Encoding parameters
	@param V component of the input ciphertext
	@param C the input ciphertext
	@param T the input HMAC tag, part of the ciphertext
	@param U the input private key for decryption
	@param M the output plaintext message
	@return 1 if successful, else 0
 */
extern int ECP_BLS12381_ECIES_DECRYPT(int h, octet *P1, octet *P2, octet *V, octet *C, octet *T, octet *U, octet *M);

/* ECDSA functions */
/**	@brief ECDSA Signature
 *
	IEEE-1363 ECDSA Signature
	@param h is the hash type
	@param R is a pointer to a cryptographically secure random number generator
        @param k Ephemeral key. This value is used when R=NULL
	@param s the input private signing key
	@param M the input message to be signed
	@param c component of the output signature
	@param d component of the output signature

 */
extern int ECP_BLS12381_SP_DSA(int h, csprng *R, octet *k, octet *s, octet *M, octet *c, octet *d);
/**	@brief ECDSA Signature Verification
 *
	IEEE-1363 ECDSA Signature Verification
	@param h is the hash type
	@param W the input public key
	@param M the input message
	@param c component of the input signature
	@param d component of the input signature
	@return 0 or an error code
 */
extern int ECP_BLS12381_VP_DSA(int h, octet *W, octet *M, octet *c, octet *d);
/*#endif*/

#endif

