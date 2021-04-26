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
 * @file hpke.h
 * @author Mike Scott 
 * @date 2nd December 2019
 * @brief HPKE Header file
 *
 * declares functions
 *
 */

#ifndef HPKE_BLS12381_H
#define HPKE_BLS12381_H

#include "ecdh_BLS12381.h"

//#define CONFIG_ID 0x2A // 01|01|010 = 1, 1, 2
//#define KEM_ID 2  // Curve X25519
//#define KEM_ID 3  // Curve X448
//#define KDF_ID 1  // HKDF-SHA256
//#define AEAD_ID 1 // AES-GCM-128

#define HPKE_OK                     0     /**< Function completed without error */
#define HPKE_INVALID_PUBLIC_KEY    -2	/**< Public Key is Invalid */
#define HPKE_ERROR                 -3	/**< HPKE Internal Error */

/* HPKE DHKEM primitives */

/**	@brief Derive a Key Pair from a seed
 *
    @param config_id is the configuration KEM/KDF/AEAD
    @param SK is the output secret key
    @param PK is the output public key
    @param SEED is the input random seed
    @return 1 if OK, 0 if failed
 */
extern int DeriveKeyPair_BLS12381(int config_id,octet *SK,octet *PK,octet *SEED);

/**	@brief Encapsulate function
 *
    @param config_id is the configuration KEM/KDF/AEAD
    @param SK is the input ephemeral secret 
    @param Z is a pointer to a shared secret DH(skE,pkR)
	@param pkE the ephemeral public key, which is skE.G, where G is a fixed generator
	@param pkR the respondents public key
 */
extern void HPKE_BLS12381_Encap(int config_id,octet *SK,octet *Z,octet *pkE,octet *pkR);

/**	@brief Decapsulate function
 *
    @param config_id is the configuration KEM/KDF/AEAD
    @param skR the respondents private key
    @param Z is a pointer to a shared secret DH(skR,pkE)
	@param pkE the ephemeral public key
	@param pkR the respondents private key
 */
extern void HPKE_BLS12381_Decap(int config_id,octet *skR,octet *Z,octet *pkE,octet *pkR);

/**	@brief Encapsulate/Authenticate function
 *
    @param config_id is the configuration KEM/KDF/AEAD
    @param skE is the input ephemeral secret 
    @param skS is the Initiators private key 
    @param Z is a pointer to a shared secret DH(skE,pkR)
	@param pkE the ephemeral public key, which is skE.G, where G is a fixed generator
	@param pkR the Respondents public key
    @param pkS the Initiators public key
 */
extern void HPKE_BLS12381_AuthEncap(int config_id,octet *skE,octet *skS,octet *Z,octet *pkE,octet *pkR,octet *pkS);

/**	@brief Decapsulate function
 *
    @param config_id is the configuration KEM/KDF/AEAD
    @param skR is the Respondents private key 
    @param Z is a pointer to a shared secret DH(skR,pkE)
	@param pkE the ephemeral public key
	@param pkR the Respondents public key
    @param pkS the Initiators public key
 */
extern void HPKE_BLS12381_AuthDecap(int config_id,octet *skR,octet *Z,octet *pkE,octet *pkR,octet *pkS);

/**	@brief KeyScheduler function
 *
    @param config_id is the configuration KEM/KDF/AEAD
    @param key  the output key for aead encryption
    @param nonce  the output nonce for aead encryption
    @param exp_secret the exporter secret
	@param mode  the mode of operation
    @param Z the shared key
    @param info application dependent info
    @param psk pre-shared key
    @param pskID identifier for the psk
 */
extern void HPKE_BLS12381_KeySchedule(int config_id,octet *key,octet *nonce,octet *exp_secret,int mode,octet *Z,octet *info,octet *psk,octet *pskID);

#endif
