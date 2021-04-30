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
 * @file newhope.h
 * @author Mike Scott
 * @brief Newhope Header File
 *
 */

/* NewHope Simple API */

#ifndef NHS_H
#define NHS_H

#include "core.h"

/** @brief NHS server first pass
 *
    @param RNG Random Number Generator handle
    @param SB seed and polynomial B concatenated - output
    @param S server secret - output

 */
extern void NHS_SERVER_1(csprng *RNG, octet *SB, octet *S);
/** @brief NHS client pass
 *
    @param RNG Random Number Generator handle
    @param SB seed and polynomial B concatenated - input
    @param UC polynomial U and compressed polynomial c - output
    @param KEY client key
 */
extern void NHS_CLIENT(csprng *RNG, octet *SB, octet *UC, octet *KEY);
/** @brief NHS server second pass
 *
    @param S server secret - input
    @param UC polynomial U and compressed polynomial c - input
    @param KEY server key
 */
extern void NHS_SERVER_2(octet *S, octet *UC, octet *KEY);

#endif
