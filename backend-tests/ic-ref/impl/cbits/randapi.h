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
 * @file randapi.h
 * @author Mike Scott
 * @brief PRNG API File
 *
 */

#ifndef RANDOM_H
#define RANDOM_H

#include "core.h"

/** @brief Initialise a random number generator
 *
    @param R is a pointer to a cryptographically secure random number generator
    @param S is an input truly random seed value
 */
extern void CREATE_CSPRNG(csprng *R, octet *S);
/** @brief Kill a random number generator
 *
    Deletes all internal state
    @param R is a pointer to a cryptographically secure random number generator
 */
extern void KILL_CSPRNG(csprng *R);

#endif

