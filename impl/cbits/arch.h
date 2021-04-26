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

/* Architecture definition header file */

/**
 * @file arch.h
 * @author Mike Scott
 * @date 23rd February 2016
 * @brief Architecture Header File
 *
 * Specify Processor Architecture
 *
 */

/* NOTE: There is only one user configurable section in this header - see below */

#ifndef ARCH_H
#define ARCH_H




/*** START OF USER CONFIGURABLE SECTION - set architecture ***/

#ifdef CMAKE
#define CHUNK @CORE_CHUNK@  /**< size of chunk in bits = wordlength of computer = 16, 32 or 64. Note not all curve options are supported on 16-bit processors - see rom.c */
#else
#define CHUNK 64      /**< size of chunk in bits = wordlength of computer = 16, 32 or 64. Note not all curve options are supported on 16-bit processors - see rom.c */
#endif

/*** END OF USER CONFIGURABLE SECTION ***/

/* Create Integer types */
/* Support for C99?  Note for GCC need to explicitly include -std=c99 in command line */

#if __STDC_VERSION__ >= 199901L
/* C99 code */
#define C99
#else
/* Not C99 code */
#endif

#ifndef C99  /* You are on your own! These are for Microsoft C */
#define byte unsigned char      /**< 8-bit unsigned integer */
#define sign32 __int32          /**< 32-bit signed integer */
#define sign8 signed char       /**< 8-bit signed integer */
#define sign16 short int        /**< 16-bit signed integer */
#define sign64 long long        /**< 64-bit signed integer */
#define unsign32 unsigned __int32 /**< 32-bit unsigned integer */
#define unsign64 unsigned long long  /**< 64-bit unsigned integer */
#else
#include <stdint.h>
#define byte uint8_t            /**< 8-bit unsigned integer */
#define sign8 int8_t            /**< 8-bit signed integer */
#define sign16 int16_t          /**< 16-bit signed integer */
#define sign32 int32_t          /**< 32-bit signed integer */
#define sign64 int64_t          /**< 64-bit signed integer */
#define unsign32 uint32_t       /**< 32-bit unsigned integer */
#define unsign64 uint64_t       /**< 64-bit unsigned integer */
#endif

#define uchar unsigned char  /**<  Unsigned char */

/* Don't mess with anything below this line unless you know what you are doing */
/* This next is probably OK, but may need changing for non-C99-standard environments */

/* This next is probably OK, but may need changing for non-C99-standard environments */

#if CHUNK==16
#ifndef C99
#define chunk __int16       /**< C type corresponding to word length */
#define dchunk __int32      /**< Always define double length chunk type if available */
#else
#define chunk int16_t       /**< C type corresponding to word length */
#define dchunk int32_t      /**< Always define double length chunk type if available */
#endif
#endif

#if CHUNK == 32
#ifndef C99
#define chunk __int32       /**< C type corresponding to word length */
#define dchunk __int64      /**< Always define double length chunk type if available */
#else
#define chunk int32_t       /**< C type corresponding to word length */
#define dchunk int64_t      /**< Always define double length chunk type if available */
#endif
#endif

#if CHUNK == 64

#ifndef C99
#define chunk __int64     /**< C type corresponding to word length */
/**< Note - no 128-bit type available    */
#else
#define chunk int64_t     /**< C type corresponding to word length */
//#ifdef __GNUC__
//#define dchunk __int128       /**< Always define double length chunk type if available - GCC supports 128 bit type  ??? */
//#endif

//#ifdef __clang__
//#define dchunk __int128
#if defined(__SIZEOF_INT128__) && __SIZEOF_INT128__ == 16
#define dchunk __int128
#endif

#endif
#endif

#ifdef dchunk
#define COMBA      /**< Use COMBA method for faster muls, sqrs and reductions */
#endif


#endif
