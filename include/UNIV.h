/*
 * Copyright (c) 2011, Andrew Sorensen
 *
 * All rights reserved.
 *
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, 
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation 
 *    and/or other materials provided with the distribution.
 *
 * Neither the name of the authors nor other contributors may be used to endorse
 * or promote products derived from this software without specific prior written 
 * permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLEXTD. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef UNIV_H
#define UNIV_H

#include <stdint.h>
#include "SchemePrivate.h"

#include <string>

namespace extemp {

//#define mk_cb(instance,class,func) (dynamic_cast<CM*>(new CMI<class>(instance,&class::func)))
    
typedef float AUDIOSAMPLE;
    
class UNIV {

#define EIGHT_BIT 127
#define SIXTEEN_BIT 32767
#define TWENTY_FOUR_BIT 8388608
#define THIRTY_TWO_BIT 214748647
    
public:
    static uint32_t CHANNELS;
    static uint32_t SAMPLERATE;
    static uint64_t TIME;
    static uint32_t HOUR;
    static uint32_t MINUTE;
    static uint32_t SECOND;
    static uint32_t FRAMES;
	static const char* PWD;

    static double midi2frq(double pitch);
	static double frqRatio(double semitones);
    static void initRand();
    static int random(int range);
    static double random();
	static bool file_check(const std::string& filename);	
    static void printSchemeCell(scheme* sc, std::stringstream& ss, pointer cell, bool = false, bool = true);
	
};

} //End Namespace
#endif
