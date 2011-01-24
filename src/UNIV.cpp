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

#include "UNIV.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <sstream>
#include <iosfwd>
#include <iomanip>

namespace extemp {

    uint32_t UNIV::FRAMES = 512;
    uint32_t UNIV::CHANNELS = 2;
    uint32_t UNIV::SAMPLERATE = 44100;
    uint32_t UNIV::SECOND = SAMPLERATE;
    uint32_t UNIV::MINUTE = SECOND * 60;
    uint32_t UNIV::HOUR = MINUTE * 60;
    uint64_t UNIV::TIME = 0l;
	const char* UNIV::PWD = "";

    void UNIV::initRand() {
#ifdef TARGET_OS_WIN
		srand(0);
#elif TARGET_OS_LINUX
        srand(0);
#else
        sranddev();
#endif
    }
    
    int UNIV::random(int range) {
        return (int)((double)rand() / (double)RAND_MAX * (double) range);
    }

    double UNIV::random() {
        return (double)rand() / (double)RAND_MAX;
    }

    double UNIV::midi2frq(double pitch)
    {
        return 220.0 * pow(2.0,(pitch - 57.0)/12);
    }
	
	double UNIV::frqRatio(double semitones)
	{
		return pow(2.0, (semitones/12.0));
	}
	
	bool UNIV::file_check(const std::string& filename)
	{
		FILE* fd = fopen(filename.c_str(),"r");
		if(fd == NULL){
			return false;
		}else{
			fclose(fd);
			return true;
		}
	}

	struct dump_stack_frame { 
		int op; 
		pointer args; 
		pointer envir; 
		pointer code; 
	}; 	
	
	void UNIV::printSchemeCell(scheme* _sc, std::stringstream& ss, pointer val, bool full, bool stringquotes)
    {
		if(val == 0) 
		{
			ss << "-ERROR BAD POINTER-";
			return;
		}
		if(pointer_type(val) > 16)
		{
			printf("Bad cell type - not printing\n");
			return;
		}
		
        if(is_string(val)) {
			if(stringquotes) {
				ss << "\"" << string_value(val) << "\"";
			}else{
				ss << string_value(val);			
			}
        }else if(is_symbol(val)){
            ss << symname(val);
        }else if(is_character(val)){
            ss << charvalue(val);
		}else if(is_environment(val)){
			ss << "#<ENVIRONMENT " << val << " ";
			if(full) {		
				if(is_vector(val->_object._cons._car)) {
					ss << "<VECTOR-FRAME>";
				}else{
					printSchemeCell(_sc, ss, val->_object._cons._car, full, stringquotes);
				}
				ss << " ";				
				printSchemeCell(_sc, ss, val->_object._cons._cdr, full, stringquotes);				
			}
			ss << ">";			
		}else if(is_proc(val)){
			ss << "#<PROC " << procname(val) << ">";
		}else if(is_foreign(val)){
			ss << "#<FOREIGN>";
		}else if(is_macro(val)){
			ss << "#<MACRO>";			
        }else if(is_closure(val)){
            ss << "#<<CLOSURE " << val << ">";
			if(full) {
				ss << "<CODE ";
				printSchemeCell(_sc, ss, val->_object._cons._car, full, stringquotes);
				ss << "> ";
				printSchemeCell(_sc, ss, val->_object._cons._cdr, full, stringquotes);				
				ss << ">>";
			}
		}else if(is_continuation(val)){
            ss << "#<<CONTINUATION " << val << ">";
			if(full) {
				unsigned int* stack = (unsigned int*) cptr_value(pair_cdr(val));
				int nframes = stack[0];
				dump_stack_frame* frames = (dump_stack_frame*)&stack[1];
				for(int j=0;j<nframes;j++)
				{
					ss << std::endl << std::endl << "FRAME(" << j << ")--------------------------";
					ss << std::endl << "OPCODE: " << frames[j].op; // << std::endl << "----------" << std::endl;
					
					// print args
					ss << std::endl << "ARGS: ";
					pointer args = frames[j].args;
				    extemp::UNIV::printSchemeCell(_sc, ss, args, true, stringquotes);
					
					// copy code
					ss << std::endl << "CODE: "; 
					pointer code = frames[j].code;
					//		ss.str("");
					extemp::UNIV::printSchemeCell(_sc, ss, code, true, stringquotes);
					//		std::cout << "CODE" << std::endl << ss.str() << std::endl << "-----------" << std::endl;
					
					ss << std::endl << "ENVIR: ";
					pointer envir = frames[j].envir;
					//		ss.str("");
					extemp::UNIV::printSchemeCell(_sc, ss, envir, true, stringquotes);
					//		std::cout << "ENVIR" << std::endl << ss.str() << std::endl << "-----------" << std::endl;					
				}
			}
			ss << std::endl << ">>";
        }else if(is_cptr(val)){
			void* p = cptr_value(val);
            ss << "#<CPTR: " << p << ">";
		}else if(is_vector(val)){
			//ss << "#<VECTOR>";
			if(true) {
				ss << "#(";
				int i;
				long long num=val->_size;//  /2+ivalue_unchecked(val)%2;
				if(num > 1000 && !full) { // exit if larger than 1000 elements
					ss << " -- " << num << " elements -- )";
					return;
				}
				//std::cout << "  NUM: " << num << std::endl;		
				for(i=0; i<num; i++) {
					/* Vector cells will be treated like ordinary cells */
					UNIV::printSchemeCell(_sc, ss, vector_elem(val,i), full, stringquotes);
					if(i+1 < num) ss << " ";
				}
				ss << ")";
			}
		}else if(is_port(val)){
			ss << "#<PORT" << val << ">";
        }else if(is_pair(val)){
			int lgth = list_length(_sc, val);	
			if(lgth<0) // is pair
			{
				ss << "(";
				printSchemeCell(_sc, ss, val->_object._cons._car, full, stringquotes);
				ss << " . ";
				printSchemeCell(_sc, ss, val->_object._cons._cdr, full, stringquotes);
				ss << ")";
			}else if(lgth>1000 && !full) {
				ss << "( -- " << lgth << " elements -- )";
				return;
			}else{ // is list
				ss << "(";				
				for(int i=0;i<lgth;i++)
				{
					printSchemeCell(_sc, ss, list_ref(_sc, i, val), full, stringquotes);
					if(i<(lgth-1)) ss << " ";
				}
				ss << ")";
			}
		}else if(is_foreign(val)){
			ss << "#<FOREIGN FUNC>";
		}else if(val == _sc->NIL){
			ss << "NIL";
		}else if(_sc->T == val){
			ss << "#t";
		}else if(_sc->F == val){
			ss << "#f";
		}else if(is_integer(val)){
			ss << ivalue(val);            			
		}else if(is_rational(val)){			
			ss << val->_object._number.value.ratvalue.n << "/" << val->_object._number.value.ratvalue.d;
		}else if(is_real(val)){
			if(full){
				ss << std::fixed << std::showpoint << std::setprecision(23) << rvalue(val);				
			}else{
				ss << std::fixed << std::showpoint << /* << std::setprecision(15) <<*/ rvalue(val);
			}
		}else{
			ss << "UNKOWN VALUE: " << val << " (GC'd?) ";
		}
		
		return;
    }
	
	
} //End Namespace
