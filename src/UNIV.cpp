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
#include "pcre.h"
#include "SchemeFFI.h"

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#else
#include <time.h>
#endif

#ifdef _MSC_VER
#include <malloc.h>
//#include <unistd.h>
#include <Windows.h>
/*
void uSleep(int waitTime){
 __int64 time1 = 0, time2 = 0, sysFreq = 0;

 QueryPerformanceCounter((LARGE_INTEGER *)&time1);
 QueryPerformanceFrequency((LARGE_INTEGER *)&sysFreq);
 do{
 QueryPerformanceCounter((LARGE_INTEGER *)&time2);

 //  }while((((time2-time1)*1.0)/sysFreq)<waitTime);
   }while( (time2-time1) <waitTime);
}
*/


enum Windows_Color_Convert
{
        Black       = 0,
        Red         = FOREGROUND_RED,
        Green       = FOREGROUND_GREEN,
        Yellow      = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_INTENSITY,
        Blue        = FOREGROUND_BLUE,
        Purple      = FOREGROUND_RED   | FOREGROUND_BLUE,
        Cyan        = FOREGROUND_GREEN | FOREGROUND_BLUE,
        White       = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY,
        LightGrey   = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_BLUE,
        Grey        = FOREGROUND_INTENSITY,
        Orange      = FOREGROUND_RED   | FOREGROUND_GREEN,
        LightRed    = FOREGROUND_RED   | FOREGROUND_INTENSITY,
        LightGreen  = FOREGROUND_GREEN | FOREGROUND_INTENSITY,
        LightBlue   = FOREGROUND_BLUE  | FOREGROUND_INTENSITY,
        LightPurple = FOREGROUND_RED   | FOREGROUND_BLUE  | FOREGROUND_INTENSITY,
        LightCyan   = FOREGROUND_GREEN | FOREGROUND_BLUE  | FOREGROUND_INTENSITY,
        //LightGrey   = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_BLUE,
};

int WINDOWS_COLORS[16] = {Black,LightRed,LightGreen,Yellow,Blue,Purple,LightCyan,White,LightGrey,Orange,Grey,LightRed,LightGreen,LightBlue,Cyan};

#endif

void ascii_text_color(int attr, int fg, int bg)
{
  // for --term=nocolor, this function should do nothing
  if (extemp::UNIV::EXT_TERM == 3) {
    return;
  }
#ifdef _MSC_VER
  if (extemp::UNIV::EXT_TERM == 1) {
    char command[13];
    if(fg>8) fg = 8;
    if(bg>9) bg = 0;
    sprintf(command, "COLOR %d%d", bg, fg);
    HANDLE console=GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleTextAttribute(console, WINDOWS_COLORS[fg]);
  }else{
    char command[13];
    /* Command is the control command to the terminal */
    sprintf(command, "%c[%d;%d;%dm", 0x1B, attr, fg + 30, bg + 40);
    printf("%s", command);
  }
#else
  char command[20];  
  if(attr>1) attr=0;
  if(bg==10) bg=9; // background default is 9 NOT 10
  // if simple term (that doesn't support defaults)
  // then default to black background and white text
  if (extemp::UNIV::EXT_TERM == 2) {
    attr=0;
    if(bg==9) bg=0;
    if(fg==9) fg=7;
  }
  sprintf(command, "%c[%d;%d;%dm", 0x1B, attr, fg + 30, bg + 40);
  command[strlen(command)]=0;
  printf("%s", command);
#endif
}

static char base64_codesafe_encoding_table[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                                                'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                                                'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                                                'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
                                                'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
                                                'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                                                'w', 'x', 'y', 'z', '0', '1', '2', '3',
                                                '4', '5', '6', '7', '8', '9', '_', '-'};

static char base64_std_encoding_table[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                                           'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                                           'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                                           'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
                                           'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
                                           'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                                           'w', 'x', 'y', 'z', '0', '1', '2', '3',
                                           '4', '5', '6', '7', '8', '9', '+', '/'};

static char *base64_std_decoding_table = NULL;
static char *base64_codesafe_decoding_table = NULL;
static int _base64_mod_table[] = {0, 2, 1};

void base64_std_build_decoding_table() {

  base64_std_decoding_table = (char*) malloc(256);

    for (int i = 0; i < 64; i++)
        base64_std_decoding_table[(unsigned char) base64_std_encoding_table[i]] = i;
}

void base64_codesafe_build_decoding_table() {

  base64_codesafe_decoding_table = (char*) malloc(256);

    for (int i = 0; i < 64; i++)
        base64_codesafe_decoding_table[(unsigned char) base64_codesafe_encoding_table[i]] = i;
}


void base64_std_cleanup() {
    free(base64_std_decoding_table);
}

void base64_codesafe_cleanup() {
    free(base64_codesafe_decoding_table);
}


char* cname_encode(char *data,
                    size_t input_length,
                    size_t *output_length) {

    *output_length = 4 * ((input_length + 2) / 3);

    //char *encoded_data = (char*) malloc(*output_length);
    char *encoded_data = (char*) malloc(*output_length+1);
    encoded_data[*output_length] = 0;
    if (encoded_data == NULL) return NULL;

    for (int i = 0, j = 0; i < input_length;) {

        uint32_t octet_a = i < input_length ? data[i++] : 0;
        uint32_t octet_b = i < input_length ? data[i++] : 0;
        uint32_t octet_c = i < input_length ? data[i++] : 0;

        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 3 * 6) & 0x3F];
        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 2 * 6) & 0x3F];
        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 1 * 6) & 0x3F];
        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 0 * 6) & 0x3F];
    }

    for (int i = 0; i < _base64_mod_table[input_length % 3]; i++) {
      encoded_data[*output_length - 1 - i] = 0; //'$';
    }
    
    //printf("ENCODE: %d:%s\n",*output_length,encoded_data);
    return encoded_data;
}


char* cname_decode(char *data,
                   size_t input_length,
                   size_t *output_length) {

    if (base64_codesafe_decoding_table == NULL) base64_codesafe_build_decoding_table();
    
    char* d2 = NULL;
    // pad with $'s
    if (input_length % 4 != 0) {
      int lgthdiff = (4-(input_length % 4));
      char* d2 = (char*) malloc(input_length+lgthdiff);
      memcpy(d2,data,input_length);
      input_length = input_length+lgthdiff;
      for(int i=0;i<lgthdiff;i++) {
        d2[input_length-1-i] = '$';
      }
      data = d2;
    }

    if (input_length % 4 != 0) return NULL;
    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '$') (*output_length)--;
    if (data[input_length - 2] == '$') (*output_length)--;

    char *decoded_data = (char*) malloc(*output_length+1);
    decoded_data[*output_length] = 0;
    if (decoded_data == NULL) return NULL;

    for (int i = 0, j = 0; i < input_length;) {

        uint32_t sextet_a = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[data[i++]];
        uint32_t sextet_b = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[data[i++]];
        uint32_t sextet_c = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[data[i++]];
        uint32_t sextet_d = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[data[i++]];

        uint32_t triple = (sextet_a << 3 * 6)
        + (sextet_b << 2 * 6)
        + (sextet_c << 1 * 6)
        + (sextet_d << 0 * 6);

        if (j < *output_length) decoded_data[j++] = (triple >> 2 * 8) & 0xFF;
        if (j < *output_length) decoded_data[j++] = (triple >> 1 * 8) & 0xFF;
        if (j < *output_length) decoded_data[j++] = (triple >> 0 * 8) & 0xFF;
    }
    if (d2) free(d2);
    //printf("DECODE: %d:%s\n",*output_length,decoded_data);
    return decoded_data;
}

char* base64_encode(const unsigned char *data,
                    size_t input_length,
                    size_t *output_length) {

    *output_length = 4 * ((input_length + 2) / 3);

    char *encoded_data = (char*) malloc(*output_length+1);
    encoded_data[*output_length]=0;

    if (encoded_data == NULL) return NULL;

    for (int i = 0, j = 0; i < input_length;) {

        uint32_t octet_a = i < input_length ? data[i++] : 0;
        uint32_t octet_b = i < input_length ? data[i++] : 0;
        uint32_t octet_c = i < input_length ? data[i++] : 0;

        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

        encoded_data[j++] = base64_std_encoding_table[(triple >> 3 * 6) & 0x3F];
        encoded_data[j++] = base64_std_encoding_table[(triple >> 2 * 6) & 0x3F];
        encoded_data[j++] = base64_std_encoding_table[(triple >> 1 * 6) & 0x3F];
        encoded_data[j++] = base64_std_encoding_table[(triple >> 0 * 6) & 0x3F];
    }

    for (int i = 0; i < _base64_mod_table[input_length % 3]; i++)
        encoded_data[*output_length - 1 - i] = '=';

    return encoded_data;
}


unsigned char* base64_decode(const char *data,
                             size_t input_length,
                             size_t *output_length) {

    if (base64_std_decoding_table == NULL) base64_std_build_decoding_table();

    if (input_length % 4 != 0) return NULL;

    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '=') (*output_length)--;
    if (data[input_length - 2] == '=') (*output_length)--;

    unsigned char *decoded_data = (unsigned char*) malloc(*output_length);
    if (decoded_data == NULL) return NULL;

    for (int i = 0, j = 0; i < input_length;) {

        uint32_t sextet_a = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[data[i++]];
        uint32_t sextet_b = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[data[i++]];
        uint32_t sextet_c = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[data[i++]];
        uint32_t sextet_d = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[data[i++]];

        uint32_t triple = (sextet_a << 3 * 6)
        + (sextet_b << 2 * 6)
        + (sextet_c << 1 * 6)
        + (sextet_d << 0 * 6);

        if (j < *output_length) decoded_data[j++] = (triple >> 2 * 8) & 0xFF;
        if (j < *output_length) decoded_data[j++] = (triple >> 1 * 8) & 0xFF;
        if (j < *output_length) decoded_data[j++] = (triple >> 0 * 8) & 0xFF;
    }

    return decoded_data;
}


bool rmatch(char* regex, char* str)
{
  //  char* data = char* strstring_value(pair_car(args));
  // char* pattern = string_value(pair_cadr(args));
  char* data = str;
  char* pattern = regex;		
  
  pcre *re; 
  const char *error; 
  int erroffset;
  
  re = pcre_compile(	pattern, /* the pattern */ 
			0, /* default options */ 
			&error, /* for error message */ 
			&erroffset, /* for error offset */ 
			NULL); /* use default character tables */
  
  int rc;
  int ovector[30]; 
  rc = pcre_exec(	re, /* result of pcre_compile() */ 
			NULL, /* we didn’t study the pattern */ 
			data, /* the subject string */ 
			strlen(data), /* the length of the subject string */ 
			0, /* start at offset 0 in the subject */ 
			0, /* default options */ 
			ovector, /* vector of integers for substring information */ 
			30); /* number of elements (NOT size in bytes) */
  
  return (rc>=0) ? true : false;
}

bool rsplit(char* regex, char* str, char* a, char* b)
{
  char* data = str;
  int length = strlen(data);
  char* pattern = regex;		
  pcre *re; 
  const char *error; 
  int erroffset; 
  //printf("dat: data\n");
  // should probably move this regex compile to global
  re = pcre_compile(	pattern, /* the pattern */ 
			0, /* default options */ 
			&error, /* for error message */ 
			&erroffset, /* for error offset */ 
			NULL); /* use default character tables */		
  int rc; 
  int ovector[60];					
  rc = pcre_exec(	re, /* result of pcre_compile() */ 
			NULL, /* we didn’t study the pattern */ 
			data, /* the subject string */ 
			strlen(data), /* the length of the subject string */ 
			0, /* start at offset 0 in the subject */ 
			0, /* default options */ 
			ovector, /* vector of integers for substring information */ 
			60); /* number of elements (NOT size in bytes) */
  
  if(rc<1 || rc>1) return false; // then we failed
  int range = ovector[0];
  int range2 = ovector[1];
  //printf("reg ranges %d:%d\n",range,range2);
  memset(a,0,range+1);
  memcpy(a,data,range);
  memset(b,0,(length-(range2+0))+1);
  memcpy(b,data+range2+0,(length-(range2+0)));
  return true;
}

// returns char* result
char* rreplace(char* regex, char* str, char* replacement, char* result) {

        char* data = str; //string_value(pair_car(args));
	char* pattern = regex; //string_value(pair_cadr(args));
	strcpy(result,replacement);
		
	pcre *re;
	const char *error; 
	int erroffset; 
	re = pcre_compile(	pattern, /* the pattern */ 
				0, /* default options */ 
				&error, /* for error message */ 
				&erroffset, /* for error offset */ 
				NULL); /* use default character tables */
		
	int rc; 
	int ovector[60];

	rc = pcre_exec(	re, /* result of pcre_compile() */ 
			NULL, /* we didn’t study the pattern */ 
			data, /* the subject string */ 
			strlen(data), /* the length of the subject string */ 
			0, /* start at offset 0 in the subject */ 
			0, /* default options */ 
			ovector, /* vector of integers for substring information */ 
			60); /* number of elements (NOT size in bytes) */

	// no match found return original string
	if(rc<1) {strcpy(result,str); return result;} // Return mk_string(_sc,data);

	// ok we have a match
	// first replace any groups in replace string (i.e. $1 $2 ...)
	char* res = (char*) "";
	char* sep = (char*) "$";
	char* tmp = 0;
	int pos,range,size = 0;
	char* p = strtok(result,sep);
	do{
	    char* cc;
	    pos = strtol(p,&cc,10);
	    range = (pos>0) ? ovector[(pos*2)+1] - ovector[pos*2] : 0;
	    size = strlen(res);
	    tmp = (char*) alloca(size+range+strlen(cc)+1);
	    memset(tmp,0,size+range+strlen(cc)+1);
	    memcpy(tmp,res,size);
	    memcpy(tmp+size,data+ovector[pos*2],range);
	    memcpy(tmp+size+range,cc,strlen(cc));
	    res = tmp;
	    p = strtok(NULL, sep);
	}while(p);
	// now we can use "rep" to replace the original regex match (i.e. ovector[0]-ovector[1])
	int lgth = (strlen(data)-range)+strlen(res)+1;
	range = ovector[1] - ovector[0];
	//char* result = (char*) alloca(lgth);
	memset(result,0,lgth);
	memcpy(result,data,ovector[0]);
	memcpy(result+ovector[0],res,strlen(res));
	memcpy(result+ovector[0]+strlen(res),data+ovector[1],strlen(data)-ovector[1]);		
	return result;
}


#ifdef __APPLE__
  double clock_clock()
  {
    return CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970 + extemp::SchemeFFI::CLOCK_OFFSET;
  } 
#elif defined(__linux__)
  double clock_clock()
  {
    struct timespec t;
    clock_gettime(CLOCK_REALTIME, &t);
    double t2 = (double)t.tv_sec + ((double)t.tv_nsec)/D_BILLION;    
    return t2+extemp::SchemeFFI::CLOCK_OFFSET;
  }
#else
   double clock_clock() {
  // not implemented on windows!
  return 0.0;
  }
#endif



namespace extemp {

    uint32_t UNIV::FRAMES = 128;
    uint32_t UNIV::CHANNELS = 2;
    uint32_t UNIV::IN_CHANNELS = 0;
    uint32_t UNIV::SAMPLERATE = 44100;
    uint32_t UNIV::SECOND = SAMPLERATE;
    uint32_t UNIV::MINUTE = SECOND * 60;
    uint32_t UNIV::HOUR = MINUTE * 60;
    uint64_t UNIV::TIME = 0l;
    uint64_t UNIV::DEVICE_TIME = 0l;
    const char* UNIV::PWD = "";
    uint32_t UNIV::AUDIO_NONE = 0; // 0 for real device, 1 for dummy device
    uint32_t UNIV::AUDIO_DEVICE = -1;
    uint32_t UNIV::AUDIO_IN_DEVICE = -1;
  std::map<std::string,std::string> UNIV::CMDPARAMS;
  std::vector<std::string> UNIV::ARCH;
  std::vector<std::string> UNIV::ATTRS;
  std::vector<std::string> UNIV::CPU;  
  
#ifdef EXT_BOOST
    boost::mt19937 UNIV::RNGGEN;
    boost::uniform_01<boost::mt19937> UNIV::RNG(UNIV::RNGGEN);
#endif

    // 0 is for ansi, 1 is for MSDos CMD shell
#ifdef _MSC_VER
    uint32_t UNIV::EXT_TERM = 1;
#else
    uint32_t UNIV::EXT_TERM = 0; 
#endif
    uint32_t UNIV::EXT_LOADSTD = 1;

    void UNIV::initRand() {
#ifdef _MSC_VER
      srand((int)UNIV::DEVICE_TIME); ///UNIV::SECOND));
#elif __linux__
      srand((int)(UNIV::DEVICE_TIME/UNIV::SECOND));
#else
      sranddev();
#endif
    }
    
    int UNIV::random(int range) {
#ifdef EXT_BOOST
        return (int) (RNG()*(double)range);
#else
        return (int)((double)rand() / (double)RAND_MAX * (double) range);
#endif
    }

    double UNIV::random() {
#ifdef EXT_BOOST
        return RNG();
#else      
	return (double)rand() / (double)RAND_MAX;
#endif
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
	if(val == 0) {
	    ss << "-ERROR BAD POINTER-";
	    return;
	}
	if(pointer_type(val) > 16) {
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
	    if(full) {
		ss << "()";
	    }else{
		ss << "NIL";
	    }
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
        }else if(_sc->EOF_OBJ == val){
          ss << "#<EOF>";
	}else{
	    ss << "UNKOWN VALUE: " << val << " (GC'd?) ";
	}
		
	return;
    }
	
	
} //End Namespace
