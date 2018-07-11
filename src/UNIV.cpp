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
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
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
#include "SchemePrivate.h"

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#include <AppKit/AppKit.h>
#else
#include <time.h>
#endif

#ifdef _WIN32
#include <malloc.h>
#include <Windows.h>

enum Windows_Color_Convert
{
    Black   = 0,
    Red     = FOREGROUND_RED,
    Green   = FOREGROUND_GREEN,
    Yellow  = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_INTENSITY,
    Blue    = FOREGROUND_BLUE  | FOREGROUND_INTENSITY,
    Magenta = FOREGROUND_RED   | FOREGROUND_BLUE,
    Cyan    = FOREGROUND_GREEN | FOREGROUND_BLUE,
    White   = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY
};

enum Windows_BGColor_convert
{
    BGBlack   = 0,
    BGRed     = BACKGROUND_RED,
    BGGreen   = BACKGROUND_GREEN,
    BGYellow  = BACKGROUND_RED   | BACKGROUND_GREEN | BACKGROUND_INTENSITY,
    BGBlue    = BACKGROUND_BLUE,
    BGMagenta = BACKGROUND_RED   | BACKGROUND_BLUE,
    BGCyan    = BACKGROUND_GREEN | BACKGROUND_BLUE,
    BGWhite   = BACKGROUND_RED   | BACKGROUND_GREEN | BACKGROUND_BLUE | BACKGROUND_INTENSITY,
};

int WINDOWS_COLORS[] = { Black, Red, Green, Yellow, Blue, Magenta, Cyan, White };
int WINDOWS_BGCOLORS[] = { BGBlack, BGRed, BGGreen, BGYellow, BGBlue, BGMagenta, BGCyan, BGWhite };

#endif

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

EXPORT char* cname_encode(char *data, size_t input_length, size_t *output_length)
{
    *output_length = 4 * ((input_length + 2) / 3);

    //char *encoded_data = (char*) malloc(*output_length);
    char *encoded_data = (char*) malloc(*output_length+1);
    encoded_data[*output_length] = 0;
    if (encoded_data == NULL) return NULL;

    for (unsigned i = 0, j = 0; i < input_length;) {

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

EXPORT char* cname_decode(char *data, size_t input_length, size_t *output_length)
{
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

    for (unsigned i = 0, j = 0; i < input_length;) {

        uint32_t sextet_a = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[unsigned(data[i++])];
        uint32_t sextet_b = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[unsigned(data[i++])];
        uint32_t sextet_c = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[unsigned(data[i++])];
        uint32_t sextet_d = data[i] == '$' ? 0 & i++ : base64_codesafe_decoding_table[unsigned(data[i++])];

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

EXPORT char* base64_encode(const unsigned char *data, size_t input_length, size_t *output_length)
{
    *output_length = 4 * ((input_length + 2) / 3);

    char *encoded_data = (char*) malloc(*output_length+1);
    encoded_data[*output_length]=0;

    if (encoded_data == NULL) return NULL;

    for (unsigned i = 0, j = 0; i < input_length;) {

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


EXPORT unsigned char* base64_decode(const char *data, size_t input_length, size_t *output_length)
{
    if (base64_std_decoding_table == NULL) base64_std_build_decoding_table();

    if (input_length % 4 != 0) return NULL;

    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '=') (*output_length)--;
    if (data[input_length - 2] == '=') (*output_length)--;

    unsigned char *decoded_data = (unsigned char*) malloc(*output_length);
    if (decoded_data == NULL) return NULL;

    for (unsigned i = 0, j = 0; i < input_length;) {

        uint32_t sextet_a = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[unsigned(data[i++])];
        uint32_t sextet_b = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[unsigned(data[i++])];
        uint32_t sextet_c = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[unsigned(data[i++])];
        uint32_t sextet_d = data[i] == '=' ? 0 & i++ : base64_std_decoding_table[unsigned(data[i++])];

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

bool rmatch(char* regex, const char* str)
{
  //  char* data = char* strstring_value(pair_car(args));
  // char* pattern = string_value(pair_cadr(args));
  const char* data = str;
  char* pattern = regex;

  pcre *re;
  const char *error;
  int erroffset;

  re = pcre_compile(    pattern, /* the pattern */
                        0, /* default options */
                        &error, /* for error message */
                        &erroffset, /* for error offset */
                        NULL); /* use default character tables */

  int rc;
  int ovector[30];
  rc = pcre_exec(       re, /* result of pcre_compile() */
                        NULL, /* we didn’t study the pattern */
                        data, /* the subject string */
                        strlen(data), /* the length of the subject string */
                        0, /* start at offset 0 in the subject */
                        0, /* default options */
                        ovector, /* vector of integers for substring information */
                        30); /* number of elements (NOT size in bytes) */

  return (rc>=0) ? true : false;
}


// bool rmatches(char* regex, char* str, struct regex_matched_buffer* result)
//     {
//   char* data = str;
//   char* pattern = regex;
//      pcre *re;
//      const char *error;
//      int erroffset;
//      re = pcre_compile(      pattern, /* the pattern */
//                              0, /* default options */
//                              &error, /* for error message */
//                              &erroffset, /* for error offset */
//                              NULL); /* use default character tables */

//      int rc;
//      int ovector[60];
//      rc = pcre_exec( re, /* result of pcre_compile() */
//                      NULL, /* we didn’t study the pattern */
//                      data, /* the subject string */
//                      strlen(data), /* the length of the subject string */
//                      0, /* start at offset 0 in the subject */
//                      0, /* default options */
//                      ovector, /* vector of integers for substring information */
//                      60); /* number of elements (NOT size in bytes) */

//      // if failed to match return empty list
//      if(rc<0 || rc>100) return false;

//   result->matches = rc;
//      for(int i=0, p=0, k=(rc-1);i<rc;i++,k--)
//      {
//          //std::cout << "RC: " << rc << " " << ovector[p] << "::" << ovector[p+1] << std::endl;
//     p=i*2;

//        if(ovector[p]==-1) {
//       strcpy(result->data[k],"");
//        }else{
//       int range = ovector[p+1] - ovector[p];
//       char* b = (char*) alloca(range+1);
//       memset(b,0,range+1);
//       char* a = data+ovector[p];
//       char* substring = strncpy(b, a, range);
//       strcpy(result->data[k],substring);
//        }
//      }
//   return true;
// }


int64_t rmatches(char* regex, char* str, char** results, int64_t maxnum)
    {
  char* data = str;
  char* pattern = regex;
        pcre *re;
        const char *error;
        int erroffset;
        re = pcre_compile(      pattern, /* the pattern */
                                0, /* default options */
                                &error, /* for error message */
                                &erroffset, /* for error offset */
                                NULL); /* use default character tables */

        // pointer to hold return results
        int rc;
        int ovector[60];
  int64_t num=0;

        while(true) {
            rc = pcre_exec(     re, /* result of pcre_compile() */
                                NULL, /* we didn’t study the pattern */
                                data, /* the subject string */
                                strlen(data), /* the length of the subject string */
                                0, /* start at offset 0 in the subject */
                                0, /* default options */
                                ovector, /* vector of integers for substring information */
                                60); /* number of elements (NOT size in bytes) */

            //std::cout << data << " RC: " << rc << " " << ovector[0] << "::" << ovector[1] << "  num " << num << " max " << maxnum << std::endl;
            if(rc<1 || num>=maxnum) {
        return num;
            }
            int range = ovector[1] - ovector[0];
            char* b = (char*) alloca(range+1);
            b[range] = '\0';
            char* a = data+ovector[0];
            char* substring = strncpy(b, a, range);
      // std::cout << "substr:" << substring << std::endl;
      char* tmp = (char*) malloc(range+1);
      tmp[range] = '\0';
      strncpy(tmp,substring,range);
      // std::cout << "adding:" << tmp << " at:" << num << std::endl;
      results[num]=tmp;
      // std::cout << "done!" << std::endl;
      num++;
            //_sc->imp_env->insert(list);
            data = data+range+ovector[0];
        }
  return 0;
}

bool rsplit(const char* regex, const char* str, char* a, char* b)
{ // TODO: harmonize with FFI
  int length = strlen(str);
  pcre *re;
  const char *error;
  int erroffset;
  //printf("dat: str\n");
  // should probably move this regex compile to global
  re = pcre_compile(    regex, /* the regex */
                        0, /* default options */
                        &error, /* for error message */
                        &erroffset, /* for error offset */
                        NULL); /* use default character tables */
  int rc;
  int ovector[60];
  rc = pcre_exec(       re, /* result of pcre_compile() */
                        NULL, /* we didn’t study the regex */
                        str, /* the subject string */
                        strlen(str), /* the length of the subject string */
                        0, /* start at offset 0 in the subject */
                        0, /* default options */
                        ovector, /* vector of integers for substring information */
                        60); /* number of elements (NOT size in bytes) */

  if(rc<1 || rc>1) return false; // then we failed
  int range = ovector[0];
  int range2 = ovector[1];
  //printf("reg ranges %d:%d\n",range,range2);
  a[range] = '\0';;
  memcpy(a, str, range);
  b[length - range2] = '\0';
  memcpy(b, str + range2, length - range2);
  return true;
}


// returns char* result
char* rreplace(char* regex, char* str, char* replacement, char* result) {

  char* data = str; //string_value(pair_car(args));
        char* pattern = regex; //string_value(pair_cadr(args));
  char* replace = replacement;
        //strcpy(result,replacement);

        pcre *re;
        const char *error;
        int erroffset;
        re = pcre_compile(      pattern, /* the pattern */
                                0, /* default options */
                                &error, /* for error message */
                                &erroffset, /* for error offset */
                                NULL); /* use default character tables */

        int rc;
        int ovector[60];

        rc = pcre_exec( re, /* result of pcre_compile() */
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
  int datalength = strlen(data);
        int pos,range,size,cnt = 0;
  strcpy(result,replace);
        char* p = strtok(result,sep);
  if(p==NULL) { strcpy(result, str); return result; };
        do{
            char* cc;
            pos = strtol(p,&cc,10);
      // std::cout << "p: " << p << " pos: " << pos << " cc:" << cc << std::endl;
            range = (pos>0 && pos<20) ? ovector[(pos*2)+1] - ovector[pos*2] : 0;
      // std::cout << "cnt: " << cnt << " rc:" << rc << " range: " << range << std::endl;
      if(pos>=rc || range < 0 || range > datalength) {
        range = 0;
        cc = p;
      }
            size = strlen(res);
            tmp = (char*) alloca(size+range+strlen(cc)+1);
            tmp[size+range+strlen(cc)] = '\0';
            memcpy(tmp,res,size);
            memcpy(tmp+size,data+ovector[pos*2],range);
            memcpy(tmp+size+range,cc,strlen(cc));
            res = tmp;
            p = strtok(NULL, sep);
      cnt++;
        }while(p);
        // now we can use "rep" to replace the original regex match (i.e. ovector[0]-ovector[1])
        int lgth = ovector[0] + strlen(res) + strlen(data) - ovector[1] + 1;
        range = ovector[1] - ovector[0];
        //char* result = (char*) alloca(lgth);
        if(lgth>4096) return str;
        result[lgth - 1] = '\0'; // TODO: lots of this can be simplified
        memcpy(result,data,ovector[0]);
        memcpy(result+ovector[0],res,strlen(res));
        memcpy(result+ovector[0]+strlen(res),data+ovector[1],strlen(data)-ovector[1]);
        return result;
}

const char* sys_sharedir(){
  return extemp::UNIV::SHARE_DIR.c_str();
}

char* sys_slurp_file(const char* fname)
{
    std::string filename(fname);
    std::string sharedir_filename(extemp::UNIV::SHARE_DIR + "/" + filename);

    // check raw path first, then prepend SHARE_DIR
    std::FILE *fp = std::fopen(filename.c_str(), "rb");
    if (!fp) {
      fp = std::fopen(sharedir_filename.c_str(), "rb");
    }

  if(fp){
    std::fseek(fp, 0, SEEK_END);
    size_t file_size = std::ftell(fp);
    char* buf = (char*)malloc(file_size*sizeof(char));
    std::rewind(fp);
    std::fread(buf, 1, file_size, fp);
    std::fclose(fp);

    buf[file_size-1] = '\0';
    return buf;
  }
  return NULL;
}

int register_for_window_events()
{
#ifdef __APPLE__
  // to give Extempore it's own dock icon, etc
  return (int)[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
#else
  // implement the required "receive events" functionality for Linux
  // or Windows if necessary
  return 1;
#endif
}

namespace extemp
{

namespace UNIV
{

std::string SHARE_DIR = std::string(EXT_SHARE_DIR);
uint32_t NUM_FRAMES = 1024;
uint32_t CHANNELS = 2;
uint32_t IN_CHANNELS = 0;
uint32_t SAMPLE_RATE = 44100;
volatile uint64_t TIME = 0l;
uint64_t DEVICE_TIME = 0l;
double AUDIO_CLOCK_NOW = 0.0;
double AUDIO_CLOCK_BASE = 0.0;
uint64_t TIME_DIVISION = 1;
bool AUDIO_NONE = false;
uint32_t AUDIO_DEVICE = -1;
uint32_t AUDIO_IN_DEVICE = -1;
std::string AUDIO_DEVICE_NAME;
std::string AUDIO_IN_DEVICE_NAME;
double AUDIO_OUTPUT_LATENCY = 0.0;
double CLOCK_OFFSET = 0.0;
std::unordered_map<std::string, std::string> CMDPARAMS;
std::string ARCH;
std::string CPU;
std::vector<std::string> ATTRS;

// 0 is for ansi, 1 is for MSDos CMD shell
#ifdef _WIN32
uint32_t EXT_TERM = 1;
#else
uint32_t EXT_TERM = 0;
#endif
bool EXT_LOADBASE = true;

double midi2frq(double pitch)
{
    return 220.0 * pow(2.0,(pitch - 57.0)/12);
}

double frqRatio(double semitones)
{
    return pow(2.0, (semitones/12.0));
}

bool file_check(const std::string& filename)
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


void printSchemeCell(scheme* _sc, std::stringstream& ss, pointer val, bool full, bool stringquotes)
{
    if(val == 0) {
        ss << "-ERROR BAD POINTER-";
        return;
    }
    if (pointer_type(val) > T_LAST_SYSTEM_TYPE) {
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
                //          ss.str("");
                extemp::UNIV::printSchemeCell(_sc, ss, code, true, stringquotes);
                //          std::cout << "CODE" << std::endl << ss.str() << std::endl << "-----------" << std::endl;

                ss << std::endl << "ENVIR: ";
                pointer envir = frames[j].envir;
                //          ss.str("");
                extemp::UNIV::printSchemeCell(_sc, ss, envir, true, stringquotes);
                //          std::cout << "ENVIR" << std::endl << ss.str() << std::endl << "-----------" << std::endl;
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
                printSchemeCell(_sc, ss, vector_elem(val,i), full, stringquotes);
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

}

} //End Namespace
