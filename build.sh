#!/bin/sh

if [ $# = 0 ]
then
echo "You must pass either 'linux' or 'osx'"
return
fi

if [ $# -lt 2 ]
then
echo "You must provide a path to llvm"
return
fi

if [ $1 = "osx" ]
then
echo "compiling for osx ..."
# COMPILE OSX
g++ -w -O3 -D_GNU_SOURCE -DTARGET_OS_MAC  -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS -Iinclude -I$2/include src/*.cpp -L$2/Release/lib -lpthread -lm -lLLVMX86AsmParser -lLLVMX86AsmPrinter -lLLVMX86CodeGen -lLLVMSelectionDAG -lLLVMAsmParser -lLLVMAsmPrinter -lLLVMXCoreInfo -lLLVMX86Info -lLLVMInterpreter -lLLVMJIT -lLLVMExecutionEngine -lLLVMCodeGen -lLLVMScalarOpts -lLLVMInstCombine -lLLVMipo -lLLVMTransformUtils -lLLVMInstrumentation -lLLVMipa -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMCore -lLLVMSupport -lLLVMSystem -lpcre -L/Users/andrew/Documents/Code/glfw-2.7/lib/cocoa -lglfw -framework opengl -framework glut -framework cocoa -framework coreaudio -o extempore
echo "done"
else
echo "compiling for linux ..."
# COMPILE LINUX
g++ -w -O3 -D_GNU_SOURCE -DTARGET_OS_LINUX  -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS -Iinclude -I$2/include src/*.cpp -L$2/Release/lib -lpthread -lm -lLLVMX86AsmParser -lLLVMX86AsmPrinter -lLLVMX86CodeGen -lLLVMSelectionDAG -lLLVMAsmParser -lLLVMAsmPrinter -lLLVMXCoreInfo -lLLVMX86Info -lLLVMInterpreter -lLLVMJIT -lLLVMExecutionEngine -lLLVMCodeGen -lLLVMScalarOpts -lLLVMInstCombine -lLLVMipo -lLLVMTransformUtils -lLLVMInstrumentation -lLLVMipa -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMCore -lLLVMSupport -lLLVMSystem -lpcre -lportaudio -lglfw -lglut /usr/lib/mesa/libGL.so.1 -o extempore
echo "done"
fi
