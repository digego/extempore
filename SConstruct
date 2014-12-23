#!/usr/bin/env scons

# This is a script used by the 'scons' build tool (http://www.scons.org/)
# Both Fedora and Ubuntu provide a scons package. On OSX you can use Homebrew.
#
# Building on Windows should be possible but probably does not work right now.
#
# Source Tarball: http://prdownloads.sourceforge.net/scons/scons-2.3.4.tar.gz

# Requirements:
# - Provide help via 'scons help', specifically a list of valid targets.
# - Check EXT_LLVM_DIR or known homebrew locations for llvm.
# - Check llvm version.
# - Check that patches have been applied to 3d party libs (llvm).
# - Support multiple platforms (Linux, OSX, Windows?).
# - Compile (and clean) extempore binary.
# - Compile (and clean) stdlib/stdlib-core/stdlib-external.
# - Run the tests. (each file) In parallel.
# - Allow compilation of selected stdlib files. See 'scons help'
# - Extract dependency information from stdlib components.
# - Support parallel builds of stdlib according to dependencies.
# - Cache all build artifacts (stdlib included).
# - Support command line flags (see --help for list, 'scons help' for example)

import os
import glob
import sys
import subprocess
from hashlib import md5

# pass --no-cache to disable this
CacheDir('.scons_build_cache')

color_dict = {
    'red'       : "\033[0;31m",
    'green'     : "\033[0;32m",
    'blue'      : "\033[0;34m",
    'purple'    : "\033[0;35m",
    'turquoise' : "\033[0;36m",
    'orange'    : "\033[0;33m",
    'white'     : "\033[0;00m",
}

# The version we check for
LLVM_VERSION = '3.4.1'

#
# MD5 digests used for verifying that patches have been applied.
# One dict per file. `fname` gets interpolated with os.environ (shell env).
PATCH_DATA =[
    dict(
        fname    = '{EXT_LLVM_DIR}/../lib/AsmParser/LLParser.cpp',
        pre_md5  = 'fe17d4ec23e7a12f35eed08349afac1c',
        post_md5 = 'c3fa2f3e6ab6f4404b63f7a929ecf14d'
    ),
]

OBJDIR = 'build/obj'

EXT_SRCFILES = \
    ['src/AudioDevice.cpp',
     'src/EXTCondition.cpp',
     'src/Extempore.cpp',
     'src/EXTLLVM.cpp',
     'src/EXTMonitor.cpp',
     'src/EXTMutex.cpp',
     'src/EXTThread.cpp',
     'src/OSC.cpp',
     'src/Scheme.cpp',
     'src/SchemeFFI.cpp',
     'src/SchemeProcess.cpp',
     'src/SchemeREPL.cpp',
     'src/TaskScheduler.cpp',
     'src/UNIV.cpp']

# Only files included here will be compiled using the stdlib-* targets
STDLIB_CORE_SRCS = [
    'core/std.xtm',
    'core/math.xtm',
    'core/audio_dsp.xtm',
    'core/instruments.xtm']

STDLIB_EXTERNAL_SRCS = [
    'external/fft.xtm',
    'external/sndfile.xtm',
    'external/audio_dsp_ext.xtm',
    'external/instruments_ext.xtm',
    'external/glib.xtm',
    'external/soil.xtm',
    'external/opengl.xtm',
    'external/shaders.xtm',
    'external/assimp.xtm',
    'external/rtmidi.xtm',
    'external/openvg.xtm']

AddOption('--EXT_BOOST',
          dest   = 'EXT_BOOST',
          action = 'store_true',
          help   = 'Whether Boost should be compiled in.'
          )

AddOption('--stdlib-sources',
          default = [],
          nargs  = 1,
          action = 'store',
          help   = 'Specify xtm files to be recompiled. For use with stdlib target.'
          )

AddOption('--no-dep-discovery',
          dest   = 'no_dep_discovery',
          action = 'store_true',
          help   = 'Skip dependency discovery when compiling stdlib (Do not use with -j)'
          )


if 'help' in  COMMAND_LINE_TARGETS :
    print("""
{green} extempore             {white} Build the extempore binary.
{green} stdlib                {white} Compile both core and external stdlib components.
{green} stdlib-core           {white} Compile core stdlib components only.
{green} stdlib-external       {white} Compile external stdlib components only.
{green} test                  {white} Run all tests.
{green} test-core             {white} Run tests tests/core/.
{green} test-external         {white} Run tests test/external.

Add -c to the command to clean up the respective target.

You can specify options (--help lists them):

    scons --EXT_BOOST

To Precompile only specific stdlib components (paths are relative to 'libs/'):

    scons --stdlib-source="external/sndfile.xtm core/std.xtm" stdlib

Build artifacts are cached by default. Use the --no-cache option to disable it.
""").format(**color_dict)

    exit(1)


def check_llvm_dir_isset(context):
    context.Message('Searching for llvm build directory...')

    paths = [os.environ.get('EXT_LLVM_DIR',''),
    '%s/Cellar/extempore-llvm/%s' % (os.environ.get('HOME'),LLVM_VERSION),
    '/usr/local/Cellar/extempore-llvm//%s' % (LLVM_VERSION),
    ]

    result = False
    for path in paths:
        if os.path.exists(path):
            # ensure it's in the environment
            os.environ['EXT_LLVM_DIR'] = path
            result = True
            break

    if result:
        llvm_config = os.path.join(path, "bin/llvm-config").strip()
        if not os.path.isfile(llvm_config):
            err_msg ='%s/bin/llvm_config does not exist.' % path
            result=False
        else:
            os.environ['EXT_LLVM_CONFIG_SCRIPT'] = llvm_config
            # make sure we pass that envar on to commands we call
            env['ENV']['EXT_LLVM_DIR'] = os.environ['EXT_LLVM_DIR']
            context.Result(path)

    if not result:
        context.Result("not found")
    return result


def check_llvm_version(context):
    context.Message('Checking for llvm-3.4.1...')
    version_cmd_l = [os.environ['EXT_LLVM_CONFIG_SCRIPT'], '--version']
    llvm_version = subprocess.check_output(version_cmd_l).strip()
    os.environ['EXT_LLVM_VERSION'] = llvm_version
    result = llvm_version == LLVM_VERSION
    context.Result(result)

    return result

def check_patches_applied(context):
    context.Message('Checking that patches have been applied...')
    result = True
    for d in PATCH_DATA:
        with open(d['fname'].format(**os.environ)) as f:
            h = md5(f.read()).hexdigest()
        if h != d['post_md5']:
            if h == d['pre_md5']:
                context.Result(fname + " has not been patched.")
            else:
                context.Result(
                    fname + " - unknown version.")
            result = False
            break
    else:
        context.Result(result)
    return result

#
# The environments determines envars when invoking tools
# and generally serves as the context for build steps.
#
# platform = ARGUMENTS.get('OS', Platform())

env = Environment(CPPPATH=['#/include'])

# By default build nothing (but we do run the checks)
Default(None)

#
# Checks - tell the user if something isn't as it should be
#

if not  COMMAND_LINE_TARGETS :
    msg = "{orange}INFO{white}: See 'scons help' for list of targets and 'scons --help' for options."
    print(msg.format(**color_dict))

conf = Configure(env, custom_tests={
    'check_llvm_dir_isset': check_llvm_dir_isset,
    'check_patches_applied': check_patches_applied,
    'check_llvm_version': check_llvm_version
})

if not conf.check_llvm_dir_isset():
    err_msg = \
        """
{red}ERROR{white}: You need to set {green}EXT_LLVM_DIR{white} to the path of your (Extempore) LLVM directory.
""".lstrip()
    sys.stderr.write(err_msg.format(**color_dict))
    Exit(1)

if not conf.check_llvm_version():
    err_msg = \
        """
{orange}WARN{white}: Extempore should be compiled against llvm {req_version} not {llvm_version}
""".lstrip()
    sys.stderr.write(err_msg.format(req_version=LLVM_VERSION,
                                    llvm_version=os.environ[
                                        'EXT_LLVM_VERSION'],
                                    **color_dict))
    # Exit(1)

if not conf.check_patches_applied():
    err_msg = \
        """
{orange}WARN{white}: Have you applied the provided patchs?
""".lstrip()
    sys.stderr.write(err_msg.format(**color_dict))
    # Exit(1)

env = conf.Finish()

################################################
# How to build the 'extempore' target
################################################

def configure_environment_linux():
    # CXXFLAGS = '-w -O3 -MMD -fexceptions -frtti'
    # CXXFLAGS = '-w -O3 -fexceptions -frtti'
    env.Append(CCFLAGS = '-fPIC -O3 -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS')
    env.Append(LINKFLAGS = '-Wl,--export-dynamic -ldl -lm -pthread -lpcre -lportaudio -lGL -lX11')

def configure_environment_darwin():
    env.Replace(CXX = 'clang++')
    env.Replace(LD = 'clang++')
    env.Append(CCFLAGS = '-O3 -DUSE_GLUT')
    env.Append(LINKFLAGS = '-pthread -lpcre -lportaudio')
    env.AppendUnique(FRAMEWORKS = Split('Cocoa CoreAudio AudioToolbox AudioUnit GLUT OpenGL'))

def configure_environment_windows():
    env.Replace(CCFLAGS = '-g -fPIC -O3 -DEXT_BOOST -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS')
    env.Replace(LINKFLAGS = '-lpcre -lportaudio -lboost_thread -lboost_system -lboost_filesystem')

    print('platform = ' + env['PLATFORM'])

def configure_environment(platform):
    if platform == 'darwin':
        configure_environment_darwin()
    elif platform == 'linux':
        configure_environment_linux()
    elif platform == 'windows':
        configure_environment_windows()
    else:
        print('Unsupported platform: ' + platform)

configure_environment(env['PLATFORM'])

# additional LLVM env vars
env.ParseConfig(os.environ['EXT_LLVM_CONFIG_SCRIPT'] + '  --cflags --ldflags --libs')

# Prepare object files
EXT_OBJ_FILES = []
for src_f in EXT_SRCFILES:
    target_o = os.path.join(OBJDIR, src_f.split('.')[0])
    EXT_OBJ_FILES.append(env.Object(target_o, src_f))

# And make the binary using them

env.Program('extempore', EXT_OBJ_FILES)

################################################
# How to build the 'stdlib-foo' targets
################################################

def comp_artifacts(xtm_file):
    "Returns compilation artifacts filenames for stdlib source file"
    "e.g. (libs/xtmfoo.so,libs/foo.xtm)"

    rootname = os.path.basename(xtm_file).split('.')[0]
    target_f = 'libs/xtm' + rootname + env['SHLIBSUFFIX'] # libs/xtmfoo.so
    xtm_header_f = 'libs/' + xtm_file.split('/')[-1]      # libs/foo.xtm
    return target_f, xtm_header_f

def get_stdlib_deps():
    """Extract inter-dependencies from stdlib component. Hackish.

    Returns:
        {'core/std.xtm': [],
         'core/math.xtm': ['libs/xtmstd.so', 'libs/std.xtm'],
         <...>}
    """
    import re
    import glob
    from collections import defaultdict

    d = defaultdict(lambda: [])

    for fname in glob.glob('libs/*/*.xtm'):
        if os.path.basename(fname).startswith("."):
            continue
        with open(fname) as f:
            s=f.read()
        deps = re.findall("^\s*\(sys:load \"(.+?)\"",s,flags=re.M)
        fname = fname.split('libs/')[-1]
        deps = [x for x in deps
                if os.path.basename(fname) != os.path.basename(x)]

        for dep in deps:

            if '-scm' in dep:
                d[fname] = d[fname]
            else:
                d[fname].extend(comp_artifacts(dep))

    return dict(d)

# Note: --noaudio for parallel builds
EXT_COMPILE_CMD = './extempore --noaudio --port {port} --nostd  --eval "(sys:precomp:compile-xtm-file \\\"libs/{xtm_file}\\\" #t #t #t)"'
EXT_STDLIB_OUTPUTS = []
EXT_STDLIB_CORE_OUTPUTS = []
EXT_STDLIB_EXTERNAL_OUTPUTS = []
EXT_STDLIB_CORE_ADDITIONAL_CLEAN = []
EXT_STDLIB_EXTERNAL_ADDITIONAL_CLEAN = []

port_base = 51004
port = port_base
port_step = 2

# Each compilation is assigned a different port, for parallel compilation
def construct_stdlib_nodes(deps):
    global port
    nodes = []

    for xtm_file, deps in deps.items():
        cmd = EXT_COMPILE_CMD.format(xtm_file=xtm_file, port=port)
        port += port_step
        port = port_base + ((port-port_base) % 5000)
        target_f, xtm_header_f = comp_artifacts(xtm_file)

        full_deps = ['libs/' + xtm_file] + deps
        env.Command(target_f, full_deps, cmd)
        env.Command(xtm_header_f,full_deps + [target_f], cmd)

        # we tell scons about the entire dep graph above, but we only include
        # specific libraries in the actual targets. i.e. not all stdlib components
        # we find is to be precompiled.
        if not(xtm_file in STDLIB_CORE_SRCS or xtm_file in STDLIB_EXTERNAL_SRCS):
            continue
        nodes.append(target_f)
        nodes.append(xtm_header_f)

    return nodes

if env.GetOption('stdlib_sources'): # user manually specified sources
    sources = [x for x in env.GetOption('stdlib_sources').split(' ') if x]
    for xtm_file in sources:
        EXT_STDLIB_OUTPUTS.extend(comp_artifacts(xtm_file))
else:
    if env.GetOption('no_dep_discovery'): # the less-moving-parts option
        deps = {xtm_file: [] for xtm_file in STDLIB_CORE_SRCS+STDLIB_EXTERNAL_SRCS}
    else:
        # Extract dependencies for source files, required for parallel builds with -j
        deps = get_stdlib_deps()

        external_deps = {xtm_file:deps for xtm_file,deps in deps.items() if 'external/' in xtm_file}
        core_deps = {xtm_file:deps for xtm_file,deps in deps.items() if 'core/' in xtm_file}
        EXT_STDLIB_EXTERNAL_OUTPUTS = construct_stdlib_nodes(external_deps)
        EXT_STDLIB_CORE_OUTPUTS = construct_stdlib_nodes(core_deps)
        EXT_STDLIB_OUTPUTS = EXT_STDLIB_CORE_OUTPUTS + EXT_STDLIB_EXTERNAL_OUTPUTS

stdlib = env.Command('stdlib', EXT_STDLIB_OUTPUTS, '')
stdlib_core = env.Command('stdlib-core', EXT_STDLIB_CORE_OUTPUTS, '')
stdlib_external = env.Command(
    'stdlib-external', EXT_STDLIB_EXTERNAL_OUTPUTS, '')


################################################
# How to build the 'test-foo' targets
################################################

EXT_TEST_CMD = """./extempore --noaudio --port {port} --eval \"(sys:run-tests '(\\"{xtm_file}\\") #t #t)\""""

blacklist = ["tests/core/builtins.xtm"]

def construct_test_nodes(glob_pat):
    global port
    nodes =[]
    for xtm_file  in glob.glob(glob_pat):
        if xtm_file in blacklist: continue
        cmd = EXT_TEST_CMD.format(xtm_file=xtm_file,port=port)
        port += port_step
        port = port_base + ((port-port_base) % 5000)
        nodes.append(env.Command(xtm_file+'.phony','',cmd))
    return nodes

EXT_CORE_TEST_NODES = construct_test_nodes('tests/core/*.xtm')
EXT_EXTERNAL_TEST_NODES = construct_test_nodes('tests/external/*.xtm')

test_tgt = env.Command('test', EXT_CORE_TEST_NODES +EXT_EXTERNAL_TEST_NODES ,'')
test_core_tgt = env.Command('test-core', EXT_CORE_TEST_NODES ,'')
test_external_tgt = env.Command('test-external', EXT_EXTERNAL_TEST_NODES ,'')
