#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import glob
import sys
import json

import argparse
argparser = argparse.ArgumentParser(description="""

""".strip())

argparser.add_argument('--shlibsuffix',
    nargs=1,
    help='Extension for shared library files: .so/.dylib/.dll',
    default=dict(Linux='.so',Darwin='.dylib').get(os.uname()[0],'.so')
    )
argparser.add_argument('-o','--output',
    default=None,
    metavar='FILE',
    help='Name of JSON output file.'
    )

def comp_artifacts(xtm_file, shlibsuffix):
    "Returns compilation artifacts filenames for stdlib source file"
    "e.g. (libs/xtmfoo.so,libs/foo.xtm)"

    rootname = os.path.basename(xtm_file).split('.')[0]
    target_f = 'libs/xtm' + rootname + shlibsuffix # libs/xtmfoo.so
    xtm_header_f = 'libs/' + xtm_file.split('/')[-1]      # libs/foo.xtm
    return target_f, xtm_header_f

def get_stdlib_deps(shlibsuffix):
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
                d[fname].extend(comp_artifacts(dep,shlibsuffix))

    return dict(d)

def main():
    deps = get_stdlib_deps(args.shlibsuffix)
    j = json.dumps(deps,indent=4)
    if args.output is None:
        print(j)
    else:
        with open(args.output,"wb") as f:
            f.write(j)


if __name__ == '__main__':
    args = argparser.parse_args()
    args.shlibsuffix = '.' + args.shlibsuffix.lstrip('.')
    import sys
    sys.exit(main())
