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

def ordered_unique(xs,key=None):
    """Takes a list of filenames (not paths) and returns a distinct list

    First-one-wins, comparison is made using the `key` function.

    Parameters
    ----------
    xs : Iterable
        input collection of values.

    key : f str -> str (Optional)
        The function takes a filename and returns a key. defaults
        to "foo.bar.baz" -> "foo"

    Returns
    -------
    list :
        subset of values in xs, in-order.

    """
    if key is None:
        key = lambda x: x.split('.')[0]
    from collections import OrderedDict
    retval = xs
    if xs:
        d = OrderedDict()
        for depname in xs:
            basename = key(depname)
            if basename not in d:
                d[basename] = depname
        retval = list(d.values())
    return retval

def comp_artifacts(xtm_file, shlibsuffix):
    """Returns compilation artifacts filenames for stdlib source file

    >>> comp_artifacts("libs/xore/foo.xtm")
    ("libs/xtmfoo.so", "libs/foo.xtm")

    """
    rootname = os.path.basename(xtm_file).split('.')[0]
    target_f = 'libs/xtm' + rootname + shlibsuffix # libs/xtmfoo.so
    xtm_header_f = 'libs/' + xtm_file.split('/')[-1]      # libs/foo.xtm
    return target_f, xtm_header_f

def get_stdlib_deps(shlibsuffix):
    """Extract inter-dependencies from stdlib component. Hackish.

    >>> get_stdlib_deps('.so')
    {'core/std.xtm': [],
     'core/math.xtm': {'global': []
                       'local': ['libs/xtmstd.so', 'libs/std.xtm'],
     'external/bullet.xtm': {
        "global": ["libBulletDynamics.so"],
        "local": ["libs/xtmstd.so"]},
     <...>}
    """
    import re
    import glob
    from collections import defaultdict, OrderedDict

    d = defaultdict(lambda: defaultdict(lambda: []))

    for fname in glob.glob('libs/*/*.xtm'):
        if os.path.basename(fname).startswith("."):
            continue
        with open(fname) as f:
            s=f.read()
        local_deps = re.findall("^\s*\(sys:load \"(.+?)\"",s,flags=re.M)
        fname = fname.split('libs/')[-1]
        local_deps = [x for x in local_deps
                if os.path.basename(fname) != os.path.basename(x)]

        for dep in local_deps:
            if '-scm' not in dep:
                target_f, xtm_header_f = comp_artifacts(dep,shlibsuffix)
                d[fname]['local'].append(target_f)

        # global deps are 3rd-party libs (GL, Cairo, Bullet)
        global_deps = []
        pats=["\(\s*sys:precomp:set-dylib-name-info.+?^\s*$",
              "\(\s*sys:load-dylib.+?^\s*$"]
        dylib_pat='"(\w.+?{})"'.format(shlibsuffix)
        for p in pats:
            for m in re.findall(p,s,flags=re.DOTALL|re.M):
                global_deps.extend(re.findall(dylib_pat,m))

        d[fname]['global'].extend(ordered_unique(global_deps))
        d[fname]['local'].extend([])

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
