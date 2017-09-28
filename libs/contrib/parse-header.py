import site
site.addsitedir("/usr/local/opt/llvm/lib/python2.7/site-packages/")
import clang.cindex
clang.cindex.Config.set_library_path("/usr/local/opt/llvm/lib/")

# index = clang.cindex.Index(clang.cindex.conf.lib.clang_createIndex(False, True))

tu = clang.cindex.TranslationUnit.from_source("/Users/ben/Code/src/nanovg/src/nanovg.h")

for cursor in tu.cursor.walk_preorder():
    print cursor.spelling, cursor.kind
