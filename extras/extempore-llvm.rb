require 'formula'

class ExtemporeLlvm < Formula
  homepage  'http://llvm.org/'
  url       'http://llvm.org/releases/3.2/llvm-3.2.src.tar.gz'
  sha1      '42d139ab4c9f0c539c60f5ac07486e9d30fc1280'
  keg_only "This is a specially patched LLVM for use in building Extempore."

  option 'with-asan', 'Include support for -faddress-sanitizer (from compiler-rt)'
  option 'all-targets', 'Build all target backends'
  option 'rtti', 'Build with C++ RTTI'
  option 'disable-assertions', 'Speeds up LLVM, but provides less debug information'

  def patches
    # patch llparser (bugfix) for Extempore use
    DATA
  end

  def install
    CompilerRt.new("compiler-rt").brew do
      (buildpath/'projects/compiler-rt').install Dir['*']
    end if build.include? 'with-asan'

    ENV['REQUIRES_RTTI'] = '1' if build.include? 'rtti'

    args = [
      "--prefix=#{prefix}",
      "--enable-optimized",
      # As of LLVM 3.1, attempting to build ocaml bindings with Homebrew's
      # OCaml 3.12.1 results in errors.
      "--disable-bindings",
    ]

    if build.include? 'all-targets'
      args << "--enable-targets=all"
    else
      args << "--enable-targets=host"
    end

    args << "--disable-assertions" if build.include? 'disable-assertions'

    system "./configure", *args
    system "make install"

  end

  def caveats; <<-EOS.undent
    This is a specifically patched version of LLVM for building Extempore.
    It shouldn't get in the way of any other LLVM install.

    If you have any problems, raise them on extemporelang@googlegroups.com
    EOS
  end

end

__END__
diff --git a/lib/AsmParser/LLParser.cpp b/lib/AsmParser/LLParser.cpp
index b24291f..f01b2ac 100644
--- a/lib/AsmParser/LLParser.cpp
+++ b/lib/AsmParser/LLParser.cpp
@@ -1349,8 +1349,14 @@ bool LLParser::ParseType(Type *&Result, bool AllowVoid) {
     // If the type hasn't been defined yet, create a forward definition and
     // remember where that forward def'n was seen (in case it never is defined).
     if (Entry.first == 0) {
-      Entry.first = StructType::create(Context, Lex.getStrVal());
-      Entry.second = Lex.getLoc();
+        // this here for extempore
+  	if (M->getTypeByName(Lex.getStrVal())) {
+           Entry.first = M->getTypeByName(Lex.getStrVal());
+           Entry.second = SMLoc();
+        } else {
+      	   Entry.first = StructType::create(Context, Lex.getStrVal());
+           Entry.second = Lex.getLoc();
+	}
     }
     Result = Entry.first;
     Lex.Lex();
