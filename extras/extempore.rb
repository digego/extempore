require 'formula'

class Extempore < Formula
  homepage 'http://extempore.moso.com.au'
  url 'http://extempore.moso.com.au/extras/extempore-Darwin-20130621.tar.gz'
  sha1 '9338ac634adf4a2e447e1a3f84e4d100a773e67e'
  keg_only "See 'Caveats' below."

  depends_on 'pcre'
  depends_on 'portaudio'

  def install
    # for building from source (currently unused)
    # system "EXT_LLVM_DIR=#{HOMEBREW_PREFIX}/Cellar/extempore-llvm/3.2 ./all.bash"

    # just download and extract the tarball
    system "mv * #{prefix}"
  end

  def caveats
    s = ''
    s += <<-EOS.undent
      This formula doesn't actually build from source, it just pulls down a
      precompiled tarball.  Extempore is now installed in #{prefix}

      You can link it to somewhere on your path if you like, but then remember to
      specify the libs/ directory in full in .xtm source files
      e.g. (load "#{prefix}/libs/foolib.xtm")

      For Extempore documentation, see http://benswift.me/extempore-docs/
    EOS
  end

end
