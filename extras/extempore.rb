require 'formula'

class Extempore < Formula
  homepage 'http://extempore.moso.com.au'
  # url 'http://extempore.moso.com.au/versions/extempore-1.1.zip'
  url 'http://mai.anu.edu.au/extempore-Darwin-20130619.tar.gz'
  sha1 'cb7243bd23e4226e7f92d78f2a4bfb39f26f5d2f'
  keg_only "See http://benswift.me/extempore-docs to get started with Extempore."

  def install
    # system "EXT_LLVM_DIR=#{HOMEBREW_PREFIX}/Cellar/extempore-llvm/3.2 ./all.bash"
    system "mv * #{prefix}"
  end

end
