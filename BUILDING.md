# Build-from-source notes

If you run into problems with the one-line build commands listed in the
`README.md`, or if just want to understand how Extempore's build process works
in more depth, here's some more information.

## Build-time deps

- a C++ compiler (`clang`, `gcc` >= 4.9, `msvc` >= VS2015)
- Git
- CMake

For platform-specific deps, see "Platform-specific notes" below.

## Options

See the top of `CMakeLists.txt` for all the available build options.

The most relevant option for new Extempore users is the `ASSETS` option. It's
off by default, but if set to `ON` the Extempore build process will download a
bunch of assets (e.g. sound files, 3D model files) which are necessary to run
many of the Extempore examples. These asset files live in a [separate
repo](https://github.com/extemporelang/extempore-assets).

This option is off by default because it's a pretty big (~300MB) download. If
you don't set `-DASSETS=ON` at build time that's ok---CMake will still create an
`assets` target which you can "build" afterwards to downoad the assets and move
them into place.

## Targets

The default target will build Extempore, all the dependencies, and AOT-compile
the standard library (for faster startup). However, in other situations the
following targets might come in handy:

- on macOS and Linux, the `install` target will move the extempore executable to
  `/usr/local/bin` (or similar) and the rest of the Extempore share directory to
  `/usr/local/share/extempore` (does nothing on Windows)

- the `aot` target will ahead-of-time compile just the core standard library,
  i.e. the pure-xtlang libraries with no external C library dependencies

- the `clean_aot` target will remove all AOT-compiled files

## Platform-specific notes

### macOS

#### macOS 10.14 Mojave

On macOS 10.14 Apple removed some of the system OpenGL headers,
which means that some of Extempore's dependencies (e.g. nanovg) won't build.
[This blog
post](https://silvae86.github.io/sysadmin/mac/osx/mojave/beta/libxml2/2018/07/05/fixing-missing-headers-for-homebrew-in-mac-osx-mojave/)
has more details, but if you just want a one-liner to fix it, it's

    sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /

#### macOS 10.15 Catalinz

Since macOS 10.15 Apple requires all binaries to be signed & notarized, and the
Extempore core team (Andy & Ben) haven't yet got an Apple Developer account set
up to do that (it's on the to-do list). So if you have problems with the macOS
Gatekeeper saying that it doesn't trust the `extempore` binary then reach out on
the [mailing list](mailto:extemporelang@googlegroups.com)---there's a workaround
which isn't ideal (disabling the "is this binary legit?" check) but we can keep
you up-to-date on the best way to deal with the issue.

### Linux

Extempore is built & tested on Ubuntu, but is also known to work with Debian,
Fedora, Arch, NixOS, and inside a docker container.

On Linux, you'll need to specify an [ALSA](http://www.alsa-project.org/) backend
for portaudio. To use the `asound` portaudio backend (the default) you'll need
the `libasound` package. Extempore also includes a legacy
[Jack](http://www.jackaudio.org/) portaudio backend, but it has bitrotted in
recent years. If you want to use Jack and you're happy to do some spelunking,
set the `JACK` CMake option with `-DJACK=ON`.

#### Ubuntu

On Ubuntu 18.04-20.04 you can get the required deps with:

    sudo apt-get install libasound2-dev xorg-dev libglu1-mesa-dev

#### Arch

There's an [AUR package](https://aur.archlinux.org/packages/extempore-git/)

### Windows

Extempore is built & tested on Windows 10 with Visual Studio 2017 and Visual
Studio 2019. If you don't already have VS installed, you can download the free
[Visual Studio
Community](https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx)---that's
perfectly fine for building Extempore (although the paid versions of VS will
work as well).

In the CMake build process, you'll need to specify which version you're
using---confusingly the Visual Studio version numbers and years don't _quite_ line up, so
the respective generators are:

- VS2017: "-G "Visual Studio 15 2017" -A x64"
- VS2019: "-G "Visual Studio 16 2019" -A x64"

#### Missing `VCRUNTIME140_1.dll`

If you ever see the error message _VCRUNTIME140_1.dll was not found_, then
you'll need to download the x64 `vc_redist.x64.exe`. Make sure you get it from
the official [Windows
website](https://support.microsoft.com/en-au/help/2977003/the-latest-supported-visual-c-downloads),
because there are lots of sketchy places on the web which will try and get you
to download theirs instead.

#### ASIO

If you want to use the **ASIO** audio backend on Windows (which _might_ give you
lower-latency audio, or it might not) you need to download the [ASIO
SDK](http://www.steinberg.net/nc/en/company/developer/sdk_download_portal.html)
from Steinberg. You have to create a [third party developer
account](http://www.steinberg.net/nc/en/company/developer/sdk_download_portal/create_3rd_party_developer_account.html),
then you can log in and download the ASIO SDK (make sure you get the right SDK).
You also need to download and install [ASIO4ALL](http://www.asio4all.com/) with
the 'offline setup panel' option enabled. After that, copy the ASIO files into
the `src/portaudio/src/hostapi/asio`, and use the `-DASIO=ON` CMake option.
