FROM gliderlabs/alpine:latest

# based on an original dockerfile by Sébastien Rannou
MAINTAINER Ben Swift <ben@benswift.me>

# get deps (listed in alphabetical order)
RUN apk add --no-cache git cmake build-base clang python alsa-lib-dev

RUN git clone --depth 1 git://github.com/digego/extempore && \
    mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install

# expose extempore primary & utility process ports
EXPOSE 7099 7098

ENTRYPOINT ["extempore"]

# Local Variables:
# compile-command: "docker build -t benswift/extempore ."
# End:
