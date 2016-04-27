FROM jkuhn/extempore_base
MAINTAINER Jim Kuhn <j.kuhn@computer.org>

RUN cd extempore/cmake-build && make -j $(($(nproc) + 1)) extended_deps
RUN cd extempore/cmake-build && make aot_extended

EXPOSE 7099 7098
CMD ["extempore/cmake-build/extempore"]
