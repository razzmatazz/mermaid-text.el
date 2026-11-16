# EMACS is the GNU release to build, e.g. 29.1, 30.2 or 31.1.  Ubuntu 26.04
# packages only Emacs 30, so every version is built the same way.
ARG EMACS=31.1

FROM ubuntu:26.04 AS build

ARG EMACS

# GnuTLS for fetching the linters; mermaid-text itself needs no library.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      build-essential pkg-config curl ca-certificates xz-utils \
      libgnutls28-dev libncurses-dev && \
    rm -rf /var/lib/apt/lists/*

# GCC 15 defaults to C23, which Emacs before 30 does not build with.
RUN curl -fsSL --retry 5 https://ftp.gnu.org/gnu/emacs/emacs-${EMACS}.tar.xz | tar xJ -C /tmp && \
    cd /tmp/emacs-${EMACS} && \
    ./configure --prefix=/opt/emacs CFLAGS="-O2 -std=gnu17" \
                --without-x --without-native-compilation \
                --without-sound --without-dbus --without-selinux && \
    make -j"$(nproc)" && \
    make install

FROM ubuntu:26.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      curl git ca-certificates libgnutls30t64 libncurses6 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/emacs /opt/emacs
ENV PATH="/opt/emacs/bin:$PATH"

# Eldev fetches itself on its first run, so that is done here, not on
# every `docker run'.  Not in root's home: the checks run as the host's
# user, see the Makefile.  A run's writes stay in its container.
ENV ELDEV_DIR=/opt/eldev
RUN curl -fsSL https://raw.githubusercontent.com/emacs-eldev/eldev/master/webinstall/eldev | sh && \
    mv /root/.local/bin/eldev /usr/local/bin/ && \
    eldev --version && \
    chmod -R a+rwX /opt/eldev
