FROM 40ants/base-lisp-image:0.17.0-sbcl-bin

EXPOSE 80
EXPOSE 4005

# These a dev dependencies to simplify log reading and support
# file search from remote Emacs.
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    set -x; \
    apt-get update && \
    apt-get install -y \
	    git \
	    gcc \
            postgresql-client && \
    mkdir -p /tmp/s6 && cd /tmp/s6 && \
    git clone https://github.com/skarnet/skalibs && cd skalibs && \
    git checkout v2.10.0.2 && \
    ./configure && make install && cd /tmp/s6 && \
    git clone https://github.com/skarnet/execline && cd execline && \
    git checkout v2.8.0.0 && \
    ./configure && make install && cd /tmp/s6 && \
    git clone https://github.com/skarnet/s6 && cd s6 && \
    git checkout v2.10.0.2 && \
    ./configure --with-lib=/usr/lib/execline && make install && \
    cd / && rm -fr /tmp/s6 && \
    apt-get remove -y --auto-remove git gcc

ENV CC=gcc
COPY qlfile qlfile.lock app-deps.asd /app/

# Here libyaml is specific to the project
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    set -x; \
    apt-get update && \
    apt-get install -y \
	    gcc \
            libyaml-0-2 && \
    install-dependencies && \
    apt-get remove -y --auto-remove gcc
    

COPY . /app

RUN qlot exec ros build \
    /app/app.ros

COPY ./docker/s6-app /etc/s6
ENTRYPOINT ["s6-svscan", "/etc/s6"]

