FROM haskell:latest
ENV motoEnv="dev"
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
RUN stack update
COPY stack.yaml /usr/src/app/
COPY package.yaml /usr/src/app
COPY motorola-challenge.cabal /usr/src/app/
COPY README.md /usr/src/app/
COPY LICENSE /usr/src/app/
COPY ChangeLog.md /usr/src/app


# whatever user code
COPY src /usr/src/app/src
COPY app /usr/src/app/app


RUN stack setup
# This command installs the  dependencies and caches them
# unless cabal file changes
RUN stack build --only-dependencies -j4

# install the user code
RUN stack build