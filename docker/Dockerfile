FROM debian:12-slim

# set work directory
WORKDIR /usr/src/app

# Print output
RUN export TERM=xterm

# install software
RUN apt update
RUN apt install -y emacs-nox

# Build
COPY build.el .
ENTRYPOINT emacs --batch --script build.el
