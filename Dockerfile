FROM haskell

WORKDIR /go/src/github.com/icedmocha/facebook-client
COPY . /go/src/github.com/icedmocha/facebook-client

RUN stack setup && stack build && stack exec facebook-client
