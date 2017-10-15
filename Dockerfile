FROM samdoshi/haskell-stack

WORKDIR /go/src/github.com/icedmocha/facebook-client
COPY . /go/src/github.com/icedmocha/facebook-client

RUN stack build --install-ghc

CMD stack exec facebook-client
