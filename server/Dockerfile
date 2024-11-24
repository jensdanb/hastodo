
# FROM haskell:9.8.2-slim-buster

# WORKDIR /app

## COPY ../hastodo.cabal .
# RUN cabal install

FROM ubuntu:latest


WORKDIR /app

COPY ./hastodo /hastodo

EXPOSE 8000
CMD ["/hastodo"]
