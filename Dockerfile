FROM haskell:8.6.5

WORKDIR /opt/disco

# COPY ./client/dist /opt/disco/static
COPY ./server /opt/disco

RUN stack build

# # Docker will cache this command as a layer, freeing us up to
# # modify source code without re-installing dependencies
# # (unless the .cabal file changes!)
# RUN cabal install --only-dependencies -j4

# # Add and Install Application Code
# COPY . /opt/example
# RUN cabal install

CMD ["example"]
