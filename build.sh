#!/usr/bin/env bash

export PROD = 1

rm -rf dist
mkdir dist
cd client 
yarn build --dest ../dist/static

cd ../server
make build
stack install --local-bin-path ../dist
cp -r templates ../dist

