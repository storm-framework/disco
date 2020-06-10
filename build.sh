#!/usr/bin/env bash

COMPRESS=false
while getopts i:c option
do
case "${option}"
in
i) DOCKERIMAGE=${OPTARG};;
c) COMPRESS=true;;
esac
done

mkdir -p dist

cd client
yarn build --dest ../dist/static
cd ..

if [ -z $DOCKERIMAGE ]
then
  cd server
  make build
  stack install --local-bin-path ../dist --
  cp -r templates ../dist
else
  echo "Building inside docker: $DOCKERIMAGE"
  docker run -w /workspace -v `pwd`/dist:/dist -v `pwd`/server:/workspace -v ~/.stack:/root/.stack $DOCKERIMAGE stack install --local-bin-path /dist --work-dir .stack-work-docker --allow-different-user
  docker run -v `pwd`/dist:/dist $DOCKERIMAGE chown $(id -u $USER):$(id -g $USER) /dist/disco
fi

if [ $COMPRESS == true ] ; then
    tar cf disco.tar.gz dist
fi
