#!/bin/sh

cd `dirname $0`

root=`pwd`

# main directories
repos_contents="$root/contents"

cd "$repos_contents"

for course in *
do
  cd "$course"
  git pull origin master
done
