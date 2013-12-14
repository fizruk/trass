#!/bin/sh

course=$1

cd `dirname $0`

root=`pwd`

# main directories
repos_bare="$root/bare"
repos_contents="$root/contents"
repos_submits="$root/submits"

# create a new bare repository
mkdir -p "$repos_bare/$course"
cd "$repos_bare/$course"
git init --bare

# create a primitive README.md
# so one can immediately clone repo
mkdir -p "$repos_contents"
cd "$repos_contents"
git clone "$repos_bare/$course"
cd "$course"
echo "$course\n===\n\nHere be README.\n" > README.md
git add .
git commit -m "added README.md"
git push -u origin master

# TODO: submits repository
