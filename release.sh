#!/bin/sh

set -e

version="$1"

if [ -z "$version" ]; then echo "Missing version"; exit 1; fi

build="$2"

if [ -z "$build" ]; then echo "Missing build"; exit 1; fi

brewdir="$3"

if [ ! -d "$brewdir" ]; then echo "Missing homebrew-pact dir"; exit 1; fi

home="/tmp/pact-builds"
vdir="$home/$version"
ldir="$vdir/linux"
mdir="$vdir/osx"

linuxid="2"
osxid="1"

cd $brewdir
brewdir="$PWD"
brewfile="pact.rb"
if [ ! -f "$brewfile" ]; then
    echo "Brew file not found: $brewdir/$brewfile"
    exit 1
fi
echo "Updating homebrew-pact"
git pull


if [ -d "$vdir" ]; then rm -r $vdir; fi
mkdir -p $ldir
mkdir -p $mdir

s3url="s3://kadena-builds/kadena-io/pact/$build/$build"
lurl="$s3url.$linuxid/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/pact/pact"
murl="$s3url.$osxid/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/pact/pact"

# linux

echo "Downloading linux s3 artifact"

cd $ldir

aws s3 cp $lurl .

md5 pact > pact.md5

zip pact-$version-linux.zip pact pact.md5

rm pact pact.md5

# osx

echo "Downloading osx s3 artifact"

cd $mdir

if [ -z "$macbuild" ]; then
    aws s3 cp $murl .
else
    cp $macbuild .
fi
md5 pact > pact.md5

zip pact-$version-osx.zip pact pact.md5

brewtgz="pact-$version-osx.tar.gz"

tar czvf $brewtgz pact

rm pact pact.md5

echo "Updating $brewdir/$brewfile"

sha=`shasum -a 256 $brewtgz | cut -d ' ' -f 1`
url="https://github.com/kadena-io/pact/releases/download/v$version/$brewtgz"

cd $brewdir

perl -p -i -e "s|url .*|url \"$url\"|" $brewfile
perl -p -i -e "s|sha256 .*|sha256 \"$sha\"|" $brewfile

find $vdir

echo "Builds ready in $vdir"
