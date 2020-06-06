#!/bin/sh

#
#
# release.sh: Pact release deploy script
#
# Downloads travis builds from s3
# Prepares zips and tarballs for linux and osx
# Updates homebrew-pact formula
#
# Instructions:
# 1. See prerequisites below.
#
# 2. Execute with version, travis build, pact-homebrew:
# Example: `./release.sh 3.0.1 3301 ../homebrew-pact`
# Version arg $VERSION must also be a tag "v$VERSION" in pact repo.
#
# 3. Create git release for tag and upload artifacts to Git.
#
# 4. Commit/push changes to homebrew-pact.
#
# Prerequisites:
# - aws client installed
# - https://github.com/kadena-io/homebrew-pact checked out, master branch
# - perl
# - shasum
# - md5
# - git
# - zip, gzip
# - tar
#
# Works on osx, not tested on linux
#
#

set -e

version="$1"

if [ -z "$version" ]; then echo "Missing version"; exit 1; fi

build="$2"

if [ -z "$build" ]; then echo "Missing travis build number"; exit 1; fi

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

s3url="s3://pact-builds/kadena-io/pact/$build/$build"
lurl="$s3url.$linuxid/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/pact/pact"
murl="$s3url.$osxid/.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/pact/pact"

# linux

echo "Downloading linux s3 artifact"

cd $ldir

aws s3 cp $lurl .

md5 pact > pact.md5

zip ../pact-$version-linux.zip pact pact.md5

rm pact pact.md5

# osx

echo "Downloading osx s3 artifact"

cd $mdir
rm -r $ldir

if [ -z "$macbuild" ]; then
    aws s3 cp $murl .
else
    cp $macbuild .
fi
md5 pact > pact.md5

zip ../pact-$version-osx.zip pact pact.md5

brewtgz="pact-$version-osx.tar.gz"

tar czvf ../$brewtgz pact

rm pact pact.md5

echo "Updating $brewdir/$brewfile"

sha=`shasum -a 256 ../$brewtgz | cut -d ' ' -f 1`
url="https://github.com/kadena-io/pact/releases/download/v$version/$brewtgz"

cd $brewdir
rm -r $mdir

perl -p -i -e "s|url .*|url \"$url\"|" $brewfile
perl -p -i -e "s|sha256 .*|sha256 \"$sha\"|" $brewfile

find $vdir -type f

echo "Builds ready in $vdir"
