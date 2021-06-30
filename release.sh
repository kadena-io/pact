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

usage='release.sh [-h] -v VERSION -b BREW_DIR [-t TRAVIS_BUILD] [-l LINUX_BUILD -m MAC_BUILD]
  VERSION: version to release, e.g. "4.0.1"
  BREW_DIR: dir containing git@github.com:kadena-io/homebrew-pact.git
  TRAVIS_BUILD: travis build number. If not supplying this, must supply -l and -m
  LINUX_BUILD: path to linux binary
  MAC_BUILD: path to mac binary'

while [ -n "$1" ]; do
    case "$1" in
        -v)
            version="$2"
            shift
            ;;
        -b)
            brewdir="$2"
            shift
            ;;
        -t)
            build="$2"
            shift
            ;;
        -l)
            linux_build_path="$2"
            shift
            ;;
        -m)
            mac_build_path="$2"
            shift
            ;;
        -h)
            echo "$usage"
            exit 0
            ;;
        *)
            echo "Unrecognized argument"
            echo "$usage"
            exit 1
            ;;
    esac
    shift
done

if [ -z "$version" ]; then echo "Missing version"; echo "$usage"; exit 1; fi

if [ ! -d "$brewdir" ]; then echo "Missing homebrew-pact dir"; echo $usage; exit 1; fi

if [ -z "$build" ]; then
    if [ ! -f "$linux_build_path" -o ! -f "$mac_build_path" ]; then
        echo "Missing travis, or valid mac+linux build path"
        echo "$usage"
        exit 1
    fi
fi



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


cd $ldir

if [ -z "$build" ]; then
    cp $linux_build_path .
else
    echo "Downloading linux s3 artifact"
    aws s3 cp $lurl .
fi

md5 pact > pact.md5

zip ../pact-$version-linux.zip pact pact.md5

rm pact pact.md5

# osx


cd $mdir
rm -r $ldir

if [ -z "$build" ]; then
    cp $mac_build_path .
else
    echo "Downloading osx s3 artifact"
    aws s3 cp $murl .
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
