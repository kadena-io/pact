#!/bin/sh

#
#
# release.sh: Pact release deploy script
#
# Downloads artifacts from github CI run.
# Prepares zip files and tarballs for upload to Github release assets.
# Updates homebrew-pact formula.
#
# Instructions:
# 1. Push git tag for release, and locate "Summary" Github actions run ID.
# Example: if summary link is "https://github.com/kadena-io/pact/actions/runs/1659693228",
# run ID is 1659693228.
#
# 2. Execute with version, run ID, pact-homebrew dir, and github token:
# Example: `./release.sh -v 3.0.1 -r 1659693228 -b ../homebrew-pact -g [your-github-token]`
# Github token must have "public_repo" access.
#
# 3. Upload zips and tarball to Github release.
#
# 4. Commit/push changes to homebrew-pact.
#
# Prerequisites:
# - https://github.com/kadena-io/homebrew-pact checked out, master branch
# - perl
# - shasum
# - git
# - zip
# - tar, gzip
# - jq
#
#

set -e

usage='release.sh [-h] -v VERSION -r RUN_ID -g GITHUB_TOKEN -b BREW_DIR
  VERSION: version tag to release, e.g. "4.0.1"
  RUN_ID: Github action run ID
  GITHUB_TOKEN: Github token with public_repo access
  BREW_DIR: dir containing git@github.com:kadena-io/homebrew-pact.git'

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
        -r)
            runid="$2"
            shift
            ;;
        -g)
            ghtoken="$2"
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

if [ -z "$runid" ]; then echo "Missing Github action run ID"; echo "$usage"; exit 1; fi

if [ -z "$ghtoken" ]; then echo "Missing Github auth token"; echo "$usage"; exit 1; fi

if [ -z "$version" ]; then echo "Missing version"; echo "$usage"; exit 1; fi

if [ ! -d "$brewdir" ]; then echo "Missing/invalid homebrew-pact dir"; echo $usage; exit 1; fi

cd $brewdir
brewdir="$PWD"
brewfile="pact.rb"
if [ ! -f "$brewfile" ]; then
    echo "Brew file not found: $brewdir/$brewfile"
    exit 1
fi
echo "Updating homebrew-pact"
git pull

home="/tmp/pact-builds"
vdir="$home/$version"

if [ -d "$vdir" ]; then rm -r $vdir; fi
mkdir -p $vdir
cd $vdir

# Get linux 20.04 artifact ==========================

echo "Looking up linux 20.04 artifact ..."
dlurl=`curl -s -H "Authorization: token $ghtoken" -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/kadena-io/pact/actions/runs/$runid/artifacts | jq -r '.artifacts[] | select(.name | contains ("9.6.ubuntu-20.04")) | .archive_download_url'`

if [ -z "$dlurl" ]; then echo "Linux artifact lookup failed!"; exit 1; fi

echo "Downloading artifact from $dlurl ..."

curl -s -L -H "Authorization: token $ghtoken" -o pact-$version-linux-20.04.zip $dlurl

# Get linux 22.04 artifact ===========================

echo "Looking up linux 22.04 artifact ..."

dlurl=`curl -s -H "Authorization: token $ghtoken" -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/kadena-io/pact/actions/runs/$runid/artifacts | jq -r '.artifacts[] | select(.name | contains ("9.6.ubuntu-22.04")) | .archive_download_url'`

if [ -z "$dlurl" ]; then echo "Linux artifact lookup failed!"; exit 1; fi

echo "Downloading artifact from $dlurl ..."

curl -s -L -H "Authorization: token $ghtoken" -o pact-$version-linux-22.04.zip $dlurl

# Get osx artifact ===========================

echo "Looking up OSX artifact ..."

dlurl=`curl -s -H "Authorization: token $ghtoken" -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/kadena-io/pact/actions/runs/$runid/artifacts | jq -r '.artifacts[] | select(.name | contains ("9.6.macOS-latest")) | .archive_download_url'`

if [ -z "$dlurl" ]; then echo "OSX artifact lookup failed!"; exit 1; fi

echo "Downloading artifact from $dlurl ..."

curl -s -L -H "Authorization: token $ghtoken" -o pact-$version-osx.zip $dlurl


# Prep osx tarball ===========================

mkdir "osx"
cd osx
unzip ../pact-$version-osx.zip

brewtgz="pact-$version-osx.tar.gz"

tar czvf ../$brewtgz pact

cd ..
rm -r osx

echo "Updating $brewdir/$brewfile"

sha=`shasum -a 256 $brewtgz | cut -d ' ' -f 1`
url="https://github.com/kadena-io/pact/releases/download/v$version/$brewtgz"

cd $brewdir

perl -p -i -e "s|url .*|url \"$url\"|" $brewfile
perl -p -i -e "s|sha256 .*|sha256 \"$sha\"|" $brewfile

find $vdir -type f

echo "Builds ready in $vdir"
