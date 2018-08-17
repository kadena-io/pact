#!/bin/sh
set -e

version="$1"

if [ -z "$version" ]; then echo "Missing version"; exit 1; fi

build="$2"

if [ -z "$build" ]; then echo "Missing build"; exit 1; fi

macbuild="$3"

home="$HOME/Sites/kadena.io/pact/builds"
vdir="$home/$version"
ldir="$vdir/ubuntu-1204"
mdir="$vdir/osx"

linuxid="2"
osxid="1"


if [ -d "$vdir" ]; then rm -r $vdir; fi
mkdir -p $ldir
mkdir -p $mdir

s3url="s3://kadena-builds/kadena-io/pact/$build/$build"
lurl="$s3url.$linuxid/.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/pact/pact"
murl="$s3url.$osxid/.stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/pact/pact"

# linux

cd $ldir

aws s3 cp $lurl .

md5 pact > pact.md5

zip pact-$version-linux.zip pact pact.md5

rm pact pact.md5

# osx

cd $mdir

if [ -z "$macbuild" ]; then
    aws s3 cp $murl .
else
    cp $macbuild .
fi
md5 pact > pact.md5

zip pact-$version-osx.zip pact pact.md5

tar czvf pact-$version.tar.gz pact

rm pact pact.md5

find $vdir

sha=`shasum -a 256 pact-$version.tar.gz | cut -d ' ' -f 1`
url="http://kadena.io/pact/builds/$version/osx/pact-$version.tar.gz"
# echo "url \"$url\""
# echo "sha256 \"$sha\""

brewfile="/Users/stuart/dev/homebrew-pact/pact.rb"
perl -p -i -e "s|url .*|url \"$url\"|" $brewfile
perl -p -i -e "s|sha256 .*|sha256 \"$sha\"|" $brewfile
echo "Updated $brewfile"

tmpfile=/tmp/pact-$version.html
echo "
          <h3>Pact $version</h3>
          <div class=\"dl-table-div\">
            <table id=\"dl-table\"><thead><tr><td>OS</td><td>Pact Version</td><td>Binary</td></tr></thead>
              <tbody>
                <tr>
                  <td>OS X (Yosemite, El Capitan, Sierra)</td>
                  <td>$version</td>
                  <td><a class=\"dl-link\" href=\"builds/$version/osx/pact-$version-osx.zip\">Download</a></td>
                </tr>
                <tr>
                  <td>Ubuntu 12.04/Linux 3.13.0-29</td>
                  <td>$version</td>
                  <td><a class=\"dl-link\" href=\"builds/$version/ubuntu-1204/pact-$version-linux.zip\">Download</a></td>
                </tr>
              </tbody>
            </table>
          </div>

" > $tmpfile

builds="$HOME/Sites/kadena.io/pact/downloads.html"
sed -i '' -e "/<\!-- BUILDS -->/r $tmpfile" $builds
echo "Updated $builds"
