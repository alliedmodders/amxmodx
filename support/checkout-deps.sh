#!/usr/bin/env bash
trap "exit" INT

if [ ! -d "amxmodx" ]; then
  git clone --recursive https://github.com/alliedmodders/amxmodx.git
fi

if [ ! -d "amxmodx/build_deps" ]; then
  mkdir amxmodx/build_deps
fi

download_archive ()
{
  if [ `command -v wget` ]; then
    wget "$url" -O "$dest"
  elif [ `command -v curl` ]; then
    curl -o $dest $url
  else
    echo "Failed to locate wget or curl. Please install one of these programs."
    exit 1
  fi
}

if [ "$1" != "--no-mysql" ]; then
  ismac=0
  iswin=0

  archive_ext=tar.gz
  decomp="tar zxf"

  if [ `uname` = "Darwin" ]; then
    ismac=1
  elif [ `uname` != "Linux" ] && [ -n "${COMSPEC:+1}" ]; then
    iswin=1
    archive_ext=zip
    decomp=unzip
  fi

  if [ $ismac -eq 1 ]; then
    mysqlver=mysql-5.5.40-osx10.6-x86
    mysqlurl=http://cdn.mysql.com/archives/mysql-5.5/$mysqlver.$archive_ext
  elif [ $iswin -eq 1 ]; then
    mysqlver=mysql-5.5.57-win32
    mysqlurl=http://cdn.mysql.com/archives/mysql-5.5/$mysqlver.$archive_ext
    # The folder in the zip archive does not contain the substring "-noinstall", so strip it
    mysqlver=${mysqlver/-noinstall}
  else
    mysqlver=mysql-5.5.57-linux-glibc2.12-i686
    mysqlurl=http://cdn.mysql.com/archives/mysql-5.5/$mysqlver.$archive_ext
  fi

  if [ ! -d "mysql-5.5" ]; then
    url=$mysqlurl
    dest=mysql.$archive_ext
    download_archive
    $decomp mysql.$archive_ext
    mv $mysqlver mysql-5.5
    rm mysql.$archive_ext
  fi
fi

checkout ()
{
  if [ ! -d "$name" ]; then
    git clone $repo -b $branch $name
    if [ -n "$origin" ]; then
      cd $name
      git remote rm origin
      git remote add origin $origin
      cd ..
    fi
  else
    cd $name
    git checkout $branch
    git pull origin $branch
    cd ..
  fi
}

name=metamod-am
branch=master
repo="https://github.com/alliedmodders/metamod-hl1"
origin=
checkout

name=hlsdk
branch=master
repo="https://github.com/alliedmodders/hlsdk"
origin=
checkout

`python -c "import ambuild2"`
if [ $? -eq 1 ]; then
  repo="https://github.com/alliedmodders/ambuild"
  origin=
  branch=master
  name=ambuild
  checkout

  cd ambuild
  if [ $iswin -eq 1 ]; then
    python setup.py install
  else
    python setup.py build
    echo "Installing AMBuild at the user level. Location can be: ~/.local/bin"
    python setup.py install --user
  fi
fi

if [ $iswin -eq 1 ]; then
  if [ ! -d "amxmodx/build_deps/nasm-2.13.03" ]; then
    url=http://www.nasm.us/pub/nasm/releasebuilds/2.13.03/win32/nasm-2.13.03-win32.zip
    dest=amxmodx/build_deps/nasm-2.13.03-win32.zip
    download_archive
    cd amxmodx/build_deps
    unzip nasm-2.13.03-win32.zip
    rm nasm-2.13.03-win32.zip
    mv nasm-2.13.03 nasm
  fi
fi
