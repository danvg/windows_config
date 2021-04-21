#!/usr/bin/bash

pacman -S --needed gperf unzip wget mingw-w64-x86_64-{gcc,libtool,cmake,make,perl,python2,ninja,diffutils}

SOURCE_URL="https://github.com/neovim/neovim/archive/nightly.zip"
INSTALL_DIR="/c/Users/danle/neovim"
GIT_EXECUTABLE="/c/Users/danle/scoop/apps/git-with-openssh/current/bin/git.exe"
GPERF_EXECUTABLE="/d/Tools/msys64/usr/bin/gperf.exe"

if [[ -f nightly.zip ]]; then
  mv 'nightly.zip' 'nightly.zip.old'
fi

if [[ -d neovim-nightly ]]; then
  mv 'neovim-nightly' 'neovim-nightly.old'
fi

wget "${SOURCE_URL}"

if [[ ! -f nightly.zip ]]; then
  echo 'Failed to download neovim-nightly'
  exit 1
fi

unzip nightly.zip

if [[ ! -d neovim-nightly ]]; then
  echo 'Unexpected unzipped file/folder'
  exit 1
fi

cd neovim-nightly
mkdir .deps
mkdir build

cmake -D CMAKE_BUILD_TYPE=Release -G "MinGW Makefiles" -D GIT_EXECUTABLE="${GIT_EXECUTABLE}" -B .deps -S third-party
cmake --build .deps

cmake -D CMAKE_BUILD_TYPE=Release -G "MinGW Makefiles" -D CMAKE_INSTALL_PREFIX="${INSTALL_DIR}" -D GPERF_PRG="${GPERF_EXECUTABLE}" -B build
cmake --build build

if [[ -d "${INSTALL_DIR}" ]]; then
  echo "Moving existing ${INSTALL_DIR} to ${INSTALL_DIR}.old"
  mv "${INSTALL_DIR}" "${INSTALL_DIR}.old"
fi

cmake --install build
