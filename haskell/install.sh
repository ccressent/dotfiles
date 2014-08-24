#!/bin/sh

# Try to automatically install the Haskell related tools I regularly use or
# that are needed by vim plugins.
# Inspired by https://github.com/begriffs/haskell-vim-now

cabal_dir="$HOME/.cabal"


install_package () {
    pkg=$1

    if command -v $pkg >/dev/null 2>&1; then
        echo "$pkg is already installed, skipping installation"
        return
    else
        dir=`mktemp -d /tmp/build-XXXXX`

        echo "Building $pkg in $dir"
        cd $dir
        cabal sandbox init
        cabal install -j --disable-documentation  \
                         --datadir=$cabal_dir/share \
                         --force-reinstalls       \
                         $pkg

        echo "Moving $pkg binaries to $cabal_dir/bin"
        cp .cabal-sandbox/bin/* $cabal_dir/bin

        echo "Cleaning up..."
        cd -
        rm -fr $dir
    fi
}


cabal update

install_package "ghc-mod"
install_package "hoogle"

echo "Building Hoogle database..."
hoogle data
