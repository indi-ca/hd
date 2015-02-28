#!/bin/bash

cabal install c2hs-0.20.1
cabal install --only-dependencies
cabal build
