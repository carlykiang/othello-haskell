#!/bin/bash
stack install random

# Compile sequential minimax (without alpha-beta prun ing) into othello
stack ghc  --package random  -- -Wall -O2 -o othello othello-minimax-seq.hs
# Compile parallel minimax (parlist) into othello-minimax-parlist
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-minimax-parlist othello-minimax-parlist.hs
# Compile parallel minimax (parbuffer) into othello-minimax-parbuffer
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-minimax-parbuffer othello-minimax-parbuffer.hs
# Compile parallel minimax (alpha-beta pruning with parlist) into othello-minimax-ab-parlist
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-minimax-ab-parlist othello-minimax-ab-parlist.hs
# Compile parallel minimax (alpha-beta pruning with parbuffer) into othello-minimax-ab-parbuffer
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-minimax-ab-parbuffer othello-minimax-ab-parbuffer.hs