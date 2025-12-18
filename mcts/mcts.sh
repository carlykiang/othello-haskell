#!/bin/bash
stack install random

# Compile sequential MCTS into othello-mcts-seq
stack ghc  --package random  -- -Wall -O2 -o othello-mcts-seq othello-mcts-seq.hs
# Compile parallel MCTS (v1) into othello-mcts-par-v1
# Note that you will need to edit the number of threads on line 409 before compiling if needed:
# let mctsRoot = runMCTSParallel rootNode 1000 gen 8 -- Change the last number to indicate number of threads we can parallelize on
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-mcts-par-v1 othello-mcts-par-v1.hs
# Compile parallel MCTS (v2) into othello-mcts-par-v2
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-mcts-par-v2 othello-mcts-par-v2.hs
# Compile parallel MCTS (v3) into othello-mcts-par-v3
stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-mcts-par-v3 othello-mcts-par-v3.hs