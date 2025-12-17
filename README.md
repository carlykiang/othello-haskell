## Parallelizing Othello in Haskell
### Overview
We parallelized two different algorithms for Othello: minimax (with and without alpha-beta pruning) and Monte Carlo Tree Search (MCTS). 
Each parallel implementation is in its own .hs file in the corresponding folder, /minimax or /mcts.
There are 4 different parallel implementations for minimax and 3 different parallel implementations for MCTS, found in the following files. 
Each file will have to be compiled to create an executable to run our benchmarks

**Minimax**
1. othello-minimax-parlist.hs
2. othello-minimax-parbuffer.hs
3. othello-minimax-ab-parlist.hs
4. othello-minimax-ab-parbuffer.hs

**MCTS**
1. othello-mcts-par-v1.hs
2. othello-mcts-par-v3.hs
3. othello-mcts-par-v3.hs


### Benchmarking
To benchmark all implementations other than othello-mcts-par-v1.hs, you can use the attached benchmark.py script in the command line by providing the path to the executable, eg:

```
python3 benchmark.py ./minimax/othello-minimax-parbuffer
```

To benchmark the parallel implementation in othello-mcts-par-v1.hs, you will need to edit the number of threads on line 408
```haskell
let mctsRoot = runMCTSParallel rootNode 1000 gen 8 -- Change the last number to indicate number of threads we can parallelize on
```

Then, you can use the attached benchmark-mcts-v1.py script  in the command line by providing the path to the othello-mcts-par-v1.hs executable and the number of threads
For example, the following command benchmarks for 3 threads:

```
python3 benchmark-mcts-v1.py ./mcts/benchmark-mcts-par-v1 3
```

The raw results from our benchmarking can be found in ```minimax_benchmark_results.txt``` and ```mcts_benchmark_results.txt```
