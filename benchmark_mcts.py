import subprocess
import re

# Benchmarking script for Monte Carlo Tree Search (MCTS) Othello implementation

NUM_RUNS = 100
p2_wins = 0
elapsed_times = []

for _ in range(NUM_RUNS):
    result = subprocess.run(
        # Compile sequential Monte Carlo Tree Search (MCTS) othello using: stack ghc  --package random  -- -Wall -O2 -o othello-mcts-seq othello-mcts-seq.hs
        # ["./othello-mcts-seq", "+RTS", "-s"], capture_output=True, text=True

        # Follow instructions in othello-mcts-par.hs to compile parallel MCTS othello
        # ["./othello-mcts-par", "+RTS", "-N1", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N2", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N3", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N4", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N5", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N6", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N7", "-s"], capture_output=True, text=True
        # ["./othello-mcts-par", "+RTS", "-N8", "-s"], capture_output=True, text=True
    )

    # Keep track of player 2's winning rate
    output = result.stdout.strip()
    if "Winner is Player 2" in output:
        p2_wins += 1

    # Extract elapsed time using regex
    err_output = result.stderr.strip()
    match = re.search(r'Total\s+time\s+\S+\s+\(\s*([\d.]+)s elapsed\)', err_output)
    if match:
        elapsed_times.append(float(match.group(1)))
    else:
        print("ERROR: Elapsed time not found in stderr.")

percentage = (p2_wins / NUM_RUNS) * 100
print(f"Player 2 win percentage: {percentage:.2f}%")
average_time = sum(elapsed_times) / len(elapsed_times) if elapsed_times else 0
print(f"Average elapsed time: {average_time} seconds")
