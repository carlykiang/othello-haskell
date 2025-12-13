import subprocess
import re

# Benchmarking script for comparing parallelizing minimax (with alpha-beta pruning) vs sequential minimax (with alpha-beta pruning) in Othello
# Result: https://docs.google.com/spreadsheets/d/1KGVDMUBALCCqr3oS2dOiXjDoIqsVjbt3bKWB6a7yBC8/edit?gid=952670711#gid=952670711

NUM_RUNS = 100
p2_wins = 0
elapsed_times = []

for _ in range(NUM_RUNS):
    result = subprocess.run(
        # Compile parallelized minimax othello using: stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello othello-minimax-par.hs
        # ["./othello", "+RTS", "-N1", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N2", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N3", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N4", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N5", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N6", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N7", "-s"], capture_output=True, text=True
        # ["./othello", "+RTS", "-N8", "-s"], capture_output=True, text=True
        # Compile sequential minimax othello with alpha-beta pruning using: stack ghc  --package random  -- -Wall -O2 -o othello othello-minimax-seq.hs
        ["./othello", "+RTS", "-s"], capture_output=True, text=True
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

