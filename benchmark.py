import subprocess
import re

# Benchmarking script for comparing parallelized minimax (without alpha-beta pruning) vs sequential minimax (with alpha-beta pruning) vs parallelized minimax (with alpha-beta pruning) in Othello
# Result: https://docs.google.com/spreadsheets/d/1KGVDMUBALCCqr3oS2dOiXjDoIqsVjbt3bKWB6a7yBC8/edit?gid=952670711#gid=952670711

# parallelized minimax (without alpha-beta pruning)
# Compile parallelized minimax othello using: stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello othello-minimax-par.hs

# sequential minimax (with alpha-beta pruning)
# Compile sequential minimax othello with alpha-beta pruning using: stack ghc  --package random  -- -Wall -O2 -o othello othello-minimax-seq.hs
# ["./othello", "+RTS", "-s"], capture_output=True, text=True

# parallelized minimax (with alpha-beta pruning)
# Compile parallelized minimax othello with alpha-beta pruning using: stack ghc  --package random  -- -Wall -O2 -threaded -rtsopts -o othello-a-b othello-minimax-par.hs

NUM_RUNS = 100
THREADS = 8
p2_wins = 0
elapsed_times = []
total_sparks = []
converted_sparks = []
dud_sparks = []
overflowed_sparks = []
gc_sparks = []
fizzled_sparks = []

minimax_commands = [
    ["./othello-parbuffer", "+RTS", "-N1", "-s"],
    ["./othello-parbuffer", "+RTS", "-N2", "-s"],
    ["./othello-parbuffer", "+RTS", "-N3", "-s"],
    ["./othello-parbuffer", "+RTS", "-N4", "-s"],
    ["./othello-parbuffer", "+RTS", "-N5", "-s"],
    ["./othello-parbuffer", "+RTS", "-N6", "-s"],
    ["./othello-parbuffer", "+RTS", "-N7", "-s"],
    ["./othello-parbuffer", "+RTS", "-N8", "-s"]
]

mcts_commands = [
    ["./othello-mcts-par", "+RTS", "-N1", "-s"],
    ["./othello-mcts", "+RTS", "-N2", "-s"],
    ["./othello-mcts", "+RTS", "-N3", "-s"],
    ["./othello-mcts", "+RTS", "-N4", "-s"],
    ["./othello-mcts", "+RTS", "-N5", "-s"],
    ["./othello-mcts", "+RTS", "-N6", "-s"],
    ["./othello-mcts", "+RTS", "-N7", "-s"],
    ["./othello-mcts", "+RTS", "-N8", "-s"]
]

for i in range(THREADS):
    p2_wins = 0
    elapsed_times = []
    total_sparks = []
    converted_sparks = []
    dud_sparks = []
    overflowed_sparks = []
    gc_sparks = []
    fizzled_sparks = []
    for _ in range(NUM_RUNS):
    
        result = subprocess.run(
            minimax_commands[i], capture_output=True, text=True
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

        # Extract spark data using regex
        spark_pattern = pattern = (
            r"SPARKS:\s*(?P<sparks>\d+)\s*"
            r"\((?P<converted>\d+)\s+converted,\s*"
            r"(?P<overflowed>\d+)\s+overflowed,\s*"
            r"(?P<dud>\d+)\s+dud,\s*"
            r"(?P<gc>\d+)\s+GC'd,\s*"
            r"(?P<fizzled>\d+)\s+fizzled\)"
        )
        spark_match = re.search(spark_pattern, err_output)
        if spark_match:
            spark_dict = spark_match.groupdict()
            total_sparks.append(int(spark_dict["sparks"]))
            converted_sparks.append(int(spark_dict["converted"]))
            dud_sparks.append(int(spark_dict["dud"]))
            overflowed_sparks.append(int(spark_dict["overflowed"]))
            gc_sparks.append(int(spark_dict["gc"]))
            fizzled_sparks.append(int(spark_dict["fizzled"]))
        else:
            print("ERROR: SPARKS data not found in stderr.")

    print("--------------------------------------------------------------------")
    print(f"Number of threads: {8}")
    percentage = (p2_wins / NUM_RUNS) * 100
    print(f"Player 2 win percentage: {percentage:.2f}%")
    average_time = sum(elapsed_times) / len(elapsed_times) if elapsed_times else 0
    print(f"Average elapsed time: {average_time} seconds")
    average_total_sparks = sum(total_sparks) / len(total_sparks) if total_sparks else 0
    print(f"Average total sparks: {average_total_sparks} sparks")
    average_converted_sparks = sum(converted_sparks) / len(converted_sparks) if converted_sparks else 0
    print(f"Average converted sparks: {average_converted_sparks} sparks")
    spark_percentage = (average_converted_sparks / average_total_sparks)*100
    print(f"Average percentage sparks converted: {spark_percentage:.2f}%")

