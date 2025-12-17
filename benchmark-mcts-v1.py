import subprocess
import re
import sys
# Benchmarking script for MCTS Version 1
# Note: MCTS Version 1 will require edits made to the source file, othello-mcts-par-v1.hs
# Check othello-mcts-par-v1.hs for more details

NUM_RUNS = 1
p2_wins = 0
elapsed_times = []
total_sparks = []
converted_sparks = []
dud_sparks = []
overflowed_sparks = []
gc_sparks = []
fizzled_sparks = []


if len(sys.argv) != 3:
    print("Please provide the name of the executable you would like to benchmark and the threadcount")
    print("python3 benchmark-mcts-v1.py ./mcts/othello-mcts-par-v1 2")
    sys.exit()

executable = sys.argv[1]
thread_count = sys.argv[2]


command = [executable, "+RTS", "-N" + thread_count, "-s"]

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
        command, capture_output=True, text=True
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
print(f"Number of threads: {thread_count}")
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

