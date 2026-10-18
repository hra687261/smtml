# Compares the times of `smtml smtzilla replay` with those of `smtml run` on
# the same queries (files `query.<id>.smt2` from `extract-queries`).
#
# usage: compare_replay.py REPLAY.csv z3=RUN_Z3.log bitwuzla=RUN_BWZ.log ...
# where each log is the output of `smtml run --print-statistics -s SOLVER`.

import re
import sys

import numpy as np
import pandas as pd

run_line = re.compile(r"Run .*query\.(\d+)\.smt2 in ([0-9.]+)")


def read_run(path):
    times = {}
    for line in open(path):
        m = run_line.search(line)
        if m:
            times[int(m.group(1))] = float(m.group(2)) * 1e9
    return pd.Series(times, name="run")


replay = pd.read_csv(sys.argv[1], usecols=["query_id", "solver", "status", "time"])
replay = replay.groupby(["query_id", "solver"]).median(numeric_only=True)["time"]

runs = {}
for arg in sys.argv[2:]:
    solver, path = arg.split("=", 1)
    run = read_run(path)
    both = pd.DataFrame(
        {"replay": replay.xs(solver, level="solver"), "run": run}
    ).dropna()
    runs[solver] = both
    ratio = both.run / both.replay
    print(f"== {solver}: {len(both)} queries")
    print(f"  total replay {both.replay.sum() / 1e9:8.1f}s  run {both.run.sum() / 1e9:8.1f}s")
    print(f"  run / replay per query: median {ratio.median():.2f}, "
          f"p10 {ratio.quantile(0.1):.2f}, p90 {ratio.quantile(0.9):.2f}")
    print(f"  Spearman correlation: {both.replay.corr(both.run, method='spearman'):.3f}")

if len(runs) == 2:
    (s1, t1), (s2, t2) = runs.items()
    ids = t1.index.intersection(t2.index)
    t1, t2 = t1.loc[ids], t2.loc[ids]
    replay_ratio = np.log10(t1.replay / t2.replay)
    run_ratio = np.log10(t1.run / t2.run)
    print(f"== {s1} vs {s2}: {len(ids)} queries")
    for name, a, b in [("replay", t1.replay, t2.replay), ("run", t1.run, t2.run)]:
        print(f"  {name:6}: total {s1} {a.sum() / 1e9:7.1f}s  {s2} {b.sum() / 1e9:7.1f}s  "
              f"{s1} faster on {(a < b).mean():.0%}")
    print(f"  same faster solver in replay and run: {((replay_ratio < 0) == (run_ratio < 0)).mean():.0%}")
    print(f"  median log10({s1}/{s2}): replay {replay_ratio.median():.2f}, run {run_ratio.median():.2f}")
