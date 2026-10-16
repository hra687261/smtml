import argparse
import json
import pandas as pd
import numpy as np
from sklearn.tree import export_text, DecisionTreeRegressor, _tree
from sklearn.model_selection import cross_val_score, KFold
from sklearn.ensemble import GradientBoostingRegressor

parser = argparse.ArgumentParser(
    prog="SMTZilla",
    description="Using machine learning to generate a model "
    "that intelligently selects the best solver for a given query using its "
    "features",
)

parser.add_argument(
    "PATH",
    help="Path to a CSV file containing the training data (query features and \
      runtimes by solver)",
)

parser.add_argument(
    "JSON",
    default=None,
    help="Optional path to a JSON file where the models will be exported",
)


parser.add_argument(
    "--gradient-boost",
    action=argparse.BooleanOptionalAction,
    default=True,
    help="Use gradient boosting regressor (default True)",
)


def str_to_int(f: str, x: str):
    try:
        i = int(x)
        if i <= 0:
            raise argparse.ArgumentTypeError(f"{f} must be > 0")
        return i
    except ValueError:
        raise argparse.ArgumentTypeError(f"{f} must be a valid integer")


predictor_group = parser.add_argument_group("Regressor parameters")

predictor_group.add_argument(
    "--n-estimators",
    type=(lambda x: str_to_int("n_estimators", x)),
    default=200,
    help="Number of estimators (must be > 0, only used if gradient-boost is True)",
)

predictor_group.add_argument(
    "--max-depth",
    type=(lambda x: str_to_int("max_depth", x)),
    default=5,
    help="Maximum depth of trees (must be > 0)",
)

parser.add_argument(
    "-s",
    "--simulation",
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Run a simulation on the test data (--test), or on the training data",
)

parser.add_argument(
    "--test",
    default=None,
    help="Path to a CSV file containing the test data (from other programs than \
      the training data)",
)

parser.add_argument(
    "--timeout-penalty",
    type=float,
    default=2.0,
    help="Factor applied to the time of timed out runs (default 2, PAR-2)",
)

parser.add_argument(
    "--frequency-weights",
    choices=["none", "log", "linear"],
    default="log",
    help="Weight of a query in the training, from its frequency (default log)",
)

parser.add_argument(
    "--delta-weight",
    type=float,
    default=0.1,
    help="Multiply the weight of a query by 1 + d / DELTA_WEIGHT, where d is the \
      time in seconds lost by choosing the wrong solver on it, so that the rare \
      queries on which a wrong choice is costly are learned (default 0.1, 0 \
      disables it)",
)

parser.add_argument(
    "--pp-stats",
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Print some stats about the training data",
)

parser.add_argument(
    "--debug",
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Print some debugging information",
)

random_state_const = 42  # for reproductibility
args = parser.parse_args()
solver_col = "solver"
time_col = "time"
query_size_col = "query_size"
delta_col = "delta"
query_id_col = "query_id"
frequency_col = "frequency"
status_col = "status"


non_feature_cols = [solver_col, time_col, query_id_col, frequency_col, status_col]


def read_data(path):
    data = pd.read_csv(path)
    # Several replays of the same queries (e.g. concatenated CSVs): median time
    # per query and solver, error if any run failed
    runs = data.groupby([query_id_col, solver_col])
    data = runs.first()
    data[time_col] = runs[time_col].median()
    data[status_col] = runs[status_col].agg(
        lambda s: "error" if (s == "error").any() else s.iloc[0]
    )
    data = data.reset_index()
    # Queries on which a solver failed have no oracle time
    errors = data.loc[data[status_col] == "error", query_id_col].unique()
    if len(errors) > 0:
        print(f"[INFO] Dropping {len(errors)} queries with errors")
    data = data[~data[query_id_col].isin(errors)].copy()
    # Penalise timeouts
    timeouts = data[status_col] == "timeout"
    data.loc[timeouts, time_col] = data.loc[timeouts, time_col] * args.timeout_penalty
    return data


def mk_data(data):
    # Drop the features that are 0 in all queries
    feature_cols = [
        c for c in data.columns if c not in non_feature_cols and (data[c] != 0).any()
    ]
    data[query_size_col] = data[feature_cols].sum(axis=1)
    # Oracle times: the minimum time achieved by any solver on the same query
    oracle_times = data.groupby(query_id_col)[time_col].transform("min")
    # Delta: the difference between current solver time and the best possible time
    data[delta_col] = data[time_col] - oracle_times
    return (data, feature_cols)


def mk_models(
    data, feature_cols, n_estimators, max_depth, gradient_boost=True, debug=False
):
    # Train one regression model per solver
    models = {}
    # Time lost by choosing the wrong solver on a query
    choice_cost = data.groupby(query_id_col)[delta_col].transform("max")
    for solver in data[solver_col].unique():
        df_solver = data[data[solver_col] == solver]
        X = df_solver[feature_cols]
        Y = df_solver[delta_col]
        if args.frequency_weights == "log":
            weights = np.log1p(df_solver[frequency_col])
        elif args.frequency_weights == "linear":
            weights = df_solver[frequency_col]
        else:
            weights = None
        if args.delta_weight > 0:
            cost_s = choice_cost[df_solver.index] / 1e9
            delta_weights = 1 + cost_s / args.delta_weight
            weights = delta_weights if weights is None else weights * delta_weights

        if gradient_boost:
            model = GradientBoostingRegressor(
                n_estimators=n_estimators,
                max_depth=max_depth,
                random_state=random_state_const,
            )
        else:
            model = DecisionTreeRegressor(
                max_depth=max_depth, random_state=random_state_const
            )

        model.fit(X, Y, sample_weight=weights)
        models[solver] = model

        if debug:
            # Cross-validation MAE (Mean Absolute Error) for this solver
            kf = KFold(n_splits=5, shuffle=True, random_state=random_state_const)
            cv_scores = cross_val_score(
                model, X, Y, cv=kf, scoring="neg_mean_absolute_error"
            )
            print(
                f"[CV] Solver {solver:10} | Average MAE = "
                f"{-cv_scores.mean():.2f} ± {cv_scores.std():.2f}\n"
            )
    return models


def pp_stats(data):
    # Compute stats per query and solver (over several replays)
    stats = (
        data.groupby([query_id_col, solver_col])[time_col]
        .agg(count="count", mean="mean", median="median", std="std")
        .reset_index()
    )

    # Compute the relative difference between mean and median
    stats["mean_median_diff"] = (stats["mean"] - stats["median"]).abs()
    stats["rel_diff"] = stats["mean_median_diff"] / stats["mean"]

    # Filter groups where dispersion is significant
    print("")
    for threshold in [0.5, 0.8, 0.9, 0.95, 0.99]:
        high_dispersion = stats[stats["rel_diff"] > threshold]
        print(
            f"[INFO] Number of groups with high dispersion "
            f"(>{int(threshold*100)}%): {len(high_dispersion)}"
        )
    print("")


def debug_gb(models, feature_cols):
    for solver, model in models.items():
        feature_importance = model.feature_importances_
        sorted_idx = np.argsort(feature_importance)[::-1]
        print(f"\n=== Top 10 Features for {solver}:")
        for i in sorted_idx[:10]:  # top 10 features
            print(f"{feature_cols[i]}: {feature_importance[i]:.4f}")

        for tree in model.estimators_[:3, 0]:  # first 3 trees
            print(export_text(tree, feature_names=feature_cols))


def debug_no_gb(data, models, feature_cols):
    for solver in data[solver_col].unique():
        X = data[data[solver_col] == solver][feature_cols]
        model = models[solver]
        print(f"\n=== Decision Tree Rules for solver {solver} ===\n")
        print(export_text(model, feature_names=list(X.columns)))
        print("=" * 60, "\n")


def tree_to_dict(tree, feature_names):
    feature_name = [
        feature_names[i] if i != _tree.TREE_LEAF else "undefined!" for i in tree.feature
    ]

    def recurse(node):
        # A node is a leaf when tree.children_left[node] == _tree.TREE_LEAF
        # and tree.children_right[node] == _tree.TREE_LEAF
        # (checking one is sufficient)
        if tree.children_left[node] == _tree.TREE_LEAF:
            return {"value": float(tree.value[node][0, 0])}
        threashold = float(tree.threshold[node])
        # Remove unreachable branches
        if threashold < 0:
            return recurse(tree.children_right[node])
        return {
            "feature": feature_name[node],
            "threshold": threashold,
            "left": recurse(tree.children_left[node]),
            "right": recurse(tree.children_right[node]),
        }

    return recurse(0)


def gbrt_to_dict(model, feature_names):
    model_dict = {
        "n_estimators": model.n_estimators,
        "init_value": (
            float(model.init_.constant_.item())
            if hasattr(model.init_, "constant_")
            else 0.0
        ),
        "trees": [],
    }
    for _, stage in enumerate(model.estimators_[:, 0]):  # regression => 1 column
        tree_dict = tree_to_dict(stage.tree_, feature_names)
        model_dict["trees"].append(tree_dict)
    return model_dict


def export_models_to_json(models, feature_cols):
    output_json = {}
    for model_name in models:
        gradient_boost = isinstance(models[model_name], GradientBoostingRegressor)
        assert gradient_boost or isinstance(models[model_name], DecisionTreeRegressor)
        if gradient_boost:
            model_dict = gbrt_to_dict(models[model_name], feature_cols)
        else:
            model_dict = tree_to_dict(models[model_name].tree_, feature_cols)
        output_json[model_name] = {
            "gradient_boost": gradient_boost,
            "model": model_dict,
        }
    return output_json


def simulation(data, models, feature_cols):
    # One row per query: its features and frequency, and the time of each solver
    queries = data.groupby(query_id_col).first()
    times = data.pivot(index=query_id_col, columns=solver_col, values=time_col)
    # Missing times (a solver not run on a query): the mean of the others
    times = times.apply(lambda row: row.fillna(row.mean()), axis=1)
    frequency = queries[frequency_col]

    # Model: the solver with the lowest predicted delta
    deltas = pd.DataFrame(
        {
            solver: model.predict(queries[feature_cols])
            for solver, model in models.items()
        },
        index=queries.index,
    )
    # Only the solvers that were run on the test data can be chosen
    predicted = deltas[[s for s in deltas.columns if s in times.columns]].idxmin(axis=1)
    predicted_times = times.to_numpy()[
        np.arange(len(times)), times.columns.get_indexer(predicted)
    ]
    best_times = times.min(axis=1)

    # Each query counts as many times as it occurs (frequency)
    oracle_total = (frequency * best_times).sum()
    regression_total = (frequency * predicted_times).sum()
    n_wrong = int((predicted_times > best_times).sum())

    print(f"\nTotal queries compared: {len(queries)} (wrong choice: {n_wrong})")
    print(f"Total time with optimal choice (oracle) : {oracle_total / 1e9:.2f} s")
    print(f"Total time via regression prediction    : {regression_total / 1e9:.2f} s")
    for solver in times.columns:
        total = (frequency * times[solver]).sum()
        print(f"Total time with fixed solver {solver}: {total / 1e9:.2f} s")
    print(f"\nModel overhead vs. oracle: {(regression_total - oracle_total)/1e9:.2f} s")


raw_data = pd.read_csv(str(args.PATH))
if args.pp_stats:
    pp_stats(raw_data)

data, feature_cols = mk_data(read_data(str(args.PATH)))

models = mk_models(
    data,
    feature_cols,
    args.n_estimators,
    args.max_depth,
    args.gradient_boost,
    args.debug,
)

if args.debug:
    if args.gradient_boost:
        debug_gb(models, feature_cols)
    else:
        debug_no_gb(data, models, feature_cols)

if args.JSON:
    models_json = export_models_to_json(models, feature_cols)
    print(f"Exporting to: {args.JSON}")
    with open(args.JSON, "w") as f:
        json.dump(models_json, f)

if args.test:
    test_data = read_data(args.test)
    # The features absent from the test data are 0
    test_data = test_data.reindex(
        columns=list(dict.fromkeys(list(test_data.columns) + feature_cols)),
        fill_value=0,
    )
    print(f"\n=== Test data: {args.test}")
    simulation(test_data, models, feature_cols)
elif args.simulation:
    print("\n=== Training data (no --test)")
    simulation(data, models, feature_cols)
