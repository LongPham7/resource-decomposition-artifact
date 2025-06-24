import numpy as np

from statistical_analysis.toolbox import categorize_runtime_cost_data


def candidate_model_boundary(xs):
    return 2 * np.ceil(np.log2(xs + 1))


def test_model_boundary(runtime_data_size_only, model_boundary, verbose):
    categorized_runtime_data = categorize_runtime_cost_data(
        runtime_data_size_only)
    is_above_model_boundary = False
    for size, list_costs in categorized_runtime_data.items():
        boundary_cost = model_boundary(size)
        max_cost = np.max(list_costs)
        if verbose:
            print("Model boundary: size = {}, boundary cost = {}, max cost = {}".format(
                size, boundary_cost, max_cost))
        if max_cost > boundary_cost:
            is_above_model_boundary = True
            break
    return is_above_model_boundary
