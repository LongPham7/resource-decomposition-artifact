import numpy as np
import matplotlib.pyplot as plt
import stan
import json
from scipy.stats import chi2
import time

from json_manipulation import read_total_cost_data_from_json
from benchmark_data.benchmark_parameters import num_counters_dict
from benchmark_data.ground_truth import benchmark_bound_dict, get_ground_truth_bound

import os
import sys
from directory_location import get_ocaml_benchmark_bin_directory
import statistical_analysis.toolbox as stat_util
import visualization.three_dimensional as vis_three_dim
import visualization.two_dimensional as vis_two_dim


# Perform data-driven analysis using Stan


stan_code = """
data {
    int<lower=0> num_observations;
    int<lower=0> num_size_measures;
    matrix<lower=0>[num_observations, num_size_measures] input_sizes;
    vector<lower=0>[num_observations] costs;

    real<lower=0> chi_square_nu;
}
parameters {
    vector<lower=0>[num_size_measures] coefficients;
}
model {
    real predicted_bound;

    coefficients ~ chi_square(chi_square_nu);

    for (n in 1:num_observations) {
        predicted_bound = input_sizes[n] * coefficients;
        if (predicted_bound < costs[n] || costs[n] < 0) {
            target += negative_infinity();
        }
        else {
            target += uniform_lpdf(costs[n] | 0, predicted_bound);
        }
    }
}
"""


# Get the number of size measures in a given input size (which is either a
# single number or a tuple)


def get_num_input_size_measures(input_size):
    if type(input_size) == int or type(input_size) == np.float64:
        num_input_size_measures = 1
    else:
        # If the sie is not a single integer, it must be a tuple
        num_input_size_measures = len(input_size)
    return num_input_size_measures


# List all possible partitions of a number d among k many buckets


def split_d_among_k(d, k):
    if k == 1:
        return [[d]]
    elif d == 0:
        return [[0] * k]
    else:
        result = []
        for i in range(d+1):
            recursive_result = split_d_among_k(d-i, k-1)
            result.extend([[i] + r for r in recursive_result])
        return result


def split_at_most_d_among_k(d, k):
    result = []
    for m in range(0, d+1):
        result.extend(split_d_among_k(m, k))
    return result


# Calculate the power of a given input size (which is either a single number or
# a tuple) according to the given partition of the degree among k many buckets,
# where k is the number of size measures in the given input size


def calculate_power(input_size, partition_degree):
    assert (get_num_input_size_measures(input_size) == len(partition_degree))
    if len(partition_degree) == 1:
        return input_size ** partition_degree[0]
    else:
        result = 1
        for size, degree in zip(input_size, partition_degree):
            result *= size ** degree
        return result


# For eac input size in the runtime data, expand it to a tuple of all values of
# size measures up to the given degree. Here, unlike in AARA, we do not use
# binomial coefficients as base resource polynomials. Instead, we simply use
# powers.


def expand_input_sizes_in_runtime_data(runtime_data_size_only, degree):
    first_input_size, _ = runtime_data_size_only[0]
    num_input_size_measures = get_num_input_size_measures(first_input_size)
    list_partitions_degree = split_at_most_d_among_k(
        degree, num_input_size_measures)

    print("List of partitions of degree {}: {}".format(
        degree, list_partitions_degree))

    list_expanded_input_sizes = []
    for input_size, _ in runtime_data_size_only:
        expanded_input_size = [calculate_power(
            input_size, partition_degree) for partition_degree in list_partitions_degree]
        list_expanded_input_sizes.append(expanded_input_size)

    return list_expanded_input_sizes


def get_data_driven_bin_directory(benchmark):
    ocaml_benchmark_bin_directory = get_ocaml_benchmark_bin_directory()
    benchmark_bin_directory = os.path.expanduser(
        os.path.join(ocaml_benchmark_bin_directory, "data_driven_analysis", benchmark))
    if not os.path.exists(benchmark_bin_directory):
        os.makedirs(benchmark_bin_directory)
    return benchmark_bin_directory


# Grid search for initial values in Stan


def generate_and_check_candidate_initial_values(array_expanded_input_sizes, array_costs, sample_points, current_partial_candidate):
    # print("sample_points = {}".format(sample_points))
    # print("current candidate = {}".format(current_partial_candidate))
    num_size_measures = len(array_expanded_input_sizes[0])
    num_filled_size_measures = len(current_partial_candidate)
    if num_filled_size_measures == num_size_measures:
        predicted_bounds = np.dot(
            array_expanded_input_sizes, np.array(current_partial_candidate))
        cost_gaps = predicted_bounds - array_costs
        if np.all(cost_gaps >= 0):
            return current_partial_candidate
        else:
            return None
    else:
        for sample_point in sample_points:
            new_candidate = current_partial_candidate.copy()
            new_candidate.append(sample_point)
            recursive_result = generate_and_check_candidate_initial_values(
                array_expanded_input_sizes, array_costs, sample_points, new_candidate)
            if recursive_result is not None:
                return recursive_result
        return None


def grid_search_initial_values(expanded_input_sizes, list_costs, chi_square_nu):
    lower_percentile = 0.1
    upper_percentile = 0.99
    chi_square_distribution = chi2(
        df=chi_square_nu, loc=0, scale=1)
    chi_square_lower_bound = chi_square_distribution.ppf(lower_percentile)
    chi_square_upper_bound = chi_square_distribution.ppf(upper_percentile)
    print("Grid search for initial values: lower bound = {}, upper bound = {}".format(
        chi_square_lower_bound, chi_square_upper_bound))

    num_samples_grid_search_each_dimension = 5

    list_initial_values = []
    multiplier = 1.5
    while list_initial_values == []:
        chi_square_points = np.linspace(
            chi_square_lower_bound, chi_square_upper_bound, num=num_samples_grid_search_each_dimension)
        array_expanded_input_sizes = np.array(expanded_input_sizes)
        array_costs = np.array(list_costs)
        result = generate_and_check_candidate_initial_values(
            array_expanded_input_sizes, array_costs, chi_square_points, [])
        if result is not None:
            print("Grid search for initial values: result = {}".format(result))
            return result
        chi_square_upper_bound *= multiplier


# Run Stan for data-driven analysis


def run_data_driven_analysis(benchmark, degree, num_chains, num_stan_samples, num_stan_warmup):
    runtime_data_size_only = read_total_cost_data_from_json(
        benchmark, bucket_size)
    num_observation = len(runtime_data_size_only)
    list_expanded_input_sizes = expand_input_sizes_in_runtime_data(
        runtime_data_size_only, degree)
    num_expanded_input_size_measures = len(list_expanded_input_sizes[0])
    list_costs = [cost for _, cost in runtime_data_size_only]

    chi_square_nu = 3.0
    stan_data = {
        "num_observations": num_observation,
        "num_size_measures": num_expanded_input_size_measures,
        "input_sizes": list_expanded_input_sizes,
        "costs": list_costs,
        "chi_square_nu": chi_square_nu
    }

    seed = 42
    posterior = stan.build(stan_code, data=stan_data, random_seed=seed)
    initial_values = grid_search_initial_values(
        list_expanded_input_sizes, list_costs, chi_square_nu)
    starting_time = time.perf_counter()
    fit = posterior.sample(
        num_chains=num_chains, num_samples=num_stan_samples, num_warmup=num_stan_warmup,
        init=[{"coefficients": initial_values}] * num_chains)
    ending_time = time.perf_counter()
    analysis_time = ending_time - starting_time

    df = fit.to_frame()
    print(df)

    # Retrieve the inferred coefficients and convert them from a numpy array to a list
    array_coefficients = fit["coefficients"]
    _, num_posterior_samples = array_coefficients.shape
    list_inferred_coefficients = [
        list(array_coefficients[:, i]) for i in range(0, num_posterior_samples)]
    inference_result_dict = {
        "runtime_data": runtime_data_size_only,
        "degree": degree,
        "inferred_coefficients": list_inferred_coefficients,
        "analysis_time": analysis_time}

    # Write the inference result to JSON
    benchmark_bin_directory = get_data_driven_bin_directory(benchmark)
    inference_result_json_filepath = os.path.join(
        benchmark_bin_directory, "inference_result.json")
    with open(inference_result_json_filepath, "w") as f:
        json.dump(inference_result_dict, f)


# Plot the inference results of data-driven analysis


def get_ground_truth_function(benchmark):

    def ground_truth_function(size):
        num_counters = num_counters_dict[benchmark]
        counters = []
        for counter_index in range(num_counters):
            ground_truth_counter_function = get_ground_truth_bound(
                benchmark, counter_index)
            ground_truth = ground_truth_counter_function(size)
            counters.append(ground_truth)

        benchmark_bound = benchmark_bound_dict[benchmark]
        return benchmark_bound(size, counters)

    return ground_truth_function


def get_predicted_bound_vector_coefficients(degree):

    def predicted_bound_vector_coefficients(array_coefficients, input_size):
        num_input_sizes = get_num_input_size_measures(input_size)
        list_degree_partitions = split_at_most_d_among_k(
            degree, num_input_sizes)
        list_input_size_powers = [calculate_power(
            input_size, partition_degree) for partition_degree in list_degree_partitions]
        return np.dot(array_coefficients, list_input_size_powers)

    return predicted_bound_vector_coefficients


def plot_two_dimensional_data_driven_inference_result(benchmark):
    benchmark_bin_directory = get_data_driven_bin_directory(benchmark)
    inference_result_json_filepath = os.path.join(
        benchmark_bin_directory, "inference_result.json")
    with open(inference_result_json_filepath, "r") as f:
        inference_result_dict = json.load(f)

    list_coefficients = inference_result_dict["inferred_coefficients"]
    runtime_data = inference_result_dict["runtime_data"]
    degree = inference_result_dict["degree"]
    predicted_bound_vector_coefficients = get_predicted_bound_vector_coefficients(
        degree)

    _, ax = plt.subplots(figsize=(3, 3), layout="compressed")
    ground_truth_function = get_ground_truth_function(benchmark)
    axis_labels = ["Input Size", "Total Cost"]
    image_dict = {"axis_labels": axis_labels, "legend": True}
    vis_two_dim.plot_posterior_distribution_percentile_range_ax(ax, runtime_data, list_coefficients,
                                                                predicted_bound_vector_coefficients,
                                                                [ground_truth_function], image_dict)
    ax.legend()
    plt.show()
    plt.close()


def plot_three_dimensional_data_driven_inference_result(benchmark):
    benchmark_bin_directory = get_data_driven_bin_directory(benchmark)
    inference_result_json_filepath = os.path.join(
        benchmark_bin_directory, "inference_result.json")
    with open(inference_result_json_filepath, "r") as f:
        inference_result_dict = json.load(f)

    list_coefficients = inference_result_dict["inferred_coefficients"]
    runtime_data = inference_result_dict["runtime_data"]
    degree = inference_result_dict["degree"]
    predicted_bound_vector_coefficients = get_predicted_bound_vector_coefficients(
        degree)

    _, ax = plt.subplots(figsize=(3, 3), layout="compressed",
                         subplot_kw={'projection': '3d'})
    axis_labels = ["Vertices", "Max Degree", "Total Cost"]
    image_dict = {"axis_labels": axis_labels, "legend": True}
    vis_three_dim.plot_median_cost_bound_ax(ax, runtime_data, list_coefficients,
                                            predicted_bound_vector_coefficients,
                                            [], image_dict)
    ax.legend()
    plt.show()
    plt.close()


# Compute the proportions of sound inferred bounds


def is_sound_inferred_coefficients(benchmark, inferred_coefficients, version):
    if benchmark == "bubble_sort":
        assert (len(inferred_coefficients) == 3)
        coefficient_degree_two = inferred_coefficients[2]
        if version == "dominant":
            return coefficient_degree_two >= 1
        else:
            coefficient_degree_zero = inferred_coefficients[0]
            coefficient_degree_one = inferred_coefficients[1]
            return coefficient_degree_zero >= 1 and coefficient_degree_one >= 2 and coefficient_degree_two >= 1
    elif benchmark == "bellman_ford_algorithm":
        assert (len(inferred_coefficients) == 10)
        coefficient_v_squared_d = inferred_coefficients[8]
        if version == "dominant":
            return coefficient_v_squared_d >= 4
        else:
            degree_zero_coefficient = inferred_coefficients[0]
            coefficient_v = inferred_coefficients[2]
            coefficient_v_squared = inferred_coefficients[5]
            return degree_zero_coefficient >= 4 and coefficient_v_squared_d >= 4 and coefficient_v >= 4 and coefficient_v_squared >= 3
    else:
        raise ValueError("The given benchmark is invalid")


def compute_proportion_sound_inferred_bounds(benchmark, version):
    benchmark_bin_directory = get_data_driven_bin_directory(benchmark)
    inference_result_json_filepath = os.path.join(
        benchmark_bin_directory, "inference_result.json")
    with open(inference_result_json_filepath, "r") as f:
        inference_result_dict = json.load(f)

    inferred_coefficients = inference_result_dict["inferred_coefficients"]

    def function_is_sound(
        x): return is_sound_inferred_coefficients(benchmark, x, version)
    num_sound_coefficients = 0
    for inferred_coefficient in inferred_coefficients:
        if function_is_sound(inferred_coefficient):
            num_sound_coefficients += 1

    num_posterior_samples = len(inferred_coefficients)
    print("Sound bounds in benchmark {}: {}/{} = {}%".format(benchmark, num_sound_coefficients,
          num_posterior_samples, num_sound_coefficients / num_posterior_samples * 100))

    stat_util.inference_result_statistics(inferred_coefficients)


if __name__ == "__main__":
    list_args = sys.argv
    mode = list_args[1]
    benchmark = list_args[2]

    if mode == "infer":
        if len(list_args) == 3:
            degree = 2 if benchmark == "bubble_sort" else 3
        else:
            degree = int(list_args[3])
        bucket_size = 1
        num_chains = 4
        num_stan_samples = 3000
        num_stan_warmup = 8000
        run_data_driven_analysis(
            benchmark, degree, num_chains, num_stan_samples, num_stan_warmup)
    elif mode == "soundness":
        version = list_args[3]
        compute_proportion_sound_inferred_bounds(benchmark, version)
    else:
        raise ValueError("The given mode is invalid")
