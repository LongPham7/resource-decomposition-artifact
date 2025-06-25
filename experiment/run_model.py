import stan
import numpy as np
from scipy.stats import chi2, lognorm

from joblib import Parallel, delayed
import os
import sys
import time

from benchmark_data.benchmark_parameters \
    import list_benchmarks_data_driven, list_benchmarks_with_multiple_measurements, \
    list_benchmarks_two_input_sizes, num_counters_dict
from json_manipulation import read_runtime_cost_data_from_json, \
    write_inference_result_to_json, write_stan_fit_object
from probabilistic_models import list_stan_code, predicted_bound_vector_sizes
import statistical_analysis.toolbox as stat_util


# Observed data and hyperparameters for Stan


def get_stan_data(runtime_data_size_only, bucket_size, model_id):
    num_observations = len(runtime_data_size_only)
    list_sizes = [size for (size, _) in runtime_data_size_only]
    list_costs = [cost for (_, cost) in runtime_data_size_only]
    stan_data = {"num_observations": num_observations,
                 "input_sizes": list_sizes,
                 "costs": list_costs,
                 "chi_square_nu": 3}

    if model_id == 0:
        stan_data["lognormal_linear_mu"] = 0
        stan_data["lognormal_linear_sigma"] = 0.1
        stan_data["cost_sigma_above_boundary"] = 1
    elif model_id == 1:
        stan_data["lognormal_linear_mu"] = 0
        stan_data["lognormal_linear_sigma"] = 0.1
        stan_data["cost_sigma_above_boundary"] = 1
        stan_data["bucket_size"] = bucket_size
    elif model_id == 2:
        stan_data["lognormal_linear_mu"] = 0
        stan_data["lognormal_linear_sigma"] = 0.1
        stan_data["cost_sigma_above_boundary"] = 1
        stan_data["bucket_size"] = 1
    return stan_data


# Initial values for the NUTS sampler


def grid_search_initial_values_logarithmic_model(stan_data):
    array_sizes = np.array(stan_data["input_sizes"])
    array_costs = np.array(stan_data["costs"])

    chi_square_nu = stan_data["chi_square_nu"]

    num_samples_grid_search_each_dimension = 10
    lower_percentile = 0.1
    upper_percentile = 0.99

    chi_square_distribution = chi2(
        df=chi_square_nu, loc=0, scale=1)
    chi_square_lower_bound = chi_square_distribution.ppf(lower_percentile)
    chi_square_upper_bound = chi_square_distribution.ppf(upper_percentile)

    # Grid search for the initial values of the logarithmic model. We first
    # search the cube with the range [chi_square_lower_bound,
    # chi_square_upper_bound] in eac dimension. If we find no feasible
    # coefficient, we then extend the upper bound chi_square_upper_bound by a
    # constant (e.g., 1.5).
    list_initial_coefficients = []
    multiplier = 1.5
    while list_initial_coefficients == []:
        chi_square_points = np.linspace(
            chi_square_lower_bound, chi_square_upper_bound, num=num_samples_grid_search_each_dimension)
        for coefficient0 in chi_square_points:
            for coefficient1 in chi_square_points:
                for coefficient2 in chi_square_points:
                    for coefficient3 in chi_square_points:
                        coefficients = [coefficient0, coefficient1,
                                        coefficient2, coefficient3]
                        array_predictions = predicted_bound_vector_sizes(
                            coefficients, array_sizes, model_id="logarithmic")
                        if np.all(array_predictions > array_costs):
                            list_initial_coefficients.append(
                                coefficients)
        chi_square_upper_bound *= multiplier

    return list_initial_coefficients


def grid_search_initial_values_linear_model(stan_data):
    array_sizes = np.array(stan_data["input_sizes"])
    array_costs = np.array(stan_data["costs"])

    chi_square_nu = stan_data["chi_square_nu"]
    lognormal_linear_mu = stan_data["lognormal_linear_mu"]
    lognormal_linear_sigma = stan_data["lognormal_linear_sigma"]

    num_samples_grid_search_each_dimension = 10
    lower_percentile = 0.1
    upper_percentile = 0.99

    chi_square_distribution = chi2(
        df=chi_square_nu, loc=0, scale=1)
    chi_square_lower_bound = chi_square_distribution.ppf(lower_percentile)
    chi_square_upper_bound = chi_square_distribution.ppf(upper_percentile)
    chi_square_points = np.linspace(
        chi_square_lower_bound, chi_square_upper_bound, num=num_samples_grid_search_each_dimension)

    lognormal_linear_distribution = lognorm(
        s=lognormal_linear_sigma, loc=lognormal_linear_mu, scale=1)
    lognormal_linear_lower_bound = lognormal_linear_distribution.ppf(
        lower_percentile)
    lognormal_linear_upper_bound = lognormal_linear_distribution.ppf(
        upper_percentile)
    lognormal_linear_points = np.linspace(
        lognormal_linear_lower_bound, lognormal_linear_upper_bound, num=num_samples_grid_search_each_dimension)

    # Grid search for the initial values of the linear model
    list_initial_coefficients = []
    for coefficient1 in lognormal_linear_points:
        for coefficient0 in chi_square_points:
            coefficients = [coefficient0, coefficient1]
            array_predictions = predicted_bound_vector_sizes(
                coefficients, array_sizes, model_id="linear")
            if np.all(array_predictions > array_costs):
                list_initial_coefficients.append(coefficients)
    if list_initial_coefficients == []:
        raise Exception("The linear model initial coefficients are undefined")

    return list_initial_coefficients


def grid_search_initial_values(model_id, stan_data):
    list_model_ids_logarithmic = [0, 1, 2]
    list_model_ids_linear = [0, 1, 2]

    if model_id in list_model_ids_logarithmic:
        logarithmic_model_initial_coefficient = grid_search_initial_values_logarithmic_model(stan_data)[
            0]
    else:
        logarithmic_model_initial_coefficient = None

    if model_id in list_model_ids_linear:
        linear_model_initial_coefficient = grid_search_initial_values_linear_model(stan_data)[
            0]
    else:
        linear_model_initial_coefficient = None

    print("Initial values for NUTS:\nLog model: {}\nLinear model: {}".format(
        logarithmic_model_initial_coefficient, linear_model_initial_coefficient))

    return logarithmic_model_initial_coefficient, linear_model_initial_coefficient


def initial_parameters(stan_data, model_id, num_chains):
    logarithmic_model_initial_coefficients, linear_model_initial_coefficients = grid_search_initial_values(
        model_id, stan_data)

    if model_id in [0, 1, 2]:
        return [{"coefficients_logarithmic": logarithmic_model_initial_coefficients,
                 "coefficients_linear": linear_model_initial_coefficients}] * num_chains
    else:
        raise ValueError("The model ID is invalid")


# Extract the inference result


def package_inference_result(runtime_data_size_only, model_id, fit, dependency_on_first_input, analysis_time):
    if model_id in [0, 1, 2]:
        inferred_coefficients_logarithmic = fit["coefficients_logarithmic"]
        inferred_coefficients_linear = fit["coefficients_linear"]
        inferred_is_logarithmic_model = fit["is_logarithmic_model"]
        inferred_coefficients = np.concatenate(
            (inferred_coefficients_logarithmic, inferred_coefficients_linear, inferred_is_logarithmic_model), axis=0)
    else:
        raise ValueError("The model ID is invalid")

    if model_id in [0, 1, 2]:
        array_mixing_probabilities = fit["mixing_probability_logarithmic_model"]
    else:
        array_mixing_probabilities = None

    inference_result_dict = {"runtime_data": runtime_data_size_only,
                             "inferred_coefficients": inferred_coefficients,
                             "inferred_mixing_probabilities": array_mixing_probabilities,
                             "analysis_time": analysis_time}
    if dependency_on_first_input is not None:
        inference_result_dict["dependency_on_first_input"] = dependency_on_first_input
    return inference_result_dict


# For graph algorithms, determine whether the cost depends more on the first
# input or the second input


def calculate_dependency_on_input(runtime_data_size_only, criterion, max_only):
    if max_only:
        num_samples_original = len(runtime_data_size_only)
        runtime_data_size_only = stat_util.extract_max_costs_in_buckets(
            runtime_data_size_only)
        num_samples_max_only = len(runtime_data_size_only)
        print("Size of the dataset: reduced from {} to {} after taking the maximum of each bucket".format(
            num_samples_original, num_samples_max_only))

    if criterion == "correlation":
        score1, score2 = stat_util.calculate_correlation_coefficients(
            runtime_data_size_only)
    else:
        score1, score2 = stat_util.calculate_mutual_information(
            runtime_data_size_only)
    if score1 > score2:
        return True
    else:
        return False


# Run a model on a specified benchmark


def run_model_id(benchmark, bucket_size, counter_index, model_id, runtime_data_size_only,
                 num_chains, num_stan_samples, num_stan_warmup, seed):
    # For the benchmarks with two-dimensional inputs, we first determine whether
    # the cost depends on the first input or the second input. We then perform
    # Bayesian inference to infer a cost bound parametric in this input.
    if benchmark in list_benchmarks_two_input_sizes:
        criterion = "mutual_information"
        max_only = True
        dependency_on_first_input = calculate_dependency_on_input(
            runtime_data_size_only, criterion, max_only)
        if dependency_on_first_input:
            runtime_data_size_only_projected = [(size1, cost) for (
                (size1, _), cost) in runtime_data_size_only]
        else:
            runtime_data_size_only_projected = [(size2, cost) for (
                (_, size2), cost) in runtime_data_size_only]
    else:
        dependency_on_first_input = None
        runtime_data_size_only_projected = runtime_data_size_only

    # If we use model 1 or 2, we extract the maximum cost of each size bucket.
    if model_id in [1, 2]:
        runtime_data_projected_max_cost_only = stat_util.extract_max_costs_in_buckets(
            runtime_data_size_only_projected)
        print("Size of the dataset: reduced from {} to {} after taking the maximum of each size bucket".format(
            len(runtime_data_size_only_projected), len(runtime_data_projected_max_cost_only)))
        stan_data = get_stan_data(
            runtime_data_projected_max_cost_only, bucket_size, model_id)
    else:
        stan_data = get_stan_data(
            runtime_data_size_only_projected, bucket_size, model_id)

    stan_code = list_stan_code[model_id][0]

    # Run Bayesian inference and measure its analysis time
    posterior = stan.build(stan_code, data=stan_data, random_seed=seed)
    starting_time = time.perf_counter()
    fit = posterior.sample(num_chains=num_chains, num_samples=num_stan_samples,
                           init=initial_parameters(stan_data, model_id, num_chains), num_warmup=num_stan_warmup)
    ending_time = time.perf_counter()
    analysis_time = ending_time - starting_time

    # df = fit.to_frame()
    # print(df)

    # Write the runtime data (before taking its projection), model ID, and
    # inference result to a JSON file
    inference_result_dict = package_inference_result(runtime_data_size_only,
                                                     model_id, fit, dependency_on_first_input, analysis_time)
    write_inference_result_to_json(
        benchmark, bucket_size, counter_index, model_id, inference_result_dict)

    # Write the fit object to a file. The fit object will later be used to plot
    # a trace using arviz.
    write_stan_fit_object(benchmark, bucket_size, counter_index, model_id, fit)


# Run all models on all bucket sizes of a specified benchmark


def run_multiple_benchmarks(benchmarks, model_ids):
    seed = 42
    num_chains = 4
    num_stan_samples = 3000
    num_stan_warmup = 8000

    list_configurations = []
    for benchmark in benchmarks:
        # if benchmark in list_benchmarks_with_multiple_measurements:
        #     list_bucket_sizes = [1]
        # else:
        #     # list_bucket_sizes = [1, 2, 5, 10]
        list_bucket_sizes = [1]
        num_counters = num_counters_dict[benchmark]
        list_configurations.extend([(benchmark, bucket_size, counter_index, model_id)
                                   for bucket_size in list_bucket_sizes for counter_index in range(num_counters) for model_id in model_ids])

    def run_experiment(benchmark, bucket_size, counter_index, model_id):
        print("Start fitting a probabilistic model: benchmark = {}, bucket size = {}, counter index = {}, model ID = {}".format(
            benchmark, bucket_size, counter_index, model_id))
        runtime_data_size_only = read_runtime_cost_data_from_json(
            benchmark, bucket_size, counter_index)
        run_model_id(benchmark, bucket_size, counter_index, model_id, runtime_data_size_only,
                     num_chains, num_stan_samples, num_stan_warmup, seed)
        print("Done with fitting a probabilistic model: benchmark = {}".format(benchmark))

    # Run all models and all bucket sizes in parallel. However, if we want to
    # measure the analysis time of Stan, we should run the processes in
    # sequence.
    # n_jobs = os.cpu_count() - 1
    n_jobs = 1
    Parallel(n_jobs=n_jobs)(delayed(run_experiment)(benchmark, bucket_size, counter_index, model_id)
                            for benchmark, bucket_size, counter_index, model_id in list_configurations)


if __name__ == "__main__":
    list_args = sys.argv
    mode = list_args[1]
    model_ids = [0]

    if mode == "all":
        run_multiple_benchmarks(list_benchmarks_data_driven, model_ids)
    elif mode == "benchmark":
        benchmarks = list_args[2:]
        run_multiple_benchmarks(benchmarks, model_ids)
    else:
        raise ValueError("Unsupported mode: {}".format(mode))
