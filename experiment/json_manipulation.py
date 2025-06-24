import os
import numpy as np
import json
import pickle

from directory_location import get_inference_result_directory, get_runtime_cost_data_directory


# Write input data to JSON


def write_input_data_to_json(benchmark, bucket_size, input_data):
    input_data_dict = {"input_data": input_data}
    runtime_cost_data_directory = get_runtime_cost_data_directory(
        benchmark, bucket_size)
    file_name = "input_data.json"
    file_path = os.path.join(runtime_cost_data_directory, file_name)

    with open(file_path, "w") as f:
        json.dump(input_data_dict, f)
    return file_path


# Read runtime cost data from JSON


def format_json_runtime_cost_data(runtime_cost_data_dict, counter_index):
    input_sizes = runtime_cost_data_dict["input_sizes"]

    # If individual sizes are not integers, they are lists. This happens when
    # the runtime cost data is two-dimensional (i.e., we have two inputs). So we
    # convert them to tuples. JSON does not support tuple types. Hence, a tuple
    # of size two is encoded as a list of length two.
    if type(input_sizes[0]) != int:
        input_sizes = [tuple(input_size) for input_size in input_sizes]
    num_counters = runtime_cost_data_dict["num_counters"]
    counter_values = runtime_cost_data_dict["counter_values"]
    assert (num_counters == len(counter_values)), "Mismatch in the number of counters"
    counter_values_index = counter_values[counter_index]

    list_input_sizes_counter_values = []
    for input_size, list_values in zip(input_sizes, counter_values_index):
        # For each round of running the benchmark program, if we have multiple
        # measurements, we only return the maximum cost.
        if len(list_values) > 0:
            list_input_sizes_counter_values.append(
                (input_size, np.max(list_values)))
    return list_input_sizes_counter_values


def read_runtime_cost_data_from_json(benchmark, bucket_size, counter_index):
    runtime_cost_data_directory = get_runtime_cost_data_directory(
        benchmark, bucket_size)
    file_name = "runtime_cost_data.json"
    file_path = os.path.join(runtime_cost_data_directory, file_name)
    with open(file_path, "r") as f:
        dict = json.load(f)

    return format_json_runtime_cost_data(dict, counter_index)


# Read total cost from JSON


def read_total_cost_data_from_json(benchmark, bucket_size):
    runtime_cost_data_directory = get_runtime_cost_data_directory(
        benchmark, bucket_size)
    file_name = "runtime_cost_data.json"
    file_path = os.path.join(runtime_cost_data_directory, file_name)
    with open(file_path, "r") as f:
        dict = json.load(f)

    input_sizes = dict["input_sizes"]
    total_costs = dict["total_costs"]
    return list(zip(input_sizes, total_costs))


# Write and read the Stan inference results


def write_inference_result_to_json(benchmark, bucket_size, counter_index, model_id,
                                   inference_result_dict):
    # Reformat the inferred coefficients from numpy arrays to lists
    array_coefficients = inference_result_dict["inferred_coefficients"]
    _, num_posterior_samples = array_coefficients.shape
    list_inferred_coefficients = [
        list(array_coefficients[:, i]) for i in range(0, num_posterior_samples)]
    inference_result_dict["inferred_coefficients"] = list_inferred_coefficients

    # Reformat the inferred mixing probabilities from numpy arrays to lists
    if "inferred_mixing_probabilities" in inference_result_dict:
        array_mixing_probabilities = inference_result_dict["inferred_mixing_probabilities"]
        list_mixing_probabilities = [array_mixing_probabilities[0, i]
                                     for i in range(0, num_posterior_samples)]
        inference_result_dict["inferred_mixing_probabilities"] = list_mixing_probabilities

    # Store the dictionary in a JSON file
    inference_result_directory = get_inference_result_directory(
        benchmark, bucket_size, counter_index, model_id)
    file_name = "inference_result.json"
    file_path = os.path.join(inference_result_directory, file_name)

    with open(file_path, "w") as f:
        json.dump(inference_result_dict, f)


def read_inference_result_from_json(benchmark, bucket_size, counter_index, model_id):
    inference_result_directory = get_inference_result_directory(
        benchmark, bucket_size, counter_index, model_id)
    file_name = "inference_result.json"
    file_path = os.path.join(inference_result_directory, file_name)
    with open(file_path, "r") as f:
        dict = json.load(f)

    return dict


# Write and read the fit object of Pystan


def write_stan_fit_object(benchmark, bucket_size, counter_index, model_id, fit):
    inference_result_directory = get_inference_result_directory(
        benchmark, bucket_size, counter_index, model_id)
    file_name = "fit_object.pkl"
    file_path = os.path.join(inference_result_directory, file_name)

    with open(file_path, "wb") as f:
        pickle.dump(fit, f)


def read_stan_fit_object(benchmark, bucket_size, counter_index, model_id):
    inference_result_directory = get_inference_result_directory(
        benchmark, bucket_size, counter_index, model_id)
    file_name = "fit_object.pkl"
    file_path = os.path.join(inference_result_directory, file_name)

    with open(file_path, "rb") as f:
        fit = pickle.load(f)
    return fit
