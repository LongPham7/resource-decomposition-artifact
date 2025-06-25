import numpy as np
import sys

from benchmark_data.benchmark_parameters \
    import list_benchmarks_data_driven, list_benchmarks_one_input_size, \
    list_benchmarks_with_trees, num_counters_dict
from json_manipulation import read_inference_result_from_json
from benchmark_data.ground_truth \
    import is_sound_asymptotically, is_sound_asymptotically_mixing_probability_logarithmic, \
    is_sound_bound, conventional_aara_result, get_ground_truth_bound, benchmark_bound_dict
from probabilistic_models import predicted_bound_vector_coefficients


# Print out the analysis time of Bayesian inference


def print_out_analysis_time():
    bucket_size = 1
    model_id = 0
    num_iterations = 3000 + 8000
    num_chains = 4

    benchmark_width = max([len(benchmark)
                          for benchmark in list_benchmarks_data_driven])
    counter_index_width = 7
    analysis_time_width = 17
    analysis_time_precision = 2
    num_iterations_width = 10
    num_chains_width = 6

    print("{:{benchmark_width}} {:{counter_index_width}} {:{analysis_time_width}} {:{num_iterations_width}} {:{num_chains_width}}".format("Benchmark", "Counter", "Analysis Time (s)", "Iterations", "Chains",
                                                                                                                                          benchmark_width=benchmark_width,
                                                                                                                                          counter_index_width=counter_index_width,
                                                                                                                                          analysis_time_width=analysis_time_width,
                                                                                                                                          num_iterations_width=num_iterations_width,
                                                                                                                                          num_chains_width=num_chains_width))

    def print_benchmark_counter_index(benchmark, counter_index):
        inference_result = read_inference_result_from_json(
            benchmark, bucket_size, counter_index, model_id)
        analysis_time = inference_result["analysis_time"]
        benchmark_name = benchmark if counter_index == 0 else ""
        print("{:{benchmark_width}} {:{counter_index_width}} {:{analysis_time_width}.{analysis_time_precision}f} {:{num_iterations_width}} {:{num_chains_width}}".format(benchmark_name, counter_index, analysis_time,
                                                                                                                                                                         num_iterations, num_chains,
                                                                                                                                                                         benchmark_width=benchmark_width,
                                                                                                                                                                         counter_index_width=counter_index_width,
                                                                                                                                                                         analysis_time_width=analysis_time_width,
                                                                                                                                                                         analysis_time_precision=analysis_time_precision,
                                                                                                                                                                         num_iterations_width=num_iterations_width,
                                                                                                                                                                         num_chains_width=num_chains_width))

    def print_benchmark(benchmark):
        num_counters = num_counters_dict[benchmark]
        for counter_index in range(num_counters):
            print_benchmark_counter_index(benchmark, counter_index)

    for benchmark in list_benchmarks_data_driven:
        print_benchmark(benchmark)


# Print out the proportions of sound cost bounds


def calculate_soundness_proportion(benchmark, counter_index, list_coefficients, soundness_function):
    num_sound_bounds = 0
    for coefficients in list_coefficients:
        if soundness_function(benchmark, counter_index, coefficients):
            num_sound_bounds += 1

    # print("Benchmark {} counter index {}: sound bounds = {}/{}".format(
    #     benchmark, counter_index, num_sound_bounds, len(list_coefficients)))

    return num_sound_bounds / len(list_coefficients)


def calculate_average_mixing_probability(benchmark, counter_index, list_mixing_probabilities_logarithmic):
    list_mixing_probabilities = [is_sound_asymptotically_mixing_probability_logarithmic(
        benchmark, counter_index, p) for p in list_mixing_probabilities_logarithmic]
    return np.mean(list_mixing_probabilities)


def print_soundness_proportions(version):
    bucket_size = 1
    model_id = 0

    benchmark_width = max([len(benchmark)
                          for benchmark in list_benchmarks_data_driven])
    conv_aara_width = 12
    counter_index_width = 7
    sound_complexity_width = 17
    sound_bounds_width = 12
    sound_bounds_precision = 1

    print("{:{benchmark_width}} {:{conv_aara_width}} {:{counter_index_width}} {:{sound_complexity_width}} {:{sound_bounds_width}}".format("Benchmark", "Conv. AARA", "Counter", "Sound Complexity", "Sound Bounds",
                                                                                                                                          benchmark_width=benchmark_width,
                                                                                                                                          conv_aara_width=conv_aara_width,
                                                                                                                                          counter_index_width=counter_index_width,
                                                                                                                                          sound_bounds_width=sound_bounds_width,
                                                                                                                                          sound_complexity_width=sound_complexity_width))

    def is_sound_bound_fixed_version(benchmark, counter_index, coefficients):
        return is_sound_bound(benchmark, counter_index, coefficients, version)

    def print_benchmark_counter_index(benchmark, counter_index):
        inference_result = read_inference_result_from_json(
            benchmark, bucket_size, counter_index, model_id)
        list_coefficients = inference_result["inferred_coefficients"]

        asymptotic_soundness_method = "mixing_probability"
        # The first way to calculate the soundness proportion is to examine the
        # posterior sample of the indicator variable (i.e., whether the cost
        # bound is logarithmic or linear). The second way to calculate the
        # soundness proportion is to look at the mixing probability for a
        # logarithmic model.
        if asymptotic_soundness_method == "posterior_sample":
            sound_complexity_percentage = calculate_soundness_proportion(
                benchmark, counter_index, list_coefficients, is_sound_asymptotically) * 100
        else:
            list_mixing_probabilities = inference_result["inferred_mixing_probabilities"]
            sound_complexity_percentage = calculate_average_mixing_probability(
                benchmark, counter_index, list_mixing_probabilities) * 100

        sound_bounds_percentage = calculate_soundness_proportion(
            benchmark, counter_index, list_coefficients, is_sound_bound_fixed_version) * 100

        benchmark_name = benchmark if counter_index == 0 else ""
        conv_aara_result = conventional_aara_result(
            benchmark) if counter_index == 0 else ""
        print("{:{benchmark_width}} {:{conv_aara_width}} {:{counter_index_width}} {:{sound_complexity_width}.{sound_bounds_precision}f}% {:{sound_bounds_width}.{sound_bounds_precision}f}%".format(benchmark_name, conv_aara_result, counter_index, sound_complexity_percentage, sound_bounds_percentage,
                                                                                                                                                                                                    benchmark_width=benchmark_width,
                                                                                                                                                                                                    conv_aara_width=conv_aara_width,
                                                                                                                                                                                                    counter_index_width=counter_index_width,
                                                                                                                                                                                                    sound_complexity_width=sound_complexity_width - 2,
                                                                                                                                                                                                    sound_bounds_width=sound_bounds_width,
                                                                                                                                                                                                    sound_bounds_precision=sound_bounds_precision))

    def print_benchmark(benchmark):
        num_counters = num_counters_dict[benchmark]
        for counter_index in range(num_counters):
            print_benchmark_counter_index(benchmark, counter_index)

    for benchmark in list_benchmarks_data_driven:
        print_benchmark(benchmark)


# Calculate relative errors


def calculate_relative_error_counter(benchmark, counter_index, input_size):
    bucket_size = 1
    model_id = 0
    inference_result = read_inference_result_from_json(
        benchmark, bucket_size, counter_index, model_id)
    list_coefficients = inference_result["inferred_coefficients"]
    dependency_on_first_input = inference_result.get(
        "dependency_on_first_input")

    if dependency_on_first_input is not None:
        if dependency_on_first_input:
            input_size_projected, _ = input_size
        else:
            _, input_size_projected = input_size
    else:
        input_size_projected = input_size

    array_coefficients = np.array(list_coefficients)
    inferred_bounds = predicted_bound_vector_coefficients(
        array_coefficients, input_size_projected, model_id)

    ground_truth_bound_benchmark = get_ground_truth_bound(
        benchmark, counter_index)
    ground_truth = ground_truth_bound_benchmark(input_size)
    relative_errors = (inferred_bounds - ground_truth) / ground_truth

    relative_error_5th_percentile = np.percentile(relative_errors, q=5)
    relative_error_50th_percentile = np.percentile(relative_errors, q=50)
    relative_error_95th_percentile = np.percentile(relative_errors, q=95)

    return relative_error_5th_percentile, relative_error_50th_percentile, relative_error_95th_percentile


def calculate_relative_error_total_cost(benchmark, input_size):
    bucket_size = 1
    model_id = 0
    num_counters = num_counters_dict[benchmark]
    benchmark_bound = benchmark_bound_dict[benchmark]

    list_array_counters = []
    for counter_index in range(num_counters):
        inference_result = read_inference_result_from_json(
            benchmark, bucket_size, counter_index, model_id)
        list_coefficients = inference_result["inferred_coefficients"]
        dependency_on_first_input = inference_result.get(
            "dependency_on_first_input")

        if dependency_on_first_input is not None:
            if dependency_on_first_input:
                input_size_projected, _ = input_size
            else:
                _, input_size_projected = input_size
        else:
            input_size_projected = input_size

        array_coefficients = np.array(list_coefficients)
        inferred_bounds_counter = predicted_bound_vector_coefficients(
            array_coefficients, input_size_projected, model_id)
        list_array_counters.append(inferred_bounds_counter)

    list_ground_truth_counters = []
    for counter_index in range(num_counters):
        ground_truth_bound_counter = get_ground_truth_bound(
            benchmark, counter_index)
        ground_truth_counter = ground_truth_bound_counter(input_size)
        list_ground_truth_counters.append(ground_truth_counter)

    inferred_bounds = benchmark_bound(input_size, list_array_counters)
    ground_truth = benchmark_bound(input_size, list_ground_truth_counters)
    relative_errors = (inferred_bounds - ground_truth) / ground_truth

    relative_error_5th_percentile = np.percentile(relative_errors, q=5)
    relative_error_50th_percentile = np.percentile(relative_errors, q=50)
    relative_error_95th_percentile = np.percentile(relative_errors, q=95)

    return relative_error_5th_percentile, relative_error_50th_percentile, relative_error_95th_percentile


# Print out relative errors


def print_relative_errors_benchmark_error_function(benchmark, relative_error_function):
    one_dimensional_input = benchmark in list_benchmarks_one_input_size
    input_size_width = 10 if one_dimensional_input else 14
    error_width = 15
    error_precision = 3
    print("{:{input_size_width}} {:>{error_width}} {:{error_width}} {:{error_width}}".format("Input Size", "5th percentile", "50th percentile", "95th percentile",
                                                                                             input_size_width=input_size_width,
                                                                                             error_width=error_width))

    if one_dimensional_input:
        if benchmark in list_benchmarks_with_trees:
            input_sizes = [2 ** 10 - 1, 2 ** 15 - 1, 2 ** 20 - 1]
            input_size_strings = ["2^{} - 1".format(i) for i in [10, 15, 20]]
        else:
            input_sizes = [2 ** 10, 2 ** 15, 2 ** 20]
            input_size_strings = ["2^{}".format(i) for i in [10, 15, 20]]
    else:
        if benchmark in list_benchmarks_with_trees:
            input_sizes = [(2 ** 10 - 1, 2 ** 9), (2 ** 15 - 1,
                                                   2 ** 14), (2 ** 20 - 1, 2 ** 19)]
            input_size_strings = ["(2^{}-1, 2^{})".format(i, i - 1)
                                  for i in [10, 15, 20]]
        else:
            input_sizes = [(2 ** 10, 2 ** 9), (2 ** 15, 2 ** 14),
                           (2 ** 20, 2 ** 19)]
            input_size_strings = ["(2^{}, 2^{})".format(i, i - 1)
                                  for i in [10, 15, 20]]

    def print_relative_errors_fixed_size(input_size, input_size_string):
        relative_errors = relative_error_function(input_size)
        error5, error50, error95 = relative_errors
        print("{:{input_size_width}} {:{error_width}.{error_precision}f} {:{error_width}.{error_precision}f} {:{error_width}.{error_precision}f}".format(input_size_string, error5, error50, error95,
                                                                                                                                                         input_size_width=input_size_width,
                                                                                                                                                         error_width=error_width,
                                                                                                                                                         error_precision=error_precision))

    for input_size, input_size_string in zip(input_sizes, input_size_strings):
        print_relative_errors_fixed_size(input_size, input_size_string)


def print_relative_errors_benchmark(benchmark):

    def get_relative_error_function_counter(counter_index):
        def relative_error_function_counter(input_size):
            return calculate_relative_error_counter(benchmark, counter_index, input_size)
        return relative_error_function_counter

    def relative_error_function_total_cost(input_size):
        return calculate_relative_error_total_cost(benchmark, input_size)

    num_counters = num_counters_dict[benchmark]
    print("Relative errors of benchmark {}".format(benchmark))
    for counter_index in range(num_counters):
        print("Counter index = {}".format(counter_index))
        relative_error_function = get_relative_error_function_counter(
            counter_index)
        print_relative_errors_benchmark_error_function(
            benchmark, relative_error_function)
        print()

    print("Total cost:")
    print_relative_errors_benchmark_error_function(
        benchmark, relative_error_function_total_cost)


# Print out relative errors for LaTex tables


def print_relative_errors_latex_benchmark_error_function(benchmark, quantity_string, relative_error_function):
    error_precision = 3

    if benchmark in list_benchmarks_one_input_size:
        if benchmark in list_benchmarks_with_trees:
            input_sizes = [2 ** 10 - 1, 2 ** 15 - 1, 2 ** 20 - 1]
            input_size_strings = [
                "2^{{{}}} - 1".format(i) for i in [10, 15, 20]]
        else:
            input_sizes = [2 ** 10, 2 ** 15, 2 ** 20]
            input_size_strings = ["2^{{{}}}".format(i) for i in [10, 15, 20]]
    else:
        if benchmark in list_benchmarks_with_trees:
            input_sizes = [(2 ** 10 - 1, 2 ** 9), (2 ** 15 - 1,
                                                   2 ** 14), (2 ** 20 - 1, 2 ** 19)]
            input_size_strings = ["(2^{{{}}}-1, 2^{{{}}})".format(i, i - 1)
                                  for i in [10, 15, 20]]
        else:
            input_sizes = [(2 ** 10, 2 ** 9), (2 ** 15, 2 ** 14),
                           (2 ** 20, 2 ** 19)]
            input_size_strings = ["(2^{{{}}}, 2^{{{}}})".format(i, i - 1)
                                  for i in [10, 15, 20]]

    def print_relative_errors_fixed_size(quantity_string, input_size, input_size_string):
        relative_errors = relative_error_function(input_size)
        error5, error50, error95 = relative_errors
        print("{} & ${}$ & {:.{error_precision}f} & {:.{error_precision}f} & {:.{error_precision}f} \\\\".format(
            quantity_string, input_size_string, error5, error50, error95, error_precision=error_precision))

    for i, (input_size, input_size_string) in enumerate(zip(input_sizes, input_size_strings)):
        if i == 0:
            quantity_string = r"\multirow{3}{*}" + \
                "{{{}}}".format(quantity_string)
        else:
            quantity_string = ""
        print_relative_errors_fixed_size(
            quantity_string, input_size, input_size_string)


def print_relative_errors_latex_benchmark(benchmark):

    table_opening_clause = r"""\begin{table}[h]
\caption{Relative errors of inferred bounds with respect to the ground-truth bounds.}
\begin{tabular}{l c r r r}
\toprule
\multirow{2}{*}{Quantity} & \multirow{2}{*}{Input Size} & \multicolumn{3}{c}{Relative Errors of Inferred Boudns}\\
\cmidrule{3-5}
& & 5\textsuperscript{th} percentile & 50\textsuperscript{th} percentile & 95\textsuperscript{th} percentile\\
\midrule"""

    def get_relative_error_function_counter(counter_index):
        def relative_error_function_counter(input_size):
            return calculate_relative_error_counter(benchmark, counter_index, input_size)
        return relative_error_function_counter

    def relative_error_function_total_cost(input_size):
        return calculate_relative_error_total_cost(benchmark, input_size)

    num_counters = num_counters_dict[benchmark]
    print(table_opening_clause)
    for counter_index in range(num_counters):
        quantity_string = "Counter {}".format(counter_index + 1)
        relative_error_function = get_relative_error_function_counter(
            counter_index)
        print_relative_errors_latex_benchmark_error_function(
            benchmark, quantity_string, relative_error_function)
        print(r"\cmidrule{2-5}")

    quantity_string = "Total Cost"
    print_relative_errors_latex_benchmark_error_function(
        benchmark, quantity_string, relative_error_function_total_cost)

    table_closing_clause = r"""\bottomrule
\end{tabular}
\end{table}"""

    print(table_closing_clause)


if __name__ == "__main__":
    list_args = sys.argv
    mode = list_args[1]
    if mode == "time":
        print_out_analysis_time()
    elif mode == "soundness":
        # The soundness version is either "all" or "dominant"
        version = list_args[2]
        print_soundness_proportions(version)
    elif mode == "error":
        benchmark = list_args[2]
        print_relative_errors_benchmark(benchmark)
    elif mode == "error_latex":
        benchmark = list_args[2]
        print_relative_errors_latex_benchmark(benchmark)
    else:
        raise ValueError("Unsupported mode: {}".format(mode))
