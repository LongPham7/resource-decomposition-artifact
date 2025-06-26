import numpy as np
import os
import sys
import subprocess

from benchmark_data.benchmark_parameters import *
from directory_location import get_runtime_cost_data_directory, get_ocaml_benchmark_bin_directory
from json_manipulation import write_input_data_to_json
from runtime_data_generation.graph_generation \
    import generate_exponentially_growing_graphs_multiple_probabilities, format_graph
from runtime_data_generation.array_generation \
    import format_integer_array, format_pair_integer_arrays, \
    generate_exponentially_growing_arrays, \
    generate_exponentially_growing_arrays_varying_max_integers, \
    generate_exponentially_growing_pairs_arrays


# Data collection


num_functions = len(list_benchmarks)


def format_input_data(benchmark, input_data):
    if benchmark in list_benchmarks_list_inputs:
        # Before storing input data in JSON, we need to convert numpy arrays of
        # int64 to a standard Python lis to integers.
        format_function = format_integer_array
    elif benchmark in list_benchmarks_binary_search_trees:
        format_function = format_pair_integer_arrays
    elif benchmark in list_benchmarks_graphs:
        format_function = format_graph
    else:
        def format_function(x): return x
    return [format_function(input) for input in input_data]


def generate_and_save_input_data(benchmark, base, max_input_size, bucket_size, prng):
    # Generate input data
    if benchmark in list_benchmarks_list_inputs:
        # We set the initial size of exponentially growing inputs
        if benchmark in list_benchmarks_with_trees:
            # Benchmarks in this branch include heap_sort and huffman_code. We
            # want input sizes to these benchmarks to be 2^n - 1 for some n. The
            # deduction of one is done inside the function
            # generate_exponentially_growing_arrays. Hence, the initial input
            # size that we should pass to the function
            # generate_exponentially_growing_arrays is 2, rather than 1.
            initial_size = 2
        else:
            initial_size = 1

        num_partitions = 2
        sequence_type = "geometric"
        # Elements in generated arrays are allowed to be repeated.
        replacement = True

        if benchmark == "quicksort_timl":
            # For the benchmark quicksort_timl, we vary not only the array sizes
            # but also the maximum integers appearing in the arrays.
            input_data = generate_exponentially_growing_arrays_varying_max_integers(
                benchmark, initial_size, base, max_input_size, num_partitions,
                sequence_type, bucket_size, prng)
        else:
            input_data = generate_exponentially_growing_arrays(
                benchmark, initial_size, base, max_input_size,
                num_partitions, sequence_type, bucket_size, prng, replacement)
    elif benchmark in list_benchmarks_binary_search_trees:
        initial_size = 2
        if benchmark == "splay_tree":
            num_partitions = 3
        else:
            num_partitions = 1
        sequence_type = "geometric"
        # We want a binary search tree to have the same size as a generated
        # numpy array. So we must ensure the elements in the array are unique.
        # Otherwise, the binary search tree would contain fewer nodes than the
        # elements in the array.
        replacement = False
        input_data = generate_exponentially_growing_pairs_arrays(benchmark, initial_size, base, max_input_size,
                                                                 num_partitions, sequence_type, bucket_size,
                                                                 prng, replacement)
    elif benchmark in list_benchmarks_undirected_graphs:
        initial_size = 4
        num_partitions = 2
        sequence_type = "geometric"
        list_probabilities = [0.1, 0.2, 0.3,
                              0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99]
        directed = False
        input_data = generate_exponentially_growing_graphs_multiple_probabilities(benchmark, initial_size, base, max_input_size,
                                                                                  num_partitions, sequence_type, list_probabilities,
                                                                                  bucket_size, prng, directed)
    elif benchmark in list_benchmarks_directed_graphs:
        initial_size = 4
        num_partitions = 2
        sequence_type = "geometric"
        list_probabilities = [0.1, 0.2, 0.3,
                              0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99]
        directed = True
        input_data = generate_exponentially_growing_graphs_multiple_probabilities(benchmark, initial_size, base, max_input_size,
                                                                                  num_partitions, sequence_type, list_probabilities,
                                                                                  bucket_size, prng, directed)
    else:
        raise ValueError(
            "Cannot generated input data for benchmark: {}".format(benchmark))

    # Format the input data before writing them to JSON
    input_data_reformatted = format_input_data(benchmark, input_data)

    # Write the input data to JSON
    write_input_data_to_json(benchmark, bucket_size, input_data_reformatted)


def collect_runtime_cost_data(benchmark, base, max_list_size, bucket_size):
    print("Start runtime cost data collection: benchmark = {}, base = {}, max_list_size = {}, bucket_size = {}".format(
        benchmark, base, max_list_size, bucket_size))
    seed = 42
    prng = np.random.default_rng(seed)

    # Generate input data and save them in a JSON file
    generate_and_save_input_data(
        benchmark, base, max_list_size, bucket_size, prng)

    # Run the benchmark function on the input data and collect the runtime cost
    # data
    runtime_cost_data_directory = get_runtime_cost_data_directory(
        benchmark, bucket_size)
    input_filepath = os.path.join(
        runtime_cost_data_directory, "input_data.json")
    output_filepath = os.path.join(
        runtime_cost_data_directory, "runtime_cost_data.json")
    raml_benchmark_bin_directory = get_ocaml_benchmark_bin_directory()
    subprocess.run(["dune", "exec", "--", "./main.exe", "data", benchmark,
                   "-i", input_filepath, "-o", output_filepath], cwd=raml_benchmark_bin_directory)

    # Remove the input file to save space before pushing the latest commit to
    # GitHub
    os.remove(input_filepath)
    print("Done with runtime cost data collection: benchmark = {}".format(benchmark))


if __name__ == "__main__":

    def collect_runtime_cost_data_fixed_benchmark_all_bucket_sizes(benchmark):
        if benchmark in list_benchmarks_with_multiple_measurements:
            list_bucket_sizes = [1]
        else:
            # list_bucket_sizes = [1, 2, 5, 10]
            list_bucket_sizes = [1]

        base = 2
        if benchmark in list_benchmarks_one_input_size:
            max_input_size = 1030
        else:
            max_input_size = 260
        # We cannot run the same benchmark functions for different bucket sizes
        # in parallel because OCaml locks the resources for each process.
        # Therefore, we need to run them in sequence.
        for bucket_size in list_bucket_sizes:
            collect_runtime_cost_data(
                benchmark, base, max_input_size, bucket_size)

    list_args = sys.argv
    first_arg = list_args[1]
    if first_arg == "all":
        benchmarks = list_benchmarks
        # benchmarks = ["merge_sort", "quicksort", "bubble_sort",
        #               "heap_sort", "huffman_code",
        #               "balanced_binary_search_tree", "unbalanced_binary_search_tree",
        #               "red_black_tree", "avl_tree", "splay_tree"]
        for benchmark in benchmarks:
            collect_runtime_cost_data_fixed_benchmark_all_bucket_sizes(
                benchmark)
    elif first_arg == "benchmark":
        benchmarks = list_args[2:]
        for benchmark in benchmarks:
            collect_runtime_cost_data_fixed_benchmark_all_bucket_sizes(
                benchmark)
    else:
        raise ValueError("Unknown argument: {}".format(first_arg))
