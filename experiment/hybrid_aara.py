import numpy as np
import os
import subprocess
import json

from runtime_data_generation.runtime_data_generation import generate_and_save_input_data
from directory_location import get_runtime_cost_data_directory


# Perform Hybrid AARA. A drawback is that it is very slow, particularly the data
# collection phase.


def convert_python_graph_to_ocaml_graph(graph):

    def convert_python_list_neighbors(list_neighbors):
        result_string = "["
        for neighbor_dict in list_neighbors:
            neighbor = neighbor_dict["neighbor"]
            weight = neighbor_dict["weight"]
            result_string += "({}, {}); ".format(neighbor, float(weight))
        return result_string + "]"

    result_string = "["
    for node_neighbors_dict in graph:
        node = node_neighbors_dict["node"]
        list_neighbors = node_neighbors_dict["neighbors"]
        result_string += "({}, {}); ".format(node,
                                             convert_python_list_neighbors(list_neighbors))
    return result_string + "]"


def append_list_graphs_to_ocaml_source_file(list_graphs):
    opening_string = "\n;;\nlet list_adjacency_lists = \n"
    closing_string = "in map bellman_ford_algorithm list_adjacency_lists\n"
    cumulative_string = "[\n"
    for graph in list_graphs:
        cumulative_string += convert_python_graph_to_ocaml_graph(graph) + ";\n"
    cumulative_string += "]\n"

    bin_directory = os.path.expanduser(os.path.join(
        "~", "experiments", "raml_code", "model_averaging", "bin"))
    hybrid_aara_bin_directory = os.path.join(
        bin_directory, "data_driven_analysis", "bellman_ford_algorithm")

    bellman_ford_algorithm_original_ocaml_filepath = os.path.join(
        hybrid_aara_bin_directory, "bellman_ford_algorithm_original.ml")
    bellman_ford_algorithm_ocaml_filepath = os.path.join(
        hybrid_aara_bin_directory, "bellman_ford_algorithm.ml")

    # Copy the original OCaml source file to a different file
    subprocess.run(["cp", bellman_ford_algorithm_original_ocaml_filepath,
                   bellman_ford_algorithm_ocaml_filepath])

    with open(bellman_ford_algorithm_ocaml_filepath, "a") as f:
        f.write(opening_string + cumulative_string + closing_string)


def prepare_bellman_ford_algorithm():
    benchmark = "bellman_ford_algorithm"
    base = 2
    max_list_size = 200
    bucket_size = 1
    seed = 42
    prng = np.random.default_rng(seed)

    # Generate input data, save them in a JSON file, and read it
    generate_and_save_input_data(
        benchmark, base, max_list_size, bucket_size, prng)
    runtime_cost_data_directory = get_runtime_cost_data_directory(
        benchmark, bucket_size)
    input_filepath = os.path.join(
        runtime_cost_data_directory, "input_data.json")
    with open(input_filepath, "r") as f:
        dict = json.load(f)
        list_graphs = dict["input_data"]

    # Reformat the input list of graphs and append it to the OCaml source file
    append_list_graphs_to_ocaml_source_file(
        list_graphs)

    # Delete the input JSON file once we no longer need it
    os.remove(input_filepath)


def run_hybrid_aara():
    bin_directory = os.path.expanduser(os.path.join(
        "~", "experiments", "raml_code", "model_averaging", "bin"))
    hybrid_aara_bin_directory = os.path.join(
        bin_directory, "data_driven_analysis", "bellman_ford_algorithm")
    bellman_ford_algorithm_ocaml_filepath = os.path.join(
        hybrid_aara_bin_directory, "bellman_ford_algorithm.ml")

    # Run Hybrid AARA
    raml_directory = os.path.expanduser(os.path.join(
        "~", "hybrid_aara", "raml"))
    bellman_ford_algorithm_function = "bellman_ford_algorithm"
    degree = 3
    config_filepath = os.path.expanduser(os.path.join(
        "~", "experiments", "raml_code", "model_averaging", "bin", "data_driven_analysis", "bayespc.json"))
    subprocess.run(["./main", "stat_analyze", "ticks", str(degree), "-m", bellman_ford_algorithm_ocaml_filepath,
                   bellman_ford_algorithm_function, "-config", config_filepath], cwd=raml_directory)

    # subprocess.run(["./main", "generate",
    #                bellman_ford_algorithm_ocaml_filepath], cwd=raml_directory)


if __name__ == "__main__":
    prepare_bellman_ford_algorithm()
    run_hybrid_aara()
