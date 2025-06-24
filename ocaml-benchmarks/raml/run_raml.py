import os
import sys
import subprocess

# List of all benchmarks
list_benchmarks = ["merge_sort", "quicksort", "bubble_sort",
                   "heap_sort", "huffman_code",
                   "unbalanced_binary_search_tree", "balanced_binary_search_tree",
                   "red_black_tree", "avl_tree", "splay_tree",
                   "prim_algorithm", "dijkstra_algorithm", "bellman_ford_algorithm", "kruskal_algorithm"]

function_name_dict = {
    "merge_sort": "merge_sort",
    "quicksort": "quicksort",
    "bubble_sort": "bubble_sort",
    "heap_sort": "heap_sort",
    "huffman_code": "huffman_code",
    "unbalanced_binary_search_tree": "unbalanced_binary_search_tree_main",
    "balanced_binary_search_tree": "balanced_binary_search_tree_main",
    "red_black_tree": "red_black_tree_main",
    "avl_tree": "avl_tree_main",
    "splay_tree": "splay_tree_main",
    "prim_algorithm": "prim_algorithm",
    "dijkstra_algorithm": "dijkstra_algorithm",
    "bellman_ford_algorithm": "bellman_ford_algorithm",
    "kruskal_algorithm": "kruskal_algorithm"
}

degree_dict = {
    "merge_sort": 2,
    "quicksort": 2,
    "bubble_sort": 2,
    "heap_sort": 2,
    "huffman_code": 2,
    "unbalanced_binary_search_tree": 2,
    "balanced_binary_search_tree": 2,
    "red_black_tree": 2,
    "avl_tree": 2,
    "splay_tree": 2,
    "prim_algorithm": 3,
    "dijkstra_algorithm": 3,
    "bellman_ford_algorithm": 3,
    "kruskal_algorithm": 2
}


def get_benchmark_directory_name(benchmark):
    if benchmark == "unbalanced_binary_search_tree" or benchmark == "balanced_binary_search_tree":
        return "binary_search_tree"
    else:
        return benchmark


def run_raml(benchmark, version):
    assert (benchmark in list_benchmarks)
    print("Start RaML for benchmark {} version {}".format(benchmark, version))

    raml_filepath = os.path.expanduser(
        os.path.join("/home", "raml", "main"))

    benchmark_directory_name = get_benchmark_directory_name(benchmark)
    benchmark_directory = os.path.expanduser(
        os.path.join("/home", "ocaml-benchmarks", "lib", benchmark_directory_name))
    if version == "standard":
        benchmark_filename = "{}.ml".format(benchmark_directory_name)
    elif version == "counter":
        benchmark_filename = "{}_counter.ml".format(benchmark_directory_name)
    else:
        raise ValueError("Invalid version: {}".format(version))
    benchmark_filepath = os.path.join(benchmark_directory, benchmark_filename)

    degree = degree_dict[benchmark]
    function_name = function_name_dict[benchmark]

    output_directory = os.path.expanduser(
        os.path.join("/home", "ocaml-benchmarks", "raml", "output", benchmark))
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)
    output_filepath = os.path.join(
        output_directory, "raml_output_{}.txt".format(version))

    # Run RaML and save the output to a file
    with open(output_filepath, "w") as f:
        subprocess.run([raml_filepath, "analyze", "ticks", str(degree), "-m", benchmark_filepath,
                        function_name], stdout=f)

    # Print out RaML's output, which has just been saved to a file
    subprocess.run(["cat", output_filepath])

    print("RaML completed for benchmark {} version {}".format(benchmark, version))


if __name__ == "__main__":
    list_args = sys.argv
    mode = list_args[1]
    if mode == "all":
        version = list_args[2]
        for benchmark in list_benchmarks:
            if version in ["standard", "both"]:
                run_raml(benchmark, "standard")
            if version in ["counter", "both"]:
                run_raml(benchmark, "counter")
    elif mode == "benchmark":
        benchmark = list_args[2]
        version = list_args[3]
        if version in ["standard", "both"]:
            run_raml(benchmark, "standard")
        if version in ["counter", "both"]:
            run_raml(benchmark, "counter")
    else:
        raise ValueError("Unsupported mode: {}".format(mode))
