# Lists of benchmarks with specific properties


# List of benchmarks that (i) take (one-dimensional) integer lists as input and
# (ii) have one input size

list_benchmarks_list_inputs_one_input_size = [
    "merge_sort", "quicksort", "bubble_sort", "heap_sort", "huffman_code"]


# List of benchmarks that (i) take (one-dimensional) integer lists as input and
# (ii) have two input sizes

list_benchmarks_list_inputs_two_input_sizes = ["quicksort_timl"]


# List of benchmarks that take in two inputs: (i) an integer list for
# constructing a tree and (ii) an integer list for successive lookups.

list_benchmarks_binary_search_trees = [
    "balanced_binary_search_tree", "unbalanced_binary_search_tree",
    "red_black_tree", "avl_tree", "splay_tree"]


# List of benchmarks that take in an undirected graph as input

list_benchmarks_undirected_graphs = ["prim_algorithm", "kruskal_algorithm"]


# List of benchmarks that take in a directed graph as input

list_benchmarks_directed_graphs = [
    "dijkstra_algorithm", "bellman_ford_algorithm"]


# List of benchmarks that take integer lists as input

list_benchmarks_list_inputs = list_benchmarks_list_inputs_one_input_size + \
    list_benchmarks_list_inputs_two_input_sizes


# List of benchmarks that have one input size

list_benchmarks_one_input_size = list_benchmarks_list_inputs_one_input_size


# List of benchmarks for graph algorithms

list_benchmarks_graphs = list_benchmarks_undirected_graphs + \
    list_benchmarks_directed_graphs


# List of benchmarks that have two input sizes

list_benchmarks_two_input_sizes = \
    list_benchmarks_list_inputs_two_input_sizes + \
    list_benchmarks_binary_search_trees + list_benchmarks_graphs


# List of all benchmarks

list_benchmarks = list_benchmarks_one_input_size + \
    list_benchmarks_two_input_sizes


# List of benchmarks for resource decomposition that integrates non-data-driven
# analysis (e.g., AARA and TiML) with data-driven analysis

list_benchmarks_data_driven = \
    list_benchmarks_one_input_size + \
    list_benchmarks_list_inputs_two_input_sizes + \
    list_benchmarks_binary_search_trees + ["prim_algorithm"] + \
    list_benchmarks_directed_graphs


# List of benchmarks where the input size must be a power of two, minus one. We
# want this property for a tree-shaped data structure so that it is perfectly
# balanced.

list_benchmarks_with_trees = ["heap_sort", "huffman_code",
                              "balanced_binary_search_tree", "unbalanced_binary_search_tree",
                              "red_black_tree", "avl_tree", "splay_tree"]


# List of benchmarks where a single execution of the function produces a
# collection of multiple counter measurements, instead of a single measurement.
# Examples are heap sort and Huffman codes.

list_benchmarks_with_multiple_measurements = [
    "heap_sort", "huffman_code", "balanced_binary_search_tree",
    "unbalanced_binary_search_tree", "red_black_tree", "avl_tree", "splay_tree",
    "prim_algorithm", "dijkstra_algorithm", "bellman_ford_algorithm",
    "quicksort_timl"]


# A dictionary storing the number of counters in benchmarks

num_counters_dict = {"merge_sort": 1, "quicksort": 1,
                     "bubble_sort": 1, "heap_sort": 2, "huffman_code": 2,
                     "balanced_binary_search_tree": 2, "unbalanced_binary_search_tree": 2,
                     "red_black_tree": 2, "avl_tree": 2, "splay_tree": 2,
                     "prim_algorithm": 3, "dijkstra_algorithm": 3,
                     "bellman_ford_algorithm": 1, "kruskal_algorithm": 2,
                     "quicksort_timl": 1}


# Succinct descriptions of (i) quantities tracked by counters and (ii) overall
# cost. These are used in titles of plots.

counter_tracking_quantities = {
    "merge_sort": ["Recursion Depth", "Total Cost"],
    "quicksort": ["Recursion Depth", "Total Cost"],
    "bubble_sort": ["Recursion Depth", "Total Cost"],
    "heap_sort": ["Heapify in Insert",  "Heapify in Extract-Min", "Heap Sort"],
    "huffman_code": ["Heapify in Insert",  "Heapify in Extract-Min", "Huffman Code"],
    "balanced_binary_search_tree": ["MergeSort", "Lookup", "Balanced BST"],
    "unbalanced_binary_search_tree": ["Insert", "Lookup", "Unbalanced BST"],
    "red_black_tree": ["Insert", "Lookup", "Red-Black Tree"],
    "avl_tree": ["Insert", "Lookup", "AVL Tree"],
    "splay_tree": ["Insert", "Lookup", "Splay Tree"],
    "prim_algorithm": ["Heapify in Extract-Min", "Decrease-Key", "Neighbors Traversal", "Prim"],
    "dijkstra_algorithm": ["Heapify in Extract-Min", "Decrease-Key", "Neighbors Traversal", "Dijkstra"],
    "bellman_ford_algorithm": ["Recursion Depth", "Total Cost"],
    "kruskal_algorithm": ["Union Find", "Merge Sort", "Kruskal"],
    "quicksort_timl": ["Integer Comparison", "Quicksort"]
}


def get_counter_title(benchmark, counter_index):
    return counter_tracking_quantities[benchmark][counter_index]


def get_total_cost_title(benchmark):
    return counter_tracking_quantities[benchmark][-1]


# Axis labels for input sizes. These are used in axis labels of plots.

input_size_axis_labels = {
    "merge_sort": ["Input Size"],
    "quicksort": ["Input Size"],
    "bubble_sort": ["Input Size"],
    "heap_sort": ["Input Size"],
    "huffman_code": ["Input Size"],
    "balanced_binary_search_tree": ["Nodes", "Lookups"],
    "unbalanced_binary_search_tree": ["Nodes", "Lookups"],
    "red_black_tree": ["Nodes", "Lookups"],
    "avl_tree": ["Nodes", "Lookups"],
    "splay_tree": ["Nodes", "Lookups"],
    "prim_algorithm": ["Vertices", "Max Degree"],
    "dijkstra_algorithm": ["Vertices", "Max Degree"],
    "bellman_ford_algorithm": ["Vertices", "Max Degree"],
    "kruskal_algorithm": ["Vertices", "Max Degree"],
    "quicksort_timl": ["Input Size", "Max Integer"]
}


# Axis labels for quantities tracked by counters. These are used in axis labels
# of plots.

counter_axis_labels = {
    "merge_sort": ["Recursion Depth"],
    "quicksort": ["Recursion Depth"],
    "bubble_sort": ["Recursion Depth"],
    "heap_sort": ["Recursion Depth", "Recursion Depth"],
    "huffman_code": ["Recursion Depth", "Recursion Depth"],
    "balanced_binary_search_tree": ["MergeSort", "Lookup"],
    "unbalanced_binary_search_tree": ["Recursion Depth", "Recursion Depth"],
    "red_black_tree": ["Recursion Depth", "Recursion Depth"],
    "avl_tree": ["Recursion Depth", "Recursion Depth"],
    "splay_tree": ["Recursion Depth", "Recursion Depth"],
    "prim_algorithm": ["Recursion Depth", "Recursion Depth", "Recursion Depth"],
    "dijkstra_algorithm": ["Recursion Depth", "Recursion Depth", "Recursion Depth"],
    "bellman_ford_algorithm": ["Recursion Depth", "Recursion Depth"],
    "kruskal_algorithm": ["Cost", "Recursion Depth"],
    "quicksort_timl": ["Cost"]
}


def get_input_size_counter_axis_labels(benchmark, counter_index, sci_exponent=None):
    input_size_labels = input_size_axis_labels[benchmark]
    counter_label = counter_axis_labels[benchmark][counter_index]
    if sci_exponent is not None and sci_exponent != 0:
        counter_label += " ($\\times 10^{{{}}}$)".format(sci_exponent)
    return input_size_labels + [counter_label]


def get_input_size_total_cost_labels(benchmark, sci_exponent=None):
    input_size_labels = input_size_axis_labels[benchmark]
    total_cost_label = "Total Cost"
    if sci_exponent is not None and sci_exponent != 0:
        total_cost_label += " ($\\times 10^{{{}}}$)".format(sci_exponent)
    return input_size_labels + [total_cost_label]


# Exponents used in scientific notation for axis tick labels. These are used in
# plots of runtime data of counter measurements and total costs.


scientific_notation_exponents = {
    "prim_algorithm": [0, 0, 0, 6],
    "dijkstra_algorithm": [0, 0, 0, 6],
    "bellman_ford_algorithm": [0, 8],
    "kruskal_algorithm": [6, 0, 6],
    "quicksort_timl": [0, 6]
}


def get_counter_scientific_notation_exponent(benchmark, counter_index):
    if benchmark in scientific_notation_exponents:
        sci_exponent = scientific_notation_exponents[benchmark][counter_index]
        if sci_exponent == 0:
            return None
        else:
            return sci_exponent
    else:
        return None


def get_total_cost_scientific_notation_exponent(benchmark):
    if benchmark in scientific_notation_exponents:
        sci_exponent = scientific_notation_exponents[benchmark][-1]
        if sci_exponent == 0:
            return None
        else:
            return sci_exponent
    else:
        return None
