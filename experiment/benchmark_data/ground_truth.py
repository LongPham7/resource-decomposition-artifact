import numpy as np


# The symbolic cost bounds of counter-instrumented code inferred by RaML. The
# bounds are parametric in both the original input size and the counter
# variable(s).


def merge_sort_bound(size, counters):
    counter = counters[0]
    return 1 + 3.5 * size * counter


def quicksort_bound(size, counters):
    counter = counters[0]
    return 1 + 0.5 * counter + 3 * size * counter


def bubble_sort_bound(size, counters):
    counter = counters[0]
    return 1 + 2 * counter + size * counter


def heap_sort_bound(size, counters):
    counter1, counter2 = counters
    return 7 + 4 * size + size * counter2 + size * counter1


def huffman_code_bound(size, counters):
    counter1, counter2 = counters
    return 7 + 9 * size + 10 * size * counter2 + 5 * size * counter1


def balanced_binary_search_tree_bound(sizes, counters):
    size1, size2 = sizes
    counter1, counter2 = counters
    return 6 + size2 + size2 * counter2 + 3.5 * size1 * counter1


def unbalanced_binary_search_tree_bound(sizes, counters):
    size1, size2 = sizes
    counter1, counter2 = counters
    return 3 + counter2 * size2 + size2 + size1 + size1 * counter1


def red_black_tree_bound(sizes, counters):
    size1, size2 = sizes
    counter1, counter2 = counters
    return 3 + counter2 * size2 + size2 + size1 + 3 * size1 * counter1


def avl_tree_bound(sizes, counters):
    size1, size2 = sizes
    counter1, counter2 = counters
    return 3 + counter2 * size2 + size2 + 13 * size1 * counter1


def splay_tree_bound(sizes, counters):
    size1, size2 = sizes
    counter1, counter2 = counters
    return 4 + 2 * counter2 * size2 + size2 + 2 * size1 + 2 * size1 * counter1


def prim_algorithm_bound(sizes, counters):
    vertices, _ = sizes
    counter1, counter2, counter3 = counters
    return 7 + 4 * counter2 * counter3 * vertices + 3 * counter3 * \
        vertices + 4 * vertices + 2 * vertices * counter1


def dijkstra_algorithm_bound(sizes, counters):
    vertices, _ = sizes
    counter1, counter2, counter3 = counters
    return 7 + 4 * counter2 * counter3 * vertices + 4 * counter3 * \
        vertices + 3 * vertices + 7 * vertices * counter1


def bellman_ford_algorithm_bound(sizes, counters):
    counter = counters[0]
    vertices, degree = sizes
    return 4 + 4 * vertices * degree * counter + 2 * \
        vertices + 3 * vertices * counter + 2 * counter


def kruskal_algorithm_bound(sizes, counters):
    vertices, max_degree = sizes
    counter1, counter2 = counters
    return 7 + max_degree * vertices + 3.5 * max_degree * vertices * counter2 + \
        3.5 * vertices + 1.5 * counter1


def quicksort_timl_bound(sizes, counters):
    counter = counters[0]
    n, _ = sizes
    return n * (n + 1) * (counter + 2) + 2 * n


benchmark_bound_dict = \
    {"merge_sort": merge_sort_bound, "quicksort": quicksort_bound,
     "bubble_sort": bubble_sort_bound, "heap_sort": heap_sort_bound,
     "huffman_code": huffman_code_bound,
     "balanced_binary_search_tree": balanced_binary_search_tree_bound,
     "unbalanced_binary_search_tree": unbalanced_binary_search_tree_bound,
     "red_black_tree": red_black_tree_bound, "avl_tree": avl_tree_bound,
     "splay_tree": splay_tree_bound, "prim_algorithm": prim_algorithm_bound,
     "dijkstra_algorithm": dijkstra_algorithm_bound,
     "bellman_ford_algorithm": bellman_ford_algorithm_bound,
     "kruskal_algorithm": kruskal_algorithm_bound,
     "quicksort_timl": quicksort_timl_bound}


# Soundness of cost bounds


def is_sound_asymptotically(benchmark, counter_index, coefficients):
    is_logarithmic = 0.99 < coefficients[6] < 1.01
    is_linear = not is_logarithmic

    benchmarks_logarithmic = ["merge_sort", "heap_sort", "huffman_code",
                              "balanced_binary_search_tree", "red_black_tree",
                              "avl_tree", "quicksort_timl"]
    benchmarks_linear = ["quicksort", "bubble_sort",
                         "unbalanced_binary_search_tree", "splay_tree",
                         "bellman_ford_algorithm"]
    benchmarks_both = ["prim_algorithm", "dijkstra_algorithm"]

    if benchmark in benchmarks_logarithmic:
        return is_logarithmic
    elif benchmark in benchmarks_linear:
        return is_linear
    elif benchmark in benchmarks_both:
        if counter_index in [0, 1]:
            return is_logarithmic
        else:
            return is_linear
    else:
        raise ValueError("The given benchmark is invalid")


def is_sound_asymptotically_mixing_probability_logarithmic(benchmark, counter_index, p):
    probability_logarithmic = p
    probability_linear = 1 - p

    benchmarks_logarithmic = ["merge_sort", "heap_sort", "huffman_code",
                              "balanced_binary_search_tree", "red_black_tree",
                              "avl_tree", "quicksort_timl"]
    benchmarks_linear = ["quicksort", "bubble_sort",
                         "unbalanced_binary_search_tree", "splay_tree",
                         "bellman_ford_algorithm"]
    benchmarks_both = ["prim_algorithm", "dijkstra_algorithm"]

    if benchmark in benchmarks_logarithmic:
        return probability_logarithmic
    elif benchmark in benchmarks_linear:
        return probability_linear
    elif benchmark in benchmarks_both:
        if counter_index in [0, 1]:
            return probability_logarithmic
        else:
            return probability_linear
    else:
        raise ValueError("The given benchmark is invalid")


# Check if a given linear bound x0 + x1 * n is sound with respect to the
# target linear bound c0 + c1 * n.


def is_sound_linear_bound(coefficients_linear, c0, c1, version):
    if version == "dominant":
        return c1 <= coefficients_linear[1]
    else:
        return c0 <= coefficients_linear[0] and c1 <= coefficients_linear[1]


# Check if a given logarithmic bound x0 + x1 * ln(1 + x2 + x3 * n) is sound with
# respect to the target logarithmic bound c0 + c1 * log2(1 + n). Note that the
# target bound uses logarithm with base 2.


def is_sound_log2(coefficients_log, c0, c1, version):
    # The coefficients in the logarithmic model inferred by Stan use the natural
    # logarithm. That is, the symbolic cost bound for the logarithmic model
    # takes the form x0 + x1 * ln(1 + x2 + x3 * n). On the other hand, in many
    # algorithms, their theoretical complexity is expressed using logarithm with
    # base 2. To convert the base of logarithm, we calculate the inverse of
    # ln(2).
    inverse_ln_2 = 1 / np.log(2)

    # The constant factor in the logarithmic bound that corresponds to c0.
    constant_coefficient = coefficients_log[0] + \
        coefficients_log[1] * np.log(coefficients_log[3])

    # The dominant factor in the logarithmic bound that corresponds to c1.
    dominant_factor = coefficients_log[1]

    if version == "dominant":
        return c1 * inverse_ln_2 <= dominant_factor
    else:
        return c0 <= constant_coefficient and c1 * inverse_ln_2 <= dominant_factor


# Check if a bound c0 + c1 * ln(1 + c2 + c3 * n) is sound with respect to
# log2(1 + n)


def is_sound_plus_one_log2(coefficients_log, version):
    inverse_ln_2 = 1 / np.log(2)
    multiplier_size = np.exp(
        coefficients_log[0] / coefficients_log[1]) * coefficients_log[3]
    dominant_factor = coefficients_log[1]

    if version == "dominant":
        return inverse_ln_2 <= dominant_factor
    else:
        return 1 <= multiplier_size and inverse_ln_2 <= dominant_factor


# Check if a bound c0 + c1 * ln(1 + c2 + c3 * n) is sound with respect to
# log_phi(sqrt(5) * F + 1).


def is_sound_avl_tree(coefficients_log, version):
    # Golden ratio. It is used in the complexity analysis of the AVL tree.
    # If an AVL tree has a height n, then its number of nodes must be at least
    # F_n, which is the n-th Fibonacci number. The n-th Fibonacci number
    # is:
    #   F_n = (phi^n - psi^n) / sqrt(5),
    # where phi = (1 + sqrt(5)) / 2 (i.e., the golden ratio) and psi = 1 - phi.
    # Let F denote the number of nodes in a given AVL tree. Our goal is to
    # estimate the height n of this AVL tree. Because |psi| < 1, we have -1 <
    # psi^n < 1. Therefore, we obtain:
    #  F_n = (phi^n + psi^n) / sqrt(5) <= F
    #  phi^n + psi^n <= sqrt(5) * F
    #  phi^n <= sqrt(5) * F + 1
    #  n <= log_phi(sqrt(5) * F + 1).
    # Therefore, given an arbitrary number F, the corresponding n is bounded
    # above by
    #   n <= log_phi(sqrt(5) * F + 1).
    phi = (1 + np.sqrt(5)) / 2
    inverse_ln_phi = 1 / np.log(phi)

    multiplier_size = np.exp(
        coefficients_log[0] / coefficients_log[1]) * coefficients_log[3]
    dominant_factor = coefficients_log[1]
    if version == "dominant":
        return inverse_ln_phi <= dominant_factor
    else:
        return np.sqrt(5) <= multiplier_size and inverse_ln_phi <= dominant_factor


def is_sound_bound(benchmark, counter_index, coefficients, version):
    assert (version == "dominant" or version ==
            "all"), "Invalid version of soundness"

    # Let the logarithmic cost bound be
    #   c0 + c1 * ln(1 + c2 + c3 * n)
    # and the linear cost bound be
    #   c0 + c1 * n.
    # We have two versions for the soundness: (i) dominant and (ii) all. In the
    # dominant version, we only check whether the most dominant factor in
    # the cost bound is sufficiently large. Here, the most dominant factor
    # refers to the coefficient c1 in both the logarithmic and linear cost
    # bounds. Otherwise, if the version is all, we consider all coefficients.
    # The logarithmic cost bound is tricky to check because its representation
    # is not canonical. For instance, we can rewrite the logarithmic bound as
    #  c0 + c1 * ln(c3) + ln(epsilon + n),
    # for some positive constant epsilon. As a result, the constant coefficient
    # is actually not just c0 but can be c0 + c1 * ln(c3), which is smaller or
    # larger than c0 depending on the value of c3.

    coefficients_logarithmic = coefficients[0:4]
    coefficients_linear = coefficients[4:6]

    if benchmark == "merge_sort":
        # In merge sort, the counter tracks its recursion depth. When the
        # input size is 1, the recursion depth is 1, rather than 0. This is
        # why we need to add 1 to log2(n) in the ground-truth recursion
        # depth.
        has_correct_coefficients = is_sound_log2(
            coefficients_logarithmic, 1, 1, version)
    elif benchmark == "quicksort":
        # In quicksort, the counter tracks its recursion depth. When the input
        # size is 1, the recursion depth is 1 because this input is handled by
        # the base case of quicksort, rather than its recursive case. Therefore,
        # the ground-truth recursion depth is n, rather than 1 + n.
        has_correct_coefficients = is_sound_linear_bound(
            coefficients_linear, 0, 1, version)
    elif benchmark == "bubble_sort":
        # In bubble sort, the counter tracks its recursion depth. If the input
        # list is already sorted, the recursion depth is 1. In the worst case,
        # we need to move a list element (n - 1) times (e.g., moving the last
        # element all the way to the beginning of the list). Therefore, we need
        # a total of 1 + (n - 1) = n many recursive calls.
        has_correct_coefficients = is_sound_linear_bound(
            coefficients_linear, 0, 1, version)
    elif benchmark == "heap_sort":
        has_correct_coefficients = is_sound_log2(
            coefficients_logarithmic, 0, 1, version)
    elif benchmark == "huffman_code":
        has_correct_coefficients = is_sound_log2(
            coefficients_logarithmic, 0, 1, version)
    elif benchmark == "balanced_binary_search_tree":
        # Counter 0 in the benchmark balanced_binary_search_tree tracks the
        # recursion depth of merge sort. Hence, its ground truth is 1 +
        # log2(n).
        #
        # Counter 1 tracks the recursion depth of the lookup operation in a
        # balanced binary search tree. The target of a the lookup is always
        # present in the tree. That is, whenever we reach a singleton tree,
        # the root should be the target element. Hence, the ground-truth
        # recursion depth is log2(1 + n). Conversely, if the target element
        # does not exist in the tree, we will reach the base case of Leaf in
        # the lookup function. As a result, we need one one more recursive
        # call, yielding the ground-truth recursion of 1 + log2(1 + n).
        if counter_index == 0:
            has_correct_coefficients = is_sound_log2(
                coefficients_logarithmic, 1, 1, version)
        else:
            has_correct_coefficients = is_sound_log2(
                coefficients_logarithmic, 0, 1, version)
    elif benchmark == "unbalanced_binary_search_tree":
        # In the benchmark unbalanced_binary_search_tree, counter 0 tracks the
        # recursion depth of an insertion to a (possibly unbalanced) binary
        # search tree. The worst-case recursion depth of an insertion to a tree
        # with n nodes is n + 1. However, because we construct a tree by n
        # insertions, the last insertion inserts an element to a tree with (n -
        # 1) nodes. Therefore, the ground-truth recursion depth for counter 0 is
        # still n, rather than 1 + n. Hence, both insertions and lookups have
        # the same ground-truth recursion depth.
        has_correct_coefficients = is_sound_linear_bound(
            coefficients_linear, 0, 1, version)
    elif benchmark == "red_black_tree":
        # In the benchmark red_black_tree, counter 0 tracks the recursion depth
        # of the insertion to a red-black tree. We build a red-black tree by
        # performing n insertions successively. In the last insertion, the
        # recursion depth is equal to the recursion-depth of the lookup for the
        # same node after the insertion. Therefore, both insertions and lookups
        # have the same ground-truth recursion depth.
        has_correct_coefficients = is_sound_log2(
            coefficients_logarithmic, 0, 2, version)
    elif benchmark == "avl_tree":
        has_correct_coefficients = is_sound_avl_tree(
            coefficients_logarithmic, version)
    elif benchmark == "splay_tree":
        has_correct_coefficients = 1 <= coefficients_linear[1]
    elif benchmark == "prim_algorithm":
        if counter_index in [0, 1]:
            has_correct_coefficients = is_sound_log2(
                coefficients_logarithmic, 0, 1, version)
        else:
            has_correct_coefficients = is_sound_linear_bound(
                coefficients_linear, 0, 1, version)
    elif benchmark == "dijkstra_algorithm":
        if counter_index in [0, 1]:
            has_correct_coefficients = is_sound_log2(
                coefficients_logarithmic, 0, 1, version)
        else:
            has_correct_coefficients = is_sound_linear_bound(
                coefficients_linear, 0, 1, version)
    elif benchmark == "bellman_ford_algorithm":
        has_correct_coefficients = is_sound_linear_bound(
            coefficients_linear, 0, 1, version)
    elif benchmark == "quicksort_timl":
        has_correct_coefficients = is_sound_log2(
            coefficients_logarithmic, 1, 1, version)
    else:
        raise ValueError("The given benchmark is invalid")

    has_correct_asymptotic_complexity = is_sound_asymptotically(
        benchmark, counter_index, coefficients)
    return has_correct_asymptotic_complexity and has_correct_coefficients


# Inference result of Conventional AARA for the non-instrumented code of
# benchmarks


def conventional_aara_result(benchmark):
    benchmarks_correct = ["quicksort",
                          "unbalanced_binary_search_tree", "splay_tree"]
    benchmarks_wrong_degree = ["merge_sort",
                               "balanced_binary_search_tree", "red_black_tree"]
    benchmarks_untypable = ["bubble_sort", "heap_sort", "huffman_code",
                            "avl_tree", "prim_algorithm", "dijkstra_algorithm", "bellman_ford_algorithm"]
    if benchmark in benchmarks_correct:
        return "Correct"
    elif benchmark in benchmarks_wrong_degree:
        return "Wrong Degree"
    elif benchmark in benchmarks_untypable:
        return "Untypable"
    else:
        return "N/A"


# Ground-truth cost bounds


def merge_sort_recursion_depth(n):
    return np.ceil(np.log2(n)) + 1


def merge_sort_recursion_depth_first_input(sizes):
    n, _ = sizes
    return np.ceil(np.log2(n)) + 1


def merge_sort_recursion_depth_flattened_nested_list(graph):
    num_vertices, max_degree = graph
    return np.ceil(np.log2(num_vertices * max_degree)) + 1


def balanced_binary_tree_height(n):
    return np.ceil(np.log2(n + 1))


def balanced_binary_tree_height_first_input(sizes):
    n, _ = sizes
    return np.ceil(np.log2(n + 1))


def red_black_tree_height_first_input(sizes):
    n, _ = sizes
    return np.ceil(2 * np.log2(n + 1))


def identity(n):
    return n


def identity_first_input(sizes):
    n, _ = sizes
    return n


def inverse_fibonacci_first_input(sizes):
    n, _ = sizes
    # The closed-form formula for Fibonacci numbers is
    #   F_n = (phi^n - psi^n) / sqrt(5),
    # where phi = (1 + sqrt(5)) / 2 (i.e., the golden ratio) and psi = 1 - phi.
    # Hence, phi is around 1.618 and psi is around -0.618.
    phi = (1 + np.sqrt(5)) / 2  # Golden ratio
    inverse_fibonacci = np.log(np.sqrt(5) * n + 1) / np.log(phi)
    return np.floor(inverse_fibonacci)


def identity_second_input(sizes):
    _, n = sizes
    return n


def bellman_ford_rounds(graph_size):
    vertices, _ = graph_size
    return vertices - 1


# This approximation of the inverse Ackermann function is taken from Section
# 19.4 of the textbook "Introduction to Algorithms" 4th edition by Cormen et al.


def inverse_ackermann_function(n):
    if 0 <= n <= 2:
        return 0
    elif n == 3:
        return 1
    elif 4 <= n <= 7:
        return 2
    elif 8 <= n <= 2047:
        return 3
    else:
        # Unless the input n is astronomically large (i.e., larger than A_4(1)),
        # the output of the inverse Ackermann function is 4.
        return 4


def kruskal_cost_union_find(graph):
    num_vertices, max_degree = graph

    # Kruskal's algorithm takes as input an undirected graph. Hence, every edge
    # appears twice in the adjacency list. Nonetheless, we do not remove
    # duplicated edges after we flatten an adjacency list (i.e., two-dimensional
    # table or a nested list of edges). Therefore, the variable num_edged
    # defined below, which is the maximum number of times we query the
    # union-find data structure, is given by num_vertices * max_degree, without
    # dividing it by 2.
    num_edges = num_vertices * max_degree

    # These amortized costs of the union-find operations come from teh paper
    # "Verifying the Correctness and Amortized Complexity of a Union-Find
    # Implementation in Separation Logic with Time Credits" by Chargueraud and
    # Pottier.
    amortized_cost_make = 3
    amortized_cost_eq = 4 * \
        np.vectorize(inverse_ackermann_function)(num_vertices) + 9
    amortized_cost_union = 4 * \
        np.vectorize(inverse_ackermann_function)(num_vertices) + 12

    total_cost = num_vertices * amortized_cost_make + num_edges * \
        amortized_cost_eq + num_vertices * amortized_cost_union
    return total_cost


def quicksort_timl_cost_comparison(sizes):
    _, n = sizes
    # Given a natural number n, its binary encoding has np.ceil(np.log2(n + 1))
    # many bits. The cost of comparing two bit vectors is given by the number of
    # function calls, and this is equal to one plus the number of bits.
    return np.ceil(np.log2(n + 1)) + 1


def get_ground_truth_bound(benchmark, counter_index):
    if benchmark == "merge_sort":
        return merge_sort_recursion_depth
    elif benchmark == "quicksort":
        return identity
    elif benchmark == "bubble_sort":
        return identity
    elif benchmark == "heap_sort":
        return balanced_binary_tree_height
    elif benchmark == "huffman_code":
        return balanced_binary_tree_height
    elif benchmark == "balanced_binary_search_tree":
        if counter_index == 0:
            return merge_sort_recursion_depth_first_input
        else:
            return balanced_binary_tree_height_first_input
    elif benchmark == "unbalanced_binary_search_tree":
        return identity_first_input
    elif benchmark == "red_black_tree":
        return red_black_tree_height_first_input
    elif benchmark == "avl_tree":
        return inverse_fibonacci_first_input
    elif benchmark == "splay_tree":
        return identity_first_input
    elif benchmark == "prim_algorithm":
        if counter_index in [0, 1]:
            return balanced_binary_tree_height_first_input
        else:
            return identity_second_input
    elif benchmark == "dijkstra_algorithm":
        if counter_index in [0, 1]:
            return balanced_binary_tree_height_first_input
        else:
            return identity_second_input
    elif benchmark == "bellman_ford_algorithm":
        return bellman_ford_rounds
    elif benchmark == "kruskal_algorithm":
        if counter_index == 0:
            return kruskal_cost_union_find
        else:
            return merge_sort_recursion_depth_flattened_nested_list
    elif benchmark == "quicksort_timl":
        return quicksort_timl_cost_comparison
    else:
        return None
