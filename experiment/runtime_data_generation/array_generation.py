import numpy as np

from benchmark_data.benchmark_parameters import list_benchmarks_with_trees


# Generate random numpy arrays. The Boolean argument replacement specifies
# whether elements in a generated array should be unique or not. If replacement
# = True, we draw integers with replacement, so the elements in a generated
# array can be repeated. Otherwise, if replacement = False, the array elements
# are unique.


def generate_random_array(list_size, prng, replacement, max_integer=None):
    # We use a pseudo random number generator (prng) provided by numpy. This
    # function returns a numpy array, instead of a Python list. So make sure to
    # use functions for numpy arrays.

    if max_integer is None:
        max_integer = 2**32
        if replacement:
            return prng.integers(low=0, high=max_integer, size=list_size)
        else:
            return prng.choice(max_integer, size=list_size, replace=False)
    else:
        # If the maximum integer is specified, we must allow for replacement in
        # sampling. This is because the user-specified maximum integer may be
        # smaller than the array size.
        assert replacement == True, "Replacement must be False if you specify the maximum integer"
        generated_array = prng.integers(
            low=0, high=max_integer, size=list_size)

        # Pick a random array index and insert max_integer at that index. We
        # thereby ensure that the maximum integer in the array is equal to
        # max_integer.
        chosen_index = prng.integers(list_size)
        generated_array[chosen_index] = max_integer
        return generated_array


def generate_random_arrays(num_lists, max_list_size, prng, replacement):
    list_sizes = prng.choices(range(1, max_list_size+1), k=num_lists)
    result = []
    for i in range(0, num_lists):
        list_size = list_sizes[i]
        generated_array = generate_random_array(list_size, prng, replacement)
        result.append(generated_array)
    return result


def generate_exponentially_growing_sizes(benchmark, initial_size, base, max_size,
                                         num_partitions, sequence_type):
    current_size = initial_size
    list_sizes = []
    while current_size <= max_size:
        if benchmark in list_benchmarks_with_trees:
            # If the benchmark uses a tree, we subtract one from the variable
            # current_size. This is useful because when when current_size is a
            # power of two, current_size_adjusted is a power of two minus one,
            # which makes a binary tree complete (i.e., the bottom layer is
            # filled).
            current_size_adjusted = int(current_size) - 1
        else:
            current_size_adjusted = int(current_size)
        list_sizes.append(current_size_adjusted)

        if sequence_type == "geometric":
            # We create more sizes between current_size and current_size * base.
            # Specifically, we divide the interval [current_size, current_size *
            # base] into num_partitions sub-intervals.
            root_of_base = np.power(base, 1 / num_partitions)
            current_size_intermediate = current_size * root_of_base
            count = 1
            while count < num_partitions and current_size_intermediate <= max_size:
                list_sizes.append(int(current_size_intermediate))
                current_size_intermediate *= root_of_base
                count += 1
        else:
            division_of_base = current_size / num_partitions
            current_size_intermediate = current_size + division_of_base
            count = 1
            while count < num_partitions and current_size_intermediate <= max_size:
                list_sizes.append(int(current_size_intermediate))
                current_size_intermediate += division_of_base
                count += 1

        current_size *= base

    # Remove all duplicates
    list_sizes_deduplicated = list(set(list_sizes))
    return list_sizes_deduplicated


def generate_exponentially_growing_arrays(benchmark, initial_size, base, max_size,
                                          num_partitions, sequence_type, bucket_size,
                                          prng, replacement):
    list_sizes = \
        generate_exponentially_growing_sizes(benchmark, initial_size, base,
                                             max_size, num_partitions,
                                             sequence_type)
    result = []
    for size in list_sizes:
        for _ in range(bucket_size):
            generated_array = generate_random_array(size, prng, replacement)
            result.append(generated_array)
    return result


def generate_exponentially_growing_arrays_varying_max_integers(benchmark, initial_size, base, max_size,
                                                               num_partitions, sequence_type, bucket_size,
                                                               prng):
    list_sizes = \
        generate_exponentially_growing_sizes(benchmark, initial_size, base,
                                             max_size, num_partitions,
                                             sequence_type)

    list_sizes_max_integers = [(size1, size2 * 4)
                               for size1 in list_sizes for size2 in list_sizes]

    # We should allow for replacement in sampling because the maximum integer
    # allowed in an array may be smaller than the size of the array. Hence, by
    # the pigeonhole principle, we must allow for replacement.
    replacement = True
    result = []
    for size, max_integer in list_sizes_max_integers:
        for _ in range(bucket_size):
            generated_array = generate_random_array(
                size, prng, replacement, max_integer)
            result.append(generated_array)
    return result


def generate_exponentially_growing_pairs_arrays(benchmark, initial_size, base, max_size,
                                                num_partitions, sequence_type, bucket_size,
                                                prng, replacement):
    list_sizes = \
        generate_exponentially_growing_sizes(benchmark, initial_size, base,
                                             max_size, num_partitions,
                                             sequence_type)
    # Cartesian product of the list of sizes with itself
    list_pair_sizes = [(size1, size2)
                       for size1 in list_sizes for size2 in list_sizes]

    result = []
    for size1, size2 in list_pair_sizes:
        for _ in range(bucket_size):
            array_insert = generate_random_array(size1, prng, replacement)
            array_lookup = prng.choice(array_insert, size=size2, replace=True)
            result.append((array_insert, array_lookup))
    return result


# Format input data before they are written to JSON


def format_integer_array(input_array):
    return [int(x) for x in input_array]


def format_pair_integer_arrays(pair_arrays):
    array_insert, array_lookup = pair_arrays
    list_insert = format_integer_array(array_insert)
    list_lookup = format_integer_array(array_lookup)
    return {"tree": list_insert,
            "lookup": list_lookup}
