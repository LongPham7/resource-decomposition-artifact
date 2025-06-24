import numpy as np
import networkx as nx


# Generate random graphs


def generate_undirected_weighted_graph(num_nodes, p, min_weight, max_weight, prng):
    G = nx.erdos_renyi_graph(n=num_nodes, p=p, seed=prng, directed=False)
    num_edges = G.number_of_edges()
    list_weights = prng.integers(
        low=min_weight, high=max_weight, size=num_edges)
    for i, (u, v) in enumerate(G.edges):
        G.edges[u, v]["weight"] = list_weights[i]
    return G


def generate_directed_weighted_graph(num_nodes, p, min_weight, max_weight, prng):
    G = nx.erdos_renyi_graph(n=num_nodes, p=p, seed=prng, directed=True)
    num_edges = G.number_of_edges()
    list_weights = prng.integers(
        low=min_weight, high=max_weight, size=num_edges)
    for i, (u, v) in enumerate(G.edges):
        G.edges[u, v]["weight"] = list_weights[i]
    return G


def generate_exponentially_growing_graphs(benchmark, initial_size, base, max_size,
                                          num_partitions, sequence_type, p, bucket_size, prng, directed):
    if directed:
        graph_generator = generate_directed_weighted_graph
    else:
        graph_generator = generate_undirected_weighted_graph

    current_size = initial_size
    list_sizes = []
    while current_size <= max_size:
        # In the generation of random graphs, we subtract one from the variable
        # current_size. This is useful because when when current_size is a power
        # of two, current_size_adjusted is a power of two minus one, which makes
        # a binary tree complete (i.e., the bottom layer is filled). This is a
        # nice property for Prim's algorithm and Dijkstra's algorithm because
        # they use a binary heap, which is a tree-shaped data structure.
        current_size_adjusted = int(current_size) - 1
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
    list_sizes = list(set(list_sizes))

    result = []
    for num_nodes in list_sizes:
        for _ in range(bucket_size):
            if benchmark == "bellman_ford_algorithm":
                min_weight, max_weight = -2, 10
            else:
                min_weight, max_weight = 1, 2 * num_nodes
            generated_graph = graph_generator(
                num_nodes, p, min_weight, max_weight, prng)
            result.append(generated_graph)
    return result


def generate_exponentially_growing_graphs_multiple_probabilities(benchmark, initial_size, base, max_num_nodes,
                                                                 num_partitions, sequence_type,
                                                                 list_probabilities, bucket_size, prng, directed):
    result = []
    for p in list_probabilities:
        list_graphs = (generate_exponentially_growing_graphs(
            benchmark, initial_size, base, max_num_nodes,
            num_partitions, sequence_type, p, bucket_size, prng, directed))
        result.extend(list_graphs)
    return result


# Format input data before they are


def format_graph(graph):
    num_nodes = graph.number_of_nodes()
    # Ensure that the set of nodes is {0, 1, ..., num_nodes - 1}
    assert (max(graph.nodes) == num_nodes -
            1), "The IDs of graph nodes must form a contiguous integer interval"

    list_nodes = []
    for node in sorted(graph.nodes):
        list_neighbors = []
        for neighbor, edge_attribute in graph.adj[node].items():
            weight = edge_attribute["weight"]
            # Make sure to convert int64 (from numpy) to int
            list_neighbors.append(
                {"neighbor": int(neighbor), "weight": float(weight)})
        list_nodes.append({"node": node, "neighbors": list_neighbors})
    return list_nodes
