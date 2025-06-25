import numpy as np
import matplotlib.pyplot as plt
from joblib import Parallel, delayed
import os
import sys

import probabilistic_models
from json_manipulation import read_inference_result_from_json, read_runtime_cost_data_from_json, \
    read_total_cost_data_from_json
from directory_location import get_image_directory
from benchmark_data.benchmark_parameters \
    import list_benchmarks_data_driven, list_benchmarks_one_input_size, \
    num_counters_dict, get_counter_title, get_total_cost_title, \
    get_input_size_counter_axis_labels, get_input_size_total_cost_labels, \
    get_counter_scientific_notation_exponent, get_total_cost_scientific_notation_exponent
from benchmark_data.ground_truth import benchmark_bound_dict, get_ground_truth_bound

import visualization.toolbox as vis_toolbox
import visualization.three_dimensional as vis_three_dim
import visualization.two_dimensional as vis_two_dim


# Scatter plot of the runtime cost data


def plot_runtime_counter_data(benchmark, bucket_size, counter_index):
    runtime_data_size_only = read_runtime_cost_data_from_json(
        benchmark, bucket_size, counter_index)
    ground_truth = get_ground_truth_bound(benchmark, counter_index)
    list_curves = [] if ground_truth is None else [ground_truth]

    # image_directory = get_image_directory(
    #     benchmark, bucket_size, counter_index)
    # image_filename = "recursion_depth_data_scatter.pdf"
    # image_path = os.path.join(image_directory, image_filename)

    axis_labels = get_input_size_counter_axis_labels(benchmark, counter_index)
    image_dict = {"image_path": None, "show": True, "axis_labels": axis_labels}

    if benchmark in list_benchmarks_one_input_size:
        vis_two_dim.plot_runtime_cost_data(runtime_data_size_only,
                                           list_curves, image_dict)
    else:
        vis_three_dim.plot_runtime_cost_data(runtime_data_size_only,
                                             list_curves, image_dict)
        vis_three_dim.plot_projection_runtime_cost_data(
            runtime_data_size_only, list_curves, image_dict)


# Categorical plot of runtime cost data with jitters


def plot_runtime_counter_data_categorical(benchmark, bucket_size, counter_index):
    runtime_data_size_only = read_runtime_cost_data_from_json(
        benchmark, bucket_size, counter_index)
    ground_truth = get_ground_truth_bound(benchmark, counter_index)
    list_curves = [] if ground_truth is None else [ground_truth]

    image_directory = get_image_directory(
        benchmark, bucket_size, counter_index)
    image_filename = "recursion_depth_data_categorical_jitters.pdf"
    image_path = os.path.join(image_directory, image_filename)

    axis_labels = ["Input Size", "Cost"]
    image_dict = {"image_path": image_path,
                  "show": True, "axis_labels": axis_labels}
    vis_two_dim.plot_runtime_cost_data_categorical(
        runtime_data_size_only, list_curves, image_dict)


# Histogram or violin plot of runtime cost data


def plot_runtime_counter_data_histogram(benchmark, bucket_size, counter_index, plot_type):
    runtime_data_size_only = read_runtime_cost_data_from_json(
        benchmark, bucket_size, counter_index)

    image_directory = get_image_directory(
        benchmark, bucket_size, counter_index)
    image_filename = "recursion_depth_data_histogram.pdf"
    image_path = os.path.join(image_directory, image_filename)
    image_dict = {"image_path": image_path, "show": True}
    vis_two_dim.plot_runtime_cost_data_histogram(
        runtime_data_size_only, plot_type, image_dict)


# Posterior distribution of a model. This function is for those algorithms that
# take in one-dimensional inputs (e.g., integer lists). The cost is the
# recursion depth.


def plot_two_dimensional_posterior_distribution_ax(ax, benchmark, bucket_size,
                                                   counter_index, model_id,
                                                   plot_ground_truth, legend):
    def post_processing(x): return np.ceil(x)

    # def prediction_vector_sizes_model_id(model_id):
    #     def prediction(coefficients, input_sizes):
    #         raw_bound = probabilistic_models.predicted_bound_vector_sizes(
    #             coefficients, input_sizes, model_id)
    #         return post_processing(raw_bound)
    #     return prediction

    def prediction_vector_coefficients_model_id(model_id):
        def prediction(array_coefficients, input_size):
            raw_bound = probabilistic_models.predicted_bound_vector_coefficients(
                array_coefficients, input_size, model_id)
            return post_processing(raw_bound)
        return prediction

    inference_result = read_inference_result_from_json(
        benchmark, bucket_size, counter_index, model_id)
    runtime_data = inference_result["runtime_data"]
    list_coefficients = inference_result["inferred_coefficients"]
    predicted_bound_vector_coefficients = prediction_vector_coefficients_model_id(
        model_id)

    if plot_ground_truth:
        ground_truth = get_ground_truth_bound(benchmark, counter_index)
        list_curves = [] if ground_truth is None else [ground_truth]
    else:
        list_curves = []

    axis_labels = get_input_size_counter_axis_labels(benchmark, counter_index)
    image_dict = {"axis_labels": axis_labels, "legend": legend}
    vis_two_dim.plot_posterior_distribution_percentile_range_ax(
        ax, runtime_data, list_coefficients, predicted_bound_vector_coefficients,
        list_curves, image_dict)


# Median cost bound of a model. This function is for those algorithms that take
# in two-dimensional inputs (e.g., graphs).


def plot_median_cost_bound_ax(ax, benchmark, bucket_size, counter_index, model_id,
                              plot_ground_truth, legend):

    def post_processing(x): return np.ceil(x)

    def prediction_vector_coefficients_model_id(model_id, dependency_on_first_input):
        if dependency_on_first_input:
            def prediction(array_coefficients, input_size):
                raw_bound = probabilistic_models.predicted_bound_vector_coefficients(
                    array_coefficients, input_size[0], model_id)
                return post_processing(raw_bound)
        else:
            def prediction(array_coefficients, input_size):
                raw_bound = probabilistic_models.predicted_bound_vector_coefficients(
                    array_coefficients, input_size[1], model_id)
                return post_processing(raw_bound)
        return prediction

    inference_result = read_inference_result_from_json(
        benchmark, bucket_size, counter_index, model_id)
    runtime_data = inference_result["runtime_data"]
    list_coefficients = inference_result["inferred_coefficients"]
    dependency_on_first_input = inference_result["dependency_on_first_input"]
    predicted_bound_vector_coefficients = prediction_vector_coefficients_model_id(
        model_id, dependency_on_first_input)

    # The 3D plot looks crowded if we plot the ground-truth cost bound in
    # addition to the median inferred cost bound.
    if plot_ground_truth:
        ground_truth = get_ground_truth_bound(benchmark, counter_index)
        list_curves = [] if ground_truth is None else [ground_truth]
    else:
        list_curves = []

    axis_labels = get_input_size_counter_axis_labels(benchmark, counter_index)
    image_dict = {"axis_labels": axis_labels, "legend": legend}
    vis_three_dim.plot_median_cost_bound_ax(
        ax, runtime_data, list_coefficients, predicted_bound_vector_coefficients,
        list_curves, image_dict)


# Projected posterior distribution of a model. This function is for those
# algorithms that take in two-dimensional inputs (e.g., graphs). The cost is the
# recursion depth.


def plot_three_dimensional_posterior_distribution_ax(ax, benchmark, bucket_size,
                                                     counter_index, model_id,
                                                     plot_ground_truth, legend):

    def post_processing(x): return np.ceil(x)

    # def prediction_vector_sizes_model_id(model_id):
    #     def prediction(coefficients, input_sizes):
    #         raw_bound = probabilistic_models.predicted_bound_vector_sizes(
    #             coefficients, input_sizes, model_id)
    #         return post_processing(raw_bound)
    #     return prediction

    def prediction_vector_coefficients_model_id(model_id):
        def prediction(array_coefficients, input_size):
            raw_bound = probabilistic_models.predicted_bound_vector_coefficients(
                array_coefficients, input_size, model_id)
            return post_processing(raw_bound)
        return prediction

    inference_result = read_inference_result_from_json(
        benchmark, bucket_size, counter_index, model_id)
    dependency_on_first_input = inference_result["dependency_on_first_input"]
    runtime_data = inference_result["runtime_data"]
    if dependency_on_first_input:
        projected_runtime_data = [(size1, cost)
                                  for ((size1, _), cost) in runtime_data]
    else:
        projected_runtime_data = [(size2, cost)
                                  for ((_, size2), cost) in runtime_data]

    list_coefficients = inference_result["inferred_coefficients"]
    predicted_bound_vector_coefficients = prediction_vector_coefficients_model_id(
        model_id)

    def ground_truth_projected(size):
        # The variable dependency_on_first_input comes from the last model in
        # list_model_ids. We implicitly assume that all models have the same
        # Boolean value for dependency_on_first_input.
        if dependency_on_first_input:
            size_augmented = (size, None)
        else:
            size_augmented = (None, size)
        return get_ground_truth_bound(benchmark, counter_index)(size_augmented)

    if plot_ground_truth:
        list_curves = [ground_truth_projected]
    else:
        list_curves = []

    axis_labels_original = get_input_size_counter_axis_labels(
        benchmark, counter_index)
    input_size_label = axis_labels_original[0] if dependency_on_first_input else axis_labels_original[1]
    axis_labels = [input_size_label, axis_labels_original[-1]]

    image_dict = {"axis_labels": axis_labels, "legend": legend}
    vis_two_dim.plot_posterior_distribution_percentile_range_ax(
        ax, projected_runtime_data, list_coefficients,
        predicted_bound_vector_coefficients, list_curves, image_dict)


# Plot posterior distribution of a model. This function is used for those
# algorithms that take in one-dimensional inputs. The cost is the total cost.


def plot_two_dimensional_posterior_distribution_total_cost_ax(ax, benchmark, bucket_size, model_id,
                                                              plot_ground_truth, legend):
    num_counters = num_counters_dict[benchmark]
    # The symbolic bound for counter-instrumented code inferred by RaML. The
    # bound is parametric in both the original input size and the counter
    # variable (which may be a vector of multiple counter variables).
    benchmark_bound = benchmark_bound_dict[benchmark]

    def ground_truth_function(size):
        counters = []
        for counter_index in range(num_counters):
            ground_truth_counter_function = get_ground_truth_bound(
                benchmark, counter_index)
            ground_truth = ground_truth_counter_function(size)
            counters.append(ground_truth)

        return benchmark_bound(size, counters)

    def post_processing(x): return np.ceil(x)

    # def prediction_vector_sizes_model_id(model_id):

    #     # The variable coefficients_all_counters is a concatenated list of the
    #     # coefficients of all counters. We first split this list into sub-lists
    #     # of coefficients for individual counters.
    #     def prediction(coefficients_all_counters, input_sizes):
    #         num_parameters_per_counter = len(
    #             coefficients_all_counters) // num_counters
    #         counters = []
    #         for counter_index in range(num_counters):
    #             coefficients_counter = coefficients_all_counters[counter_index * num_parameters_per_counter:(
    #                 counter_index + 1) * num_parameters_per_counter]
    #             counter = post_processing(probabilistic_models.predicted_bound_vector_sizes(
    #                 coefficients_counter, input_sizes, model_id))
    #             counters.append(counter)
    #         return benchmark_bound(input_sizes, counters)

    #     return prediction

    def prediction_vector_coefficients_model_id(model_id):

        # The variable array_coefficients_all_counters is a concatenated array
        # of the coefficients of all counters.
        def prediction(array_coefficients_all_counters, input_size):
            _, num_all_parameters = array_coefficients_all_counters.shape
            num_parameters_per_counter = num_all_parameters // num_counters
            counters = []
            for counter_index in range(num_counters):
                array_coefficients_counter = array_coefficients_all_counters[:, counter_index * num_parameters_per_counter:(
                    counter_index + 1) * num_parameters_per_counter]
                counter = post_processing(probabilistic_models.predicted_bound_vector_coefficients(
                    array_coefficients_counter, input_size, model_id))
                counters.append(counter)
            return benchmark_bound(input_size, counters)

        return prediction

    total_cost_data = read_total_cost_data_from_json(benchmark, bucket_size)

    inference_result_first_counter = read_inference_result_from_json(
        benchmark, bucket_size, 0, model_id)
    # The variable list_coefficients_all_counters is initialized to the
    # inferred coefficients for the first counter. If we have additional
    # counters, we extend list_coefficients_all_counters with the extra
    # counters' inferred coefficients.
    list_coefficients_all_counters = inference_result_first_counter["inferred_coefficients"]
    for counter_index in range(1, num_counters):
        inference_result_counter = read_inference_result_from_json(
            benchmark, bucket_size, counter_index, model_id)
        list_coefficients = inference_result_counter["inferred_coefficients"]
        list_coefficients_all_counters = [acc + coefficients for acc, coefficients in zip(
            list_coefficients_all_counters, list_coefficients)]
    list_coefficients = list_coefficients_all_counters

    predicted_bound_vector_coefficients = prediction_vector_coefficients_model_id(
        model_id)

    if plot_ground_truth:
        list_curves = [ground_truth_function]
    else:
        list_curves = []

    axis_labels = get_input_size_total_cost_labels(benchmark)
    image_dict = {"axis_labels": axis_labels, "legend": legend}
    vis_two_dim.plot_posterior_distribution_percentile_range_ax(
        ax, total_cost_data, list_coefficients,
        predicted_bound_vector_coefficients, list_curves, image_dict)


# Posterior distribution of a model. This function is for those algorithms that
# take in two-dimensional inputs (e.g., graphs). The cost is the total cost.


def plot_median_cost_bound_total_cost_ax(ax, benchmark, bucket_size, model_id,
                                         plot_ground_truth, legend):
    num_counters = num_counters_dict[benchmark]
    benchmark_bound = benchmark_bound_dict[benchmark]

    def ground_truth_function(size):
        counters = []
        for counter_index in range(num_counters):
            ground_truth_counter_function = get_ground_truth_bound(
                benchmark, counter_index)
            ground_truth = ground_truth_counter_function(size)
            counters.append(ground_truth)

        return benchmark_bound(size, counters)

    def post_processing(x): return np.ceil(x)

    def prediction_vector_coefficients_model_id(model_id, list_dependency_on_first_input):
        assert (num_counters == len(list_dependency_on_first_input)
                ), "Each counter must have a Boolean flag for dependency on the first input"

        def prediction(array_coefficients_all_counters, input_size):
            _, num_all_parameters = array_coefficients_all_counters.shape
            num_parameters_per_counter = num_all_parameters // num_counters
            counters = []
            for counter_index in range(num_counters):
                array_coefficients_counter = array_coefficients_all_counters[:, counter_index * num_parameters_per_counter:(
                    counter_index + 1) * num_parameters_per_counter]
                dependency_on_first_input = list_dependency_on_first_input[counter_index]
                input_size_projected = input_size[0] if dependency_on_first_input else input_size[1]
                counter = post_processing(probabilistic_models.predicted_bound_vector_coefficients(
                    array_coefficients_counter, input_size_projected, model_id))
                counters.append(counter)
            return benchmark_bound(input_size, counters)

        return prediction

    total_cost_data = read_total_cost_data_from_json(benchmark, bucket_size)

    inference_result_first_counter = read_inference_result_from_json(
        benchmark, bucket_size, 0, model_id)
    list_coefficients_all_counters = inference_result_first_counter["inferred_coefficients"]
    list_dependency_on_first_input = [
        inference_result_first_counter["dependency_on_first_input"]]
    for counter_index in range(1, num_counters):
        inference_result_counter = read_inference_result_from_json(
            benchmark, bucket_size, counter_index, model_id)
        list_coefficients = inference_result_counter["inferred_coefficients"]
        list_coefficients_all_counters = [acc + coefficients for acc, coefficients in zip(
            list_coefficients_all_counters, list_coefficients)]
        list_dependency_on_first_input.append(
            inference_result_counter["dependency_on_first_input"])

    list_coefficients = list_coefficients_all_counters

    predicted_bound_vector_coefficients = prediction_vector_coefficients_model_id(
        model_id, list_dependency_on_first_input)

    if plot_ground_truth:
        list_curves = [ground_truth_function]
    else:
        list_curves = []

    # Decide whether we should use a scientific notation for axis tick labels.
    # Graph algorithms (e.g., Prim, Dijkstra, and Bellman-Ford) and
    # quicksort_timl have large total costs, so we should use a scientific
    # notation.
    sci_exponent = get_total_cost_scientific_notation_exponent(benchmark)
    axis_labels = get_input_size_total_cost_labels(benchmark, sci_exponent)
    image_dict = {"axis_labels": axis_labels, "legend": legend}
    if sci_exponent is not None:
        ax.ticklabel_format(style="sci")
        ax.zaxis.offsetText.set_visible(False)

    vis_three_dim.plot_median_cost_bound_ax(
        ax, total_cost_data, list_coefficients, predicted_bound_vector_coefficients,
        list_curves, image_dict)


# Posterior distributions of all counters and the total cost


def plot_posterior_distributions(benchmark, bucket_size, legend_location="top"):
    model_id = 0

    num_counters = num_counters_dict[benchmark]
    ncols = num_counters + 1
    ax_height = 2.4
    legend_width = 2

    with plt.rc_context(vis_toolbox.latex_fig_rc):
        # To show the bounding box of the figure, we can set
        # facecolor="lightskyblue" inside plt.figure.
        fig = plt.figure(figsize=(ax_height * ncols + legend_width, ax_height),
                         layout="compressed")
        # layout="constrained")

        # Plot the posterior distributions of all counters' inferred bounds
        for counter_index in range(num_counters):
            # We only add a legend to the first subplot in the figure. All
            # subplots will share the same legend. Hence, to remove duplicates,
            # we should only assign legend to the first subplot.
            legend = True if counter_index == 0 else False
            ax = fig.add_subplot(1, ncols, counter_index + 1)
            if benchmark in list_benchmarks_one_input_size:
                plot_two_dimensional_posterior_distribution_ax(
                    ax, benchmark, bucket_size, counter_index, model_id,
                    plot_ground_truth=True, legend=legend)
            else:
                plot_three_dimensional_posterior_distribution_ax(
                    ax, benchmark, bucket_size, counter_index, model_id,
                    plot_ground_truth=True, legend=legend)

            ax.set_box_aspect(1.0)
            ax.set_title(get_counter_title(benchmark, counter_index))

        # For 3D plots, their z-axis labels are oftentimes cropped. To prevent
        # them from being cropped, we pass a list of artists (i.e., Artist
        # objects such as axis labels and legends) that should be taken into
        # account when calculating the bounding box around in the method
        # plt.savefig. I found this workaround in this post:
        # https://stackoverflow.com/questions/10101700/moving-matplotlib-legend-outside-of-the-axis-makes-it-cutoff-by-the-figure-box.
        extra_artists = []

        # Plot the posterior distribution of overall cost bounds
        if benchmark in list_benchmarks_one_input_size:
            ax = fig.add_subplot(1, ncols, ncols)
            plot_two_dimensional_posterior_distribution_total_cost_ax(
                ax, benchmark, bucket_size, model_id,
                plot_ground_truth=True, legend=False)
            ax.set_box_aspect(1.0)
            ax.set_title(get_total_cost_title(benchmark))
        else:
            ax = fig.add_subplot(1, ncols, ncols, projection="3d")
            plot_median_cost_bound_total_cost_ax(
                ax, benchmark, bucket_size, model_id,
                plot_ground_truth=False, legend=False)
            ax.set_title(get_total_cost_title(benchmark))
            extra_artists.append(ax.zaxis.get_label())

        # Figure out the placement of a figure-level legend
        if legend_location == "top":
            fig_legend = fig.legend(loc="outside upper center", ncols=4)
            extra_artists.append(fig_legend)
        elif legend_location == "right":
            if benchmark in list_benchmarks_one_input_size:
                fig_legend = fig.legend(loc="outside right center", ncols=1)
            else:
                # bbox_anchor = (1.08, 0.5)
                if ncols == 2:
                    # bbox_anchor = (1.44, 0.5)
                    bbox_anchor = (1.12, 0.5)
                elif ncols == 3:
                    bbox_anchor = (1.08, 0.5)
                elif ncols == 4:
                    bbox_anchor = (1.03, 0.5)
                else:
                    raise ValueError("The number of subplots is not supported")
                # fig_legend = fig.legend(loc="outside right center", ncols=1)
                fig_legend = fig.legend(loc="outside right center", ncols=1,
                                        bbox_to_anchor=bbox_anchor)
            extra_artists.append(fig_legend)

        image_directory = get_image_directory(
            benchmark, bucket_size, counter_index=None)

        if legend_location == "top":
            image_filename = "posterior_distributions.pdf"
        elif legend_location == "right":
            image_filename = "posterior_distributions_legend_right_center.pdf"
        else:
            image_filename = "posterior_distributions_no_legend.pdf"

        image_path = os.path.join(
            image_directory, image_filename)
        image_dict = {"image_path": image_path, "show": False}
        vis_toolbox.save_and_show_image(image_dict, extra_artists)

        # Verify that the scientific notation exponents of z-axis tick labels
        # are as expected.
        vis_toolbox.verify_sci_exponent(benchmark, fig)


# Analysis results of resource decomposition integrating AARA with interactive
# resource analysis. The inference results are plotted for all counters and the
# total cost.


def kruskal_soundness_check():
    benchmark = "kruskal_algorithm"
    bucket = 1
    counter_index = 0
    runtime_data_size_only = read_runtime_cost_data_from_json(
        benchmark, bucket, counter_index)
    largest_ratio = 1
    for size, cost in runtime_data_size_only:
        ground_truth_counter_function = get_ground_truth_bound(
            benchmark, counter_index)
        inferred_bound = ground_truth_counter_function(size)
        # print("Cost = {} inferred bound = {}".format(cost, inferred_bound))
        largest_ratio = max(largest_ratio, inferred_bound / cost)
        if inferred_bound < cost:
            print("Inferred bound is smaller than the cost")
    print("We are done with soundness checking: largest ratio = {}".format(largest_ratio))


def plot_interactive_resource_analysis(benchmark, bucket_size, legend_location="top"):
    assert (benchmark == "kruskal_algorithm"), "Plotting for interactive analysis is specific to Kruskal's algorithm"
    num_counters = num_counters_dict[benchmark]
    ncols = num_counters + 1
    ax_height = 2.4
    legend_width = 2

    benchmark_bound = benchmark_bound_dict[benchmark]

    def ground_truth_function(size):
        counters = []
        for counter_index in range(num_counters):
            ground_truth_counter_function = get_ground_truth_bound(
                benchmark, counter_index)
            ground_truth = ground_truth_counter_function(size)
            counters.append(ground_truth)

        return benchmark_bound(size, counters)

    with plt.rc_context(vis_toolbox.latex_fig_rc):
        # To show the bounding box of the figure, we can set
        # facecolor="lightskyblue" inside plt.figure.
        fig = plt.figure(figsize=(ax_height * ncols + legend_width, ax_height))

        # Plot the runtime data and ground-truth bounds of individual counters
        for counter_index in range(num_counters):
            ax = fig.add_subplot(1, ncols, counter_index + 1, projection="3d")
            runtime_data_size_only = read_runtime_cost_data_from_json(
                benchmark, bucket_size, counter_index)
            ground_truth = get_ground_truth_bound(benchmark, counter_index)
            list_curves = [] if ground_truth is None else [ground_truth]

            sci_exponent = get_counter_scientific_notation_exponent(
                benchmark, counter_index)
            axis_labels = get_input_size_counter_axis_labels(
                benchmark, counter_index, sci_exponent)
            legend = True if counter_index == 0 else False
            image_dict = {"axis_labels": axis_labels, "legend": legend,
                          "color": "blue", "curve_label": "Inferred Bound"}
            vis_three_dim.plot_runtime_cost_data_ax(
                ax, runtime_data_size_only, list_curves, image_dict)

            # Remove padding between axis lines (e.g., x-axis) and tick labels
            ax.tick_params(pad=0)

            # Remove padding between axis lines and axis labels
            ax.xaxis.labelpad = 0
            ax.yaxis.labelpad = 0
            ax.zaxis.labelpad = 0

            # Use scientific notation for axis tick labels if the numbers are
            # too large. According to the default setting, the power limits for
            # scientific notation is [-5, 6]. That is, if axis tick labels are
            # smaller than 10 ** (-5) or larger than 10 ** 6, scientific
            # notation is activated.
            ax.ticklabel_format(style="sci")
            ax.zaxis.offsetText.set_visible(False)
            ax.set_title(get_counter_title(benchmark, counter_index))

        # Plot the runtime data and ground-truth bound of the total cost
        total_cost_data = read_total_cost_data_from_json(
            benchmark, bucket_size)
        ax = fig.add_subplot(1, ncols, ncols, projection="3d")
        ground_truth_total_cost = [ground_truth_function]

        axis_labels = get_input_size_total_cost_labels(
            benchmark, sci_exponent=6)
        image_dict = {"axis_labels": axis_labels,
                      "legend": False,
                      "color": "blue"}
        vis_three_dim.plot_runtime_cost_data_ax(
            ax, total_cost_data, ground_truth_total_cost, image_dict)

        ax.tick_params(pad=0)
        ax.xaxis.labelpad = 0
        ax.yaxis.labelpad = 0
        ax.zaxis.labelpad = 0
        ax.ticklabel_format(style="sci")
        ax.zaxis.offsetText.set_visible(False)
        ax.set_title(get_total_cost_title(benchmark))

        # For 3D plots, their z-axis labels are oftentimes cropped. To prevent
        # them from being cropped, we pass a list of artists (i.e., Artist
        # objects such as axis labels and legends) that should be taken into
        # account when calculating the bounding box around in the method
        # plt.savefig. I found this workaround in this post:
        # https://stackoverflow.com/questions/10101700/moving-matplotlib-legend-outside-of-the-axis-makes-it-cutoff-by-the-figure-box.
        extra_artists = [ax.zaxis.get_label(), ax.title]

        # Figure out the placement of a figure-level legend
        if legend_location == "top":
            bbox_anchor = (0.5, 1.15)
            fig_legend = fig.legend(loc="outside upper center", ncols=2,
                                    bbox_to_anchor=bbox_anchor)
            extra_artists.append(fig_legend)
        elif legend_location == "right":
            bbox_anchor = (1.14, 0.5)
            fig_legend = fig.legend(loc="outside right center", ncols=1,
                                    bbox_to_anchor=bbox_anchor)
            extra_artists.append(fig_legend)

        image_directory = get_image_directory(
            benchmark, bucket_size, counter_index=None)

        if legend_location == "top":
            image_filename = "interactive_analysis_result.pdf"
        elif legend_location == "right":
            image_filename = "interactive_analysis_result_legend_right_center.pdf"
        else:
            image_filename = "interactive_analysis_result_no_legend.pdf"

        image_path = os.path.join(image_directory, image_filename)
        image_dict = {"image_path": image_path, "show": True}
        vis_toolbox.save_and_show_image(image_dict, extra_artists)

        # Verify that the scientific notation exponents of z-axis tick labels
        # are as expected.
        vis_toolbox.verify_sci_exponent(benchmark, fig)


# Plot a legend only


def plot_legend():
    with plt.rc_context(vis_toolbox.latex_fig_rc):
        ax_height = 2.4
        _, ax = plt.subplots(ncols=1, nrows=1, figsize=(ax_height * 2, 1),
                             layout="compressed")
        vis_toolbox.plot_legend(ax)

        # Save the image
        root_images_directory = os.path.expanduser(
            os.path.join("~", "experiments", "raml_code", "model_averaging", "images"))
        miscellaneous_directory = os.path.join(
            root_images_directory, "miscellaneous")
        if not os.path.exists(miscellaneous_directory):
            os.makedirs(miscellaneous_directory)

        image_filename = "legend.pdf"
        image_path = os.path.join(
            miscellaneous_directory, image_filename)
        image_dict = {"image_path": image_path, "show": True}
        vis_toolbox.save_and_show_image(image_dict, [])


if __name__ == "__main__":

    def draw_all_posterior_distribution(benchmark, legend_location):
        print("Start drawing a posterior distribution: benchmark = {}, legend location = {}".format(
            benchmark, legend_location))
        bucket_size = 1
        plot_posterior_distributions(benchmark, bucket_size, legend_location)
        print("Finished drawing: benchmark = {}, legend location = {}".format(
            benchmark, legend_location))

    list_args = sys.argv
    mode = list_args[1]

    list_benchmarks_legend_right_center = [
        "heap_sort", "red_black_tree", "quicksort_timl"]
    list_benchmarks_no_legend = ["merge_sort", "bubble_sort"]

    n_jobs = os.cpu_count() - 1
    # n_jobs = 1
    if mode == "plot":
        legend_location_arg = list_args[2]
        if legend_location_arg == "all":
            list_benchmarks_legend_locations = []
            for benchmark in list_benchmarks_data_driven:
                list_benchmarks_legend_locations.append((benchmark, "top"))
                if benchmark in list_benchmarks_legend_right_center:
                    list_benchmarks_legend_locations.append(
                        (benchmark, "right"))
                if benchmark in list_benchmarks_no_legend:
                    list_benchmarks_legend_locations.append(
                        (benchmark, "no_legend"))
        elif legend_location_arg == "top":
            list_benchmarks_legend_locations = [
                (benchmark, "top") for benchmark in list_benchmarks_data_driven]
        elif legend_location_arg == "right":
            list_benchmarks_legend_locations = [
                (benchmark, "right") for benchmark in list_benchmarks_legend_right_center]
        elif legend_location_arg == "no_legend":
            list_benchmarks_legend_locations = [
                (benchmark, "no_legend") for benchmark in list_benchmarks_no_legend]
        else:
            raise ValueError("The given mode is invalid")

        Parallel(n_jobs=n_jobs)(delayed(draw_all_posterior_distribution)(benchmark, legend_location)
                                for benchmark, legend_location in list_benchmarks_legend_locations)
    elif mode == "legend":
        plot_legend()
    elif mode == "interactive_analysis":
        benchmark = "kruskal_algorithm"
        bucket_size = 1
        legend_location = "no_legend"
        plot_interactive_resource_analysis(
            benchmark, bucket_size, legend_location)
    elif mode == "quicksort_timl":
        benchmark = "quicksort_timl"
        bucket_size = 1
        legend_location = "right"
        plot_posterior_distributions(benchmark, bucket_size, legend_location)
    elif mode == "kruskal_soundness_check":
        kruskal_soundness_check()
    else:
        raise ValueError("Unsupported mode: {}".format(mode))
