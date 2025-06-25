import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import math

from statistical_analysis.toolbox import categorize_runtime_cost_data
from visualization.toolbox import save_and_show_image


# Runtime cost data with one input size


def plot_runtime_cost_data(runtime_data, list_curves, image_dict):
    _, ax = plt.subplots()

    # Plot runtime cost data
    vector_input_sizes = [size for (size, _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]
    ax.scatter(vector_input_sizes, vector_costs,
               color="black", label="Runtime Data", marker=".", alpha=0.3)

    # Plot a curve (e.g., a ground-truth cost bound) if it is provided as
    # input to this function
    xmax = max(vector_input_sizes)
    # We start from x = 1 instead of x = 0 because log(0) is ill-defined.
    xs = np.linspace(1, xmax, num=200)
    for curve in list_curves:
        ys = curve(xs)
        ax.plot(xs, ys, color="red")

    axis_labels = image_dict.get("axis_labels")
    ax.set_xlabel(axis_labels[0])
    ax.set_ylabel(axis_labels[1])
    ax.legend()

    save_and_show_image(image_dict)


def plot_runtime_cost_data_categorical(runtime_data, list_curves, image_dict):
    _, ax = plt.subplots()

    # To avoid the overlapping of multiple data points that have the exactly
    # same value, we add jitters.
    vector_input_sizes = [size for (size, _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]
    sns.stripplot(x=vector_input_sizes, y=vector_costs,
                  size=3, jitter=0.25, alpha=0.5, ax=ax)

    # Get a list of categories (i.e., unique input sizes)
    unique_xs = list(set(vector_input_sizes))
    for curve in list_curves:
        unique_ys = curve(np.array(unique_xs))
        sns.stripplot(x=unique_xs, y=unique_ys, size=3, color="red", ax=ax)

    axis_labels = image_dict.get("axis_labels")
    ax.set_xlabel(axis_labels[0])
    ax.set_ylabel(axis_labels[1])

    save_and_show_image(image_dict)


def get_histogram_integral_bins(xs):
    min_x, max_x = math.floor(min(xs)), math.ceil(max(xs))
    if min_x == max_x:
        return [min_x, min_x + 1]
    else:
        # We need to include max_x + 1 in the output list of bins.
        return list(range(min_x, max_x + 2))


def plot_runtime_cost_data_histogram(runtime_data, plot_type, image_dict):
    categorized_data = categorize_runtime_cost_data(runtime_data)
    num_categories = len(categorized_data)
    unique_categories = list(categorized_data.keys())
    unique_categories.sort()

    ncols = 4
    nrows = int(np.ceil(num_categories / ncols))
    ax_height = 2.5
    _, axs = plt.subplots(nrows, ncols,
                          layout="constrained",
                          figsize=(ax_height * ncols, ax_height * nrows))
    for i in range(num_categories):
        input_size = unique_categories[i]
        vector_costs = categorized_data[input_size]

        ax = axs[i//ncols, i % ncols]
        if plot_type == "hist":
            bins = get_histogram_integral_bins(vector_costs)
            _, bins, _ = ax.hist(vector_costs, bins=bins)
        else:
            sns.violinplot(x=vector_costs, inner="stick", ax=ax)
        ax.set_xlabel("Cost")
        ax.set_ylabel("Frequency")
        ax.set_title("Size {}".format(input_size))

    for i in range(num_categories, nrows * ncols):
        axs[i//ncols, i % ncols].remove()

    save_and_show_image(image_dict)


# Posterior distributions with one input size


def plot_posterior_distribution_selected_cost_bounds_ax(ax, runtime_data, list_coefficients,
                                                        predicted_bound_vector_sizes,
                                                        list_curves, image_dict):
    num_posterior_samples_plot = min(100, len(list_coefficients))
    seed = 42
    prng = np.random.default_rng(seed)
    selected_indices = prng.choice(
        range(0, len(list_coefficients)), size=num_posterior_samples_plot, replace=False)
    list_selected_coefficients = [list_coefficients[i]
                                  for i in selected_indices]

    vector_input_sizes = [size for (size, _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]
    size_max = max(vector_input_sizes)
    cost_max = max(vector_costs)

    # Whether we should add labels to plots
    show_legend = image_dict["legend"]

    # Plot the selected cost bounds
    xmax = size_max * 1.2
    xs = np.linspace(0, xmax, num=200)
    for i, coefficients in enumerate(list_selected_coefficients):
        ys = predicted_bound_vector_sizes(coefficients, xs)
        # Only add a label to the first posterior sample
        if i == 0:
            if show_legend:
                ax.plot(xs, ys, color="blue", alpha=0.3,
                        linestyle="solid", label="Posterior Sample")
            else:
                ax.plot(xs, ys, color="blue", alpha=0.3, linestyle="solid")
        else:
            ax.plot(xs, ys, color="blue", alpha=0.3, linestyle="solid")

    # Plot runtime cost data
    if show_legend:
        ax.scatter(vector_input_sizes, vector_costs, color="black",
                   marker=".", label="Observed Data")
    else:
        ax.scatter(vector_input_sizes, vector_costs, color="black", marker=".")

    # Plot the supplied curves (e.g., ground-truth cost bounds). We start with x
    # = 1, instead of x = 0, so that we have do not need to evaluate log(0).
    xs = np.linspace(1, xmax, num=200)
    for curve in list_curves:
        ys = curve(xs)
        if show_legend:
            ax.plot(xs, ys, color="red", linestyle="dashed",
                    label="Ground Truth")
        else:
            ax.plot(xs, ys, color="red", linestyle="dashed")

    ax.set_xlim(0, xmax)
    ax.set_ylim(0, cost_max * 2)

    axis_labels = image_dict.get("axis_labels")
    ax.set_xlabel(axis_labels[0])
    ax.set_ylabel(axis_labels[1])


def plot_posterior_distribution_percentile_range_ax(ax, runtime_data, list_coefficients,
                                                    predicted_bound_vector_coefficients,
                                                    list_curves, image_dict):
    vector_input_sizes = [size for (size, _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]
    size_max = max(vector_input_sizes)
    cost_max = max(vector_costs)

    # Whether we should add labels to plots
    show_legend = image_dict["legend"]

    # Plot the percentile range of the posterior distribution
    xmax = size_max * 1.2
    xs = np.linspace(0, xmax, num=200)

    def percentile_range(x, q):
        ys = predicted_bound_vector_coefficients(
            np.array(list_coefficients), x)
        return np.percentile(ys, q)

    ys_lower = [percentile_range(x, 5) for x in xs]
    ys_upper = [percentile_range(x, 95) for x in xs]
    if show_legend:
        ax.fill_between(xs, ys_lower, ys_upper, color="blue",
                        alpha=0.2, label="5-95th Percentiles")
    else:
        ax.fill_between(xs, ys_lower, ys_upper, color="blue", alpha=0.2)

    # Plot the median cost bound
    ys_median = [percentile_range(x, 50) for x in xs]
    if show_legend:
        ax.plot(xs, ys_median, color="blue", linestyle="solid", label="Median")
    else:
        ax.plot(xs, ys_median, color="blue", linestyle="solid")

    # Plot runtime cost data
    if show_legend:
        ax.scatter(vector_input_sizes, vector_costs, color="black",
                   marker=".", label="Observed Data")
    else:
        ax.scatter(vector_input_sizes, vector_costs, color="black", marker=".")

    # Plot the supplied curves (e.g., ground-truth cost bounds)
    xs = np.linspace(1, xmax, num=200)
    for curve in list_curves:
        ys = curve(xs)
        if show_legend:
            ax.plot(xs, ys, color="red", linestyle="dashed",
                    label="Ground Truth")
        else:
            ax.plot(xs, ys, color="red", linestyle="dashed")

    ax.set_xlim(0, xmax)
    ax.set_ylim(0, cost_max * 2)

    axis_labels = image_dict.get("axis_labels")
    ax.set_xlabel(axis_labels[0])
    ax.set_ylabel(axis_labels[1])


def plot_posterior_distribution_multiple_models(list_runtime_data, list_list_coefficients,
                                                list_predicted_bound_vector_sizes,
                                                list_predicted_bound_vector_coefficients,
                                                list_curves, image_dict):
    num_models = len(list_list_coefficients)
    ax_height = 2.5
    nrows = 2
    ncols = num_models
    _, axs = plt.subplots(nrows, ncols, sharex=True, sharey=True,
                          layout="constrained",
                          figsize=(ax_height * ncols, ax_height * nrows))
    for i in range(num_models):
        list_coefficients = list_list_coefficients[i]

        # Plot the selected cost bounds
        ax = axs[0, i]
        runtime_data = list_runtime_data[i]
        predicted_bound_vector_sizes = list_predicted_bound_vector_sizes[i]
        plot_posterior_distribution_selected_cost_bounds_ax(
            ax, runtime_data, list_coefficients, predicted_bound_vector_sizes,
            list_curves, image_dict)
        list_titles = image_dict.get("list_titles")
        if list_titles is not None:
            ax.set_title(list_titles[i])

        # Plot the percentile range of the posterior distribution
        ax = axs[1, i]
        predicted_bound_vector_coefficients = list_predicted_bound_vector_coefficients[i]
        plot_posterior_distribution_percentile_range_ax(
            ax, runtime_data, list_coefficients, predicted_bound_vector_coefficients,
            list_curves, image_dict)

    save_and_show_image(image_dict)


# Cost gaps in runtime cost data with one input size


def plot_cost_gaps_histogram_ax(ax, runtime_data, list_coefficients,
                                predicted_bound_vector_sizes, plot_type):
    array_costs = np.array([cost for (_, cost) in runtime_data])
    array_coefficients = np.array(list_coefficients)
    array_input_sizes = np.array([size for (size, _) in runtime_data])

    # Compute the cost gaps
    list_cost_gaps = []
    for coefficients in array_coefficients:
        array_predictions = predicted_bound_vector_sizes(
            coefficients, array_input_sizes)
        array_cost_gaps = array_predictions - array_costs
        list_cost_gaps += list(array_cost_gaps)

    # Plot the cost gaps
    if plot_type == "hist":
        # bins = get_histogram_bins(list_cost_gaps)
        ax.hist(list_cost_gaps, bins=20)
    else:
        sns.violinplot(x=list_cost_gaps, inner="box", ax=ax)

    ax.set_xlabel("Cost Gap")
    ax.set_ylabel("Frequency")


def plot_cost_gasp_histogram_multiple_models(list_runtime_data, list_list_coefficients,
                                             list_predicted_bound_vector_sizes, list_titles,
                                             plot_type, image_dict):
    num_models = len(list_list_coefficients)
    ax_height = 2.5
    _, axs = plt.subplots(nrows=1, ncols=num_models,
                          layout="constrained",
                          figsize=(ax_height * num_models, ax_height))
    for i in range(num_models):
        ax = axs[i] if num_models > 1 else axs
        runtime_data = list_runtime_data[i]
        list_coefficients = list_list_coefficients[i]
        predicted_bound_vector_sizes = list_predicted_bound_vector_sizes[i]
        plot_cost_gaps_histogram_ax(ax,
                                    runtime_data, list_coefficients, predicted_bound_vector_sizes, plot_type)
        ax.set_xlabel("Cost Gap")
        ax.set_ylabel("Frequency")
        ax.set_title(list_titles[i])

    save_and_show_image(image_dict)


# Histogram of cost gaps categorized by buckets


def plot_cost_gaps_histogram_categorized_by_bucket(list_runtime_data, list_list_coefficients,
                                                   list_predicted_bound_vector_coefficients, list_labels,
                                                   image_dict):
    # We assume that all datasets in list_runtime_data contain the same buckets.
    categorized_data_representative = categorize_runtime_cost_data(
        list_runtime_data[0])
    num_categories = len(categorized_data_representative)
    unique_categories = list(categorized_data_representative.keys())
    unique_categories.sort()

    ncols = 4
    nrows = int(np.ceil(num_categories / ncols))
    ax_height = 3
    _, axs = plt.subplots(nrows, ncols, layout="constrained",
                          figsize=(ax_height * ncols, ax_height * nrows))

    num_models = len(list_list_coefficients)
    for i in range(num_categories):
        ax = axs[i//ncols, i % ncols]
        input_size = unique_categories[i]

        for model_id in range(num_models):
            list_coefficients = list_list_coefficients[model_id]
            array_coefficients = np.array(list_coefficients)
            predicted_bound_vector_coefficients = list_predicted_bound_vector_coefficients[
                model_id]
            array_predictions = predicted_bound_vector_coefficients(
                array_coefficients, input_size)

            categorized_data = categorize_runtime_cost_data(
                list_runtime_data[model_id])
            array_costs = np.array(categorized_data[input_size])
            array_cost_gaps = [
                prediction - cost for prediction in array_predictions for cost in array_costs]

            # bins = get_histogram_bins(array_cost_gaps)
            ax.hist(array_cost_gaps, bins=20, histtype='step',
                    label=list_labels[model_id])

        ax.set_xlabel("Cost Gap")
        ax.set_ylabel("Frequency")
        ax.set_title("Size {}".format(input_size))

    for i in range(num_categories, nrows * ncols):
        axs[i//ncols, i % ncols].remove()

    axs[0, 0].legend()
    save_and_show_image(image_dict)
