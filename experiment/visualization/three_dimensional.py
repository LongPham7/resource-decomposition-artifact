import matplotlib.pyplot as plt
import numpy as np

from statistical_analysis.toolbox import evaluate_percentile
from visualization.toolbox import save_and_show_image, latex_fig_rc


# Runtime cost data with two input sizes


def plot_runtime_cost_data_ax(ax, runtime_data, list_curves, image_dict):
    # Plot runtime cost data
    vector_input_sizes1 = [size1 for ((size1, _), _) in runtime_data]
    vector_input_sizes2 = [size2 for ((_, size2), _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]

    show_legend = image_dict.get("legend", True)
    if show_legend:
        ax.scatter(vector_input_sizes1, vector_input_sizes2,
                   vector_costs, marker=".", color="black", label="Observed Data")
    else:
        ax.scatter(vector_input_sizes1, vector_input_sizes2,
                   vector_costs, marker=".", color="black")

    size_max1 = max(vector_input_sizes1)
    size_max2 = max(vector_input_sizes2)
    xmax = size_max1 * 1.2
    ymax = size_max2 * 1.2

    # Plot curves
    num_plotting_points = 100
    # We start from x = 1 instead of x = 0 because log(0) is ill-defined.
    xs = np.linspace(1, xmax, num=num_plotting_points)
    ys = np.linspace(1, ymax, num=num_plotting_points)
    X, Y = np.meshgrid(xs, ys)
    Z = np.zeros(X.shape)

    color = image_dict.get("color", "red")
    curve_label = image_dict.get("curve_label", None)

    for curve in list_curves:
        Z = curve((X, Y))
        if show_legend:
            ax.plot_wireframe(X, Y, Z, color=color, ccount=10, rcount=10, label=curve_label)
        else:
            ax.plot_wireframe(X, Y, Z, color=color, ccount=10, rcount=10)

    ax.set_xlim(0, xmax)
    ax.set_ylim(0, ymax)

    axis_labels = image_dict.get("axis_labels")
    if axis_labels is None:
        ax.set_xlabel("First Input")
        ax.set_ylabel("Second Input")
        ax.set_zlabel("Cost")
    else:
        ax.set_xlabel(axis_labels[0])
        ax.set_ylabel(axis_labels[1])
        ax.set_zlabel(axis_labels[2])


def plot_runtime_cost_data(runtime_data, list_curves, image_dict):
    _, ax = plt.subplots(nrows=1, ncols=1, subplot_kw={'projection': '3d'})
    plot_runtime_cost_data_ax(ax, runtime_data, list_curves, image_dict)
    ax.legend()
    save_and_show_image(image_dict)


def plot_projection_runtime_cost_data(runtime_data, list_curves, image_dict):
    _, axs = plt.subplots(nrows=1, ncols=2, layout="constrained")

    # Plot runtime cost data
    vector_input_sizes1 = [size1 for ((size1, _), _) in runtime_data]
    vector_input_sizes2 = [size2 for ((_, size2), _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]

    size_max1 = max(vector_input_sizes1)
    size_max2 = max(vector_input_sizes2)
    cost_max = max(vector_costs)
    xmax = size_max1 * 1.2
    ymax = size_max2 * 1.2
    zmax = cost_max * 1.3

    num_plotting_points = 100
    xs = np.linspace(1, xmax, num=num_plotting_points)
    ys = np.linspace(1, ymax, num=num_plotting_points)

    # Projection on the first input. For the curves, we plot their values at
    # second_input = size_max2.
    axs[0].scatter(vector_input_sizes1,
                   vector_costs, marker=".", color="black", label="Runtime Data")
    for curve in list_curves:
        zs = curve((xs, np.repeat(size_max2, len(xs))))
        axs[0].plot(xs, zs, color="red")

    # Projection on the second input. For the curves, we plot their values at
    # first_input = size_max1.
    axs[1].scatter(vector_input_sizes2,
                   vector_costs, marker=".", color="black", label="Runtime Data")
    for curve in list_curves:
        zs = curve((np.repeat(size_max1, len(xs)), ys))
        axs[1].plot(ys, zs, color="red")

    for i in range(0, 2):
        ax = axs[i]
        ax.set_xlim(0, xmax if i == 0 else ymax)
        ax.set_ylim(0, zmax)

        axis_labels = image_dict.get("axis_labels")
        if axis_labels is None:
            ax.set_xlabel("First Input" if i == 0 else "Second Input")
            ax.set_ylabel("Cost")
        else:
            ax.set_xlabel(axis_labels[i])
            ax.set_ylabel(axis_labels[2])
        ax.set_box_aspect(1)
        # ax.legend()

    save_and_show_image(image_dict)


# Posterior distributions with two input sizes


def plot_median_cost_bound_ax(ax, runtime_data, list_coefficients, predicted_bound_vector_coefficients,
                              list_curves, image_dict):
    array_selected_coefficients = np.array(list_coefficients)

    vector_input_sizes1 = [size1 for ((size1, _), _) in runtime_data]
    vector_input_sizes2 = [size2 for ((_, size2), _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]

    size_max1 = max(vector_input_sizes1)
    size_max2 = max(vector_input_sizes2)
    xmax = size_max1 * 1.2
    ymax = size_max2 * 1.2

    # Whether we show labels of plots
    show_legend = image_dict["legend"]

    # Plot the surface of the median cost bound
    num_plotting_points = 100
    xs = np.linspace(0, xmax, num=num_plotting_points)
    ys = np.linspace(0, ymax, num=num_plotting_points)
    X, Y = np.meshgrid(xs, ys)
    num_xs, num_ys = X.shape
    Z = np.zeros(X.shape)
    for i in range(0, num_xs):
        for j in range(0, num_ys):
            size1 = X[i, j]
            size2 = Y[i, j]
            Z[i, j] = evaluate_percentile(
                array_selected_coefficients, (size1, size2), predicted_bound_vector_coefficients, 50)
    # ax.plot_surface(xs, ys, zs_matrix, color="blue", alpha=0.3)
    if show_legend:
        ax.plot_wireframe(X, Y, Z, color="blue", ccount=10,
                          rcount=10, label="Median")
    else:
        ax.plot_wireframe(X, Y, Z, color="blue", ccount=10, rcount=10)

    # Plot runtime cost data
    if show_legend:
        ax.scatter(vector_input_sizes1, vector_input_sizes2,
                   vector_costs, marker=".", color="black", label="Runtime Data")
    else:
        ax.scatter(vector_input_sizes1, vector_input_sizes2,
                   vector_costs, marker=".", color="black")

    # Plot the supplied curves (e.g., ground-truth cost bounds)
    for curve in list_curves:
        Z = curve((X, Y))
        if show_legend:
            ax.plot_wireframe(X, Y, Z, color="red", ccount=10,
                              rcount=10, label="Ground Truth")
        else:
            ax.plot_wireframe(X, Y, Z, color="red", ccount=10, rcount=10)

    ax.set_xlim(0, xmax)
    ax.set_ylim(0, ymax)

    axis_labels = image_dict.get("axis_labels")
    if axis_labels is None:
        ax.set_xlabel("First Input Size")
        ax.set_ylabel("Second Input Size")
        ax.set_zlabel("Cost")
    else:
        ax.set_xlabel(axis_labels[0])
        ax.set_ylabel(axis_labels[1])
        ax.set_zlabel(axis_labels[2])

    # ax.ticklabel_format(style="plain")


def plot_median_cost_bounds_multiple_models(list_runtime_data, list_list_coefficients,
                                            list_predicted_bound_vector_coefficients,
                                            list_curves, image_dict):
    num_models = len(list_list_coefficients)
    ax_height = 3.2
    _, axs = plt.subplots(nrows=1, ncols=num_models,
                          subplot_kw={'projection': '3d'},
                          layout="constrained",
                          figsize=(ax_height * num_models, ax_height))
    for i in range(num_models):
        ax = axs[i] if num_models > 1 else axs
        plot_median_cost_bound_ax(ax, list_runtime_data[i], list_list_coefficients[i],
                                  list_predicted_bound_vector_coefficients[i],
                                  list_curves, image_dict)
        # ax.set_box_aspect(aspect=None, zoom=0.7)
    save_and_show_image(image_dict)


# Projection of posterior distributions with two input sizes


def plot_projection_first_input(runtime_data, list_coefficients, predicted_bound_vector_sizes):
    num_posterior_samples_plot = min(100, len(list_coefficients))
    seed = 42
    prng = np.random.default_rng(seed)
    selected_indices = prng.sample(
        range(0, len(list_coefficients)), size=num_posterior_samples_plot, replace=False)
    list_coefficients = [list_coefficients[i] for i in selected_indices]

    vector_input_sizes1 = [size1 for ((size1, _), _) in runtime_data]
    vector_input_sizes2 = [size2 for ((_, size2), _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]

    fig, axes = plt.subplots(nrows=1, ncols=2, layout="constrained")
    ax1, ax2 = axes[0], axes[1]
    fig.set_figheight(15)

    xmax = max(vector_input_sizes1) * 1.3
    ymax = 0  # This value will be used as the ylim of the y-axis in both plots

    xs = np.linspace(0, xmax, num=200)
    for coefficients in list_coefficients:
        ys1 = predicted_bound_vector_sizes(
            coefficients, (xs, np.repeat(0, len(xs))))
        ax1.plot(xs, ys1, color="blue", alpha=0.3)
        ymax = max(ymax, max(ys1))

    ax1.scatter(vector_input_sizes1, vector_costs,
                color="black", label="Runtime Data")

    for coefficients in list_coefficients:
        max_input_size2 = max(vector_input_sizes2)
        ys2 = predicted_bound_vector_sizes(
            coefficients, (xs, np.repeat(max_input_size2, len(xs))))
        ax2.plot(xs, ys2, color="blue", alpha=0.3)
        ymax = max(ymax, max(ys2))

    ax2.scatter(vector_input_sizes1, vector_costs,
                color="black", label="Runtime Data")

    ax1.set_xlim(0, xmax)
    ax1.set_ylim(0, ymax)
    ax1.set_xlabel("First input list")
    ax1.set_ylabel("Cost")
    ax1.set_box_aspect(1)
    ax1.legend()
    ax1.set_title("Second input size = 0")

    ax2.set_xlim(0, xmax)
    ax2.set_ylim(0, ymax)
    ax2.set_xlabel("First input list")
    ax2.set_ylabel("Cost")
    ax2.set_box_aspect(1)
    ax2.legend()
    ax2.set_title("Second input size = {}".format(max(vector_input_sizes2)))

    plt.show()


def plot_projection_second_input(runtime_data, list_coefficients, predicted_bound_vector_sizes):
    num_posterior_samples_plot = min(100, len(list_coefficients))
    seed = 42
    prng = np.random.default_rng(seed)
    selected_indices = prng.sample(
        range(0, len(list_coefficients)), size=num_posterior_samples_plot, replace=False)
    list_coefficients = [list_coefficients[i] for i in selected_indices]

    vector_input_sizes1 = [size1 for ((size1, _), _) in runtime_data]
    vector_input_sizes2 = [size2 for ((_, size2), _) in runtime_data]
    vector_costs = [cost for (_, cost) in runtime_data]

    fig, axes = plt.subplots(nrows=1, ncols=2, layout="constrained")
    ax1, ax2 = axes[0], axes[1]
    fig.set_figheight(15)

    xmax = max(vector_input_sizes2) * 1.3
    ymax = 0  # This value will be used as the ylim of the y-axis in both plots

    xs = np.linspace(0, xmax, num=200)
    for coefficients in list_coefficients:
        ys1 = predicted_bound_vector_sizes(
            coefficients, (np.repeat(0, len(xs)), xs))
        ax1.plot(xs, ys1, color="blue", alpha=0.3)
        ymax = max(ymax, max(ys1))

    ax1.scatter(vector_input_sizes2, vector_costs,
                color="black", label="Runtime Data")

    for coefficients in list_coefficients:
        max_input_size1 = max(vector_input_sizes1)
        ys2 = predicted_bound_vector_sizes(
            coefficients, (np.repeat(max_input_size1, len(xs)), xs))
        ax2.plot(xs, ys2, color="blue", alpha=0.3)
        ymax = max(ymax, max(ys2))

    ax2.scatter(vector_input_sizes2, vector_costs,
                color="black", label="Runtime Data")

    ax1.set_xlim(0, xmax)
    ax1.set_ylim(0, ymax)
    ax1.set_xlabel("Second input list")
    ax1.set_ylabel("Cost")
    ax1.set_box_aspect(1)
    ax1.legend()
    ax1.set_title("First input size = 0")

    ax2.set_xlim(0, xmax)
    ax2.set_ylim(0, ymax)
    ax2.set_xlabel("Second input list")
    ax2.set_ylabel("Cost")
    ax2.set_box_aspect(1)
    ax2.legend()
    ax2.set_title("First input size = {}".format(max(vector_input_sizes1)))

    plt.show()
