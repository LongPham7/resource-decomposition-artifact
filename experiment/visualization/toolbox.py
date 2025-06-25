import matplotlib.pyplot as plt

from benchmark_data.benchmark_parameters \
    import get_counter_scientific_notation_exponent, \
    get_total_cost_scientific_notation_exponent


# Save and show an image


def save_and_show_image(image_dict, extra_artists=[]):
    image_path = image_dict.get("image_path")
    show = image_dict["show"]

    if image_path is not None:
        plt.savefig(image_path, format="pdf", bbox_inches="tight",
                    bbox_extra_artists=extra_artists)
    if show:
        plt.show()
    plt.close()


# Create a plot with only a legend by plotting empty data with label


def plot_legend(ax):
    ax.fill_between([], [], [], color="blue", alpha=0.2,
                    label="5-95th Percentiles")
    ax.plot([], [], color="blue", linestyle="solid", label="Median")
    ax.scatter([], [], color="black", marker=".", label="Observed Data")
    ax.plot([], [], color="red", linestyle="dashed", label="Ground Truth")

    ax.legend(ncols=4)
    ax.set_axis_off()


# Get an exponent used in the scientific notation for z-axis tick labels in all
# axes (i.e., all subplots) within an input figure


def get_sci_exponent(fig):
    list_axes = fig.get_axes()
    list_sci_exponents = [None for _ in range(len(list_axes))]
    for i, ax in enumerate(list_axes):
        if hasattr(ax, "zaxis"):
            z_formatter = ax.zaxis.get_major_formatter()
            # Extract the exponent used in the scientific notation of z-axis
            if hasattr(z_formatter, "orderOfMagnitude"):
                z_exponent = z_formatter.orderOfMagnitude
                if z_exponent != 0:
                    list_sci_exponents[i] = z_exponent

    return list_sci_exponents


# Verify that the scientific notation exponents of z-axis tick labels in all
# subplots in a figure are as expected


def verify_sci_exponent(benchmark, fig):
    list_sci_exponents = get_sci_exponent(fig)
    for i, sci_exponent in enumerate(list_sci_exponents):
        if sci_exponent is not None and sci_exponent != 0:
            if i != len(list_sci_exponents) - 1:
                # If this is not the last subplot, the plot is for a counter's
                # runtime data.
                expected_sci_exponent = get_counter_scientific_notation_exponent(
                    benchmark, i)
                assert sci_exponent == expected_sci_exponent, "Unexpected scientific notation exponent (counter {}): {}".format(
                    i, sci_exponent)
            else:
                # If this is the last subplot, the plot is for the total cost.
                expected_sci_exponent = get_total_cost_scientific_notation_exponent(
                    benchmark)
                assert sci_exponent == expected_sci_exponent, "Unexpected scientific notation exponent (total cost): {}".format(
                    sci_exponent)


# Matplotlib runtime configuration suitable for LatTex figures


latex_fig_rc = {
    "font.family": "serif",
    "text.usetex": False
    # "text.usetex": True,
    # 'text.latex.preamble': r"""
    #     \usepackage[tt=false]{libertine}
    #     \usepackage[T1]{fontenc}
    #     \usepackage[varqu]{zi4}
    #     \usepackage[libertine]{newtxmath}"""
}
