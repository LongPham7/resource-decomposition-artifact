import os


# Relevant directory paths


ocaml_benchmark_directory = os.path.expanduser(os.path.join(
    "/home", "ocaml-benchmarks"))


experiment_analysis_root_directory = os.path.expanduser(os.path.join(
    "/home", "experiment"))


# Directory storing Python source files for running experiments


def get_experiment_analysis_directory():
    return os.path.expanduser(experiment_analysis_root_directory)


# Bin directory of the OCaml benchmark suite


def get_ocaml_benchmark_bin_directory():
    return os.path.join(ocaml_benchmark_directory, "bin")


# Directory for runtime cost data


def get_runtime_cost_data_directory(benchmark, bucket_size):
    experiment_analysis_directory = get_experiment_analysis_directory()
    bin_directory = os.path.join(experiment_analysis_directory, "bin")
    function_directory = benchmark
    bucket_directory = "bucket{}".format(bucket_size)
    runtime_cost_data_directory = "runtime_cost_data"
    target_directory = os.path.join(
        bin_directory, function_directory, bucket_directory, runtime_cost_data_directory)

    if not os.path.exists(target_directory):
        os.makedirs(target_directory)
    return target_directory


# Directory for the inference results


def get_inference_result_directory(benchmark, bucket_size, counter_index, model_id):
    experiment_analysis_directory = get_experiment_analysis_directory()
    bin_directory = os.path.join(experiment_analysis_directory, "bin")
    function_directory = benchmark
    bucket_directory = "bucket{}".format(bucket_size)
    counter_directory = "counter{}".format(counter_index)
    model_directory = "model{}".format(model_id)
    inference_result_directory = os.path.join(
        bin_directory, function_directory, bucket_directory, counter_directory, model_directory)

    if not os.path.exists(inference_result_directory):
        os.makedirs(inference_result_directory)

    return inference_result_directory


# Directory for images


def get_image_directory(benchmark, bucket_size, counter_index):
    experiment_analysis_directory = get_experiment_analysis_directory()
    root_images_directory = os.path.join(
        experiment_analysis_directory, "images")
    function_directory = benchmark
    bucket_directory = "bucket{}".format(bucket_size)
    if counter_index is None:
        images_directory = os.path.join(
            root_images_directory, function_directory, bucket_directory)
    else:
        counter_directory = "counter{}".format(counter_index)
        images_directory = os.path.join(
            root_images_directory, function_directory, bucket_directory, counter_directory)

    if not os.path.exists(images_directory):
        os.makedirs(images_directory)
    return images_directory
