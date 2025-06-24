from json_manipulation import read_inference_result_from_json, read_runtime_cost_data_from_json
import statistical_analysis.toolbox as stat_util


# Display statistics of inferred coefficients and cost_sigmas


def inference_result_statistics_model_id(fun_id, bucket_size, counter_index, model_id):
    inference_result_dict = read_inference_result_from_json(
        fun_id, bucket_size, counter_index, model_id)
    list_coefficients = inference_result_dict["inferred_coefficients"]
    list_cost_sigmas = inference_result_dict.get("inferred_cost_sigmas")
    stat_util.inference_result_statistics(list_coefficients, list_cost_sigmas)


# Calculate Pearson's correlation coefficients and mutual information for
# two-dimensional runtime cost data. This function is specifically for graph
# algorithms where we have two inputs: the number of vertices and the maximum
# degree of the graph.


def correlation_statistics(benchmark, bucket_size, counter_index, max_only):
    runtime_data_size_only = read_runtime_cost_data_from_json(
        benchmark, bucket_size, counter_index)

    if max_only:
        num_samples_original = len(runtime_data_size_only)
        runtime_data_size_only = stat_util.extract_max_costs_in_buckets(
            runtime_data_size_only)
        num_samples_max_only = len(runtime_data_size_only)
        print("Size of the dataset: reduced from {} to {} after taking the maximum of each bucket".format(
            num_samples_original, num_samples_max_only))

    correlation1, correlation2 = stat_util.calculate_correlation_coefficients(
        runtime_data_size_only)
    mutual_info1, mutual_info2 = stat_util.calculate_mutual_information(
        runtime_data_size_only)
    print("Pearson's correlation coefficients: vertices = {:.4f}, max degree = {:.4f}".format(
        correlation1.statistic, correlation2.statistic))
    print("P-values of the correlation: vertices = {:.4f}, max degree = {:.4f}".format(
        correlation1.pvalue, correlation2.pvalue))
    print("Mutual information: vertices = {:.4f}, max degree = {:.4f}".format(
        mutual_info1, mutual_info2))
