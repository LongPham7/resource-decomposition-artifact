import numpy as np
import math
from scipy.stats import norm, pearsonr
from sklearn.feature_selection import mutual_info_regression


# Evaluate percentiles


def evaluate_percentile(array_coefficients, input_sizes, predicted_bound_vector_coefficients, q):
    vector_predicted_bounds = predicted_bound_vector_coefficients(
        array_coefficients, input_sizes)
    return np.percentile(vector_predicted_bounds, q)


# Categorize one-dimensional runtime cost data


def categorize_runtime_cost_data(runtime_data_size_only):
    vector_input_sizes = [size for (size, _) in runtime_data_size_only]
    unique_xs = list(set(vector_input_sizes))
    categorized_data = {x: [] for x in unique_xs}
    for (size, cost) in runtime_data_size_only:
        categorized_data[size].append(cost)
    return categorized_data


# Calculate total cost gaps and their log likelihood


def cost_gaps_and_log_likelihood(array_cost_gaps, array_predictions, cost_sigma):
    assert (array_cost_gaps.shape == array_predictions.shape)
    total_cost_gaps = np.sum(array_cost_gaps, axis=1)
    min_cost_gap = np.min(total_cost_gaps)

    log_likelihood_inputs = norm.logpdf(array_cost_gaps, loc=0, scale=cost_sigma) \
        - np.log(norm.cdf(array_predictions, loc=0, scale=cost_sigma) - 0.5)
    log_likelihoods = np.sum(log_likelihood_inputs, axis=1)
    max_cost_gap_log_likelihood = np.max(log_likelihoods)
    return min_cost_gap, max_cost_gap_log_likelihood


# Display statistics of inferred coefficients and cost_sigmas


def inference_result_statistics(list_coefficients, list_cost_sigmas=None):
    array_coefficients = np.array(list_coefficients)
    q_lower, q_upper = 5, 95
    print("Inferred coefficients")
    print("{}th percentiles = {}".format(
        q_lower, np.percentile(array_coefficients, q=q_lower, axis=0)))
    print("Medians = {}".format(np.median(array_coefficients, axis=0)))
    print("{}th percentiles = {}".format(
        q_upper, np.percentile(array_coefficients, q=q_upper, axis=0)))
    # print("Standard deviations = {}".format(array_stdvs = np.std(array_coefficients, axis=0)))

    if list_cost_sigmas is not None:
        array_cost_sigmas = np.array(list_cost_sigmas)
        print("Inferred cost sigmas")
        print("{}th percentile = {}".format(
            q_lower, np.percentile(array_cost_sigmas, q_lower)))
        print("Median = {}".format(np.median(array_cost_sigmas)))
        print("{}th percentile = {}".format(
            q_upper, np.percentile(array_cost_sigmas, q_upper)))
        # print("Standard deviation = {}".format(np.std(array_cost_sigmas)))


# Estimate an information criterion.


def estimate_information_criterion(log_likelihood_tuple, num_cost_samples,
                                   num_parameters, criterion_type):
    aic_information, dic_information, waic_information = log_likelihood_tuple
    if criterion_type == "aic":
        max_log_likelihood = aic_information
        return 2 * num_parameters - 2 * max_log_likelihood
    elif criterion_type == "bic":
        max_log_likelihood = aic_information
        return num_parameters * math.log(num_cost_samples) - 2 * max_log_likelihood
    elif criterion_type == "dic1":
        mean_log_likelihood, log_likelihood_representative, _ = dic_information
        # DIC1 (i..e, the first version of DIC) is defined as (-2) *
        # (log_likelihood_representative - p_DIC), where p_DIC (effective number
        # of parameters) is (-2) * mean_log_likelihood - (-2) *
        # log_likelihood_representative. Therefore, DIC is equal to (-4) *
        # mean_log_likelihood + 2 * log_likelihood_representative.
        return -4 * mean_log_likelihood + 2 * log_likelihood_representative
    elif criterion_type == "dic2":
        _, log_likelihood_representative, var_log_likelihood = dic_information
        # DIC2 (i.e., the second version of DIC) is defined as (-2) *
        # (log_likelihood_representative - p_DIC), where p_DIC (effective number
        # of parameters) is 2 * var_log_likelihood.
        return 4 * var_log_likelihood - 2 * log_likelihood_representative
    elif criterion_type == "waic1":
        sum_log_mean_likelihood_inputs, sum_mean_log_likelihood_inputs, _ = waic_information
        # WAIC1 (i.e. the first version of WAIC) is defined as (-2) *
        # (sum_log_mean_likelihood_inputs -p_WAIC), where p_WAIC (effective
        # number of parameters) is 2 * (sum_log_mean_likelihood_inputs -
        # sum_mean_log_likelihood_inputs). Therefore, WAIC1 is equal to (-4) *
        # sum_mean_log_likelihood_inputs + 2 * sum_log_mean_likelihood_inputs.
        return -4 * sum_mean_log_likelihood_inputs + 2 * sum_log_mean_likelihood_inputs
    elif criterion_type == "waic2":
        sum_log_mean_likelihood_inputs, _, sum_var_log_likelihood_inputs = waic_information
        # WAIC2 (i.e., the second version of WAIC) is defined as (-2) *
        # (sum_log_mean_likelihood_inputs - p_WAIC), where p_WAIC (effective
        # number of parameters) is sum_var_log_likelihood_inputs.
        return -2 * sum_log_mean_likelihood_inputs + 2 * sum_var_log_likelihood_inputs
    else:
        raise ValueError("The given criterion type is invalid")


# Select the best model according to information criteria


def select_best_model(list_runtime_data_model_data, verbose=False):
    list_aic = []
    list_bic = []
    list_dic1 = []
    list_dic2 = []
    list_waic1 = []
    list_waic2 = []
    for (runtime_data, log_likelihood_tuple, num_parameters) in list_runtime_data_model_data:
        num_cost_samples = len(runtime_data)
        aic = estimate_information_criterion(
            log_likelihood_tuple, num_cost_samples, num_parameters, criterion_type="aic")
        bic = estimate_information_criterion(
            log_likelihood_tuple, num_cost_samples, num_parameters, criterion_type="bic")
        dic1 = estimate_information_criterion(
            log_likelihood_tuple, num_cost_samples, num_parameters, criterion_type="dic1")
        dic2 = estimate_information_criterion(
            log_likelihood_tuple, num_cost_samples, num_parameters, criterion_type="dic2")
        waic1 = estimate_information_criterion(
            log_likelihood_tuple, num_cost_samples, num_parameters, criterion_type="waic1")
        waic2 = estimate_information_criterion(
            log_likelihood_tuple, num_cost_samples, num_parameters, criterion_type="waic2")
        list_aic.append(aic)
        list_bic.append(bic)
        list_dic1.append(dic1)
        list_dic2.append(dic2)
        list_waic1.append(waic1)
        list_waic2.append(waic2)

    # Print out the scores of all models
    if verbose:
        for i in range(0, len(list_runtime_data_model_data)):
            print("Model {}: AIC = {:.2f}, BIC = {:.2f}, DIC1 = {:.2f}, DIC2 = {:.2f}, WAIC1 = {:.2f}, WAIC2 = {:.2f}".format(
                i, list_aic[i], list_bic[i], list_dic1[i], list_dic2[i], list_waic1[i], list_waic2[i]))

    model_min_aic = np.argmin(list_aic)
    model_min_bic = np.argmin(list_bic)
    model_min_dic1 = np.argmin(list_dic1)
    model_min_dic2 = np.argmin(list_dic2)
    model_min_waic1 = np.argmin(list_waic1)
    model_min_waic2 = np.argmin(list_waic2)
    return model_min_aic, model_min_bic, model_min_dic1, model_min_dic2, model_min_waic1, model_min_waic2


# Figure out the dependency between variables


def extract_max_costs_in_buckets(runtime_data_size_only):
    categorized_data = categorize_runtime_cost_data(runtime_data_size_only)
    result = []
    for size, list_costs in categorized_data.items():
        result.append((size, max(list_costs)))
    return result


def calculate_correlation_coefficients(runtime_data_size_only):
    # We focus on the case of the runtime cost data containing two size
    # variables
    list_sizes1 = [size1 for ((size1, _), _) in runtime_data_size_only]
    list_sizes2 = [size2 for ((_, size2), _) in runtime_data_size_only]
    list_costs = [cost for (_, cost) in runtime_data_size_only]

    correlation1 = pearsonr(list_sizes1, list_costs)
    correlation2 = pearsonr(list_sizes2, list_costs)
    return correlation1, correlation2


def calculate_mutual_information(runtime_data_size_only):
    list_sizes1 = [size1 for ((size1, _), _) in runtime_data_size_only]
    list_sizes2 = [size2 for ((_, size2), _) in runtime_data_size_only]
    list_costs = [cost for (_, cost) in runtime_data_size_only]

    X = np.stack((np.array(list_sizes1), np.array(list_sizes2)), axis=1)
    y = np.array(list_costs)
    return mutual_info_regression(X, y, discrete_features=False)
