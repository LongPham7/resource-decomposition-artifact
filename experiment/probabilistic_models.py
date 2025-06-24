import numpy as np


# Model averaging of the logarithmic and linear models where we vary the noise
# distribution depending on whether the prediction is above or below the model
# boundary (e.g., 2 * log2(n) from the red-black tree). We do not use empirical
# Bayes. Instead, the mixing probability is fixed to 0.5.


stan_code_model0 = """
data {
    int<lower=0> num_observations;
    vector<lower=0>[num_observations] input_sizes;
    vector<lower=0>[num_observations] costs;

    real<lower=0> chi_square_nu;
    real<lower=0> lognormal_linear_mu;
    real<lower=0> lognormal_linear_sigma;

    real<lower=0> cost_sigma_above_boundary;
}
transformed data {
    real<lower=0, upper=1> lambda = 0.5;
}
parameters {
    vector<lower=0>[4] coefficients_logarithmic;
    vector<lower=0>[2] coefficients_linear;
}
model {
    real predicted_bound;
    real size;
    real log_likelihood_logarithmic = log(lambda);
    real log_likelihood_linear = log1m(lambda);

    coefficients_logarithmic ~ chi_square(chi_square_nu);
    coefficients_linear[1] ~ chi_square(chi_square_nu);
    coefficients_linear[2] ~ lognormal(lognormal_linear_mu, lognormal_linear_sigma);

    for (n in 1:num_observations) {
        size = input_sizes[n];

        // Logarithmic model
        predicted_bound = coefficients_logarithmic[1] \
            + coefficients_logarithmic[2]*log(1 + coefficients_logarithmic[3] + coefficients_logarithmic[4]*size);
        if (predicted_bound < costs[n] || costs[n] < 0) {
            target += negative_infinity();
        }
        else {
            if (predicted_bound <= 2 * log2(1 + size)) {
                log_likelihood_logarithmic += uniform_lpdf(costs[n] | 0, predicted_bound);
            }
            else {
                log_likelihood_logarithmic += normal_lpdf(costs[n] | predicted_bound, cost_sigma_above_boundary) \
                    - log_diff_exp(log(0.5), normal_lcdf(0 | predicted_bound, cost_sigma_above_boundary));
            }
        }

        // Linear model
        predicted_bound = coefficients_linear[1] + coefficients_linear[2] * size;
        if (predicted_bound < costs[n] || costs[n] < 0) {
            target += negative_infinity();
        }
        else {
            log_likelihood_linear += uniform_lpdf(costs[n] | 0, predicted_bound);
        }
    }
    target += log_sum_exp(log_likelihood_logarithmic, log_likelihood_linear);
}
generated quantities {
    real predicted_bound;
    real size;
    real log_prior_density = 0;
    real log_joint_density_logarithmic = log(lambda);
    real log_joint_density_linear = log1m(lambda);

    for (n in 1:4) {
        log_prior_density += chi_square_lpdf(coefficients_logarithmic[n] | chi_square_nu);
    }

    log_prior_density += chi_square_lpdf(coefficients_linear[1] | chi_square_nu);
    log_prior_density += lognormal_lpdf(coefficients_linear[2] | lognormal_linear_mu, lognormal_linear_sigma);

    log_joint_density_logarithmic += log_prior_density;
    log_joint_density_linear += log_prior_density;

    for (n in 1:num_observations) {
        size = input_sizes[n];

        // Logarithmic model
        predicted_bound = coefficients_logarithmic[1] \
            + coefficients_logarithmic[2]*log(1 + coefficients_logarithmic[3] + coefficients_logarithmic[4]*size);
        if (predicted_bound <= 2 * log2(1 + size)) {
            log_joint_density_logarithmic += uniform_lpdf(costs[n] | 0, predicted_bound);
        }
        else {
            log_joint_density_logarithmic += normal_lpdf(costs[n] | predicted_bound, cost_sigma_above_boundary) \
                - log_diff_exp(log(0.5), normal_lcdf(0 | predicted_bound, cost_sigma_above_boundary));
        }

        // Linear model
        predicted_bound = coefficients_linear[1] + coefficients_linear[2] * size;
        log_joint_density_linear += uniform_lpdf(costs[n] | 0, predicted_bound);
    }

    int<lower=1, upper=2> is_logarithmic_model = categorical_logit_rng([log_joint_density_logarithmic, log_joint_density_linear]');
    real<lower=0, upper=1> mixing_probability_logarithmic_model = exp(log_joint_density_logarithmic - log_sum_exp(log_joint_density_logarithmic, log_joint_density_linear));
}
"""


# Model averaging of the logarithmic and linear models where we vary the noise
# distribution depending on whether the prediction is above or below the model
# boundary (e.g., 2 * log2(n) from the red-black tree). In this model, we only
# generate the maximum of each size bucket.


stan_code_model1 = """
data {
    int<lower=0> num_observations;
    vector<lower=0>[num_observations] input_sizes;
    vector<lower=0>[num_observations] costs;

    real<lower=0> chi_square_nu;
    real<lower=0> lognormal_linear_mu;
    real<lower=0> lognormal_linear_sigma;

    real<lower=0> cost_sigma_above_boundary;
    int<lower=1> bucket_size;
}
transformed data {
    real<lower=0, upper=1> lambda = 0.5;
}
parameters {
    vector<lower=0>[4] coefficients_logarithmic;
    vector<lower=0>[2] coefficients_linear;
}
model {
    real predicted_bound;
    real size;
    real log_likelihood_logarithmic = log(lambda);
    real log_likelihood_linear = log1m(lambda);

    coefficients_logarithmic ~ chi_square(chi_square_nu);
    coefficients_linear[1] ~ chi_square(chi_square_nu);
    coefficients_linear[2] ~ lognormal(lognormal_linear_mu, lognormal_linear_sigma);

    for (n in 1:num_observations) {
        size = input_sizes[n];

        // Logarithmic model
        predicted_bound = coefficients_logarithmic[1] \
            + coefficients_logarithmic[2]*log(1 + coefficients_logarithmic[3] + coefficients_logarithmic[4]*size);
        if (predicted_bound < costs[n] || costs[n] < 0) {
            target += negative_infinity();
        }
        else {
            if (predicted_bound <= 2 * log2(1 + size)) {
                log_likelihood_logarithmic += log(bucket_size) + (bucket_size-1)*log(costs[n]) - bucket_size*log(predicted_bound);
            }
            else {
                log_likelihood_logarithmic += normal_lpdf(costs[n] | predicted_bound, cost_sigma_above_boundary) \
                    - log_diff_exp(log(0.5), normal_lcdf(0 | predicted_bound, cost_sigma_above_boundary));
            }
        }

        // Linear model
        predicted_bound = coefficients_linear[1] + coefficients_linear[2] * size;
        if (predicted_bound < costs[n] || costs[n] < 0) {
            target += negative_infinity();
        }
        else {
            log_likelihood_linear += log(bucket_size) + (bucket_size-1)*log(costs[n]) - bucket_size*log(predicted_bound);
        }
    }
    target += log_sum_exp(log_likelihood_logarithmic, log_likelihood_linear);
}
generated quantities {
    real predicted_bound;
    real size;
    real log_prior_density = 0;
    real log_joint_density_logarithmic = log(lambda);
    real log_joint_density_linear = log1m(lambda);

    for (n in 1:4) {
        log_prior_density += chi_square_lpdf(coefficients_logarithmic[n] | chi_square_nu);
    }

    log_prior_density += chi_square_lpdf(coefficients_linear[1] | chi_square_nu);
    log_prior_density += lognormal_lpdf(coefficients_linear[2] | lognormal_linear_mu, lognormal_linear_sigma);

    log_joint_density_logarithmic += log_prior_density;
    log_joint_density_linear += log_prior_density;

    for (n in 1:num_observations) {
        size = input_sizes[n];

        // Logarithmic model
        predicted_bound = coefficients_logarithmic[1] \
            + coefficients_logarithmic[2]*log(1 + coefficients_logarithmic[3] + coefficients_logarithmic[4]*size);
        if (predicted_bound <= 2 * log2(1 + size)) {
            log_joint_density_logarithmic += log(bucket_size) + (bucket_size-1)*log(costs[n]) - bucket_size*log(predicted_bound);
        }
        else {
            log_joint_density_logarithmic += normal_lpdf(costs[n] | predicted_bound, cost_sigma_above_boundary) \
                - log_diff_exp(log(0.5), normal_lcdf(0 | predicted_bound, cost_sigma_above_boundary));
        }

        // Linear model
        predicted_bound = coefficients_linear[1] + coefficients_linear[2] * size;
        log_joint_density_linear += log(bucket_size) + (bucket_size-1)*log(costs[n]) - bucket_size*log(predicted_bound);
    }

    int<lower=1, upper=2> is_logarithmic_model = categorical_logit_rng([log_joint_density_logarithmic, log_joint_density_linear]');
    real<lower=0, upper=1> mixing_probability_logarithmic_model = exp(log_joint_density_logarithmic - log_sum_exp(log_joint_density_logarithmic, log_joint_density_linear));
}
"""


# Model 2 has the same Stan code as model 1. The only difference is that model 2
# uses the bucket size of one regardless of the bucket size used in the runtime
# cost data collection.


list_stan_code = [(stan_code_model0, 7), (stan_code_model1, 7),
                  (stan_code_model1, 7)]


def predicted_bound_vector_coefficients(array_coefficients, input_size, model_id):
    _, num_parameters = array_coefficients.shape
    size = input_size
    if model_id in [0, 1, 2]:
        assert (num_parameters == list_stan_code[model_id]
                [1]), "Mismatch in the number of model parameters"

    if model_id in [0, 1, 2]:
        coefficients_logarithmic = array_coefficients[:, 0:4]
        coefficients_linear = array_coefficients[:, 4:6]
        # is_logarithmic_model = 1 indicates that the model is logarithmic;
        # otherwise, is_logarithmic_model = 2 indicates the model is linear.
        is_logarithmic_model = array_coefficients[:, 6]

        predicted_bound_logarithmic = coefficients_logarithmic[:, 0] + coefficients_logarithmic[:, 1]*np.log(
            1 + coefficients_logarithmic[:, 2] + coefficients_logarithmic[:, 3]*size)
        predicted_bound_linear = coefficients_linear[:,
                                                     0] + coefficients_linear[:, 1] * size

        return np.where(is_logarithmic_model < 1.01, predicted_bound_logarithmic, predicted_bound_linear)
    elif model_id == "logarithmic":
        return array_coefficients[:, 0] + array_coefficients[:, 1]*np.log(1 + array_coefficients[:, 2] + array_coefficients[:, 3]*size)
    elif model_id == "linear":
        return array_coefficients[:, 0] + array_coefficients[:, 1]*size
    else:
        raise ValueError("The given model ID is invalid")


def predicted_bound_vector_sizes(coefficients, input_size, model_id):
    size = input_size
    num_parameters = len(coefficients)
    if model_id in [0, 1, 2]:
        assert (num_parameters == list_stan_code[model_id]
                [1]), "Mismatch in the number of model parameters"

    if model_id in [0, 1, 2]:
        coefficients_logarithmic = coefficients[0:4]
        coefficients_linear = coefficients[4:6]
        # is_logarithmic_model = 1 indicates that the model is logarithmic;
        # otherwise, is_logarithmic_model = 2 indicates the model is linear.
        is_logarithmic_model = coefficients[6]

        if is_logarithmic_model < 1.01:
            return coefficients_logarithmic[0] + coefficients_logarithmic[1]*np.log(1 + coefficients_logarithmic[2] + coefficients_logarithmic[3]*size)
        else:
            return coefficients_linear[0] + coefficients_linear[1] * size
    elif model_id == "logarithmic":
        return coefficients[0] + coefficients[1]*np.log(1 + coefficients[2] + coefficients[3]*size)
    elif model_id == "linear":
        return coefficients[0] + coefficients[1]*size
    else:
        raise ValueError("The given model ID is invalid")


def cost_bound_encoded_by_model(model_id):
    if model_id == 0:
        return "Mixture with fixed lambda"
    elif model_id == 1:
        return "Mixture for max costs"
    elif model_id == 2:
        return "Mixture for max costs"
    else:
        raise ValueError("The given model ID is invalid")
