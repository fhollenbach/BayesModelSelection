data{
  int<lower=1> n;// Number of data
  int<lower=1> K;// Number of covariates
  matrix[K, n] X;
  real y[n];
  real m_exp; // expected number of non-zero slopes
  }// slab_scale = 5, slab_df = 25 -> 8 divergences
transformed data{
  real slab_scale = 3;// Scale for large slopes
  real slab_scale2 = square(slab_scale);
  real slab_df = 25;// Effective degrees of freedom for large slopes
  real half_slab_df = 0.5 * slab_df;
}
parameters{
  vector[K] beta_tilde;
  vector<lower=0>[K] lambda;
  real<lower=0> c2_tilde;
  real<lower=0> tau_tilde;
  real alpha;
  real<lower=0> sigma;
  }
transformed parameters{
  vector[K] beta;
  {real tau0 = (m_exp / (K - m_exp)) * (sigma / sqrt(1.0 * n));
  real tau = tau0 * tau_tilde;// tau ~ cauchy(0, tau0) // c2 ~ inv_gamma(half_slab_df, half_slab_df * slab_scale2)
  // Implies that marginally beta ~ student_t(slab_df, 0, slab_scale)
    real c2 = slab_scale2 * c2_tilde;
    vector[K] lambda_tilde = sqrt(c2 * square(lambda) ./ (c2 + square(tau) * square(lambda)) );
      beta = tau * lambda_tilde .* beta_tilde;}
    }
model{
  beta_tilde ~ normal(0, 1);
  lambda ~ cauchy(0, 1);
  tau_tilde ~ cauchy(0, 1);
  c2_tilde ~ inv_gamma(half_slab_df, half_slab_df);
  alpha ~ normal(0, 2);
  sigma ~ normal(0, 2);
  y ~ normal(X'* beta + alpha, sigma);
  }

