// generated with brms 2.7.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
} 
transformed data {
}
parameters {
  vector[K] b;  // population-level effects
  real<lower=0> sigma;  // residual SD
}
transformed parameters {
}
model {
  vector[N] mu = X * b;
  // priors including all constants
  target += normal_lpdf(b | 0,5);
  target += student_t_lpdf(sigma | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  // likelihood including all constants
   target += normal_lpdf(Y | mu, sigma);
} 
generated quantities {
real dev;
vector[N] mu = X * b;
  dev = 0;
  dev = (-2)*normal_lpdf( Y | mu, sigma );
}