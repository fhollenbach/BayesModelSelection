data{ 
  int<lower=1> N;  // total number of observations 
  vector[N] Y;  // response variable 
  int<lower=1> K;  // number of population-level effects 
  matrix[N, K] X;  // population-level design matrix 
}
transformed data { 
  int Kc = K - 1; 
  matrix[N, K - 1] Xc;  // centered version of X 
  vector[K - 1] means_X;  // column means of X before centering 
  for (i in 2:K) { 
    means_X[i - 1] = mean(X[, i]); 
    Xc[, i - 1] = X[, i] - means_X[i - 1]; 
  } 
} 
parameters { 
  vector[Kc] b;  // population-level effects 
  real temp_Intercept;  // temporary intercept 
  real<lower=0> sigma;  // residual SD 
} 
model{ 
  vector[N] mu = temp_Intercept + Xc * b;
  // priors including all constants 
  target += normal_lpdf(b | 0,5);
  target += normal_lpdf(temp_Intercept | 0,5);
    target += student_t_lpdf(sigma | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  // likelihood including all constants 
  target += normal_lpdf(Y | mu, sigma);
}
generated quantities{
  real b_Intercept = temp_Intercept - dot_product(means_X, b); 
  vector[N] mu2 = b_Intercept + Xc * b;
  real dev;
  dev = 0;
  dev = (-2)*normal_lpdf( Y | mu2 , sigma );
}

