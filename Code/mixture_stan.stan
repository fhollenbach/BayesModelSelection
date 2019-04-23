data {
  int N;
  int K1;
  int K2;
  int K3;
  int K4;
  vector[N] y;
  int n_groups;
  matrix[N, K1] x1; // predictor variables for group1
  matrix[N, K2] x2; // predictor variables for group2
  matrix[N, K3] x3; // predictor variables for group2
  matrix[N, K4] x4; // predictor variables for group2
}
parameters {
  vector[K1] beta1; //beta coefs 1
  vector[K2] beta2; //beta coefs 2
  vector[K3] beta3; //beta coefs 2
  vector[K4] beta4; //beta coefs 2
  real<lower = 0> sigma[4];
  real<lower=0, upper=1> theta;
}
model {
      vector[n_groups] mu[N]; // matrix of independent variables (including an intercept vector)
      sigma[1] ~ student_t(10,0,3);
      sigma[2] ~ student_t(10,0,3);
      sigma[3] ~ student_t(10,0,3);
      sigma[4] ~ student_t(10,0,3);
      beta1 ~ normal(0, 5);
      beta2 ~ normal(0, 5);
      beta3 ~ normal(0, 5);
      beta4 ~ normal(0, 5);
      theta ~ beta(5, 5);
      for(i in 1:N){
      	    mu[i,1] =  x1[i]*beta1; // model group 1
  	    mu[i,2] =  x2[i]*beta2; // model group 2
            mu[i,3] =  x3[i]*beta3; // model group 3
  	    mu[i,4] =  x4[i]*beta4; // model group
            target+=  log_mix(theta, normal_lpdf(y[i] | mu[i,1], sigma[1]), normal_lpdf(y[i] | mu[i,2], sigma[2]), normal_lpdf(y[i] | mu[i,3], sigma[3]), normal_lpdf(y[i] | mu[i,4], sigma[4]));
            }
}
