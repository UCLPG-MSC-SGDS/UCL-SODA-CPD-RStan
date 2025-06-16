
data {
  int<lower=0> N;                           // Number of row observations (964 MSOA areas)
  array[N] int<lower=0> obesity;            // Number of those obese in an MSOA
  array[N] int<lower=0> total;              // Total number of those in an MSOA
  array[N] real deprivation;                // Deprivation
  array[N] int<lower=0, upper=1> rcat1;     // RESTCAT1
  array[N] int<lower=0, upper=1> rcat2;     // RESTCAT2
  array[N] int<lower=0, upper=1> rcat3;     // RESTCAT3
  array[N] int<lower=0, upper=1> rcat4;     // RESTCAT4
}

parameters {
	// intercept
	real beta0;
	// beta1 to beta5
	vector[5] beta; 
}

transformed parameters {
	// create array for prevalence
	array[N] real<lower=0, upper=1> prevalence;     
	// apply link function to the regression model for the prevalence
	for (i in 1:N){
		prevalence[i] = inv_logit(beta0 + beta[1]*deprivation[i] + beta[2]*rcat1[i] + beta[3]*rcat2[i] + beta[4]*rcat3[i] + beta[5]*rcat4[i]);
	}
}

model {
	// priors on intercept (beta0) and coefficients
	beta0 ~ normal(0, 1);
	beta ~ normal(0, 1);
	
	// likelihood function
	for (i in 1:N){
		obesity[i] ~ binomial(total[i], prevalence[i]);
	}
}

generated quantities {
  real OR_deprivation = exp(beta[1]);
  real OR_rcat1       = exp(beta[2]);
  real OR_rcat2       = exp(beta[3]);
  real OR_rcat3       = exp(beta[4]);
  real OR_rcat4       = exp(beta[5]);
}

