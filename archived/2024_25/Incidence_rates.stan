
data {
	int<lower = 0> N;
	array[N] int<lower = 0> t;
	array[N] int<lower = 0> y;
}

parameters {
	real<lower = 0> I0;
	real<lower = 0> eta;
	real r;
}

model {
	I0 ~ gamma(2, 0.1);
	eta ~ normal(0, 0.5);
	r ~ normal(0, 1);
	
	for (n in 1:N) {
		real lambda = I0 * exp(r * eta * t[n]);
		y[n] ~ poisson(lambda);
	}
}

generated quantities {
  vector[N] y_pred;
  for (n in 1:N) {
    y_pred[n] = poisson_rng(I0 * exp(r * eta * t[n]));
  }
}

