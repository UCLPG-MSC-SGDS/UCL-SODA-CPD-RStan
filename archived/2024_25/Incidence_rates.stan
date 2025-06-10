
data {
	int<lower = 0> N;
	array[N] int<lower = 0> t;
	array[N] int<lower = 0> y;
}

parameters {
	real<lower = 0> D0;
	real r;
}

model {
	D0 ~ gamma(2, 0.1);
	r ~ normal(0, 1);
	
	for (n in 1:N) {
		real lambda = D0 * exp(r * t[n]);
		y[n] ~ poisson(lambda);
	}
}

generated quantities {
  vector[N] y_pred;
  for (n in 1:N) {
    y_pred[n] = poisson_rng(D0 * exp(r * t[n]));
  }
}

