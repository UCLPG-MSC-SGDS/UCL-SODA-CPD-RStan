
data {
	// define N (120)
	int<lower = 0> N;
	// create an array of size 120 to store BTI values
	array[N] real bti;
}
parameters {
	// defined the mean as mu
	real<lower = 0> mu;
	// defined the SD as sigma
	real<lower = 0> sigma;
}
model {
  bti ~ normal(mu, sigma);
}

