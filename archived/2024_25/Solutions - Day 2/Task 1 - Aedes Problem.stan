
data {
	int<lower = 0> N;
	int<lower = 0> Infested;
}

parameters {
	real<lower = 0, upper=1> prevalence;
}

model {
	// using a beta(2, 5) prior distribution suggests that the prevalence of 20-30% is most likely to be observed
	prevalence ~ beta(2, 5);
	// likelihood function
	Infested ~ binomial(N, prevalence);
}

