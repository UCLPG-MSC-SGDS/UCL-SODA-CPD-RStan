// Example of using stan to perform a multivariable linear regression model
// Assessing the association between average LSOA house price with IMD, income and PTAL scores (all log transformed outside of Stan!)

data {
	int N;                               // defining the number of observations
	int k;                               // defining the number of parameters for K variables
	
	array[N] real logincome;             // specification for average income variable
	array[N] real logdeprivation;        // specification for IMD variable
	array[N] real logptaindex;           // specification for PTAL index variable
	array[N] real logprice;              // specification for AVEPRICE to be used as outcome in model
}

parameters {
	vector[k] beta;                      // defining the parameters to estimate
	real<lower=0> sigma;                 // defining the standard deviation as positive real number
}

transformed parameters{
	array[N] real mu;
	
	for (i in 1:N) {
		mu[i] = beta[1] + beta[2]*logincome[i] + beta[3]*logdeprivation[i] + beta[4]*logptaindex[i];
	}
}

model {
	// define mu i.e., predicted outcome for house prices 
	beta ~ normal(0, 100);
	sigma ~ cauchy(0, 100);
	
	// define the likelihood function to compute Bayesian inference
	for(i in 1:N) {
		logprice[i] ~ normal(mu[i], sigma);  
	}
}
