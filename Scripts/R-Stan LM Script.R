
setwd("/Users/anwarmusah/Desktop/Model Catalogue/Stan Scripts/")


# note that r-stan only accept the data in a list format
data <- list(N=4968, K=4, AVEPRICE=dataset[,2], AVEINCOME=dataset[,3], IMDSCORE=dataset[,4], PTAINDEX=dataset[,5])

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

options(mc.cores = 4)

# run stan model
results = stan(file = "Multivariable linear regression Stan-script.stan", data = data, chains = 3, iter = 3000, warmup = 500, thin = 10)
print(results, pars = c("beta", "sigma"))

# graph the posterior samples
params <- extract(results)
hist(params$mu)
hist(params$sigma)

# model <- lm(AVEPRICE~AVEINCOME+IMDSCORE+PTAINDEX, data = dataset)
# options(scipen = 999)
# summary(model)