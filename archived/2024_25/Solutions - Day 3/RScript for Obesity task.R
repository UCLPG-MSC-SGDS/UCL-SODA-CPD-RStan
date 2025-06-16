rm(list = ls())
gc()

obesity <- read.csv("Obesity and Fastfoods in MSOAs data.csv")

# RESTCAT is categorical so see categories:
table(obesity$RESTCAT)

# use zero (no junk food outlet)as the reference category
# create dummy variables

# Create dummy variables for choice
obesity$RESTCAT1 <- ifelse(obesity$RESTCAT == 1, 1, 0)
obesity$RESTCAT2 <- ifelse(obesity$RESTCAT == 2, 1, 0)
obesity$RESTCAT3 <- ifelse(obesity$RESTCAT == 3, 1, 0)
obesity$RESTCAT4 <- ifelse(obesity$RESTCAT == 4, 1, 0)

# Pass data to list() object

stan_obesity_dataset <- list(
	N=nrow(obesity), 
	obesity = obesity$OBESE,
	total = obesity$TOTAL,
	deprivation = obesity$IMDMSOA,
	rcat1 = obesity$RESTCAT1,
	rcat2 = obesity$RESTCAT2,
	rcat3 = obesity$RESTCAT3,
	rcat4 = obesity$RESTCAT4
)

# run stan model
results = stan(file = "Stan script for obesity task.stan", 
	data = stan_obesity_dataset, 
	chains = 3, iter = 3000)

print(results, pars = c("OR_deprivation", "OR_rcat1", "OR_rcat2", "OR_rcat3", "OR_rcat4"))

# Example interpretation of `OR_cat3`
# Relative to MSOA areas no junk food outlets, young adolescents in MSOA areas with junk food outlets density ranging between 26-50 are 
# --- 1.10 times (or 10%) more likely to become obese (95% CrI: 1.02 to 1.19). This increased risk is significant because 95% CrI excludes 
# --- the null value (1). 