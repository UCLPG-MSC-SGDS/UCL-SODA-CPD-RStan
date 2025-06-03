rm(list = ls())

# Create a sequence of numbers between 15 and 32 incrementing by 0.1.
x <- seq(15, 32, by = .1)
# Choose the mean as 23 and standard deviation as 0.5 (very tiny).
y1 <- dnorm(x, mean = 23, sd = 0.5)
# Choose the mean as 23 and standard deviation as 1.5 (small).
y2 <- dnorm(x, mean = 23, sd = 1.5)
# Choose a different mean as 18 and standard deviation as 0.5 (very tiny).
y3 <- dnorm(x, mean = 18, sd = 0.3)
# Choose a different mean as 27 and standard deviation as 0.5 (very tiny).
y4 <- dnorm(x, mean = 27.5, sd = 0.3)

plot(x,y1, type = "l", col="red", ylim = c(0,1.5), 
	xlab = "BMI estimates [range: 15 to 32]", ylab = "Probability density", 
	lwd=3, main = "Sample size (N) = 171")
lines(x,y2,col="green", lwd=3)
lines(x,y3,col="orange", lwd=3)
lines(x,y4,col="purple", lwd=3)

# slide number #

# Create a sequence of numbers between 15 and 32 incrementing by 0.1.
x <- seq(15, 32, by = .1)
# Choose the mean as 23 and standard deviation as 0.5 (very tiny).
y1 <- dnorm(x, mean = 23, sd = 1.2)
den <- data.frame(y=y1, x=x)

plot(
	x,
	y1, 
	type = "l", 
	col="red", 
	xlab = "BMI estimates [range: 15 to 32]", 
	ylab = "Cumulative Density function (CDF)", 
	lwd=3
	)

# create shaded area
polygon(
	c(den$x[den$x <= 22.1 ], 22.1), 
	c(den$y[den$x <= 22.1 ], 0),
	col = "red",
	border = "red"
	)

# compute the cumulative probability of BMI 15 to 22.1
pnorm(22.1, mean = 23, sd = 1.2, lower.tail = FALSE)





# Public health school example for children in reception; 

# Suppose that 35% (0.35) of children in reception usually detected to be underweight (BMI < 17.5). 
# What would be the probability of observing 7 children kids of 30 being underweight? 

id <- 0:30
probability <- dbinom(id, size=30, prob=0.35)
prevalence <- probability*100

barplot(probability, 
	names.arg = id, 
	ylim = c(0, 0.2),
	xlab = "Number of kids underweight",
	ylab = "Probability density")

dbinom(7, size=30, prob=0.35)

# CMF plot
barplot(probability, 
	names.arg = id, 
	ylim = c(0, 0.2),
	xlab = "Number of kids underweight",
	ylab = "Probability density",
	col = c("red", "red", "red", "red",
		"red", "red", "red", "red", "white", "white",
		"white","white","white","white","white","white","white",
		"white","white","white","white","white","white","white",
		"white","white","white","white","white","white","white"))

