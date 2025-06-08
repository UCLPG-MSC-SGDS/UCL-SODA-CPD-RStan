
# Base-R codes for slide 34
# parameters
mu  <- 25
sd  <- 5
x   <- seq(7, 43, by = 0.1)
y   <- dnorm(x, mean = mu, sd = sd)

## base-R plot
plot(x, y, type = "l", lwd = 2,
	main = "Normal Distribution Plot: Larvicidal Concentration in Standing Water Source (mg/L)",
	xlab = "Concentration (mg/L)",
	ylab = "Density")
abline(v = mu, lty = 2)            # vertical line at the mean

# shade area for BTI >= 30
x_shade <- x[x >= 30]
y_shade <- y[x >= 30]
polygon(c(x_shade, rev(x_shade)),
	c(y_shade, rep(0, length(y_shade))),
	col = rgb(1, 0, 0, alpha = 0.3), border = NA)
abline(v = 30, lty = 3)            # dashed line at the cutoff

# compute the plausibility for 30 mg/L when mu = 25 and sd = 5
dnorm(30.0, mean = 25, sd = 5)

# compute the cumulative probability for 30 mg/L when its assumed mu is 25 and sd is 5
pnorm(30.0, mean = 25, sd = 5)
# compute the exceedance probability for 30 mg/L when its assumed mu is 25 and sd is 5
1 - pnorm(30.0, mean = 25, sd = 5)


# Base-R codes for slide 35
# parameters
mean_val <- 38
shapes   <- c(1, 2, 5, 7, 10, 12, 15, 20, 25, 30, 40)
scales   <- mean_val / shapes    # θ = mean/α

# x-grid
x <- seq(0, 200, length.out = 500)

# Compute densities (one column per α)
dens_mat <- sapply(seq_along(shapes), function(i) {
	dgamma(x, shape = shapes[i], scale = scales[i])
})

# Set up empty plot with y-limits
ymax <- max(dens_mat, na.rm = TRUE)
plot(
	x, dens_mat[,1], type = "n",
	ylim = c(0, ymax),
	main = expression(paste("Gamma(", alpha, " = 1,2,5,7,10,12,15,20,25,30,40;  ", mu, "=38)")),
	xlab = "x", ylab = "Density"
)

# Colours & line‐types (one for each α)
cols <- c("red", "black", "blue", "green4", "purple", "orange", "brown", "pink", "yellow", "grey", "black")
ltys <- 1:length(shapes)

# Overlay each curve
for(i in seq_along(shapes)) {
	lines(
		x, dens_mat[,i],
		col = cols[i], lty = ltys[i], lwd = 2
	)
}

# Add vertical dashed line at the mean
abline(v = mean_val, lty = 2, col = "darkgray", lwd = 2)

# Legend
legend(
	"topright",
	legend = paste0("α = ", shapes),
	col    = cols,
	lty    = ltys,
	lwd    = 2,
	bty    = "n"
)

# Base-R code plot show on slide 36

# Set up
x <- seq(0, 200, length.out = 1000)          # x values
mu = 20
shape <- 2                                   # shape parameter < scale → skewed right
scale <- mu/shape                            # scale parameter

# Gamma PDF
y <- dgamma(x, shape = shape, scale = scale)
# estimate its median (50th percentile)
med_estimate <- qgamma(0.5, shape = shape, scale = scale)

# Plot
plot(x, y, type = "l", lwd = 2, col = "red",
	main = "Gamma Distribution Plot: Ambient Anti-mosquito (DEET) Concentration in Households (mg/L)",
	ylab = "Density", xlab = "DEET (microgram per cubic metre)")

# shade area for DEET >= 50
x_shade <- x[x >= 50]
y_shade <- y[x >= 50]
polygon(c(x_shade, rev(x_shade)),
	c(y_shade, rep(0, length(y_shade))),
	col = rgb(1, 0, 0, alpha = 0.3), border = NA)
abline(v = 50, lty = 3)                      # dashed line at the cutoff
abline(v = (2-1)*scale, lty = 3)            # dashed line at the cutoff

pgamma(50, shape = shape, scale = scale)
1 - pgamma(50, shape = shape, scale = scale)


# Codes for slides 37-40 for Beta Distribution
# Scenario 1: alpha > beta, beta = 2
x  <- seq(0, 1, length.out = 1000)
y1 <- dbeta(x, shape1 = 3, shape2 = 2)
y2 <- dbeta(x, shape1 = 4, shape2 = 2)
y3 <- dbeta(x, shape1 = 5, shape2 = 2)
y4 <- dbeta(x, shape1 = 6, shape2 = 2)
# set y–limits to accommodate the tallest curve
ylim <- c(0, max(y1, y2, y3, y4))

plot(x, y1, type = "l", lwd = 2, col = "blue", ylim = ylim,
	xlab = "p", ylab = "Density",
	main = "Scenario 1: Beta(α,2) as α increases: 3→6")
lines(x, y2, lwd = 2, col = "red")
lines(x, y3, lwd = 2, col = "darkgreen")
lines(x, y4, lwd = 2, col = "purple")
legend("topright",
	legend = c("α=3,β=2","α=4,β=2","α=5,β=2","α=6,β=2"),
	col    = c("blue","red","darkgreen","purple"),
	lwd    = 2,
	bty    = "n")

# Scenario 2: beta > alpha, alpha = 2
x  <- seq(0, 1, length.out = 1000)
y1 <- dbeta(x, shape1 = 2, shape2 = 3)
y2 <- dbeta(x, shape1 = 2, shape2 = 4)
y3 <- dbeta(x, shape1 = 2, shape2 = 5)
y4 <- dbeta(x, shape1 = 2, shape2 = 6)
ylim <- c(0, max(y1, y2, y3, y4))

plot(x, y1, type = "l", lwd = 2, col = "blue", ylim = ylim,
	xlab = "p", ylab = "Density",
	main = "Scenario 2: Beta(2,β) as β increases: 3→6")
lines(x, y2, lwd = 2, col = "red")
lines(x, y3, lwd = 2, col = "darkgreen")
lines(x, y4, lwd = 2, col = "purple")
legend("topright",
	legend = c("α=2,β=3","α=2,β=4","α=2,β=5","α=2,β=6"),
	col    = c("blue","red","darkgreen","purple"),
	lwd    = 2,
	bty    = "n")

# Scenario 3: alpha = beta
x  <- seq(0, 1, length.out = 1000)
y1 <- dbeta(x, shape1 = 1, shape2 = 1)
y2 <- dbeta(x, shape1 = 2, shape2 = 2)
y3 <- dbeta(x, shape1 = 3, shape2 = 3)
y4 <- dbeta(x, shape1 = 4, shape2 = 4)
ylim <- c(0, max(y1, y2, y3, y4))

plot(x, y1, type = "l", lwd = 2, col = "blue", ylim = ylim,
	xlab = "p", ylab = "Density",
	main = "Scenario 3: Beta(k,k): symmetry tightens as k increases")
lines(x, y2, lwd = 2, col = "red")
lines(x, y3, lwd = 2, col = "darkgreen")
lines(x, y4, lwd = 2, col = "purple")
legend("topright",
	legend = c("k=1 (flat)","k=2","k=3","k=4"),
	col    = c("blue","red","darkgreen","purple"),
	lwd    = 2,
	bty    = "n")

# scenario 4
mu = 0.25
sigma = 0.05

beta = (mu - 1) + (mu *(1-mu)^2)/(sigma^2)
alpha = (beta * mu)/(1-mu)

alpha
beta

x  <- seq(0, 1, length.out = 1000)
y <- dbeta(x, shape1 = 19, shape2 = 56)

plot(x, y, type = "l", lwd = 2, col = "blue",
	xlab = "p", ylab = "Density",
	main = "Scenario 4: Beta(19, 56) when assumed p = 0.25 (25%) [SD = 0.05]")

abline(v = mu, lty = 2, col = "darkgray", lwd = 2)

# codes for slides 41 and 42 for binomial distribution
# parameters
p <- 0.35    # assumed infestation prevalence
n <- 30      # total houses surveyed
y <- 7       # observed infested
k   <- 0:n
pmf <- dbinom(k, size = n, prob = p)

# probability of exactly y = 7
prob_y <- dbinom(y, size = n, prob = p)
cat("P(Y = 7) =", round(prob_y, 4), "\n")

# plot the PMF
barplot(pmf,
	names.arg = k,
	col       = ifelse(k == y, "tomato", "lightblue"),
	border    = NA,
	main      = "Binomial(30, 0.35)",
	xlab      = "Number of infested households",
	ylab      = "")

# annotate the highlighted bar
text(x = which(k == y),
	y = pmf[k == y] + 0.005,
	labels = paste0("P=", round(prob_y, 3)),
	col = "tomato")

# parameters
p <- 0.35    # assumed infestation prevalence
n <- 30      # total houses surveyed
y <- 7       # threshold for "at most y"

# PMF and CDF values
k        <- 0:n
pmf      <- dbinom(k, size = n, prob = p)
cum_prob <- pbinom(y, size = n, prob = p)

# print the result
#cat("P(Y <= ", y, ") =", round(cum_prob, 4), "\n", sep="")

# plot the PMF with shading for k <= y
barplot(pmf,
	names.arg = k,
	col       = ifelse(k <= y, "tomato", "lightblue"),
	border    = NA,
	main      = "Binomial(30, 0.35)",
	xlab      = "Number of infested households",
	ylab      = "")

# annotate the shaded region
#mtext(text = paste0("P(Y ≤ ", y, ") = ", round(cum_prob, 3)),
#	side = 3, line = 0.5, adj = 1, col = "tomato")