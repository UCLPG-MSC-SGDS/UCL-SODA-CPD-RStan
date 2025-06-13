
setwd("~/Documents/Websites/UCL-SODA-CPD-RStan/datasets")

# Load the packages with library()
library("sf")
library("tmap")
library("spdep")
library("rstan")
library("geostan")
library("SpatialEpi")
library("tidybayes")
library("tidyverse")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# load the shape files
england_LA_shp <- read_sf("England Local Authority Shapefile.shp")
england_Region_shp <- read_sf("England Regions Shapefile.shp")

# load in the cross-sectional road accident dataset
road_accidents <- read.csv("Road Casualties Data 2015-2020.csv")

# calculate the expected number of cases
road_accidents$ExpectedNum <- round(expected(population = road_accidents$Population, cases = road_accidents$Casualties, n.strata = 1), 0)

# merge the attribute table to the shapefile
spatial.data <- merge(england_LA_shp, road_accidents, by.x = c("LAD21CD", "LAD21NM"), by.y = c("LAD21CD", "LAD21NM"), all.x = TRUE)

# reordering the columns
spatial.data <- spatial.data[, c(3,1,2,4,5,7,6)]

# Add tutorials on mapping here

# continue

# need to be coerced into a spatial object
sp.object <- as(spatial.data, "Spatial")

# needs to be coerced into a matrix object
adjacencyMatrix <- shape2mat(sp.object)

# we extract the components for the ICAR model
extractComponents <- prep_icar_data(adjacencyMatrix)

n <- as.numeric(extractComponents$group_size)
nod1 <- extractComponents$node1
nod2 <- extractComponents$node2
n_edges <- as.numeric(extractComponents$n_edges)

y <- spatial.data$Casualties
x <- spatial.data$IMDScore
e <- spatial.data$ExpectedNum

# put all components into a list object
stan.spatial.dataset <- list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, Y=y, X=x, Off_set=e)

icar_poisson_fit = stan("/Users/anwarmusah/Documents/Websites/UCL-SODA-CPD-RSTAN/archived/2024_25/icar_poisson_model.stan", data=stan.spatial.dataset, iter=3000, chains=3, verbose = FALSE)

# diagnostic check on the rHats - put everything into a data frame
diagnostic.checks <- as.data.frame(summary(icar_poisson_fit, pars=c("alpha", "beta", "rr_alpha", "rr_beta", "rr_mu", "sigma", "phi", "lp__"), probs=c(0.025, 0.5, 0.975))$summary)
# create binary variable
diagnostic.checks$valid <- ifelse(diagnostic.checks$Rhat < 1.05, 1, 0)
# tabulate it
table(diagnostic.checks$valid)


# extraction key posterior results for the generated quantities 
relativeRisk.results <- as.data.frame(summary(icar_poisson_fit, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)
# now cleaning up this table up
# first, insert clean row numbers to new data frame
row.names(relativeRisk.results) <- 1:nrow(relativeRisk.results)
# second, rearrange the columns into order
relativeRisk.results <- relativeRisk.results[, c(1,4,5,7)]
# third, rename the columns appropriately
colnames(relativeRisk.results)[1] <- "rr"
colnames(relativeRisk.results)[2] <- "rrlower"
colnames(relativeRisk.results)[3] <- "rrupper"
colnames(relativeRisk.results)[4] <- "rHAT"

# view clean table 
head(relativeRisk.results)

# now, we proceed to generate our risk maps
# align the results to the areas in shapefile
spatial.data$rr <- relativeRisk.results[, "rr"]
spatial.data$rrlower <- relativeRisk.results[, "rrlower"]
spatial.data$rrupper <- relativeRisk.results[, "rrupper"]

# create categories to define if an area has significant increase or decrease in risk, or nothing all 
spatial.data$Significance <- NA
spatial.data$Significance[spatial.data$rrlower<1 & spatial.data$rrupper>1] <- 0    # NOT SIGNIFICANT
spatial.data$Significance[spatial.data$rrlower==1 | spatial.data$rrupper==1] <- 0  # NOT SIGNIFICANT
spatial.data$Significance[spatial.data$rrlower>1 & spatial.data$rrupper>1] <- 1    # SIGNIFICANT INCREASE
spatial.data$Significance[spatial.data$rrlower<1 & spatial.data$rrupper<1] <- -1   # SIGNIFICANT DECREASE

table(spatial.data$Significance)

# For map design for the relative risk -- you want to understand or get a handle on what the distribution for risks look like
# this would inform you of how to create the labelling for the legends when make a map in tmap
summary(spatial.data$rr)
hist(spatial.data$rr)

# creating the labels
RiskCategorylist <- c(">0.0 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 & <1.01",
	"1.01 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00")

# next, we are creating the discrete colour changes for my legends and want to use a divergent colour scheme
# scheme ranges from extreme dark blues to light blues to white to light reds to extreme dark reds
# you can pick your own colour choices by checking out this link [https://colorbrewer2.org]

RRPalette <- c("#65bafe","#98cffe","#cbe6fe","#dfeffe","white","#fed5d5","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")

# categorising the risk values to match the labelling in RiskCategorylist object
spatial.data$RelativeRiskCat <- NA
spatial.data$RelativeRiskCat[spatial.data$rr>= 0 & spatial.data$rr <= 0.25] <- -4
spatial.data$RelativeRiskCat[spatial.data$rr> 0.25 & spatial.data$rr <= 0.50] <- -3
spatial.data$RelativeRiskCat[spatial.data$rr> 0.50 & spatial.data$rr <= 0.75] <- -2
spatial.data$RelativeRiskCat[spatial.data$rr> 0.75 & spatial.data$rr < 1] <- -1
spatial.data$RelativeRiskCat[spatial.data$rr>= 1.00 & spatial.data$rr < 1.01] <- 0
spatial.data$RelativeRiskCat[spatial.data$rr>= 1.01 & spatial.data$rr <= 1.10] <- 1
spatial.data$RelativeRiskCat[spatial.data$rr> 1.10 & spatial.data$rr <= 1.25] <- 2
spatial.data$RelativeRiskCat[spatial.data$rr> 1.25 & spatial.data$rr <= 1.50] <- 3
spatial.data$RelativeRiskCat[spatial.data$rr> 1.50 & spatial.data$rr <= 1.75] <- 4
spatial.data$RelativeRiskCat[spatial.data$rr> 1.75 & spatial.data$rr <= 2.00] <- 5
spatial.data$RelativeRiskCat[spatial.data$rr> 2.00 & spatial.data$rr <= 10] <- 6

# check to see if legend scheme is balanced - if a number is missing that categorisation is wrong!
table(spatial.data$RelativeRiskCat)

# map of relative risk
rr_map <- tm_shape(spatial.data) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tm_shape(spatial.data) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))


tm_shape(spatial.data) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))


spatial.data$SIR <- spatial.data$Casualties/spatial.data$ExpectedNum

# skeleton
#tmap v3.3-3
tm_shape(spatial.data) + tm_polygons(alpha = 0.05, border.alpha = 0.4) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0, border.alpha = 1) + tm_text("name", size = "AREA")

#tmap v4
tm_shape(spatial.data) + tm_polygons(fill_alpha = 0.05, col_alpha = 0.4) +
	tm_shape(england_Region_shp) + tm_polygons(fill_alpha = 0) + tm_text("name", size = "AREA")


#tmap v3.3-3
tm_shape(spatial.data) + tm_fill("SIR", midpoint = 1, palette = "-RdYlGn") +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0, border.alpha = 1) + tm_text("name", size = "AREA")

install.packages("remotes")
remotes::install_version("tmap", version = "3.3-4")

