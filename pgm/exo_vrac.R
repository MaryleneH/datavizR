library(lattice)
data(USCancerRates, package = "latticeExtra")
str(USCancerRates)

xyplot(rate.female~rate.male, data = USCancerRates
       )

a <- lm(rate.female~rate.male, data = USCancerRates)


# Use the 'airquality' dataset
str(airquality)

# Create the histogram
histogram(~ Ozone, data = airquality, 
          # Specify number of bins
          nint = 15,
          # Specify quantity displayed on y-axis
          type = "density")

# Create a density plot
densityplot(~ Ozone, data = airquality, 
            # Choose how raw data is shown
            plot.points = "jitter")

bwplot(state~rate.male, data = USCancerRates)

library(dplyr)
USCancerRates <- mutate(USCancerRates,
                        states_ordered = 
                          reorder(state,rate.male, na.rm = T))
bwplot(states_ordered~rate.male, data = USCancerRates)

xyplot(rate.female~rate.male|state, USCancerRates,
       grid = T, abline = c(0,1))

histogram(~rate.female + rate.male,USCancerRates,outer = T,
          layout = c(1,2))

# USCancerRates has been pre-loaded
str(USCancerRates)

# Create a density plot
densityplot( ~ rate.male + rate.female,
             data = USCancerRates, 
             outer = TRUE,
             # Suppress data points
             plot.points = FALSE,
             # Add a reference line
             ref = TRUE)


panel.histdens <- function(x, ...){
  panel.histogram(x,...)
  panel.lines(density(x,na.rm = T))
}

histogram(~rate.female + rate.male, USCancerRates,
          type = "density", layout = c(1,2),
          scale = list(x = list(log = T, equispaced.log = F)),
          panel = panel.histdens)
