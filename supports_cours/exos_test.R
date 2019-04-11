library(learnr)

install.packages("wordcloud")
install.packages("MASS")

library(ggplot2)
cars_table <- table(mtcars$mpg)
str(mtcars)
library(MASS)
str(Cars93)

# Create mfr_table of manufacturer frequencies
mfr_table <- table(Cars93$Manufacturer)

# Create the default wordcloud from this table
wordcloud(words = names(mfr_table), 
          freq = as.numeric(mfr_table), 
          scale = c(2, 0.25))

# Change the minimum word frequency
wordcloud(words = names(mfr_table), 
          freq = as.numeric(mfr_table), 
          scale = c(2, 0.25), 
          min.freq = 1)

# Create model_table of model frequencies
model_table <- table(Cars93$Model)

# Create the wordcloud of all model names with smaller scaling
wordcloud(words = names(model_table), 
          freq = as.numeric(model_table), 
          scale = c(0.75, 0.25), 
          min.freq = 1)


# Pour séparer en 2 diapos
# <div class="columns-2">
#   
#   ![](images/mini_tweet_dplyr.png){width=30"}
# <div class="green3">
#   
#   <br></br>
#   <br></br>
#   <font size="6">
#   
#   - Langage `R` Base ?
#   
#   - Le tidyverse ?
#   
#   </font>
#   </div>
#   
#   </div>
install.packages("insuranceData")
install.packages("tabplot")
# # Load the insuranceData package
# library(insuranceData)
# 
# # Use the data() function to load the dataCar data frame
# data(dataCar)
# 
# # Load the tabplot package
# suppressPackageStartupMessages(library(tabplot))
# 
# # Generate the default tableplot() display
# tableplot(dataCar)

# {data-background=images/tidyverse.png data-background-size=cover}

str(airquality)
library(lattice)
# Create scatterplot
xyplot(Ozone ~ Temp, data = airquality,
       # Add main label
       main = "Environmental conditions in New York City (1973)", 
       # Add axis labels
       xlab = "Temperature (Fahrenheit)",
       ylab = "Ozone (ppb)", abline = c(0,1))

str(airquality)

# Create a density plot
densityplot(~ Ozone, data = airquality, 
            # Choose how raw data is shown
            plot.points = FALSE)

# Create a density plot
densityplot(~ Ozone, data = airquality, 
            # Choose how raw data is shown
            plot.points = T)
# Create a density plot
densityplot(~ Ozone, data = airquality, 
            # Choose how raw data is shown
            plot.points = "jitter")


# 'USCancerRates' is pre-loaded
str(USCancerRates)

# Create reordered variable
library(dplyr)
USCancerRates <-
  mutate(USCancerRates, 
         state.ordered = reorder(state, rate.female, median, na.rm = TRUE))

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


# USCancerRates has been pre-loaded
str(USCancerRates)

# Create a density plot
densityplot(~rate.male + rate.female,
            data = USCancerRates,
            # Set value of 'outer' 
            outer = FALSE,
            # Add x-axis label
            xlab = "Rate (per 100,000)",
            # Add a legend
            auto.key = TRUE,
            plot.points = FALSE,
            ref = TRUE)

# Create a density plot
densityplot(~rate.male + rate.female,
            data = USCancerRates,
            # Set value of 'outer' 
            outer = TRUE,
            # Add x-axis label
            xlab = "Rate (per 100,000)",
            # Add a legend
            auto.key = TRUE,
            plot.points = FALSE,
            ref = TRUE)
str(airquality)

xyplot(Ozone ~ Temp, airquality, groups = Month,
       # Complete the legend spec
       auto.key = list(space = "right", 
                       title = "Month", 
                       text = month.name[5:9]))


bwplot(state ~ rate.male + rate.female,
       data = USCancerRates, 
       outer = TRUE, 
       # Add a label for the x-axis
       xlab = "Rate (per 100,000)",
       # Add strip labels
       strip = strip.custom(c("Male","Female")))

xyplot(Petal.Length ~ Petal.Width | Species, iris,
       strip = strip.custom(style = 4))

xyplot(Petal.Length ~ Petal.Width | Species, iris,
       strip = FALSE,
       strip.left = strip.custom(style = 4, horizontal = FALSE))


