#######################
## Première exemple ###
#######################


WorldPhonesDF <- 
  as.data.frame(as.table(WorldPhones[-1, ]), 
                responseName = "Phones")

library(lattice,lib.loc = "/rpi701/fichiers/drs/R/x86_64-redhat-linux-gnu-library/3.5")
library(latticeExtra,lib.loc = "/rpi701/fichiers/drs/R/x86_64-redhat-linux-gnu-library/3.5")
# Create the dot plot
dotplot(Var1 ~ Phones | Var2,
        data = WorldPhonesDF, 
        as.table = TRUE,
        scales = list(x = list(log = TRUE,
                               equispaced.log = FALSE, 
                               relation = "sliced")),
        # Fill in suitable value of par.settings
        par.settings = ggplot2like(),
        # Fill in suitable value of lattice.options
        lattice.options = ggplot2like.opts())


############################
### Deuxième exemple  ######
############################


# Create factor variable
airquality$Month.Name <- 
  factor(airquality$Month, levels = 1:12, 
         labels = month.name)

# Create histogram of Ozone, conditioning on Month
histogram(~ Ozone | Month.Name,
          data = airquality, as.table = TRUE,
          # Set border to be transparent
          border = "transparent", 
          # Set fill color to be mid-gray
          col = "grey50")

############################
# Troisième exemple   ######
############################

# Create the dot plot
dotplot(Cause ~ Rate | Status, data = USMortality,
        groups = Sex, auto.key = list(columns = 2),
        scales = list(x = list(log = TRUE, 
                               equispaced.log = FALSE)), 
        # Provide pch values for the two groups
        pch = c(3,1))

dotplot(Cause ~ Rate | Sex, USMortality, groups = Status,
         auto.key = list(column = 2),
         pch = 16, col =c("#96281b","#34495e"),
         scales = list(x = list(log =2,equispaced.log = FALSE )))

dotplot(Cause ~ Rate | Sex, USMortality, groups = Status,
        auto.key = list(column = 2),
        par.settings = simpleTheme(pch = 16, col =c("#96281b","#34495e")),
        scales = list(x = list(log =2,equispaced.log = FALSE )))


############################### 
# Utilisation RColorBrewer ####
###############################

# Obtain colors from RColorBrewer
library(RColorBrewer)
my.colors <- brewer.pal(n = 12, name = "Set1")


# Convert the Month code into a new variable Month.Name containing month names, suitably ordering the levels (code to do this is already provided). Check that the Month.Name variable has twelve levels.
# 
# Drop empty levels of Month.Name using droplevels(). Check that the Month.Name variable has five levels remaining after this step.
# 
# Obtain five colors from the RColorBrewer function brewer.pal(). In the call to brewer.pal(), n should be the desired number of colors.
# 
# Create a density plot of Ozone grouped by Month.Name, with
# 
# line colors taken from RColorBrewer as described above,
# line width doubled, and
# legend placed on the right of the figure.

# Create factor variable
airquality$Month.Name <- 
  factor(airquality$Month, levels = 1:12, 
         labels = month.name)
levels(airquality$Month.Name)

# Drop empty levels
airquality$Month.Name <- droplevels(airquality$Month.Name)
levels(airquality$Month.Name)

# Obtain colors from RColorBrewer
library(RColorBrewer)
my.colors <- brewer.pal(n = 5, name = "Set1")

# Density plot of ozone concentration grouped by month
densityplot(~ Ozone, data = airquality, groups = Month.Name,
            plot.points = FALSE,
            auto.key = list(space = "right"),
            # Fill in value of col
            par.settings = simpleTheme(col = my.colors, 
                                       # Fill in value of lwd
                                       lwd = 2))

###############################
## Panel function #############
###############################

panel.xyrug <- function(x, y, ...)
{
  # Reproduce standard scatter plot
  panel.xyplot(x, y, ...)
  
  # Identify observations with x-value missing
  x.missing <- is.na(x)
  
  # Identify observations with y-value missing
  y.missing <- is.na(y)
  
  # Draw rugs along axes
  panel.rug(x = x[y.missing], y = y[x.missing])
}

airquality$Month.Name <- 
  factor(month.name[airquality$Month], levels = month.name)

xyplot(Ozone ~ Solar.R | Month.Name, data = airquality,
       panel = panel.xyrug, as.table = TRUE)


##################################################
# Créer un violin plot ###########################
##################################################

# Create factor variable with month names
airquality$Month.Name <- 
  factor(month.name[airquality$Month], levels = month.name)

# Create box-and-whisker plot
bwplot(Month.Name ~ Ozone + Temp, airquality, 
       # Specify outer
       outer = TRUE, 
       # Specify x-axis relation
       scales = list(x = list(relation = "free")),
       # Specify layout
       layout = c(2, 1),
       # Specify x-axis label
       xlab = "Measured value")

# Create violin plot
bwplot(Month.Name ~ Ozone + Temp, airquality, 
       # Specify outer
       outer = TRUE, 
       # Specify x-axis relation
       scales = list(x = list(relation = "free")),
       # Specify layout
       layout = c(2, 1),
       # Specify x-axis label
       xlab = "Measured value",
       # Replace default panel function
       panel = panel.violin)


# Create a custom panel function named panel.ss that should:
#   
#   Call panel.smoothScatter() to draw a 2-D density plot of the data. You need to pass the x and y arguments. (The ... arguemnt is already pased for you.)
# Add a nonparametric LOESS smooth of the data in red (using panel.loess()). You need to pass the x, y, and col arguments.
# Add a y=x reference line (use panel.abline()). You need to pass the a and b arguments.
# Use this panel function to create a plot of rate.female against rate.male.


# Create panel function
panel.ss <- function(x, y, ...) {
  # Call panel.smoothScatter()
  panel.smoothScatter(x, y, ...)
  # Call panel.loess()
  panel.loess(x, y, col = "red")
  # Call panel.abline()
  panel.abline(0, 1)
}

# Create plot
xyplot(rate.female ~ rate.male, data = USCancerRates,
       panel = panel.ss,
       main = "County-wise deaths due to cancer")



# The default panel function for bwplot() has two additional arguments that you have not used before:
#   
#   pch = "|" replaces the black dot representing the median inside the box by a line segment dividing the box into two smaller rectangles.
# 
# notch = TRUE puts "notches" on the side of the boxes that indicate a confidence interval for the median; the overlapping of notches for two subgroups suggests that the true medians of the two subgroups are not significantly different.


# Create the box and whisker plot
bwplot(division.ordered ~ rate.male,
       data = USCancerRates,
       # Indicate median by line instead of dot
       pch = "|", 
       # Include notches for confidence interval
       notch = TRUE,
       # The x-axis should plot log-transformed values
       scales = list(x = list(log = TRUE, equispaced.log = FALSE)),
       xlab = "Death Rate in Males (per 100,000)")




