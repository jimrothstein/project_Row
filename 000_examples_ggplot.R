# GGPLOT2 EXAMPLES  

# DUPLICATE  --file in DS folder
# NOT part of DS
# USEFUL info  https://gist.github.com/Emaasit/bec9825315fae54964c4

## Examples for LVRUG meetup, Nov. 4, 2014
## All examples use either generated or built-in R/ggplot2 data frames

library(ggplot2)
################## Single-layer plots ######################

set.seed(5409)   # for reproducibility
# 1000 rows x 2 columns: a letter and random var
DF <- data.frame(gp = sample(LETTERS[1:3], 1000, replace = TRUE),
                 x = rnorm(1000))

##############################
# Bar chart of gp

#group on gp (letter)
ggplot(DF, aes(x = gp)) + geom_bar(fill = "darkorange")


# Other ways to make this plot by modifying base layer
# These are more relevant in multi-layer ggplots

# Pass aesthetic mapping to the geom - makes it local
ggplot(DF) + geom_bar(aes(x = gp), fill = "darkorange")

# Pass both the data and the aesthetic mapping to the geom
# Both the data and mapping are local to geom_bar()
ggplot() + geom_bar(data = DF, aes(x = gp), fill = "darkorange")
##############################

# Histogram of x + plot title
# x is N(0,1)
ggplot(DF, aes(x = x)) +
    geom_histogram(binwidth = 0.2, fill = "purple") +
    ggtitle("Standard normal simulated data")

# What happens if we map an aesthetic that should be set?
ggplot(DF, aes(x = x)) +
    geom_histogram(binwidth = 0.2, aes(fill = "purple")) 

# Kernel density plot of x, ie outline
ggplot(DF, aes(x = x)) + geom_density(color = "blue", size = 1)


### qplot() is useful for single-layer plots, but 
### set aesthetics have to be protected by I()

qplot(x, data = DF, geom = "histogram", fill = I("purple"),
      binwidth = I(0.2))

qplot(x, data = DF, geom = "density", colour = I("blue"), size = I(1))

#################### Two-layer plots #######################

# Data and aesthetics are common to both layers, so specify
# in ggplot()
# ..density.. is a variable created by stat_bin(), the stat
# associated with geom_histogram()
pp <- ggplot(DF, aes(x = x, y = ..density..)) +
    theme_bw() +
    geom_histogram(binwidth = 0.2, fill = "skyblue") 

#################################################################
# Add density plots in various ways
pp + geom_density(color = "darkorange", size = 1)

# Density plots often look better with fill color
# Add scales package for next example to access alpha() function
# alpha seems to smooth out, not looks like ragged histogram

library(scales)
# alpha - transparency?
ggplot(DF, aes(x = x)) +
    geom_density(color = "darkorange", fill = alpha("orange", 0.4))

# Histogram + density plot without line at bottom
pp1 <- pp + stat_density(geom = "path", color = "darkorange", size = 1)

# Add a standard normal pdf to histogram
pp1 + stat_function(fun = dnorm, size = 1, color = "blue")

## Hmmm...problem. We used ..density.. as the y-aesthetic and 
## stat_function() picked up on that. Let's try again....

ggplot(DF, aes(x = x)) +
    theme_bw() +
    geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "skyblue") +
    stat_density(geom = "path", color = "darkorange", size = 1) +
    stat_function(fun = dnorm, color = "blue", size = 1)

# Moral: Sometimes mapped aesthetics need to be localized.
###################################################################

# New example: (jittered) stripcharts and boxplots

# Boxplots and jittered points by gp
# First example where a legend guide is produced
# geom_boxplot() 'understands' the fill aesthetic,
# geom_point() does not (with a certain exception)

ggplot(DF, aes(x = gp, y = x, fill = gp)) +
    geom_boxplot(outlier.colour = NA) +  # need 'colour' here
    geom_point(position = position_jitter(width = 0.1))

# How about connecting the group medians with a line?
# Problem 1: gp is a factor.  By default, ggplot2 does not plot
#            lines across factor levels.
#   Solution: Use the group aesthetic.
# Problem 2: The medians are not in the data. 
#   Solution: Use stat_summary() to compute them in ggplot2.

last_plot() + 
    stat_summary(fun.y = median, aes(group = 1), geom = "line",
                 colour = "blue", size = 1)



################### Section 3: Faceting #####################
#############################################################

# facet_wrap() facets by levels of one categorical variable,
# but one can optionally 'wrap' plots into multiple rows or columns

# Facet by gp
pp1 + facet_wrap(~ gp)
pp1 + facet_wrap(~ gp, nrow = 2)
pp1 + facet_wrap(~ gp, ncol = 2)

# Manipulate display of x/y scales
pp1 + facet_wrap(~ gp, ncol = 2, scales = "free_y")
pp1 + facet_wrap(~ gp, ncol = 2, scales = "free")



############# Section 4: Basic use of scales ###############

# (a)  Positional scales: 
#      Need to pay attention to class of x or y variable
#
# Typical arguments in positional scales:
#   * breaks    where to position the axis ticks
#   * labels    how to label the axis tick labels

# Example of use of a log 10 scale

ggplot(ChickWeight, aes(x = Time, y = weight, group = Chick)) +
    geom_line()
last_plot() + scale_y_log10()    # built-in scale type

# Better (ignore the warning)
last_plot() + scale_y_log10(breaks = c(50, 100, 200, 300))

# Example of relabeling a factor in an x-scale

ggplot(InsectSprays, aes(x = spray, y = count)) +
    geom_boxplot(aes(fill = spray)) +
    scale_x_discrete(labels = paste("Spray", levels(InsectSprays$spray))) +
    labs(x = "Treatment", y = "Count", fill = "Spray",
         title = "Box plots of insect spray distributions") 


## Examples of attribute scales

## Standard arguments for legend guides:
#    - breaks: levels/values of mapped variable
#    - values: values of the _aesthetic_ to associate with breaks
#    - labels: legend key labels to associate with breaks

# Color lines by diet and customize legend color

ggplot(ChickWeight, aes(x = Time, y = weight, group = Chick, color = Diet)) +
    geom_line() +
    scale_color_manual(values = c("blue", "orange", "darkgreen", "brown"))

# In this case, it may be better to facet:

ggplot(ChickWeight, aes(x = Time, y = weight, group = Chick)) +
    geom_line() + facet_wrap(~ Diet, ncol = 2)

# Example with two legends

ggplot(mpg, aes(x = displ, y = hwy, shape = factor(year), size = cyl)) +
    geom_point() +
    scale_shape_manual(values = c(1, 16))

# Apply a few tweaks
# Note: the color and shape legends are merged
ggplot(mpg, aes(x = displ, y = hwy, shape = factor(year), size = cyl)) +
    geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
    scale_size(range = c(2, 4)) +
    scale_shape_manual(values = c(1, 16)) +
    labs(x = "Engine displacement", y = "Highway mileage",
         shape = "Year", size = "Cylinders", color = "Year") +
    geom_smooth(aes(group = year, color = factor(year)), se = FALSE, size = 1)

# scale_identity() uses the given values in the data to be 
# values of the aesthetic in the legend

ggplot(mpg, aes(x = displ, y = hwy, shape = factor(year), size = cyl)) +
    geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
    scale_size_identity() +
    scale_shape_manual(values = c(1, 16)) +
    labs(x = "Engine displacement", y = "Highway mileage",
         shape = "Year", size = "Cylinders", color = "Year") 

# By default, scale_identity() does not produce a legend. To get one,
# use guide = "legend"

last_plot() + scale_size_identity(guide = "legend")


# Example of a continuous aesthetic

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(color = displ))

last_plot() + 
    scale_color_gradient2(low = "blue", mid = "darkorange", high = "yellow")

# Problem is that default midpoint is zero, so reset:
ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(color = displ),
               position = position_jitter(width = 0.1, height = 0.1)) +
    scale_color_gradient2(low = "blue", mid = "darkorange", high = "yellow",
                          midpoint = 4.5)


#################################################################

# An example using the theming system

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
    labs(x = "Engine Displacement", y = "Highway Mileage") +
    ggtitle("Highway mileage vs. displacement") +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.y = element_text(face = "bold.italic"),
          plot.title = element_text(size = 20, face = "italic", 
                                    hjust = 0))





Raw  scriptPostTalk.R
# Supplemental code script for LVRUG ggplot2 talk
# Contains more advanced examples than those in presentation
# Several of these are not trivial and will require some study

# Load packages
library(ggplot2, quietly = TRUE)
library(scales, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(plyr, quietly = TRUE)
library(reshape2, quietly = TRUE)

## Introductory graph to show off basic elements of ggplot2

# Toy data set to simulate a white noise series
DF <- data.frame(t = seq(40), y = rnorm(40))

# Build the plot step by step

# p1 contains the base layer, including default theme and scales 
# geom_blank() generates an empty plot layer
p1 <- ggplot(DF, mapping = aes(x = x, y = y)) +
    geom_blank()

# Change default theme to black-and-white
p2 <- p1 + theme_bw()

# Add lines and points
p3 <- p2 + geom_line()
p4 <- p3 + geom_point()

# Add a title - grabs some vertical space
p5 <- p4 + ggtitle("White noise series") +
    labs(x = "Time", y = "Z")

# Add a line of text
p6 <- p5 + annotate("text", x = 0, y = 2, label = "Some text")

# Show the incremental layers/modifications of the plot
# Plot layers added as you go from upper left to 
# lower right by row
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)


######## Some toying around with bar charts and histograms ########

## (1) 
##  Investigate different positional adjustments with bar charts

# This toy data frame contains two factors plus their joint frequencies

DF0 <- data.frame(f1 = gl(2, 3, labels = c("A", "B")),
                  f2 = gl(3, 1, length = 6, labels = c("I", "II", "III")),
                  freq = c(10, 16, 14, 20, 21, 18))

# Default position adjustment is to stack
# Use stat = "identity" when the frequencies are provided
ggplot(DF0, aes(x = f1, y = freq, fill = f2)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("yellow", "darkorange", "royalblue")) +
    labs(x = "Factor 1", y = "Frequency", fill = "Factor 2")

# Create side-by-side bar charts by dodging:
ggplot(DF0, aes(x = f1, y = freq, fill = f2)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("yellow", "darkorange", "royalblue")) +
    labs(x = "Factor 1", y = "Frequency", fill = "Factor 2")

# Stack to a total of 1 with position = "fill"
# This plots conditional distributions of f2 for each level of f1
ggplot(DF0, aes(x = f1, y = freq, fill = f2)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = c("yellow", "darkorange", "royalblue")) +
    labs(x = "Factor 1", y = "Relative frequency", fill = "Factor 2")

# A common request is to plot the (relative) frequency values as text
# annotations. Here is the trick to doing this:

require(plyr, quietly = TRUE)
ypos <- ddply(DF0, .(f1), mutate, 
              ypos = cumsum(freq) - freq/2,
              yrelpos = round(ypos/sum(freq), 3),
              yrelval = round(freq/sum(freq), 3))

# Conditional frequency distributions with annotation
ggplot(DF0, aes(x = f1, y = freq, fill = f2)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("yellow", "darkorange", "royalblue")) +
    labs(x = "Factor 1", y = "Frequency", fill = "Factor 2") +
    geom_text(data = ypos, aes(y = ypos, label = freq), size = 6)

# Conditional relative frequency distributions
ggplot(DF0, aes(x = f1, y = freq, fill = f2)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = c("yellow", "darkorange", "royalblue")) +
    labs(x = "Factor 1", y = "Relative Frequency", fill = "Factor 2") +
    geom_text(data = ypos, aes(y = yrelpos, label = yrelval), size = 6)

# Conditional percent frequency distributions
# Uses a function from the scales package

require(scales, quietly = TRUE)
ggplot(DF0, aes(x = f1, y = freq, fill = f2)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = c("yellow", "darkorange", "royalblue")) +
    labs(x = "Factor 1", y = "Relative Frequency", fill = "Factor 2") +
    geom_text(data = ypos, aes(y = yrelpos, label = 100 * yrelval), size = 6) +
    scale_y_continuous(labels = percent)

# Relative and percent frequency charts

# Relative frequency histograms are a PITA - 
# you need to adjust the density in each class by the binwidth.
# When the binwidth is 1, the density histogram *is* the relative
# frequency histogram. Otherwise, the relative frequency in a histogram
# is ..density.. * binwidth. Here's a way to specify a relative
# frequency histogram in ggplot2 (non-faceted!):

DF1 <- data.frame(x = rnorm(1000))

ggplot(DF1, aes(x = x)) +
    geom_histogram(aes(y = ..density.. * 0.2),
                   binwidth = 0.2, fill = "blue") +
    ylab("Relative frequency")

# Unfortunately, you need to specify the value of the binwidth
# explicitly when doing it this way.

# To plot percentages on the y-axis, first plot the y-values as
# proportions and then add
# ... + scale_y_continuous(labels = percent)
#
# where the percent_format() function comes from the scales package

#################################################################


## Linear regression example

# Remove the 5-cylinder cars from the mpg data frame,
# which is built into the ggplot2 package
mpg0 <- subset(mpg, cyl != 5)

# Regress highway mileage on city mileage
m <- lm(hwy ~ cty, data = mpg0)

# Construct the basic plot
# Jittered points + fitted line with 95% confidence limits for
# the conditional mean
p <- ggplot(mpg0, aes(x = cty, y = hwy)) +
    geom_point(position = position_jitter(width = 0.4, height = 0.4)) +
    geom_smooth(method = lm, size = 1)

p    # plots all of the data together
p + facet_wrap(~ cyl)     # facet by no. cylinders

betas <- round(coef(m), 3)   # extract fitted model coefficients
r2 <- round(summary(m)$r.square, 3)
# Create a data frame to pass to ggplot2 for annotation purposes.
# txt consists of character strings representing the LHS of an
# equality, val represents the values on the RHS

# Note: the code used to create the strings comes from plotmath
# Type ?plotmath to view its help page
regout <- data.frame(cty = 10, hwy = c(35, 45, 40),
                     txt = c("R^2 ==", "b[0] ==", "b[1] =="),
                     val = c(r2, betas),
                     stringsAsFactors = FALSE)

# Add information about regression model by pasting the pieces
# together as a character string - parse = TRUE converts the string
# into a plotmath expression
p + geom_text(data = regout, aes(label = paste(txt, val)),
              size = 6, parse = TRUE, hjust = 0)

# Let's plot the fitted line and R^2 in the overall model
regout2 <- data.frame(cty = 10, hwy = c(45, 40),
                      txt = c(paste0("hat(y) == ",
                                     betas[1], "+ ", betas[2], "~x"),
                              paste0("R^2 == ", r2)),
                      stringsAsFactors = FALSE)
p + geom_text(data = regout2, aes(label = txt), hjust = 0, size = 6,
              parse = TRUE)

# This is too easy - let's do this by cylinder :)

# Function to grab the coefficients and R^2 from a model object m
# with input data d (a subset of mpg0)
foo <- function(d) 
{
    m <- lm(hwy ~ cty, data = d)
    v <- round(c(coef(m), summary(m)$r.square), 3)
    names(v) <- c("b0", "b1", "r2")
    v
}

# Get fitted model coefficients by cyl from mpg0
mcoefs <- ddply(mpg0, .(cyl), foo)

# Next step: construct a data object that constructs the same
# types of strings as in the last plot

formfun <- function(d)
{
    data.frame(cty = 10, hwy = c(45, 40),
               txt = c(paste0("hat(y) == ",
                              d$b0, "+ ", d$b1, "~x"),
                       paste0("R^2 == ", d$r2)),
               stringsAsFactors = FALSE)
}

formdat <- ddply(mcoefs, .(cyl), formfun)

# Let's party!
p + geom_text(data = regout2, aes(label = txt), hjust = 0, size = 6,
              parse = TRUE) +
    facet_wrap(~ cyl)

# Look carefully at what we've done: the hard work was to create
# the first graphic - then we applied a couple of wrapper functions
# and calls to ddply() to generalize the text strings and then
# used facet_wrap() to apply them to all panels. A key step, which
# you may not have noticed, is to include cyl as a variable in
# formdat - this is why geom_text() can map the parsed strings to
# each panel.


## Some fun with geom_rect() and geom_text()
## This example comes from the ggplot2 book, using the
## unemp and presidential data frames.

## When using geom_rect() as a background layer, it's best to
## do it first so that other layers draw on top of it.

presidents <- presidential[-(1:2), ]
presidents$mid <- with(presidents, start + difftime(end, start)/2)
presidents$name[1] <- "Johnson"


yrng <- range(economics$uempmed) # range of unemployment rates
xrng <- with(presidents, c(min(start), max(end)))     # range of dates


unemp <- ggplot(economics, aes(x = date, y = uempmed)) +
    theme_bw() +
    geom_rect(aes(x = NULL, y = NULL, 
                  xmin = start, xmax = end, fill = party), 
              ymin = -Inf, ymax = Inf, 
              data = presidents)  +
    geom_line(size = 1) + 
    geom_vline(data = presidents, aes(xintercept = as.numeric(start)),
               color = "grey50") +
    labs(x = "", y = "No. unemployed (1000s)", fill = "Party") + 
    scale_fill_manual(breaks = unique(presidents$party),
                      values =  alpha(c("blue", "red"), 0.2)) +
    geom_text(aes(x = mid, y = yrng[1], label = name), 
              data = presidents, size = 4, vjust = 0) +
    theme(panel.grid.major = element_blank()) +
    scale_x_date(limits = xrng)

# Add a caption to the graph
caption <- paste(strwrap("Unemployment rates in the US have 
  varied a lot over the years", 40), collapse="\n")
unemp + geom_text(aes(x, y, label = caption), 
                  data = data.frame(x = xrng[2], y = yrng[2]), 
                  hjust = 1, vjust = 1, size = 4)
