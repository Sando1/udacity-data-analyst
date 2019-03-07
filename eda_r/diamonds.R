# install and load packages 
# install.packages('ggplot2')
library(ggplot2)
# install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
# install.packages('gridExtra')
library(gridExtra)
library(scales)

# load dataset 
data("diamonds")

# find all info available on the dataset 
help("diamonds")

# no of rows in data set
nrow(diamonds)

# number of ordered factors 
summary(diamonds)
 
# highest level 
help("diamonds")

# make a histogram of the price of
# all the diamonds in the diamond data set
ggplot(diamonds, aes(x = price)) + 
  geom_histogram(color = "black", fill = "LightBlue", binwidth = 1000) + 
  scale_x_continuous(breaks = seq(0, 20000, 1000)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Price in $") + ylab("Count") 

summary(diamonds$price)
# COMMENTS ON THE PLOT
# The histogram is negtively skewed with the long shot of at the 
# 1000-2000 line. The mean is $3933 while the range is (18823-326)
# of 18497. The Median is of course less than the mean being 2401 
# proving that this is a right tailed plot. 

# How many diamonds cost less than $500 = 1729
summary(diamonds$price < 500)
# How many diamonds cost less than $250 = 0
summary(diamonds$price < 250)
# How many diamonds cost $15000 or more = 1656
summary(diamonds$price >= 15000)

# exploring the largest bin in the histogram plotted before
# i.e the 1000 bin 
ggplot(diamonds, aes(x = price)) + 
  geom_histogram(color = "black", fill = "LightGreen", binwidth = 50) + 
  scale_x_continuous(breaks = seq(500, 1500, 50)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_cartesian(c(500,1500)) +
  xlab("Price in $") + ylab("Count") 
 
# There are no diamonds that cost $1500.
# For diamonds that cost less than $2,000, 
# the most common price of a diamond is around $700 

# Break out the histogram of diamond prices by cut.
ggplot(diamonds, aes(x = price)) + 
  geom_histogram(color = "black", fill = "DarkOrange", binwidth = 50) + 
  scale_x_continuous(breaks = seq(0, 4000, 100)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_cartesian(c(0,4000)) +
  facet_grid(cut~.) + 
  xlab("Price") + ylab("Count")

# Which cut has the highest priced diamond ? Premium 
by(diamonds$price, diamonds$cut, max)
# Which cut has the lowest price diamond? Very Good, Premium and Ideal
by(diamonds$price, diamonds$cut, min)
# Which cut has the lowest median price? Ideal 
by(diamonds$price, diamonds$cut, median)

# In the last exercise, we looked at the summary statistics
# for diamond price by cut. If we look at the output table, the
# the median and quartiles are reasonably close to each other.

# diamonds$cut: Fair
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     337    2050    3282    4359    5206   18570 
# ------------------------------------------------------------------------ 
# diamonds$cut: Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     327    1145    3050    3929    5028   18790 
# ------------------------------------------------------------------------ 
# diamonds$cut: Very Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     336     912    2648    3982    5373   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Premium
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326    1046    3185    4584    6296   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Ideal
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326     878    1810    3458    4678   18810 

# This means the distributions should be somewhat similar,
# but the histograms we created don't show that.

# The 'Fair' and 'Good' diamonds appear to have 
# different distributions compared to the better
# cut diamonds. They seem somewhat uniform
# on the left with long tails on the right.

# Let's look in to this more.

# Look up the documentation for facet_wrap in R Studio.
# Then, scroll back up and add a parameter to facet_wrap so that
# the y-axis in the histograms is not fixed. You want the y-axis to
# be different for each histogram.

qplot(x = price, data = diamonds, binwidth = 100) + 
  facet_wrap(~cut, scales = "free")

# Create a histogram of price per carat
# and facet it by cut.

ggplot(diamonds, aes(x = price/carat)) + 
  geom_histogram(color = "black", fill = "DarkRed", binwidth = .05) + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_log10(expression(paste(Log[10], " of Price")),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  facet_grid(cut~., scale = "free") + ylab("Count")

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_boxplot(varwidth = TRUE, outlier.colour = "red", outlier.shape = 1) + 
  theme(axis.text.x = element_text(angle = 0)) +
  facet_grid(color~., margins = TRUE)

# a) What is the price range for the 
#middle 50% of the diamonds with color D?
# b) What is the price range for the 
#middle 50% of the diamonds with color J?
# c) What is the IQR for diamonds with the best color?
# d) What is the IQR for diamonds with the worst color?

by(diamonds$price, diamonds$color, summary) 
IQR(subset(diamonds, color == 'D')$price)
IQR(subset(diamonds, color == 'J')$price)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

ggplot(diamonds, aes(x = color, y = price/carat)) + 
  geom_boxplot(varwidth = TRUE, outlier.colour = "red", outlier.shape = 1) + 
  theme(axis.text.x = element_text(angle = 0)) +
  xlab("Color") + ylab("Price per Carat")

# Investigate the weight of the diamonds (carat) using a 
#frequency polygon. Use different bin widths to see how the 
#frequency polygon changes. What carat size has a count greater 
#than 2000? Check all that apply.
sizes = c(0.1, 0.3, 0.8, 1.01, 1.6, 2.0, 3.0, 5.0)
ggplot(diamonds, aes(x=carat)) + 
  geom_freqpoly(binwidth=0.1, alpha = 0.75) + 
  scale_x_continuous(breaks=sizes, expand = c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  geom_vline(xintercept = c(0.1, 0.3, 0.8, 1.01, 1.6, 2.0, 3.0, 5.0), color = "darkblue", alpha = 0.5) +
  geom_hline(yintercept = 2000, color = "brown", alpha = 0.5) + 
  xlab("Carat Size") + ylab("Count")