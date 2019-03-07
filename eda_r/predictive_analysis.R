# Predictive Analysis

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(dplyr)
#install.packages('GGally')
library(GGally)
#install.packages('memisc')
library(memisc)
library(lattice)
library(MASS)
#install.packages('car')
library(car)
library(reshape2)
library(dplyr)

# load dataset 
data("diamonds")

# find all info available on the dataset 
help("diamonds")

# Create a scatterplot of price (y) vs carat weight (x).
# Limit the x-axis and y-axis to omit the top 1% of values.
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(color="darkgreen", alpha = 1/10 , shape = 21) +
  scale_x_continuous(limits=c(min(diamonds$carat), quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(limits=c(min(diamonds$price), quantile(diamonds$price, 0.99)))

# Non- Linear Relationship

ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(color="darkgreen", alpha = 1/10 , shape = 21) +
  stat_smooth(method="lm") +
  scale_x_continuous(limits=c(min(diamonds$carat), quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(limits=c(min(diamonds$price), quantile(diamonds$price, 0.99)))

# Linear Model results in a seemingly incorrect model. We can miss
# prediction in many places.

set.seed(20022012)
diamonds_samp <- diamonds[sample(1:length(diamonds$price),10000),]
ggpairs(diamond_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

# We notice the followings:
# diamond price is almost linearly correlated with x, y, z and carat; 
# These are the critical factors driving price
# diamond price seems related to cut/color/clarity but is not very 
# clear from this plot 
# diamond price seems not directly related to depth and table

# Create two histograms of the price variable
# and place them side by side on one output image.

plot1 <- ggplot(diamonds,aes(x=price))+
  geom_histogram(color='darkblue',fill = 'darkblue',  binwidth=100)+
  scale_x_continuous()+
  ggtitle('Price')

plot2 <- ggplot(diamonds,aes(x=price))+
  geom_histogram(color='darkred',fill='darkred',  binwidth=0.01)+
  scale_x_log10()+
  ggtitle('Price(log10)')

grid.arrange(plot1, plot2, ncol = 2)

# It’s obvious that the price histogram is skewed to the left
# side while the log10(price) seems to be a bell curve distributed.
# Also, the two peaks in the log10(price) plot coincides with 
# the 1st and 3rd quantile of price going side by side of the rich/poor
# theory explained. 

### Create a new function to transform the carat variable
cuberoot_trans = function() trans_new('cuberoot',
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

### Use the cuberoot_trans function
ggplot(aes(carat, price), data = diamonds) + 
  geom_point(color = 'darkblue',
             alpha = 1/10, 
             size = 0.75,
             position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

# Now the log10(price) is almost linear with cuberoot of carat
# we can move on to the modeling.

# Explain the Price change by colour, clarity and cut.

library(RColorBrewer)

ggplot(aes(x = carat, y = price, color=clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +                         
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

# It’s clear that Clarity factors into the diamond price - 
# a better clarity almost always has higher price than lower 
# end clarity.

ggplot(aes(x = carat, y = price, color=cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +                         
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')

# Don't think  it really matters alot. We do not see much variation
# on the cut.

ggplot(aes(x = carat, y = price, color=color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) +                         
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

# This looks similar with previous Clarity plot and 
# Color should be also considered as an factor for price.

# PREDICT
# Price is the outcome and carat is the predictor variable.

m1 <- lm(I(log10(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1,~ . +carat)
m3 <- update(m2,~ . +cut)
m4 <- update(m3,~ . +color)
m5 <- update(m4,~ . +clarity)
mtable(m1,m2,m3,m4,m5,sdigits = 3)

# the linear model for the diamond price is:
# Log(price)= 0.18+3.97carat1/3−0.474carat+pc5∗cutcoef+pc7∗colorcoef+pc8∗claritycoef
# where pc5, pc7 and pc8 are polynomial contrast with n=5, 7, 8, respectively.

thisDiamond <- data.frame(carat=1, cut='Very Good',
                          color='G',clarity='VS2')
modelEstimate <- predict(m5,newdata = thisDiamond,
                         interval = "prediction",level = .95)
10^modelEstimate

# The predicted price is $5,232.11 vs. actual price $5,600.