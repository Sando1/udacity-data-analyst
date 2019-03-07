#Two Variable Analysis

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(dplyr)

# load dataset 
data("diamonds")

# find all info available on the dataset 
help("diamonds")

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.
ggplot(diamonds, aes(x = x, y = price)) +
  geom_point()

# the plot is exponential in nature with some obvious outliers

# What is the correlation between price and x? 0.8844352 
cor.test(diamonds$price, diamonds$x)
# What is the correlation between price and y? 0.8654209  
cor.test(diamonds$price, diamonds$y)
# What is the correlation between price and z? 0.8612494  
cor.test(diamonds$price, diamonds$z)  

# Create a simple scatter plot of price vs depth.
ggplot(diamonds, aes(x = depth, y = price)) +
  geom_point()

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units.
ggplot(diamonds, aes(x = depth, y = price)) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0,80,2))

# Most diamonds are between the depth 59-64

# What is the correlation between price and depth? -0.0106474  
cor.test(diamonds$price, diamonds$depth)  

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

ggplot(diamonds, aes(x = carat, y = price)) +
  xlim(0,quantile(diamonds$carat,0.99)) +
  ylim(0,quantile(diamonds$price,0.99)) +
  geom_point(alpha = 1/100) 

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(diamonds, aes(x = diamonds.volume, y = price)) +
  geom_point() 
  
# Some volumes are 0! There's an expensive diamond with 
# a volume near 4000, and a cheaper diamond with a volume 
# near 900.
# What is the correlation between price and depth? 0.9235455  
with(subset(diamonds, (volume > 0) & (volume <= 800)),cor.test(volume,price))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

ggplot(data = subset(diamonds, (volume > 0) & (volume <= 800)),
       aes(x = volume, y = price)) +
  geom_point() 

p1 <- ggplot(data = subset(diamonds, (volume > 0) & (volume <= 800)),
             aes(x = volume, y = price)) +
  geom_point() 

# Default smoother
p2 <- p1 + geom_smooth()

# looking at a linear fit,
p3 <- p1 + stat_smooth(method = "lm", formula = y ~ x, size = 1) + coord_cartesian(ylim = c(0,20000))

# Looking at polynimoal functions of order 2
p4 <- p1 + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + coord_cartesian(ylim = c(0,20000))

# Looking at polynimoal functions of order 3
p5 <- p1 + stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1) + coord_cartesian(ylim = c(0,20000))

grid.arrange(p2,p3,p4,p5,ncol =2)

# linear model seems to fair better than a quadratic one.

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price), 
            median_price = median(price), 
            min_price = min(price), 
            max_price = max(price), 
            n= n())
diamondsByClarity

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

p1  <- ggplot(diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) +
  geom_bar(stat = "identity")

p2 <- ggplot(diamonds_mp_by_color, aes(x = color, y = mean_price)) +
  geom_bar(stat = "identity")

grid.arrange(p1,p2, ncol =1)
These trends seem to go against our intuition.

# Mean price tends to decrease as clarity improves. 
# The same can be said for color.

diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))

ggplot(diamonds_mp_by_cut, aes(x = cut, y = mean_price)) +
  geom_bar(stat = "identity")