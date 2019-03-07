# Multivariate Analysis

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(dplyr)

# load dataset 
data("diamonds")

# find all info available on the dataset 
help("diamonds")

# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

ggplot(data = subset(diamonds, (price < 10000)), aes(x=price)) + 
  geom_histogram(aes(fill = cut), binwidth = 100) + 
  facet_wrap(~color) +
  scale_fill_brewer(type = 'qual')

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

ggplot(data = subset(diamonds, (price < 10000)), aes(x=table, y=price, color=cut)) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(43,95,2), labels = seq(43,95,2)) +
  scale_fill_brewer(type = 'qual')

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

diamonds$volume <- diamonds$x * diamonds$y + diamonds$z

ggplot(data = subset(diamonds, volume < quantile(volume,probs = 0.99)), 
       aes(x=volume,y=price, color = clarity)) +
  geom_point() +
  scale_y_log10() +
  scale_color_brewer(type = 'div')

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

ggplot(diamonds, aes(x = cut, y = price/carat, color= color)) +
  geom_jitter() + 
  facet_wrap(~ clarity) +
  scale_color_brewer(type = 'div')