# February 27th, 2025
# In-Class Exercise
# Derin Gezgin

# install.packages("ggplot2")
library(ggplot2)

# Make a scatterplot between carat (x-axis) and price (y-axis).
# Change theme of this plot.

plot.theme = theme(panel.background = element_rect(fill = 'white'),
                   panel.border = element_blank(),
                   axis.line.x = element_line(colour = "black",
                                              size = 1.5,
                                              lineend = "butt"),
                   axis.line.y = element_line(colour = "black",
                                              size=1.5))

carat.price.plot = ggplot(data = diamonds, aes(x = carat, y = price)) +
    geom_point() +
    xlab("Carat") +
    ylab("Price") +
    labs(title = "Fig.1 Carat vs. Price",
         caption = "Source: ggplot2 Diamonds Dataset") +
    plot.theme
carat.price.plot

# Add a regression line for method="lm" using geom_smooth()
regressed.carat.price = carat.price.plot +
    geom_smooth(method = "lm") +
    labs(title = "Fig.2 Carat vs. Price with Linear Regression Line",
         caption = "Source: ggplot2 Diamonds Dataset")
regressed.carat.price

# Add a non-linear fit using geom_smooth() with default method (we will talk about it.)
nonlinear.carat.price = carat.price.plot +
    geom_smooth() +
    labs(title = "Fig.3 Carat vs. Price with Non-Linear Fit",
         caption = "Source: ggplot2 Diamonds Dataset")
nonlinear.carat.price

# In the scatterplot (without any fitted lines), color the dots by type of cut
cut.carat.price = ggplot(data = diamonds, aes(x = carat, y = price, color=cut)) +
    geom_point() +
    xlab("Carat") +
    ylab("Price") +
    labs(title = "Fig.4 Carat vs. Price",
         subtitle = "Colored by Quality of the Cut",
         caption = "Source: ggplot2 Diamonds Dataset") +
    plot.theme
cut.carat.price

# Linear line
linear.cut.carat.price = cut.carat.price +
    geom_smooth(method = "lm") +
    labs(title = "Fig.5 Carat vs. Price with Linear Regression Line",
         subtitle = "Colored by Quality of the Cut",
         caption = "Source: ggplot2 Diamonds Dataset")
linear.cut.carat.price

# Non-Linear Line
nonlinear.cut.carat.price = cut.carat.price +
    geom_smooth() +
    labs(title = "Fig.6 Carat vs. Price with Non-Linear Fit",
         subtitle = "Colored by Quality of the Cut",
         caption = "Source: ggplot2 Diamonds Dataset")
nonlinear.cut.carat.price

# Make plot in d using facets for type of cut instead of color.

carat.price.plot.facet = carat.price.plot +
    facet_wrap(~cut) +
    labs(title = "Fig.7 Carat vs. Price",
         subtitle = "Faceted by Quality of the Cut",
         caption = "Source: ggplot2 Diamonds Dataset")
carat.price.plot.facet

#  Add smoothed fit to the facets (lm and default) in f.

linear.cut.carat.price = carat.price.plot.facet +
    geom_smooth(method = "lm") +
    labs(title = "Fig.8 Carat vs. Price with Linear Regression Line",
         subtitle = "Faceted by Quality of the Cut",
         caption = "Source: ggplot2 Diamonds Dataset")
linear.cut.carat.price

nonlinear.cut.carat.price = carat.price.plot.facet +
    geom_smooth() +
    labs(title = "Fig.9 Carat vs. Price with Non-Linear Fit",
         subtitle = "Faceted by Quality of the Cut",
         caption = "Source: ggplot2 Diamonds Dataset")
nonlinear.cut.carat.price


