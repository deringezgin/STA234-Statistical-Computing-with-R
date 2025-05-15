### STA234: Statistical Computing with R
### Midterm Exam
### This is my R-Script that has my code for the
### questions in the midterm. I did not include
### my answers for the interpretation questions.
### I also did not included the interpretation
### part of the question prompts

########## Import Statements ##################################################
library(tidyverse)
library(ggalt)
library(UsingR)
library(nycflights13)


###############################################################################
########## Problem 1 - [50 points] ############################################
##### Using the flights data from nycflights13 package, answer the following ##
###############################################################################


##### 1.(a) [2 points] ##############################################
### Extract flights from carrier Alaska (AS) leaving NYC in 2013.
### Use this subset for parts (b) through (f).
alaskaFlights = flights %>%
    filter(carrier == "AS")


##### 1.(b) [5 points] ##############################################
### Show scatterplot to explain the relation between dep_delay (x-axis) and arr_delay (y-axis).
depArrDelaySplot = ggplot(data = alaskaFlights,
                          aes(x = dep_delay,
                              y = arr_delay)) +
    geom_point(size = 1) +
    labs(title = "Fig.1 Departure Delay vs. Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013",
         x = "Departure Delay (in minutes)",
         y = "Arrival Delay (in minutes)",
         caption = "Source: nycflights2013 Dataset") +
    theme_classic()

depArrDelaySplot

### How would you try to spread out the points in the cluster? Show the plot.
alaskaFlightsShifted = alaskaFlights %>%
    mutate(arr_delay = arr_delay - min(arr_delay, na.rm = TRUE) + 1)

depLogArrDelaySplot = depArrDelaySplot %+% alaskaFlightsShifted +
    scale_y_continuous(trans = "log10") +
    labs(title = "Fig.2 Departure Delay vs. Log of Arrival Delay",
         y = "Adjusted Log of Arrival Delay (in Minutes)")

depLogArrDelaySplot


##### 1.(c) [2 points] ##############################################
### Fit a regression and a non-linear line to the above scatterplots
# Original Plot with the Linear Regression Line
depArrDelaySplotLinReg = depArrDelaySplot +
    geom_smooth(method = "lm") +
    labs(title = "Fig.3 Departure Delay vs. of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013\nwith Linear Regression Line")

depArrDelaySplotLinReg

# Original Plot with the Non-Linear Regression Line
depArrDelaySplotNonLinReg = depArrDelaySplot +
    geom_smooth() +
    labs(title = "Fig.4 Departure Delay vs. of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013\nwith Non-Linear Regression Line")

depArrDelaySplotNonLinReg

# Log-Transformed Plot with the Linear Regression Line
depLogArrDelaySplotLinReg = depLogArrDelaySplot +
    geom_smooth(method = "lm") +
    labs(title = "Fig.5 Departure Delay vs. Log of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013\nwith Linear Regression Line")

depLogArrDelaySplotLinReg

# Log-Transformed Plot with the Non-Linear Regression Line
depLogArrDelaySplotNonLinReg = depLogArrDelaySplot +
    geom_smooth() +
    labs(title = "Fig.6 Relationship Between the Departure Delay and the Log of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013\nwith Non-Linear Regression Line")

depLogArrDelaySplotNonLinReg


##### 1.(d) [2 points] ##############################################
### Make a jittered plot to show the points scattered in the scatterplot from part(b).
depLogArrDelayJittered = depLogArrDelaySplot +
    geom_jitter() +
    labs(title = "Fig.7 Relationship Between the Departure Delay and Log of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013")

depLogArrDelayJittered


##### 1.(e) [2 points] ##############################################
### Make a counts plot for the scatterplot in part (b).
depLogArrDelayCounts = depLogArrDelaySplot +
    geom_count(col = "darkblue") +
    labs(title = "Fig.8 Relationship Between the Departure Delay and Log of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013",
         size = "Flight Count")

depLogArrDelayCounts


##### 1.(f) [2 points] ##############################################
### Change the transparency of dots in the scatterplot from part (b).
depLogArrDelaySplotTransparent = depLogArrDelaySplot +
    geom_point(alpha = 0.1,
               color = "lightblue") +
    labs(title = "Fig.9 Relationship Between the Departure Delay and the Log of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013\nwith Reduced Dot Transparency")

depLogArrDelaySplotTransparent


##### 1.(g) [2 points] ##############################################
### Change the theme and background color of plot original plot in part (b).
depLogArrDelaySplotCustom = depLogArrDelaySplot +
    theme_set(theme_bw()) +
    theme(panel.background = element_rect(fill = "gray")) +
    labs(title = "Fig.10 Relationship Between the Departure Delay and the Log of Arrival Delay",
         subtitle = "for Alaska Airlines Flights Departing New York in 2013\nwith Custom Theme")

depLogArrDelaySplotCustom


##### 1.(h) [8 points] ##############################################
### From original flights data, extract data for UA and AA airlines.
flightsUAandAA = flights %>%
    filter(carrier %in% c("UA", "AA"))

### Make a boxplot of average departure delays.
departureDelayBoxplot = ggplot(data = flightsUAandAA,
                               aes(x = carrier,
                                   y = dep_delay)) +
    geom_boxplot() +
    scale_y_continuous(trans = "log10") +
    labs(title = "Fig.11 Departure Delays for United Airlines (UA) and American Airlines (AA)",
         subtitle = "for Flights Departing New York in 2013",
         x = "Airline",
         y = "Log of Departure Delay (in minutes)",
         caption = "Source: nycflights2013 Dataset") +
    theme_classic()

departureDelayBoxplot

### Then, make boxplots comparing
### two airlines departure delay by three different origin airports.
departureDelayBoxplotAirport = departureDelayBoxplot +
    facet_wrap(~ origin) +
    labs(title = "Fig.12 Departure Delays for United Airlines (UA) and American Airlines (AA)",
         subtitle = "for Flights Departing Different Airports in New York in 2013\nFacetted by Origin Airport")

departureDelayBoxplotAirport

### Prepare summary of departure delay for two airlines using the function summary.
summaryUA = summary(flightsUAandAA$dep_delay[flightsUAandAA$carrier == "UA"])
summaryUA

summaryAA = summary(flightsUAandAA$dep_delay[flightsUAandAA$carrier == "AA"])
summaryAA


##### 1.(i) [5 points] ##############################################
### In the entire flights data, make a barplot comparing the number
### of flights from different carriers?
countData = flights %>%
    group_by(carrier, origin) %>%
    summarise(flight_count = n()) %>%
    mutate(flightPercentage = flight_count / sum(flight_count))

basePlot = ggplot(aes(x = carrier), data = flights) +
    labs(subtitle = "for Flights Departing New York in 2013",
         x = "Carrier",
         y = "Flight Count",
         caption = "Source: nycflights2013 Dataset") +
    theme_classic()

carrierBarplot = basePlot +
    geom_bar(data = countData,
             aes(y = flight_count),
             stat = "identity") +
    labs(title = "Fig.13 Number of Flights from Different Carriers")

carrierBarplot

### Make y-axis a percentage?
percentPlot = basePlot +
    geom_bar(data = countData,
             aes(y = flight_count / sum(flight_count)),
             stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Fig.14 Proportion of Flights by Carrier",
         y = "Flight Percentage")

percentPlot


##### 1.(j) [5 points] ##############################################
### Within the above plot (part i) add categorical variable origin to show stacked
### bar chart for the three airports (EWR, JFK, and LGA).
originPlot = basePlot +
    geom_bar(data = countData,
             aes(y = flight_count / sum(flight_count),
                 fill = origin),
             stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Fig.15 Proportion of Flights by Carrier",
         subtitle = "for Flights Departing New York in 2013\nColored by Origin Airport",
         y = "Flight Percentage",
         fill = "Origin Airport")

originPlot

### Then, show a plot with position dodge.
dodgePlot = basePlot +
    geom_bar(data = countData,
             aes(y = flight_count / sum(flight_count),
                 fill = origin),
             stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Fig.16 Proportion of Flights by Carrier",
         subtitle = "for Flights Departing New York in 2013\nColored by Origin Airport",
         y = "Flight Percentage",
         fill = "Origin Airport")

dodgePlot


##### 1.(k) [4 points] ##############################################
### Make bar plots in part (i) for each origin using facets.
facetCarrierPlot = percentPlot +
    facet_wrap(~origin) +
    coord_flip() +
    labs(title = "Fig.17 Proportion of Flights by Carrier",
         subtitle = "for Flights Departing New York in 2013\nFacetted by Origin Airport")

facetCarrierPlot

### Then, put them all in one column as facets.
facetCarrierPlot = percentPlot +
    facet_wrap(~origin,
               nrow = 3, ncol = 1) +
    labs(title = "Fig.18 Proportion of Flights by Carrier",
         subtitle = "for Flights Departing New York in 2013\nFacetted by Origin Airport")

facetCarrierPlot


##### 1.(l) [6 points] ##############################################
### Which carrier should you take if you want to have the shortest on average
### distance from the NYC flights information at 2013?
minAvgDistance = flights %>%
    group_by(carrier) %>%
    summarise(avg_distance = mean(distance, na.rm = TRUE)) %>%
    arrange(avg_distance) %>%
    slice(1)

minAvgDistance

### Which carrier should you take if you want to have the shortest on average
### arrival delay from the NYC flights information at 2013?
minAvgArrDelay = flights %>%
    group_by(carrier) %>%
    summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
    arrange(avg_arr_delay) %>%
    slice(1)

minAvgArrDelay


##### 1.(m) [5 points] ##############################################
### Suppose you want to go to St. Louis (STL) in July, and you don’t care about the date,
### which airport should you fly from in terms of shortest airtime? And which carrier would you choose?
flightsJulySTL = flights %>%
    filter(month == 7, dest == "STL")

flightsJulySTLAirportCarrier = flightsJulySTL %>%
    group_by(origin, carrier) %>%
    summarise(median_airtime = median(air_time, na.rm = TRUE)) %>%
    arrange(median_airtime)

flightsJulySTLAirportCarrier



###############################################################################
########## Problem 2 - [15 points] ############################################
##### Using the weather data from the package nycflights13 package ############
##### do the following: #######################################################
###############################################################################

##### 2.(a) [4 points] ##############################################
### Make a histogram for variable temp.
### Try options for different arguments within geom_hist() to make the graph better.
temperatureHist = ggplot(data = weather,
                         aes(x = temp)) +
    geom_histogram(binwidth = 2,
                   color = "black",
                   fill = "orange") +
    labs(title = "Fig.19 Distribution of Temperature",
         subtitle = "from Hourly Meterological Data in New York Airports",
         x = "Temperature (in Fahrenheit)",
         y = "Frequency",
         caption = "Source: nycflights13 package") +
    theme_classic()

temperatureHist


##### 2.(b) [4 points] ##############################################
### Suppose we were interested in looking at how the histogram of hourly temperature
### recordings at the three NYC airports we saw in (a) differed in each month.
### To do that make a facet plot which splits the histogram in (a) by the 12 possible months in a given year.
### In other words, make histograms of temp for each month separately using facets for month.
### Make the facets with four rows and three columns. Add both range fixed and range free plots in the facets.
temperatureHistFixedFacet = temperatureHist +
    facet_wrap(~ month,
               nrow = 4, ncol = 3) +
    labs(title = "Fig.20 Distribution of Temperature",
         subtitle = "Facetted by Month")

temperatureHistFixedFacet

tempratureHistFreeFacet = temperatureHist +
    facet_wrap(~ month,
               nrow = 4, ncol = 3,
               scales = "free_y") +
    labs(title = "Fig.21 Distribution of Temperature",
         subtitle = "Facetted by Month and Free Y Scales")

tempratureHistFreeFacet


##### 2.(c) [7 points] ##############################################
#### Convert the numerical variable month into a factor categorical variable to make
### a comparative boxplot for the temperatures in 12 months.
weather$month = factor(weather$month,
                       levels = c(1:12),
                       labels = c("Jan", "Feb", "Mar",
                                  "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep",
                                  "Oct", "Nov", "Dec"))

temperatureBoxplot = ggplot(data = weather,
                            aes(x = month,
                                y = temp)) +
    geom_boxplot() +
    labs(title = "Fig.22 Monthly Temperature Distribution",
         subtitle = "from Hourly Meterological Data in New York Airports",
         x = "Month",
         y = "Temperature in Fahrenheit",
         caption = "Source: nycflights13 package") +
    theme_classic()

temperatureBoxplot



###############################################################################
########## Problem 3 - [15 points] ############################################
##### For the midwest data in ggplot2 package, answer the following ###########
###############################################################################

##### 3.(a) [5 points] ##############################################
### Make a scatter plot between area (x-axis) and population total (y-axis)
### with dots colored by states and dot sizes varied by popdensity.
### Include the fitted loess (R’s default) line without the 95% confidence band.
areaPopulationPlot = ggplot(data = midwest,
                            aes(x = area,
                                y = poptotal)) +
    geom_point(aes(color = state,
                   size = popdensity)) +
    scale_y_continuous(labels = scales::comma) +
    geom_smooth(se = FALSE) +
    labs(title = "Fig.23 Area vs. Total Population for Midwest Counties",
         subtitle = "Colored by State and Sized by Population Density",
         x = "Area",
         y = "Total Population",
         color = "State",
         size = "Population Density",
         caption = "Source: 2000 US Census") +
    theme_classic()

areaPopulationPlot


##### 3.(b) [10 points] #############################################
### Make a subset with total population between (350,000, 500,000] and area between
### (0.01 and 0.1). Using this subset and function geom_encircle(), encircle the points
### in scatter plot from (a) who are in this subset. In other words, using the new dataframe
### with only the points (rows) of interest, you are highlighting (encircling)
### the points in scatterplot made from the midwest data in part (a).

midwestSubset = midwest %>%
    filter(350000 < poptotal,
           poptotal <= 500000,
           0.01 < area,
           area < 0.1)

areaPopulationPlotEncircle = areaPopulationPlot +
    geom_encircle(data = midwestSubset,
                  aes(x = area,
                      y = poptotal),
                  size = 3) +
    labs(title = "Fig.24 Area vs. Total Population for Midwest Counties",
         subtitle = "Colored by State and Sized by Population Density\nTotal population in (350,000, 500,000] and total area in (0.01, 0.1) is encircled.")

areaPopulationPlotEncircle



###############################################################################
########## Problem 4 - [20 points] ############################################
##### Using the dataset movie_data_2011 from UsingR package ###################
##### (see help file for more details) answer the following ###################
###############################################################################

##### 4.(a) [5 points] ##############################################
### Report the genre types with the number of movies in each genre.
genreSummary = movie_data_2011 %>%
    group_by(Genre) %>%
    summarise(Number_of_Movies = n())

genreSummary


##### 4.(b) [5 points] ##############################################
### In part(a) there is one Genre with no name.
### Delete the movies which belong to this no name genre.
filteredMovies = movie_data_2011 %>%
    filter(Genre != "")


##### 4.(c) [5 points] ##############################################
### Prepare a summary of the gross per current weekend for each
### genre including mean, median and sd of gross for each genre.
grossSummary = filteredMovies %>%
    group_by(Genre) %>%
    summarise(Mean_Gross = mean(Gross, na.rm = TRUE),
              Median_Gross = median(Gross, na.rm = TRUE),
              SD_Gross = sd(Gross, na.rm = TRUE))

grossSummary


##### 4.(d) [5 points] ##############################################
### Make comparative boxplots for average total gross to the date
### for the movie’s genres.
grossBoxplot = ggplot(data = movie_data_2011,
                      aes(x = Genre,
                          y = log(Gross))) +
    geom_boxplot(width = 0.4) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Fig.25 Comparison of the Average Total Gross for Different Movie Genres",
         x = "Movie Genre",
         y = "Log of Average Total Gross",
         caption = "Source: movie_data_2011 dataset") +
    coord_flip() +
    theme_classic()

grossBoxplot

### Name the top three genre in terms of high total gross.
top3AverageGross = movie_data_2011 %>%
    group_by(Genre) %>%
    summarise(MaxGross = max(TotalGross, na.rm = TRUE)) %>%
    arrange(desc(MaxGross)) %>%
    slice(1:3)

top3AverageGross



###############################################################################
########## Bonus Problem - [5 points] #########################################
###############################################################################

##### Bonus.(i) [2.5 points] #############################################
### A dataset named df has been read in R and missing values in this data
### have been read as NA. Which of the following codes will not give the
### number of missing values in each column?

## i.(A) colSums(is.na(df))
## i.(B) apply(is.na(df),2,sum)
## i.(C) sapply(df,function(x) sum(is.na(x))
## i.(D) table(is.na(df))

df = data.frame(
    name = c("Derin", "Johnny", NA, "Muhammed"),
    age = c(20, 19, 30, NA),
    score = c(NA, NA, 100, 100)
)

colSums(is.na(df))

apply(is.na(df), 2, sum)

sapply(df, function(x) sum(is.na(x)))

table(is.na(df))


##### Bonus.(ii) [2.5 points] ############################################
### Which of the following command will help us remove the duplicate rows in the dataframe names df?

## ii.(A) df[!duplicated(df),]
## ii.(B) unique(df)
## ii.(C) dplyr::distinct(df)
## ii.(D) All of the above
## ii.(E) None of the above

df = data.frame(
    name = c("Derin", "Johnny", "Derin", "Muhammed"),
    age = c(20, 19, 20, 18),
    score = c(97, 98, 97, 100)
)

df[!duplicated(df),]

unique(df)

dplyr::distinct(df)
