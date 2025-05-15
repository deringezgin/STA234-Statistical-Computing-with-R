#############################################################################################################
########## Derin Gezgin #####################################################################################
#############################################################################################################
########## STA234 Statistical Computing with R ##############################################################
########## Final Paper Code #################################################################################
#############################################################################################################

#############################################################################################################
########## Setup ############################################################################################
#############################################################################################################

# Set the current working directory to the path of the script.
# It is important that you leave the folder structure as it is where
# the sub-folder data has the sub-folders for datasets
current_dir = rstudioapi::getSourceEditorContext()$path
setwd(dirname(current_dir))

# For non-scientific number representation
options(scipen = 999)

# Setting seed for reproducibility
set.seed(42)

# Create the directories to save the plots and the model outputs
if (!dir.exists("plots")) dir.create("plots")
if (!dir.exists("modeloutputs")) dir.create("modeloutputs")

#############################################################################################################
########## Import Statements ################################################################################
#############################################################################################################

library(tidyverse)
library(ggmap)
library(gt)
library(pROC)
library(gtExtras)
library(jtools)
library(gridExtra)
library(factoextra)
library(ggthemes)

# Constant DPI variable for the plots
DPI = 500

# Global theme for all the plots
APA_THEME = theme_apa(legend.pos = "bottom",
                      facet.title.size = 16,
                      x.font.size = 14,
                      y.font.size = 14)

# Coordinate bounds of San Francisco
LEFT = -122.52
RIGHT = -122.35
BOTTOM = 37.70
TOP = 37.85

# This is my private API key. If you ever want to share this code, please remove it :)
API_KEY = "30cc96f4-7081-4f0e-9c6e-61f5c5b0bf82"
register_stadiamaps(API_KEY)

#############################################################################################################
########## DATA WRANGLING ###################################################################################
#############################################################################################################

# Reading the datasets
traffic.data = read.csv("data/ca_san_francisco_2020_04_01.csv")
race.data = read.csv("data/population_race_data.csv")

traffic.data = traffic.data %>%
    mutate(date = as.Date(date),                           # Converting the date and time into
           time = strptime(time, format = "%H:%M:%S"),     # date/time objects
           Year = as.numeric(format(date, "%Y"))) %>%
    filter(date < as.Date("2016-01-01")) %>%               # Filtering the first half of 2016
    select(-location,                                      # Selecting the columns we will use
           -district,
           -type,
           -raw_search_vehicle_description,
           -raw_result_of_contact_description,
           -raw_row_number,
           -reason_for_stop,
           -search_basis,
           -search_vehicle) %>%
    mutate(subject_sex = factor(x = subject_sex,           # Converting categorical variables
                                levels = c("male",         # into factors
                                           "female"),
                                labels = c("Male",
                                           "Female")),
           subject_race = factor(x = subject_race,
                                 levels = c("asian/pacific islander",
                                            "black",
                                            "hispanic",
                                            "white",
                                            "other"),
                                 labels = c("Asian/Pacific Islander",
                                            "Black",
                                            "Hispanic",
                                            "White",
                                            "Other")),
           outcome = replace_na(outcome, "No Action"),     # Replacing NA values in the outcome with No Action
           outcome = factor(x = outcome,
                            levels = c("citation",
                                       "warning",
                                       "arrest",
                                       "No Action"),
                            labels = c("Citation",
                                       "Warning",
                                       "Arrest",
                                       "No Action")),
           search_conducted = factor(x = search_conducted,
                                     levels = c(FALSE, TRUE),
                                     labels = c(0, 1)),
           arrest_made = factor(x = arrest_made,
                                levels = c(FALSE, TRUE),
                                labels = c(0, 1))) %>%
    left_join(y = race.data,                               # Joining the traffic data
              by = c("subject_race", "Year")) %>%          # and the race data
    mutate(generalWeights = (1 / Population_Percentage) / sum(1 / Population_Percentage, na.rm = TRUE)) %>%
    # Remove the NA values in the columns we will use in our modeling
    drop_na(subject_race, subject_sex, subject_age, lat, lng) %>%
    filter(between(x = lat, BOTTOM, TOP),                  # Filtering the out of range coordinates
           between(x = lng, LEFT, RIGHT))                  # for the map plotting

#############################################################################################################
########## Exploratory Data Analysis ########################################################################
#############################################################################################################

########## Graphing the race proportions ####################################################################

# Group data by race and year
race.by.year = aggregate(race.data$Population_Percentage,
                         by = list(race.data$subject_race,
                                   race.data$Year),
                         FUN = sum)

colnames(race.by.year) = c("subject_race",
                           "Year",
                           "Population_Percentage")

race.line.plot = ggplot(data = race.by.year,
                        aes(x = Year,
                            y = Population_Percentage,
                            color = subject_race)) +
    geom_line(linewidth = 1) +
    labs(x = "Year",
         y = "Percentage of Population",
         color = "Race",
         caption = "Source: US Census Data") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_tableau("Tableau 10") +
    APA_THEME

race.line.plot

ggsave(filename = "plots/raceDistribution.png",
       plot = race.line.plot,
       width = 7, height = 4, dpi = DPI)

########## Creating a table of proportion of races in population and stops ##################################

# Calculate the stop proportions per year
stops_by_race_year = traffic.data %>%
    mutate(Year = as.numeric(Year)) %>%
    group_by(subject_race, Year) %>%
    summarise(Stop_Share = n(), .groups = "drop") %>%
    group_by(Year) %>%
    mutate(Stop_Share = Stop_Share / sum(Stop_Share)) %>%
    ungroup()

# Merge with the population data
merged_data = left_join(stops_by_race_year,
                        race.data,
                        by = c("subject_race", "Year")) %>%
    mutate(Stop_Share = round(Stop_Share * 100, 2),
           Population_Percentage = round(Population_Percentage * 100, 2))

# Stack the data of stop and population shares
merged_data = merged_data %>%
    mutate(Combined_Data = paste0(Stop_Share, "%", "\n(", Population_Percentage, "%)"))

# Reshape the data
wide_table = merged_data %>%
    select(subject_race, Year, Combined_Data) %>%
    pivot_wider(names_from = Year, values_from = Combined_Data) %>%
    arrange(subject_race)

# Output the table
race_table = wide_table %>%
    gt(rowname_col = "subject_race") %>%
    tab_header(title = "Stop % and Population % by Race and Year",
               subtitle = "Cells showing Stop Share and (Population Share)") %>%
    fmt_markdown(everything()) %>%
    cols_align(align = "center") %>%
    gt_theme_nytimes()

race_table

########## Creating a histogram of the age of the stopped drivers ###########################################

### Distribution of age of the stopped drivers
stops.age.hist = ggplot(traffic.data,
                        aes(x = subject_age,
                            fill = subject_race,
                            weight = generalWeights)) +
    geom_histogram(binwidth = 2, color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_tableau("Tableau 10") +
    xlab("Age of the Driver") +
    ylab("Proportion of Total Stops (%)") +
    labs(caption = "Sources: Stanford Open Policing Project\nUS Census Data",
         fill = "Driver Race") +
    APA_THEME

stops.age.hist

ggsave(filename = "plots/ageDistribution.png",
       plot = stops.age.hist,
       width = 7, height = 4, dpi = DPI)

########## Creating a histogram of the time #################################################################

stops.time.hist = ggplot(traffic.data, aes(x = as.numeric(format(time, "%H")),
                                           fill = subject_race,
                                           weight = generalWeights)) +
    geom_histogram(binwidth = 1, color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_tableau("Tableau 10") +
    xlab("Hour of the Day") +
    ylab("Proportion of Total Stops (%)") +
    labs(caption = "Sources: Stanford Open Policing Project\nUS Census Data",
         fill = "Driver Race") +
    APA_THEME

stops.time.hist

ggsave(filename = "plots/hourDistribution.png",
       plot = stops.time.hist,
       width = 7, height = 4, dpi = DPI)

########## Boxplot of Outcome ###############################################################################

stop.race.total = aggregate(traffic.data$generalWeights,
                            by = list(traffic.data$subject_race,
                                      traffic.data$outcome),
                            FUN = sum)

colnames(stop.race.total)= c("Race", "Outcome", "Count")

stops.outcome.bplot = ggplot(data = stop.race.total,
                             aes(x = Outcome,
                                 y = Count,
                                 fill = Race)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_tableau("Tableau 10") +
    xlab("Outcome") +
    ylab("Proportion of Total Stops (%)") +
    labs(caption = "Sources: Stanford Open Policing Project\nUS Census Data",
         fill = "Driver Race") +
    facet_wrap(~ Outcome, scales = "free_y") +
    APA_THEME

stops.outcome.bplot

ggsave(filename = "plots/outcomeDistribution.png",
       plot = stops.outcome.bplot,
       width = 7, height = 4, dpi = DPI)

########## Map of the stops ################################################################################

sf_map = get_stadiamap(bbox = c(left = LEFT,
                                bottom = BOTTOM,
                                right = RIGHT,
                                top = TOP),
                       zoom = 12,
                       maptype = "alidade_smooth")

stop.map.race = ggmap(sf_map) +
    geom_density2d_filled(data = traffic.data,
                          aes(x = lng, y = lat),
                          alpha = 0.6,
                          na.rm = TRUE) +
    scale_fill_viridis_d(option = "rocket") +
    labs(fill = "Stop Count",
         caption = "Source: Stanford Open Policing Project") +
    xlab("Longitude") +
    ylab("Latitude") +
    facet_wrap(~ subject_race, nrow = 2) +
    APA_THEME +
    theme(panel.spacing.x = unit(2, "lines"))

stop.map.race

ggsave(filename = "plots/spatialDistribution.png",
       plot = stop.map.race,
       width = 14, height = 10, dpi = DPI)

#############################################################################################################
########## Statistical Analysis #############################################################################
#############################################################################################################

########## Hit Rate of the stops ############################################################################

# Filtering the data by searches
searches = traffic.data %>%
    filter(search_conducted == 1) %>%
    mutate(subject_race = factor(subject_race),
           found_contraband = as.integer(contraband_found))

# Calculating the success rates
hit_rates = searches %>%
    group_by(subject_race) %>%
    summarise(n_searches = n(),
              n_hits = sum(found_contraband),
              hit_rate = n_hits / n_searches)

hitRatePlot = ggplot(data = hit_rates,
                     aes(x = subject_race,
                         y = hit_rate)) +
    geom_col(fill = "lightblue",
             color = "black") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Driver Race",
         y = "Hit Rate (contraband found)",
         caption = "Source: Stanford Open Policing Project") +
    APA_THEME

hitRatePlot

ggsave(filename = "plots/hitRateDistribution.png",
       plot = hitRatePlot,
       width = 7, height = 4, dpi = DPI)

######### Logistic Regression Models ########################################################################

# Helper function for logistic regression
plotROC = function(model,
                   data,
                   responseVar) {
    predicted_probs = predict(model, type = "response")
    roc_obj = roc(data[[responseVar]], predicted_probs, quiet = TRUE)
    auc_value = auc(roc_obj)

    AUCplot = ggroc(data = roc_obj,
                    size = 1.5) +
        labs(x = "Specificity",
             y = "Sensetivity") +
        annotate("text",
                 x = 0.2, y = 0.25,
                 label = paste("AUC =", round(auc_value, 3)),
                 size = 8, fontface = "bold") +
        theme_apa()
    return(AUCplot)
}

########## Search Model #####################################################################################

search_model = glm(search_conducted ~
                       subject_race + subject_sex + subject_age,
                   data = traffic.data,
                   family = binomial)

search_0 = glm(search_conducted ~ 1,
               data = traffic.data,
               family = binomial)

sink("modeloutputs/search_regression_outputs.txt")
summary(search_model)
exp(coef(search_model))
anova(search_model, search_0, test = "Chisq")
sink()

searchAUCplot = plotROC(search_model, traffic.data, "search_conducted")

searchAUCplot

ggsave(filename = "plots/searchROC.png",
       plot = searchAUCplot,
       width = 7, height = 4, dpi = DPI)

########## Arrest Model #####################################################################################

arrest_model = glm(arrest_made ~
                       subject_race + subject_sex + subject_age,
                   data = traffic.data,
                   family = binomial)

arrest_0 = glm(arrest_made ~ 1,
               data = traffic.data,
               family = binomial)

sink("modeloutputs/arrest_regression_outputs.txt")
summary(arrest_model)
exp(coef(arrest_model))
anova(arrest_model, arrest_0, test = "Chisq")
sink()

arrestAUCplot = plotROC(arrest_model, traffic.data, "arrest_made")

arrestAUCplot

ggsave(filename = "plots/arrestROC.png",
       plot = arrestAUCplot,
       width = 7, height = 4, dpi = DPI)

########## Clustering Data by Location ######################################################################

# Filtering the data to lattitude and longitude
lat.lng.data = traffic.data %>%
    select(lat, lng)

# Sampling 25000 rows as its not possible to perform the elbow test
# with the whole dataset due to memory limitations
sampled.data = lat.lng.data %>%
    slice_sample(n = 25000)

# Perform the elbow test
elbow_test = fviz_nbclust(sampled.data,
                          kmeans,
                          method = "wss",
                          k.max = 15)

# Plot the elbow test
elbow_plot = ggplot(elbow_test$data,
                    aes(x = clusters,
                        y = y)) +
    geom_line(color = "black",
              linewidth = 1,
              group = 1) +
    geom_vline(xintercept = 6, linetype = "dashed") +
    labs(x = "Number of Clusters (k)",
         y = "Within-Cluster Sum of Squares (WSS)") +
    APA_THEME

elbow_plot

ggsave(filename = "plots/elbowPlot.png",
       plot = elbow_plot,
       width = 7, height = 4, dpi = DPI)

# Cluster the data with 6 clusters
k_means_clustered = kmeans(lat.lng.data,
                           centers = 6,
                           nstart = 30)

# Merge the clusters with the data
lat.lng.data = lat.lng.data %>%
    mutate(cluster = factor(k_means_clustered$cluster))

# Plot on the map
stop.map.clusters = ggmap(sf_map) +
    geom_point(data = lat.lng.data,
               aes(x = lng, y = lat, color = cluster),
               alpha = 0.1, size = 0.03) +
    scale_color_viridis_d("rocket") +
    labs(caption = "Source: Stanford Open Policing Project") +
    xlab("Longitude") +
    ylab("Latitude") +
    APA_THEME +
    theme_apa(legend.pos = "right") +
    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

stop.map.clusters

ggsave(filename = "plots/spatialClusters.png",
       plot = stop.map.clusters,
       width = 7, height = 6, dpi = DPI)

#############################################################################################################
########## Resources ########################################################################################
#############################################################################################################

# 1. How to extract the hour from the HH:MM
### Resource 1.1: StackOverflow:
###### https://stackoverflow.com/q/10705328

# 2. How to create a heat-map of the San Francisco police stops
### Resource 2.1: ggmap:
###### https://github.com/dkahle/ggmap
### Resource 2.2: Stadia Documentation:
###### https://docs.stadiamaps.com/tutorials/getting-started-in-r-with-ggmap/
### Resource 2.3: ggplot2 Density Map:
###### https://ggplot2.tidyverse.org/reference/geom_density_2d.html

# 3. How to plot a line-graph?
### Resource 3.1: R Graph Gallery:
###### https://r-graph-gallery.com/line-plot.html

# 4. How to add spacing between facets?
### Resource 4.1: StackOverflow:
###### https://stackoverflow.com/q/3681647

# 5. Modifying the tick frequency in the axis
### Resource 5.1: Online Tutorial:
###### https://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels

# 6. How to merge two strings in R?
### Resource 6.1: StackOverflow:
###### https://stackoverflow.com/q/7201341

# 7. General reading about the Logistic Regression
### Resource 7.1: Online Resource:
###### https://daviddalpiaz.github.io/r4sl/logistic-regression.html

# 8. How to plot the ROC curve
### Resource 8.1: Online Resource:
###### https://www.statology.org/roc-curve-ggplot2/

# 9. How to make tables in R
### Resource 9.1: Online Tutorial:
###### https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r

# 10. How to reshape the data from Long to Wide
### Resource 10.1: Online Tutorial:
###### https://shanghai.hosting.nyu.edu/data/r/reshaping.html

# 11. Date and Time in R
### Resource 11.1: Online Tutorial:
###### https://www.stat.berkeley.edu/~s133/dates.html

# 12. Create a folder if it does not exist
### Resource 12.1: StackOverflow:
###### https://stackoverflow.com/q/4216753

# 13. How to save a ggplot figure
### Resource 13.1: ggplot2 Documentation:
###### https://ggplot2.tidyverse.org/reference/ggsave.html

# 14. How to merge two dataframes in R
### Resource 14.1: StackOverflow:
###### https://stackoverflow.com/q/1299871

# 15. How to use the APA theme in ggplot
### Resource 15.1: Online Tutorial:
###### https://rdrr.io/cran/jtools/man/theme_apa.html

# 16. How to export the R output into a .txt file
### Resource 16.1: Posit Community Question:
###### https://forum.posit.co/t/export-r-output-to-txt-file/60732

# 17. How to change the point size in the scatterplot legend while keeping the points in the plot small
### Resource 17.1: StackOverflow:
###### https://stackoverflow.com/q/20415963

# 18. How to get the path of the script
### Resource 18.1: Online Tutorial:
###### https://www.geeksforgeeks.org/how-to-get-the-path-of-current-script-in-r/

# 19. How to add vertical line to a plot
### Resource 19.1: StackOverflow:
###### https://stackoverflow.com/q/67656324

#############################################################################################################
######### End of File #######################################################################################
#############################################################################################################
