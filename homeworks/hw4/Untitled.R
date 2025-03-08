setwd("/Users/deringezgin/Documents/2024-2025/SPRING 2025/STA234 Statistical Computing with R/STA234_codes/DATA")
library(ggplot2)
library(tidyr)
### Load datasets ###
traffic.data = read.csv("ca_sf_vehicle_2007_2016.csv")
race.data = read.csv("population_race_data.csv")


########################################## DATA CLEANING FOR TRAFFIC DATA ########################################################
traffic.data$subject_sex = factor(traffic.data$subject_sex,
                                  levels = c("male", "female"),
                                  labels = c("Male", "Female"))

traffic.data$subject_race = factor(traffic.data$subject_race,
                                   levels = c("asian/pacific islander",
                                              "black",
                                              "hispanic",
                                              "white",
                                              "other"),
                                   labels = c("Asian/Pacific Islander",
                                              "Black",
                                              "Hispanic",
                                              "White",
                                              "Other"))

traffic.data$date = as.Date(traffic.data$date)
traffic.data$time = strptime(traffic.data$time, format="%H:%M:%S")

traffic.data$outcome[is.na(traffic.data$outcome)] = "No Action"

traffic.data$outcome = factor(traffic.data$outcome,
                              levels = c("citation",
                                         "warning",
                                         "arrest",
                                         "No Action"),
                              labels = c("Citation",
                                         "Warning",
                                         "Arrest",
                                         "No Action"))

traffic.data$Year = format(traffic.data$date, "%Y")

traffic.data <- merge(traffic.data,
                      race.data,
                      by = c("subject_race", "Year"),
                      all.x = TRUE)

colnames(traffic.data)[ncol(traffic.data)] <- "proportioned_value"

traffic.data$search_conducted = factor(traffic.data$search_conducted,
                                      levels = c("FALSE", "TRUE"),
                                      labels = c("No", "Yes"))

traffic.data$Year = NULL


traffic.data$generalWeights = (1 / (traffic.data$proportioned_value)) / sum(1/(traffic.data$proportioned_value))




library(dplyr)
library(ggalluvial)

# Aggregate stops by Gender, Race, Search Conducted, and Outcome
sankey_data <- aggregate(traffic.data$generalWeights,
                         by = list(traffic.data$subject_sex,
                                   traffic.data$subject_race,
                                   traffic.data$search_conducted,
                                   traffic.data$outcome),
                         FUN = sum)

# Rename columns for clarity
colnames(sankey_data) <- c("Gender", "Race", "Search_Conducted", "Outcome", "Count")

ggplot(data = sankey_data, aes(axis1 = Gender, axis2 = Race, axis3 = Search_Conducted, axis4 = Outcome,
                               y = Count)) +
    geom_alluvium(aes(fill = Race), width = 0.3, alpha = 0.7) +  # Flow lines
    geom_stratum(width = 0.5, fill = "grey", color = "black") +  # Nodes (stratum)
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +  # Labels
    scale_fill_brewer(type = "qual", palette = "Set2") +

    # Rename X-axis ticks
    scale_x_discrete(labels = c("Gender", "Race", "Search Conducted", "Outcome")) +

    labs(title = "Traffic Stops Flow: Gender → Race → Search → Outcome",
         subtitle = "Visualizing Stop Progression Through Categories",
         x = "Stop Stages",
         y = "Adjusted Number of Stops",
         fill = "Race",
         caption = "Source: Stanford Open Policing Project")






































































####################################################################################################################################################################################################

# # race.agg = aggregate(race.data$Population_Percentage,
# #                      by = list(race.data$subject_race,
# #                                race.data$Year),
# #                      FUN = sum)
# #
# #
# # # Plot the racial population share over time
# # race.line.plot = ggplot(data = race.agg,
# #                         aes(x = Year,
# #                             y = Population_Percentage,
# #                             color = subject_race)) +
# #     geom_line(size = 1) +
# #     labs(title = "Share of Each Race in the Population Over Time",
# #          x = "Year",
# #          y = "Percentage of Population",
# #          color = "Race") +
# #     scale_y_continuous(labels = scales::percent) +
# #     theme_classic()
# #
# # race.line.plot
#
# #
# # # Plotting the race data in a line graph
# # # Plot the racial population share over time
# # race.line.plot = ggplot(data = race.data, aes(x = Year,
# #                                               y = Population_Percentage,
# #                                               color = subject_race,
# #                                               group = subject_race)) +
# #     geom_line(size = 1) +
# #     labs(title = "Share of Each Race in the Population Over Time",
# #          x = "Year",
# #          y = "Percentage of Population",
# #          color = "Race") +
# #     scale_y_continuous(labels = scales::percent) +
# #     theme_classic()
# #
# # race.line.plot
#
# ############## ORIGINAL PLOT ######################################
# # group.race.sex.total = aggregate(traffic.data$raw_row_number,
# #                                  by = list(traffic.data$subject_race,
# #                                            format(traffic.data$date, "%Y")),
# #                                  FUN = length)
# #
# # colnames(group.race.sex.total)= c("subject_race", "Year", "Count")
# #
# # g = ggplot(data = group.race.sex.total, aes(x = Year,
# #                                             y = Count,
# #                                             fill = subject_race)) +
# #     geom_bar(stat = "identity", width = 0.5) +
# #     xlab("Gender") +
# #     ylab("Number of Stops") +
# #     labs(title = "Fig.1 Traffic Stops by Gender",
# #          subtitle = "Colored by race",
# #          caption = "Source: Stanford Open Policing Project") +
# #
# #     scale_y_continuous(labels = scales::comma)
# #
# # g
#
#
# ##################### WEIGHTS BY RACE ################################################################################
# # group.race.proportions = merge(group.race.sex.total, race.data, by = c("subject_race", "Year"))
# # group.race.proportions$weights = (group.race.proportions$Count / (group.race.proportions$Population_Percentage * 100))
# #
# #
# # g = ggplot(data = group.race.proportions, aes(x = Year,
# #                                             y = weights,
# #                                             fill = subject_race)) +
# #     geom_bar(stat = "identity", width = 0.5) +
# #     xlab("Race") +
# #     ylab("Relative Stop Rate (Stops / % of Population)") +
# #     labs(title = "Fig.XXX Traffic Stops by Race",
# #          subtitle = "Proportionate with Race",
# #          caption = "Source: Stanford Open Policing Project",
# #          fill = "Subject Race") +
# #     scale_y_continuous(labels = scales::comma) +
# #     theme_classic()
# #
# # g
#
#
# # Normalize the weights so they sum to total number of stops
# traffic.data$weight <- (1 / (traffic.data$proportioned_value)) / sum(1/(traffic.data$proportioned_value))
#
# # Plot with normalized weights
# g = ggplot(traffic.data, aes(x = as.numeric(format(time, "%H")),
#                              fill = subject_race,
#                              weight = weight)) +
#     geom_histogram(binwidth = 1, color = "black") +
#     xlab("Hour of the Day") +
#     ylab("Adjusted Number of Stops") +
#     scale_y_continuous(labels = scales::comma) +
#     labs(title = "Fig.XXX Traffic Stops by Hour",
#          subtitle = "Proportionate with Race (Normalized)",
#          caption = "Source: Stanford Open Policing Project",
#          fill = "Subject Race")
#
# g
#
# g = ggplot(traffic.data, aes(x = subject_age,
#                              fill = subject_race,
#                              weight = weight)) +  # Apply normalized weight
#     geom_histogram(binwidth = 2, color = "black") +
#     xlab("Age of the Subject") +
#     ylab("Adjusted Number of Stops") +  # Change Y-label to reflect adjustment
#     labs(title = "Fig.XXX Traffic Stops by Subject Age",
#          subtitle = "Proportionate with Race (Normalized)",
#          caption="Source: Stanford Open Policing Project",
#          fill="Subject Race") +
#     scale_y_continuous(labels = scales::comma)
#
# g
