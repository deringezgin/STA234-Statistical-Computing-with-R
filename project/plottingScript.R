# traffic.data = read.csv("../DATA/ca_sf_vehicle_2007_2016.csv")
#
# traffic.data$subject_sex = factor(traffic.data$subject_sex,
#                                   levels = c("male", "female"),
#                                   labels = c("Male", "Female"))
#
# traffic.data$subject_race = factor(traffic.data$subject_race,
#                                    levels = c("asian/pacific islander","black","hispanic","white", "other"),
#                                    labels = c("Asian/Pacific Islander", "Black", "Hispanic", "White", "Other"))
#
# traffic.data$date = as.Date(traffic.data$date)
# traffic.data$time = strptime(traffic.data$time, format="%H:%M:%S")
#
# traffic.data$outcome[is.na(traffic.data$outcome)] = "No Action"
#
# traffic.data$outcome = factor(traffic.data$outcome,
#                               levels = c("citation", "warning", "arrest", "No Action"),
#                               labels = c("Citation", "Warning", "Arrest", "No Action"))


# group.gender.total = aggregate(traffic.data$raw_row_number,
#                                by = list(traffic.data$subject_sex),
#                                FUN = length)
# colnames(group.gender.total) = c("Gender", "Count")
#
# total.gender.plot = ggplot(data = group.gender.total, aes(x = Gender, y = Count)) +
#     geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
#     xlab("Gender") +
#     ylab("Number of Stops") +
#     ggtitle("Total Traffic Stops by Gender from 2007 to 2016") +
#     scale_y_continuous(labels = scales::comma)
#
# total.gender.plot


# group.race.total = aggregate(traffic.data$raw_row_number,
#                              by = list(traffic.data$subject_race),
#                              FUN = length)
# colnames(group.race.total)= c("Race", "Count")
#
# g = ggplot(data = group.race.total, aes(x = Race, y = Count)) +
#     geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
#     xlab("Gender") +
#     ylab("Number of Stops") +
#     ggtitle("Total Traffic Stops by Gender from 2007 to 2016") +
#     scale_y_continuous(labels = scales::comma)
#
# g


# group.race.sex.total = aggregate(traffic.data$raw_row_number,
#                              by = list(traffic.data$subject_race, traffic.data$subject_sex),
#                              FUN = length)
#
# colnames(group.race.sex.total)= c("Race", "Gender", "Count")
#
# g = ggplot(data = group.race.sex.total, aes(x = Gender, y = Count, fill=Race)) +
#     geom_bar(stat = "identity", width = 0.5) +
#     xlab("Gender") +
#     ylab("Number of Stops") +
#     ggtitle("Total Traffic Stops by Gender from 2007 to 2016") +
#     scale_y_continuous(labels = scales::comma)
#
# g

# g = ggplot(traffic.data, aes(x = as.numeric(format(time, "%H")), fill=subject_race)) +
#     geom_histogram(binwidth = 1, color = "black") +
#     xlab("Hour of the Day") +
#     ylab("Number of Stops") +
#     ggtitle("Distribution of Traffic Stops by Hour") +
#     scale_y_continuous(labels = scales::comma)
#
# g


# stop.race.total = aggregate(traffic.data$raw_row_number,
#                              by = list(traffic.data$subject_race, traffic.data$outcome),
#                              FUN = length)
#
# colnames(stop.race.total)= c("Race", "Outcome", "Count")
#
# g = ggplot(data = stop.race.total, aes(x = Outcome, y = Count, fill=Race)) +
#     geom_bar(stat = "identity", width = 0.5) +
#     xlab("Outcome") +
#     ylab("Log of Number of Stops") +
#     ggtitle("Outcome of the Stops by Race") +
#     scale_y_continuous(labels = scales::comma, trans = "log10")
#
# g

# register_stadiamaps("30cc96f4-7081-4f0e-9c6e-61f5c5b0bf82")
#
# # Get a map of San Francisco
# sf_map <- get_stadiamap(bbox = c(left = -122.52, bottom = 37.70, right = -122.35, top = 37.85),
#                         zoom = 12, maptype = "alidade_smooth")
#
# # Plot
# ggmap(sf_map) +
#     geom_density2d_filled(data = traffic.data, aes(x = lng, y = lat), alpha = 0.6) +
#     scale_fill_viridis_d(option = "rocket") +
#     labs(title = "Police Stops in San Francisco (2008) - Density Map")

# Plot ages in a histogram

g = ggplot(traffic.data, aes(x = subject_age, fill=subject_race)) +
    geom_histogram(binwidth = 5, color = "black") +
    xlab("Age of the Subject") +
    ylab("Number of Stops") +
    ggtitle("Distribution of Traffic Stops by Age and Race") +
    scale_y_continuous(labels = scales::comma)

g




