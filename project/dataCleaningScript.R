# Read the original data
traffic.data = read.csv("../DATA/ca_san_francisco_2020_04_01.csv")
library(ggmap)
# Remove the columns that are not needed
toRemove = c("location",
             "district",
             "type",
             "raw_search_vehicle_description",
             "raw_result_of_contact_description")

new.data = traffic.data[, !(names(traffic.data) %in% toRemove)]

# Convert the date column to a date object
new.data$date = as.Date(new.data$date, format = "%Y-%m-%d")

# Remove the dates after 2016.01.01
new.data = new.data[new.data$date < as.Date("2016-01-01"), ]

# Write the cleaned data to a new file
write.csv(new.data, file = "../DATA/ca_sf_vehicle_2007_2016.csv", row.names = FALSE)

# Read the data and see what is in it
check.new = read.csv("../DATA/ca_sf_vehicle_2007_2016.csv")


library(ggmap)
library(ggplot2)

register_stadiamaps("30cc96f4-7081-4f0e-9c6e-61f5c5b0bf82")

# Get a map of San Francisco
sf_map <- get_stadiamap(bbox = c(left = -122.52, bottom = 37.70, right = -122.35, top = 37.85),
                        zoom = 12, maptype = "alidade_smooth")

# Plot
ggmap(sf_map) +
    geom_density2d_filled(data = check.new, aes(x = lng, y = lat), alpha = 0.6) +
    scale_fill_viridis_d(option = "rocket") +  # Alternative color scale
    labs(title = "Police Stops in San Francisco (2008) - Density Map",
         caption = "Data Source: Your Dataset") +
    theme_minimal()


