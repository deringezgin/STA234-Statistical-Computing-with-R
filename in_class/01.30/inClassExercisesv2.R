ozone2020 = read.csv("././DATA/hourly_44201_2020.csv", header=TRUE)
head(ozone2020)
names(ozone2020)
length(unique(ozone2020$State.name))
