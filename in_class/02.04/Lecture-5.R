################################################
## R code for Lecture-4
## STA 234, Spring2025
## Date: 1/30/25
## Written by P.Kohli
## Reference: Recommended Textbook 2 in Syllabus
################################################
## step1: need to download Ozone hourly data #
## step2: it's a zip file, so extract data
## step3: save it in your working directory
## where this code is also saved
## https://aqs.epa.gov/aqsweb/airdata/download_files.html
ozone.2020 <- read.csv("DATA/hourly_44201_2020.csv",header=TRUE)
#check names, atributes, top, bottom of this data
names(ozone.2020)
str(ozone.2020)
head(ozone.2020)
tail(ozone.2020)
#selected variables, that is, columns 6,7 and 10
head(ozone.2020[,c(6:7,10)])
tail(ozone.2020 [,c(6:7,10)])
#summaries
table(ozone.2020$Time.Local)
unique(ozone.2020$State.Name)
length(unique(ozone.2020$State.Name))

summary(ozone.2020$Sample.Measurement)
quantile(ozone.2020$Sample.Measurement,seq(0,1,0.1))

#simple aggregation
group.states <- aggregate(ozone.2020$Sample.Measurement,by=list(ozone.2020$State.Name),FUN=mean)
head(group.states)
View(group.states)

group.counties <- aggregate(ozone.2020$Sample.Measurement,by=list(ozone.2020$State.Name,ozone.2020$County.Name),FUN=mean)
head(group.counties)
View(group.counties)
colnames(group.counties) <- c("State","County","Ozone")
ranked <- group.counties[order(group.counties$Ozone,decreasing=TRUE),]
head(ranked,10)

tail(ranked,10)

######################################
# Practice and Report your findings #
####################################
#1. Extract data from New York State and CA to compare state level with summaries
# Get the summary for each state
group.states.summary <- aggregate(ozone.2020$Sample.Measurement,by=list(ozone.2020$State.Name),FUN=summary)
names(group.states.summary) # Check the names of the summary
head(group.states.summary$x) # Check the first 6 rows of the summary
#state level NY AND CA
NY <- group.states.summary[group.states.summary$Group.1=="New York",]  # Get the summary of the NY data
CA <- group.states.summary[group.states.summary$Group.1=="California",]  # Get the summary of the CA data
TX <- group.states.summary[group.states.summary$Group.1=="Texas",]  # Get the summary of the CA data

rbind(NY,CA, TX) # Combine the two summaries

#county level NYC AND LA with mean ozone levels
#identify and extract NY City name
index.NY <- which(group.counties$State=="New York")  # Get the counties that are in NY
group.counties[index.NY,]  # Check the counties in NY
#identify and extract LA name
index.CA <- which(group.counties$State=="California") # Get the counties that are in CA
group.counties[index.CA,] # Check the counties in CA

index.TX <- which(group.counties$State=="Texas") # Get the counties that are in CA
group.counties[index.TX,] # Check the counties in CA

rbind(group.counties[group.counties$County=="New York",], group.counties[group.counties$County=="Los Angeles",])
group.counties[group.counties$County==c("New York","Los Angeles"),]

group.counties.summary <- aggregate(ozone.2020$Sample.Measurement,by=list(ozone.2020$State.Name,ozone.2020$County.Name),FUN=summary)
names(group.counties.summary)
head(group.counties.summary)
rbind(group.counties.summary[group.counties.summary$Group.2=="New York",],
group.counties.summary[group.counties.summary$Group.2=="Los Angeles",])

