# Read in csv files

datacsv1 <- read.csv("DM_2305_ExcelExample_plots.csv")
datacsv2 <- read.csv("DM_2305_ExcelExample_sites.csv")

head(datacsv1)

# Load xlsx library
library("xslx") # Need to install java before will work

dataexcel1 <- read.xslx("DM_2305_ExcelExample.xslx", sheetName = "Plot Measurements")

head(dataexcel1)

# Loading data from Access using ODBC driver
# Check version of R
sessionInfo()
# If using 64-bit need to change to 32-bit - change in tools > global options > general > R version
# Find ODBC driver in windows, open windows explorer > c drive > search for ODBCad32 > sysWOW64
# > odbcad32 application > add new database > type in data source name and description & select database
# Connect to database
library(RODBC)

connection <- odbcConnect("Database_DMcourse")

# Section 2
iris <- iris

iris$Plot <- rep(c(rep(1,10), rep(2, 10), rep(3,5)),3)
lm1 <- lm(Sepal.Length ~ Species + Plot, data = iris)
summary(lm1)

is.factor(iris$Plot)
iris$Plotf <- factor(iris$Plot)
is.factor(iris$Plotf)

lm2 <- lm(Sepal.Length ~ Species + Plotf, data = iris)
summary(lm2)

iris.list <- as.list(iris)
str(iris.list)

# Check for duplicates in data - sees if any rows exactly match any other rows
duplicated(iris[,3:4])
iris.unique <- iris[!duplicated(iris[,3:4]),] #The exclamation marks means 'not'
nrow(iris.unique) #102 rows remain in this dataset from the 150 original rows

library(dplyr)
iris.unique2 <- distinct(iris, Petal.Length, Petal.Width)
nrow(iris.unique2)

# Removing missing data
iris.NA <- iris
iris.NA[1:4,1] <- NA #replace the first four entries in column 1 with NA
head(iris.NA)
summary(iris.NA)
iris.NA[is.na(iris.NA$Sepal.Length),]

iris.NA.cc <- iris.NA[complete.cases(iris.NA),]
head(iris.NA.cc)
summary(iris.NA.cc)

# Reshaping data
library(reshape2)
iris.melt <- melt(iris)
summary(iris.melt)

iris.cast <- dcast(iris.melt, value~variable)
head(iris.cast)
iris.cast2 <- dcast(iris.melt, Species~variable, fun=mean)
head(iris.cast2)

# Add extra data to dataframe
iris.extra <- data.frame(Sepal.Length = rnorm(10, 5, 0.7), Sepal.Width = rnorm(10,3.2,0.5),
                         Petal.Length = rnorm(10,1.3,0.3), Petal.Width = rnorm(10,0.2, 0.001), 
                         Species = "setosa", Plot = 4)
iris.extra$Plotf <- factor(iris.extra$Plot)
iris.extra$LogSepLength <- log(iris.extra$Sepal.Length)
iris$LogSepLength <- log(iris$Sepal.Length)
iris.all <- rbind(iris, iris.extra)


# Add unique identifier column to dataframe
iris.all$ObsID <- paste0(iris.all$Species, row.names(iris.all))

irisspdata <- data.frame(Species = unique(iris$Species), avgheight = c(42.3, 33.5, 35.7), colour = c("violet", "blue", "blue"))
irisindivdata <- data.frame(ObsID = iris.all$ObsID, noseeds = c(rpois(50, 10), rpois(50,8), rpois(50, 9), rpois(10,10)))
irisindivdata$germprop <- c(rpois(50, 3), rpois(50,2), rpois(50, 3), rpois(10,3))/irisindivdata$noseeds

library(sqldf)
# Need to change name of object as . has meaning in sql package
iris_all <- iris.all
irismatchsp <- sqldf("select * from iris_all, irisspdata where iris_all.Species = irisspdata.Species")
irismatchindiv <- sqldf("select * from irismatchsp, irisindivdata where irismatchsp.ObsID = irisindivdata.ObsID") # doesnt work

irismatchsp2 <- sqldf("select t1.*, t2.avgheight, t2.colour from iris_all as t1, irisspdata as t2 where t1.Species = t2.Species")
irismatchindiv <- sqldf("select * from irismatchsp2, irisindivdata where irismatchsp2.ObsID = irisindivdata.ObsID")
head(irismatchindiv)

x <- seq(1,20,2)

