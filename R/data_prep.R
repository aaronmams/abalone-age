rm(list=ls())
# load the library
library(RCurl)
library(rerddap)
# specify the URL for the Iris data CSV
urlfile <-'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
# download the file
downloaded <- getURL(urlfile, ssl.verifypeer=FALSE)
# treat the text data as a steam so we can read from it
connection <- textConnection(downloaded)
# parse the downloaded data as CSV
abalone <- read.csv(connection, header=FALSE)
# preview the first 5 rows
head(abalone)

names(abalone) <- c('sex','length','diameter','height','whole_weight','shucked_weight','viscera_weight','shell_weight','ring')

#--------------------------------------------------------------------
# not a real part of the analysis but I'm going to add a totally made up
# location field so we can illustrate a data join in the first 
# pipe of our pipeline

lat <- c(rep(39,1000),rep(38.975,1000),rep(38.85,1000),rep(38.5,1177))
long <- c(runif(min=-123.94,max=-123.72,n=1000),
          runif(min=-123.93,max=-123.725,n=1000),
          runif(min=-123.9,max=-123.65,n=1000),
          runif(min=-123.59,max=-123.225,n=1177))
abalone$lat <- lat
abalone$long <- long
#--------------------------------------------------------------------


bathy <- read.csv('data/bathy.csv',skip=1)
names(bathy) <- c('lat','long','meters')

#=====================================================================
# get bottom depths for each observation
library(gstat)
library(sp)
library(maptools)
library(dplyr)

#format the abalone locations
abs.locs <- abalone %>% mutate(x=long,y=lat) %>% select(x,y)
coordinates(abs.locs) = ~x+y

#form the bounding box
bb.lats <- c(max(abalone$lat)+0.04,min(abalone$lat)-0.04)
bb.longs <- c(min(abalone$long)-0.04,max(abalone$long)+0.04)

depths <- tbl_df(bathy) %>% 
          filter(lat < bb.lats[1]) %>%
          filter(lat > bb.lats[2]) %>%
          filter(long > bb.longs[1]) %>%
          filter(long < bb.longs[2])

depths <- depths %>% mutate(x=long,y=lat) %>% select(x,y,meters)
coordinates(depths) = ~x+y

idw <- idw(formula=meters~1,locations=depths,newdata=abs.locs)
tmp <- data.frame(idw)
#=====================================================================

#add bottom depths to the abalone data
abalone <- data.frame(abalone,tmp$var1.pred)
names(abalone) <- c('sex','length','diameter','height','whole_weight','shucked_weight',
                    'viscera_weight','shell_weight','ring','lat','long','depth_meters')
saveRDS(abalone,"data/abalone.RDA")
