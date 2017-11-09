mydata<-read.csv('project/2017-Q1-Trips-History-Data.csv')

library(RCurl)  #request geocode
library(RJSONIO) #decode json for geocode
library(plyr)    #encode url

library(geosphere) #HAVERSINE distance calculation
#google api key (unused)
#mykey= AIzaSyDktb-wEIWTuPYtKsMLuofZkY5cYk7jlI4

####### compile url for accessing google map api ####### 
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

####### request geocode ####### 
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- gsub("%20&%20","%20%26%20",url(address))  #"&" in address need to be convert to %26
  doc <- getURL(u) #send request 
  x <- fromJSON(doc,simplify = FALSE) #decode json result
  if(x$status=="OK") { #return parts of result
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5) #google API limits number of requests per second.  Thus sleep.
  } else {
    return(c(NA,NA,NA, NA))
  }
}

####### edit start station address ####### 
edited.start.station<-as.character(mydata$Start.station)
for (j in 1:nrow(mydata)){
  address<-edited.start.station[j]
  if (grepl('/',address)==TRUE){   #if there is '/' in adress, google api won't process
    splited<-strsplit(address,'/')
    for (i in 1:2){
      #keep the station name instead of street address
      if (grepl("\\d", splited[[1]][i]) ==FALSE){ #if number in address
        if (grepl('St',splited[[1]][i]) ==FALSE){ #if 'St' in address
          address<-splited[[1]][i]}
      }}
  }
  edited.start.station[j]<-address
}

####### edit end station address (similar to above) ####### 
edited.end.station<-as.character(mydata$End.station)
for (j in 1:nrow(mydata)){
  address<-edited.end.station[j]
  if (grepl('/',address)==TRUE){
    splited<-strsplit(address,'/')
    for (i in 1:2){
      if (grepl("\\d", splited[[1]][i]) ==FALSE){
        if (grepl('St',splited[[1]][i]) ==FALSE){
          address<-splited[[1]][i]}
      }}
  }
  edited.end.station[j]<-address
}

unique.station<-unique(c(edited.start.station,edited.end.station))
unique.org.station<-unique(c(as.character(mydata$Start.station),as.character(mydata$End.station)))
lon<-rep(NA,length(unique.station))
lat<-rep(NA,length(unique.station))
geocodeaddress<-rep(NA,length(unique.station))


#nalst<-which(is.na(lon))
for (i in 1:length(unique.station)){
#for (i in nalst){
  address<-as.character(unique.station[i])
  trytimes=1
  result[1]<-NA
  while (is.na(result[1]) ==TRUE & trytimes<6){
      result<-geoCode(paste(address,", Washington, DC",sep=""))
      trytimes=trytimes+1}
  lat[i]<-result[1]
  lon[i]<-result[2]
  geocodeaddress[i]<-result[4]
  }


test.df<-data.frame(as.numeric(lon),as.numeric(lat),geocodeaddress,unique.station)
selfcompute<-which(is.na(test.df$geocodeaddress))
#87 264 300 385 416 452
test.df$unique.station[selfcompute]
#[1] "22nd & P ST NW"                     "King St Metro North / Cameron St"  
#[3] "DOES"                               "Sunset Hills Rd & Discovery Square"
#[5] "Garland Ave & Walden Rd"            "Traville Gateway Dr & Gudelsky Dr" 
#self imputation for lat and lon from google map
names(test.df)<-c("lon","lat","geocodeaddress","unique.station")
test.df$lat[87]<-38.909835
test.df$lon[87]<--77.048872
test.df$lat[264]<-38.807407
test.df$lon[264]<--77.059965
test.df$lat[300]<-38.899155
test.df$lon[300]<--76.946834
test.df$lat[385]<-38.959509
test.df$lon[385]<--77.371299
test.df$lat[416]<-39.001025
test.df$lon[416]<--77.001176
test.df$lat[452]<-39.097931
test.df$lon[452]<--77.204221

for (i in 1:length(test.df$geocodeaddress)){ #CHECK all geocode are reasonable
  if (grepl("DC",test.df$geocodeaddress[i]) ==FALSE &
      grepl(" VA",test.df$geocodeaddress[i]) ==FALSE &
      grepl(" MD",test.df$geocodeaddress[i]) ==FALSE){
    print(i)
    print(as.character(test.df$geocodeaddress[i]))
    print(as.character(test.df$unique.station[i]))
  }
}

test.df<-data.frame(test.df,unique.org.station)

for (i in  1:nrow(mydata)){
  testindex.st<-which(test.df$unique.org.station==as.character(mydata$Start.station[i]))
  lat.st<-test.df$lat[testindex.st]
  lon.st<-test.df$lon[testindex.st]
  
  testindex.ed<-which(test.df$unique.org.station==as.character(mydata$End.station[i]))
  lat.ed<-test.df$lat[testindex.ed]
  lon.ed<-test.df$lon[testindex.ed]
  
  mydata$traveldist[i]<-as.numeric(distm (c(lon.st, lat.st), c(lon.ed, lat.ed), fun = distHaversine))
  #https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r
  #distance is in meter
  #if(i%%1000==0){print(i)}
  }

write.table(mydata, file="2017Q1_w_dist.csv", row.names=F, sep=",")