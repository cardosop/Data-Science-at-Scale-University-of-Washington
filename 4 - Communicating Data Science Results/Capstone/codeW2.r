# Milestone week 2

# Functions
getGeoDetails <- function(address){   
   #use the gecode function to query google servers
   geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
   #now extract the bits that we need from the returned list
   answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
   answer$status <- geo_reply$status
 
   #if we are over the query limit - want to pause for an hour
   while(geo_reply$status == "OVER_QUERY_LIMIT"){
       print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
       time <- Sys.time()
       print(as.character(time))
       Sys.sleep(60*60)
       geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
       answer$status <- geo_reply$status
   }
 
   #return Na's if we didn't get a match:
   if (geo_reply$status != "OK"){
       return(answer)
   }   
   #else, extract what we need from the Google server reply into a dataframe:
   answer$lat <- geo_reply$results[[1]]$geometry$location$lat
   answer$long <- geo_reply$results[[1]]$geometry$location$lng   
   if (length(geo_reply$results[[1]]$types) > 0){
       answer$accuracy <- geo_reply$results[[1]]$types[[1]]
   }
   answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
   answer$formatted_address <- geo_reply$results[[1]]$formatted_address
 
   return(answer)
}


# ------------//----------//------------
# Loading Libraries
library(stringr)
library(ggmap)
library(sp)
library(rgdal)
library(tmap)
library(rworldmap)
library(maps)
library(tspmeta)
library(RgoogleMaps)
library(ggplot2)
data(usaMapEnv)

options(digits = 6)

# Loading data
d311 <- read.csv("./data/detroit-311.csv")
dBV <- read.csv("./data/detroit-blight-violations.csv")
dC <- read.csv("./data/detroit-crime.csv")
dDP <- read.delim("./data/detroit-demolition-permits.tsv")


# --------------//----------//----------
# Correcting Lat and Lon
# d311 detroit-311
d311$LatLon <- strsplit(gsub("[\\)\\( ]", "", str_extract(d311$location, "\\(.*\\)")), ",")
d311$lon <- sapply(d311$LatLon, "[[", 2)
d311$lat <- sapply(d311$LatLon, "[[", 1)

# dBV detroit-blight-violations
dBV$LatLon <- strsplit(gsub("[\\)\\( ]", "", str_extract(dBV$ViolationAddress, "\\(.*\\)")), ",")
dBV$lon <- sapply(dBV$LatLon, "[[", 2)
dBV$lat <- sapply(dBV$LatLon, "[[", 1)

# dC detroit-crime
dC$LatLon <- strsplit(gsub("[\\)\\( ]", "", str_extract(dC$LOCATION, "\\(.*\\)")), ",")
dC$lon <- dC$LON
dC$lat <- dC$LAT
n <- length(dC$LatLon)
for(i in 1:n){
	if(is.na(dC$LatLon[i]) == TRUE){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	} else if(is.na(dC$lon[i]) == TRUE){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	} else if(is.na(dC$lat[i]) == TRUE){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	}
}
for(i in 1:n){
	if(dC$lon[i] > -81){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	} 
	
}
for(i in 1:n){
	if(dC$lon[i] < -85){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	}
}
for(i in 1:n){
	if(dC$lat[i] > 43){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	} 
}
for(i in 1:n){
	if(dC$lat[i] < 40){
		t1 <- data.frame()
		t2 <- getGeoDetails(paste0(dC$ADDRESS[i], ", Detroit, USA"))
		t1 <- rbind(t1,t2)
		dC$lat[i] <- t1$lat
		dC$lon[i] <- t1$long
	}  
}	
t3 <- NULL
for(i in 1:n({#selecting number of bad rows
	if(dC$lon[i] > -81){
		t3 <- c(t3 , i)
		print(i)
	} 
	if(dC$lon[i] < -85){
		t3 <- c(t3 , i)
		print(i)
	}
	if(dC$lat[i] > 43){
		t3 <- c(t3 , i)
		print(i)
	}
	if(dC$lat[i] < 40){
		t3 <- c(t3 , i)
		print(i)
	}  
}
t3 <- unique(t3)
dC1 <- dC[-c(t3),] #removing bad rows

# dDP detroit-demolition-permits
dDP$LatLon <- strsplit(gsub("[\\)\\( ]", "", str_extract(dDP$site_location, "\\(.*\\)")), ",")
n <- length(dDP$LatLon)
for(i in 1:n){
	if(is.na(dDP$LatLon[i]) == TRUE){
        geoCoded <- data.frame()
		vTemp <- getGeoDetails(paste0(dDP$SITE_ADDRESS[i], ", Detroit, USA"))
		geoCoded <- rbind(geoCoded, vTemp)
		dDP$lat[i] <- geoCoded$lat
		dDP$lon[i] <- geoCoded$long
    } else if(is.na(dDP$LatLon[i]) == FALSE){
		dDP$lat[i] <- sapply(dDP$LatLon[i], "[[", 1)
		dDP$lon[i] <- sapply(dDP$LatLon[i], "[[", 2)
	}
}
dDP$lat[6054] <- 42.340895 # manualy adding lat and lon because was not possible automaticly 
dDP$lon[6054] <- -83.055347

for(i in 1:length(dDP$LatLon)){
    if(is.na(dDP$LatLon[i]) == TRUE){
        dDP$LatLon[i] <- c(dDP$lat, dDP$lon)
    }
}
################## SAVE 11

# ----------//----------//--------------
# Making smaller and Spatial
d311$lat <- as.double(d311$lat)
d311$lon <- as.double(d311$lon)
d311ll <- data.frame()
d311ll <- d311[, c(17,12)]
n <- length(d311ll$lat)
t3 <- d311[, c(17,12)]
d311ll$base[1:n] <- "A"
d311ll$id[1:n] <- 1:n
d311ll <- SpatialPointsDataFrame(coords = t3, data = d311ll,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# dBV
dBV$lat <- as.double(dBV$lat)
dBV$lon <- as.double(dBV$lon)
dBVll <- data.frame()
dBVll <- dBV[, c(33,34)]
n <- length(dBVll$lat)
t3 <- dBV[, c(33,34)]
dBVll$base[1:n] <- "B"
dBVll$id[1:n] <- 1:n
dBVll <- SpatialPointsDataFrame(coords = t3, data = dBVll,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# dC1
dC1$lat <- as.double(dC1$lat)
dC1$lon <- as.double(dC1$lon)
dCll <- data.frame()
dCll <- dC1[, c(19,20)]
n <- length(dCll$lat)
t3 <- dC1[, c(19,20)]
dCll$base[1:n] <- "C"
dCll$id[1:n] <- 1:n
dCll <- SpatialPointsDataFrame(coords = t3, data = dCll,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# dDP
dDP$lat <- as.double(dDP$lat)
dDP$lon <- as.double(dDP$lon)
dDPll <- dDP[, c(58,57)]
t3 <- dDP[, c(58,57)]
n <- length(dDPll$lat)
dDPll$base[1:n] <- "D"
dDPll$id[1:n] <- 1:n
dDPll <- SpatialPointsDataFrame(coords = t3, data = dDPll,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

################## SAVE 22

# ----------//----------//--------------

















# Indexing by lon/lat
# d311

t3 <- as.integer(factor(with(d311, paste(lon, lat))))

t3 <- with(d311, paste(lon, lat))
within(t3, ID <- match(t3, unique(t3)))

t3 <- d311[,c("lon","lat")]
t3 <- transform(t3, ID = as.numeric(interaction(lat, lon, drop=TRUE)))
d311$ID <-  t3$ID

# dBV
t3 <- dBV[,c("lon","lat")]
t3 <- transform(t3, ID = as.numeric(interaction(lat, lon, drop=TRUE)))
dBV$ID <-  t3$ID

# dC
t3 <- dC1[,c("lon","lat")]
t3 <- transform(t3, ID = as.numeric(interaction(lat, lon, drop=TRUE)))
dC1$ID <-  t3$ID

# dDP
t3 <- dDP[,c("lon","lat")]
t3 <- transform(t3, ID = as.numeric(interaction(lat, lon, drop=TRUE)))
dDP$ID <-  t3$ID

