# Loading data
d311 <- read.csv("./data/detroit-311.csv")
dBV <- read.csv("./data/detroit-blight-violations.csv")
dC <- read.csv("./data/detroit-crime.csv")
dDP <- read.delim("./data/detroit-demolition-permits.tsv")

# ------------//----------//------------
# Loading Libraries and Global configurations
options(digits = 7)
library(ggmap)
library(plyr)
library(sp)
library(caret)
library(dplyr)
library(tree)
library(MASS)

# ------------//----------//------------
# Functions
## Parsing GPS coordinates define regular expression pattern
gpsParsing <- function(addr, p="\\(.*\\)"){
  r <- regexpr(p, addr)
  out <- rep(NA, length(r))
  out[r != -1] <- regmatches(addr, r)
  ## strip the \\( and \\)
  out <- gsub("[()]", "", out)
  lats <- unlist(lapply(out, function(x) as.numeric(strsplit(x, split=",")[[1]][1])))
  lngs <- unlist(lapply(out, function(x) as.numeric(strsplit(x, split=",")[[1]][2])))
  list(lat=lats, lng=lngs)
}

## creating building ID
	# This function take a point (lat, lng) and the aggregate building index coordI
	# as arguments and return the id in the coordID
	# Condition: the point x = (lat, lng) comes from the population of (lat, lng)
	# which is huge, so this is not valid for outsider points (lat, lng)
	# Well for outsiders or NAs, it just returns NA
findIds <- function(x = c(0.0, 0.0), coordID, bycolumn="id", digits = 4){
  	x <- round(x, digits = digits)
	return (coordID[which(coordID[,"lat"] == x[1] & coordID[,"lng"] == x[2]),"id"])
}

#  sum of the total of violations of neighbors
	# This function take a point (lat, lng) and the aggregate building index batsID
	# as arguments and return the id in the batsID
	# Condition: the point x = (lat, lng) comes from the population of (lat, lng)
	# which is huge, so this is not valid for outsider points (lat, lng)
	# Well for outsiders or NAs, it just returns NA
neighbors_total <- function(x=c(0.0, 0.0), df){
	epsilon <- 0.0001  
	return(sum(df[which(abs(df[,"latR"] - x[1]) < epsilon & abs(df[,"lngR"] - x[2]) < epsilon), "freq"]))
}
 
# ------------//----------//------------
# Data Cleaning
## Setting global configuratyions for cleaning
options(digits = 8)
epsilon <- 0.0001

## dBV dataset
dBVcoords <- gpsParsing(dBV$ViolationAddress)
dBV$lat <- dBVcoords$lat
dBV$lng <- dBVcoords$lng
dBV$FineAmt <- as.numeric(gsub("\\$","", as.character(dBV$FineAmt)), na.rm=FALSE)
dBV$FineAmt[is.na(dBV$FineAmt)] <- 0
dBV$AdminFee <- as.numeric(gsub("\\$","", as.character(dBV$AdminFee)), na.rm=FALSE)
dBV$AdminFee[is.na(dBV$AdminFee)] <- 0
dBV$StateFee <- as.numeric(gsub("\\$","", as.character(dBV$StateFee)), na.rm=FALSE)
dBV$StateFee[is.na(dBV$StateFee)] <- 0
dBV$LateFee <- as.numeric(gsub("\\$","", as.character(dBV$LateFee)), na.rm=FALSE)
dBV$LateFee[is.na(dBV$LateFee)] <- 0
dBV$CleanUpCost <- as.numeric(gsub("\\$","", as.character(dBV$CleanUpCost)), na.rm=FALSE)
dBV$CleanUpCost[is.na(dBV$CleanUpCost)] <- 0
dBV$JudgmentAmt <- as.numeric(gsub("\\$","", as.character(dBV$JudgmentAmt)), na.rm=FALSE)
dBV$JudgmentAmt[is.na(dBV$JudgmentAmt)] <- 0
dBV$totalfee <- dBV$FineAmt + dBV$AdminFee + dBV$StateFee + dBV$LateFee + dBV$CleanUpCost + dBV$JudgmentAmt

## dC dataset
dC1 <- names(dC)
dC1 <- tolower(dC1)
dC1[15:16] <- c("lng","lat")
names(dC) <- dC1
rm(dC1)

## dDP dataset
dDPcoords<- gpsParsing(dDP$site_location)
dDP$lat <- dDPcoords$lat
dDP$lng <- dDPcoords$lng

# Restore default options
options(digits = 7)

# ------------//----------//------------
# Data Visualization
## Loading Detroit map
dMap <- get_googlemap(center = c(lon=-83.119128,lat=42.384713),maptype = "roadmap", size=c(640,640),zoom = 11)
ggmap(dMap)
# ------------//----------//------------

## Calculating frequencies of locations of dBV dataset
dBVloc <- ddply(dBV, .(dBV$lat, dBV$lng), nrow)
names(dBVloc) <- c("lat", "lng", "freq")
dBVloc <- dBVloc[order(-dBVloc$freq),]

## Presenting the information aquire above
dBVplot <- ggmap(dMap) + geom_point(aes(x=lng,y=lat,size=freq),data=dBVloc[2:1000,], color = ("green"))
dBVplot

# ------------//----------//------------
## Calculating frequencies of locations of d311 dataset
d311loc <- ddply(d311, .(d311$lat, d311$lng), nrow)
names(d311loc) <- c("lat", "lng", "freq")
d311loc <- d311loc[order(-d311loc$freq),]

## Presenting the information aquire above
d311plot <- ggmap(dMap) + geom_point(aes(x=lng,y=lat,size=freq),data=d311loc[2:1000,], color = ("orange"))
d311plot

# ------------//----------//------------
## Calculating frequencies of locations of dDP dataset
dDPloc <- ddply(dDP, .(dDP$lat, dDP$lng), nrow)
names(dDPloc) <- c("lat", "lng", "freq")
dDPloc <- dDPloc[order(-dDPloc$freq),]

## Presenting the information aquire above
dDPplot <- ggmap(dMap) + geom_point(aes(x=lng,y=lat,size=freq),data=dDPloc[2:1000,], color = ("purple"))
dDPplot

# ------------//----------//------------
## Calculating frequencies of locations of dC dataset
dCloc <- ddply(dC, .(dC$lat, dC$lng), nrow)
names(dCloc) <- c("lat", "lng", "freq")
dCloc <- dCloc[order(-dCloc$freq),]

## Presenting the information aquire above
dCplot <- ggmap(dMap) + geom_point(aes(x=lng,y=lat,size=freq),data=dCloc[2:1000,], color = ("red"))
dCplot

# Finding outliers
dBVfreq <- dBV[which(dBV$lat == dBVloc[2,"lat"] & dBV$lng == dBVloc[2, "lng"]),]

# ------------//----------//------------
## Creating lat lng subset
latList <- c(d311$lat, dBV$lat, dC$lat, dDP$lat)
lngList <- c(d311$lng, dBV$lng, dC$lng, dDP$lng)
coordList <- data.frame(lat = round(latList, digits =4), lng = round(lngList, digits =4))
## Calculating frequency of coordList
coordFreq <- ddply(coordList, .(lat = coordList$lat, lng = coordList$lng), nrow)
names(coordFreq) <- c("lat", "lng", "freq")
coordFreq <- coordFreq[-coordFreq$freq,]
## Unique locations
coordID <- unique(coordList)
## Removing NA
coordID <- coordID[!is.na(coordID$lat),]
## Adding ID collumn 
coordID$id <- 1:dim(coordID)[[1]]

###### SAVE 1

# ------------//----------//------------
# creating ID on the main datasets
## Rounding lat and lng ond the main datasets
dBV$latR <- round(dBV$lat, digits=4)
dBV$lngR <- round(dBV$lng, digits=4)
dC$latR <- round(dC$lat, digits=4)
dC$lngR <- round(dC$lng, digits=4)
d311$latR <- round(d311$lat, digits=4)
d311$lngR <- round(d311$lng, digits=4)
dDP$latR <- round(dDP$lat, digits=4)
dDP$lngR <- round(dDP$lng, digits=4)
## using function findID to populate id collumn
dBV$id <- as.numeric(apply(matrix(c(dBV$latR, dBV$lngR), ncol=2), 1, findIds, coordID)) 
dC$id <- as.numeric(apply(matrix(c(dC$latR, dC$lngR), ncol=2), 1, findIds, coordID))
d311$id <- as.numeric(apply(matrix(c(d311$latR, d311$lngR), ncol=2), 1, findIds, coordID))
dDP$id <- as.numeric(apply(matrix(c(dDP$latR, dDP$lngR), ncol=2), 1, findIds, coordID))

# Data saving
write.csv(dBV, "./data/save/dBV.csv")
write.csv(dC, "./data/save/dC.csv")
write.csv(d311, "./data/save/d311.csv")
write.csv(dDP, "./data/save/dDP.csv")
write.csv(coordID,"./data/save/coordID.csv")

###### SAVE 2

# ------------//----------//------------
## Labeling blight or nonBlight
coordID$blight <- FALSE
blightID <- dDP[which(dDP$PERMIT_DESCRIPTION != ""),"id"]
blightID <- unique(blightID)
coordID[which(coordID$id %in% blightID), "blight"] <- TRUE

# ------------//----------//------------
# Adding the number of blight incidents
coordID$nB <- 0
dBVids <-  plyr::count(dBV$id)
names(dBVids) <- c("id","freq")
coordID$nB[1:(nrow(dBVids) -1)] <- dBVids[c(-nrow(dBV)), "freq"]
## Creating training dataset
nblight <- table(coordID$blight)[[2]]
bFalse <- sample_n(coordID[which(coordID$blight == FALSE),], size = nblight)
bTrue <- sample_n(coordID[which(coordID$blight == TRUE),], size = nblight)
b <- rbind(bFalse, bTrue)
b[sample(1:nrow(b),5),]
write.csv(b, "./data/save/dBlight.csv") 

###### SAVE 3

# ------------//----------//------------
# Creating data model
## Adding collumn neighbor on coordID
nbviols_df <- plyr::count(dBV, vars = c("latR","lngR"))
mainData <- coordID
coordID$neighbor <- as.numeric(apply(as.matrix(coordID[,c("lat","lng")]), 1, neighbors_total, df=nbviols_df), na.rm=FALSE)
## adding crime collumns on coordID
crimesBuild <- c("ARSON", "DAMAGE TO PROPERTY", "ENVIRONMENT","RUNAWAY")
otherCrimesRelated <- c("AGGRAVATED ASSAULT", "DRUNKENNESS", "EMBEZZLEMENT", "HOMICIDE","JUSTIFIABLE HOMICIDE","LARCENY","NEGLIGENT HOMICIDE","OTHER BURGLARY","OUIL DISPOSE OF VEHICLE TO AVOID FORFEITURE","STOLEN VEHICLE","VAGRANCY (OTHER)", "ASSAULT","BURGLARY","DANGEROUS DRUGS", "HEALTH-SAFETY", "IMMIGRATION", "KIDNAPING","LIQUOR", "WEAPONS OFFENSES", "STOLEN PROPERTY", "ROBBERY")
dC1 <- plyr::count(dC[which(dC$category %in% crimesBuild),], vars = c("latR","lngR"))
coordID$crimesBuild <- as.numeric(apply(as.matrix(coordID[,c("lat","lng")]), 1, neighbors_total, df=dC1), na.rm=FALSE)
coordID$otherCrimesRelated <-  as.numeric(apply(as.matrix(coordID[,c("lat","lng")]), 1, neighbors_total, df=dC1), na.rm=FALSE)
# adding d311 calls types collumn on coordID
typeCall <- c("Traffic Sign Issue", "Traffic Signal Issue","Street Light Pole Down","Test (internal use only, public issue)")
d311call <- plyr::count(d311[-which(d311$issue_type %in% typeCall),], vars = c("latR","lngR"))
coordID$ncalls <-  as.numeric(apply(as.matrix(coordID[,c("lat","lng")]), 1, neighbors_total, df=d311call), na.rm=FALSE)
# ------------//----------//------------

# SAVE 4

# ------------//----------//------------
## adding total violation fee collumn on coordID
dBV1 <- dBV[, c("latR","lngR","totalfee")]
names(dBV1) <- c("latR","lngR","freq")
coordID$fee <- as.numeric(apply(as.matrix(coordID[,c("lat","lng")]), 1, neighbors_total, df=dBV1), na.rm=FALSE)

# ------------//----------//------------

# SAVE 5

# ------------//----------//------------
# Regression and prediction Models
## creating train and test subsets
n <- nrow(coordID)
coordSample <- sample(n, size = n * 0.8)
train <- coordID[coordSample,]
test <- coordID[-coordSample,]
# Saving train and test
write.csv(train, "./data/save/train.csv", row.names = FALSE)
write.csv(test, "./data/save/test.csv", row.names = FALSE)
# Randomly shuffle the data
train <- train[sample(nrow(train)),]
# Create 5 equally size folds
fiveFolds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)

# ------------//----------//------------

# SAVE 6

# ------------//----------//------------
## 5 fold cross validation 
## LDA 
error <- 1:5
for(i in 1:5){
	validIndex <- which(fiveFolds==i,arr.ind=TRUE)
	validData <- train[validIndex, ]
	trainData <- train[-validIndex, ]
	mod_fit <- lda(blight ~ log(1+nB) + log(1+neighbor)+ log(1 + crimesBuild) + log(1+ otherCrimesRelated) + log(1+ ncalls) + log(1 + fee),data=trainData)
	mod_probs <- predict(mod_fit, validData)
	mod_preds <- mod_probs$class
	error[i] <- mean(mod_preds != validData$blight)
}
mean(error)

####### [1] 0.0216037

## binomial
error <- 1:5
for(i in 1:5){
	validIndex <- which(fiveFolds==i,arr.ind=TRUE)
	validData <- train[validIndex, ]
	trainData <- train[-validIndex, ]
	mod_fit <- glm(blight ~ log(1+nB) + log(1+neighbor)+ log(1 + crimesBuild) + log(1+ otherCrimesRelated) + log(1+ ncalls) + log(1 + fee), data=trainData, family = binomial)
	mod_probs <- predict(mod_fit, newdata = validData, type="response")
	mod_preds <- rep(FALSE, length(mod_probs))
	mod_preds[mod_probs > 0.5] <- TRUE
	error[i] <- mean(mod_preds != validData$blight)
}
mean(error) 

####### [1] 0.0216037

## Tree
ttrain <- train
ttrain$blight <- factor(ttrain$blight)
tree <- tree(blight ~ nB + neighbor + crimesBuild +otherCrimesRelated + ncalls + fee, data = ttrain)
treePred <- predict(tree, test, type = "class")
mean(treePred != test$blight)
summary(treePred)

####### [1] 0.02131314

