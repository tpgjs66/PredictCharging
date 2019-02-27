library(party)
library(MCMCglmm)
library(CHAID)
library(gdistance)

setwd("~/ActivityPriority/PredictCharging/")
sched <- read.csv("~/ActivityPriority/PredictCharging/data/sched-coords-Helmond-cs-loc.csv")

################################################################################
######################      Calculate route distance      ######################
################################################################################

## Function for route information using routino
routingSched <- function(data, Lat1, Lon1, Lat2, Lon2) {
  if (strsplit(Lat2,split="_")[[1]][1] != strsplit(Lon2,split="_")[[1]][1]) {
    stop("Lat2 and Lon2 is differ!")
  }
  # Create new column name for route info
  cstype <- strsplit(Lat2,split="_")[[1]][1]
  dur <- paste0(cstype,"_dur") 
  dist <- paste0(cstype,"_dist")
  
  # Routino routine
  fileloc <- "/Users/KimSeheon/routino/quickest-all.txt"  
  #This is the default working directory
  setwd("/Users/KimSeheon/routino/")
  n <- nrow(data)
  routeresults <- c()
  for (i in 1:n) {
    print(i)
    # Coordinates of charging station by type
    lat2 <- data[Lat2][i,]
    lon2 <- data[Lon2][i,]
    
    # Skip the first episode
    if (data$EpisodeID[i] == 0){
      routeresults[i] <- list(NULL)
      next
    }
    
    # Coordinates of Activity episode
    lat1 <- data[Lat1][i,]
    lon1 <- data[Lon1][i,]
    
    # Assign transport mode to route (Always walk)
    tmode <- "foot"
    if (cstype == "schedSnellader") {
      tmode <- "motorcar"
    }
    
    # Command implementation
    router <- paste("router --transport=", tmode,
                    " --prefix=nl",
                    " --quickest",
                    " --lat1=", lat1,
                    " --lon1=", lon1,
                    " --lat2=",lat2,
                    " --lon2=",lon2,
                    # "--translations=/Users/KimSeheon/routino/routino-translations.xml",
                    # "--profiles=/Users/KimSeheon/routino/xml/routino-profiles.xml",
                    " --output-text-all",
                    # "--output-stdout",
                    " --quiet --dir=/Users/KimSeheon/routino/", sep = "")
    
    system(router, wait = TRUE)  # Send the routing command
    
    # Read in the txt instructions to extract the network distance
    routeresults[[i]] <- read.delim(fileloc, header = F, sep = "\t", skip = 6)
    colnames(routeresults[[i]]) <- c('lat', 'lng', 'node', 'type',
                                     'seg.distance', 'seg.duration', 'distance',
                                     'duration', 'speed', 'bearing', 'highway')
    
  }
  
  # For leaflet visualization
  lines <- c()
  index <- c()
  for (i in 1:n) {
    if (is.null(routeresults[[i]])) {
      next
    }
    index <- append(index, i)
    lines[i] <- (list(sp::Lines(sp::Line(routeresults[[i]][2:1]),
                                ID = data$SchedID[[i]])))
  }
  filtered.lines <- Filter(Negate(is.null), lines)
  filtered.lines <- SpatialLines(filtered.lines,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  filtered.sched <- data[index,]
  
  routes <- SpatialLinesDataFrame(sl = filtered.lines, data = filtered.sched, match.ID = FALSE)
  
  data[dur] <- NA
  data[dist] <- NA
  # Collect info from routino results
  for (i in 1:nrow(data[index,])) {
    if (is.null(routeresults[[i]])) {
      next
    }
    data[,dur][i] <- routeresults[[i]][nrow(routeresults[[i]]),]$duration
    data[,dist][i] <- routeresults[[i]][nrow(routeresults[[i]]),]$distance
  }
  return(list(data,routes))
}

# Execute the routing function (sched)
sched.Krachtstroom <- routingSched(sched, Lat1 = "DestLat", Lon1 = "DestLng", Lat2="krachtstroom_lat",Lon2="krachtstroom_long")
sched <- sched.Krachtstroom[[1]]

sched.Snellader <- routingSched(sched, Lat1 = "DestLat", Lon1 = "DestLng", Lat2="snellader_lat",Lon2="snellader_lon")
sched <- sched.Snellader[[1]]

sched.Stopcontact <- routingSched(sched, Lat1 = "DestLat", Lon1 = "DestLng", Lat2="schedStopcontact_Y",Lon2="schedStopcontact_X")
sched <- sched.Stopcontact[[1]]

# Write out schedule data
write.csv(sched,"~/ActivityPriority/PredictCharging/sched.csv")

################################################################################
######################      Predict charging decision     ######################
################################################################################

sched <- read.csv("sched-coords-Helmond.csv")

######################        Categorizing variables      ######################

## Count charging stations in activity location (PC4)
pc4sp = rgdal::readOGR("~/ActivityPriority/PredictCharging/data/ppcs_single_cs.shp", layer = "ppcs_single_cs")
pc4sp$krachtstro <- as.numeric(pc4sp$krachtstro)
pc4sp$snellader_ <- as.numeric(pc4sp$snellader_)
pc4sp$krachtstro[is.na(pc4sp$krachtstro)] <- 0
pc4sp$snellader_[is.na(pc4sp$snellader_)] <- 0

coords <- cbind(sched$DestLng,sched$DestLat)
coords <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
pc4 <- over(coords,pc4sp)

sched$KrachtstroomN <- pc4$krachtstro
sched$SnelladerN <- pc4$snellader_

# Time conversion
leaveTime <- strsplit(as.character(sched$LeaveTime),":|\\+")
leaveTime <- data.frame(matrix(unlist(leaveTime), nrow=length(leaveTime), byrow=T))
leaveTime$X1 <- as.integer(as.character(leaveTime$X1))
leaveTime$X2 <- as.integer(as.character(leaveTime$X2))
leaveTime$X3 <- as.integer(as.character(leaveTime$X3))
leaveTime$min <- leaveTime$X1*60 + leaveTime$X2 + leaveTime$X3*24*60
sched$LeaveTime <- leaveTime$min
rm(leaveTime)

beginTime <- strsplit(as.character(sched$BeginTime),":|\\+")
beginTime <- data.frame(matrix(unlist(beginTime), nrow=length(beginTime), byrow=T))
beginTime$X1 <- as.integer(as.character(beginTime$X1))
beginTime$X2 <- as.integer(as.character(beginTime$X2))
beginTime$X3 <- as.integer(as.character(beginTime$X3))
beginTime$min <- beginTime$X1*60 + beginTime$X2 + beginTime$X3*24*60
sched$BeginTime <- beginTime$min
rm(beginTime)

endTime <- strsplit(as.character(sched$EndTime),":|\\+")
endTime <- data.frame(matrix(unlist(endTime), nrow=length(endTime), byrow=T))
endTime$X1 <- as.integer(as.character(endTime$X1))
endTime$X2 <- as.integer(as.character(endTime$X2))
endTime$X3 <- as.integer(as.character(endTime$X3))
endTime$min <- endTime$X1*60 + endTime$X2 + endTime$X3*24*60
sched$EndTime <- endTime$min
rm(endTime)

# Activity type conversion
# names(sched)[names(sched) == "ActivityType"] <- "Act"
sched$ActivityType <- as.character(sched$ActivityType)

sched$ActivityType[sched$ActivityType %in% c("Home")] <- as.character("0")
sched$ActivityType[sched$ActivityType %in% c("Work")] <- as.character("1")
sched$ActivityType[sched$ActivityType %in% c("Business")] <- as.character("2")
sched$ActivityType[sched$ActivityType %in% c("BringGet")] <- as.character("3")
sched$ActivityType[sched$ActivityType %in% c("Groceries")] <- as.character("4")
sched$ActivityType[sched$ActivityType %in% c("NonGroc")] <- as.character("5")
sched$ActivityType[sched$ActivityType %in% c("Services")] <- as.character("6")
sched$ActivityType[sched$ActivityType %in% c("Social")] <- as.character("7")
sched$ActivityType[sched$ActivityType %in% c("Leisure")] <- as.character("8")
sched$ActivityType[sched$ActivityType %in% c("Touring")] <- as.character("9")
sched$ActivityType[sched$ActivityType %in% c("Other")] <- as.character("10")
# sched$ActivityType[sched$ActivityType %in% c("")] <- as.character("11")
sched$ActivityType <- as.factor(sched$ActivityType)

# Mode conversion
sched$Mode <- as.character(sched$Mode)
sched$Mode[sched$Mode %in% c("Walking or Biking")] <- as.character("0")
sched$Mode[sched$Mode %in% c("Car")] <- as.character("1")
sched$Mode[sched$Mode %in% c("Car as Passenger")] <- as.character("2")
sched$Mode[sched$Mode %in% c("Public Transport")] <- as.character("3")
sched$Mode <- as.factor(sched$Mode)

# Attach household info to schedule
hh <- read.csv("hh-coords-Helmond.txt")
library(plyr)
newSched <- join(sched, hh, by = "HHID")

# Attach locs-nl data to schedule
locsnl <- read.csv("locs-nl-cleaned.txt",sep = "\t")
library(plyr)
locsnl$Home <- locsnl$PPC
newSched <- join(newSched, locsnl, by = "Home")

# Get Travel time and Activity duration
newSched$TT <- newSched$BeginTime - newSched$LeaveTime
newSched$ActDurT <- newSched$EndTime - newSched$BeginTime

# Get Prev and Next info
newSched$TTPrev <- NA
levels(newSched$TTPrev)<-c("FirstEpisode",0:5)
newSched$ModePrev <- NA
levels(newSched$ModePrev)<-c("FirstEpisode",0:3)
newSched$ActPrev <- NA
levels(newSched$ActPrev)<-c("FirstEpisode",0:11)
newSched$TTNext <- NA
levels(newSched$TTNext)<-c(0:5,"LastEpisode")
newSched$ModeNext <- NA
levels(newSched$ModeNext)<-c(0:3,"LastEpisode")
newSched$ActNext <- NA
levels(newSched$ActPrev)<-c(0:11,"LastEpisode")
newSched$Evtype <- NA

# newSched$ElapsedCharging <- NA
# levels(newSched$ElapsedCharging) <- c(0:5)
newSched$SOC <- NA
levels(newSched$SOC) <- c(0,1,2)

newSched$HomeCharging <- NA
levels(newSched$HomeCharging) <- c("FastCharging", "NoCharging", "PrivateCharging", "PublicCharging")
# newSched$OutHomeCharging <- NA
# levels(newSched$OutHomeCharging) <- c("FastCharging", "NoCharging", "PrivateCharging", "PublicCharging")
newSched$OutHomeChargingNode <- NA

# Rename column names

names(newSched)[names(newSched) == "pAge"] <- "partnerAge"
names(newSched)[names(newSched) == "Age"] <- "pAge"
names(newSched)[names(newSched) == "NumCars"] <- "Ncar"
names(newSched)[names(newSched) == "Gender"] <- "Gend"
names(newSched)[names(newSched) == "Wstat"] <- "wstat"
names(newSched)[names(newSched) == "BeginTime"] <- "BT"
names(newSched)[names(newSched) == "ActivityType"] <- "Act"

names(newSched)[names(newSched) == "XDAG"] <- "Xdag"
names(newSched)[names(newSched) == "XNONDAG"] <- "Xndag"
names(newSched)[names(newSched) == "XARB"] <- "Xarb"
names(newSched)[names(newSched) == "XPOP"] <- "Xpop"

names(newSched)[names(newSched) == "DDAG"] <- "Ddag"
names(newSched)[names(newSched) == "DNONDAG"] <- "Dndag"
names(newSched)[names(newSched) == "DARB"] <- "Darb"
names(newSched)[names(newSched) == "DPOP"] <- "Dpop"

# names(newSched)[names(newSched) == "schedKrachtstroom_dist"] <- "chargingKrachtstroom_dist"
# names(newSched)[names(newSched) == "schedStopcontact_dist"] <- "chargingStopcontact_dist"
# names(newSched)[names(newSched) == "schedSnellader_dist"] <- "chargingSnellader_dist"

# Classify variables
newSched$Xdag <- as.ordered(cut(newSched$Xdag,breaks = c(0,115,253,307,507,675,Inf),include.lowest = TRUE))
levels(newSched$Xdag) <- c(0,1,2,3,4,5)
newSched$Xndag <- as.ordered(cut(newSched$Xndag,breaks = c(0,395,635,762,938,2525,Inf),include.lowest = TRUE))
levels(newSched$Xndag) <- c(0,1,2,3,4,5)
newSched$Xarb <- as.ordered(cut(newSched$Xarb,breaks = c(0,8785,12995,16120,20199,70314,Inf),include.lowest = TRUE))
levels(newSched$Xarb) <- c(0,1,2,3,4,5)
newSched$Xpop <- as.ordered(cut(newSched$Xpop,breaks = c(0,5050,8845,13217,16833,22884,Inf),include.lowest = TRUE))
levels(newSched$Xpop) <- c(0,1,2,3,4,5)
newSched$Ddag <- as.ordered(cut(newSched$Ddag,breaks = c(0,71,127,165,202,346,Inf),include.lowest = TRUE))
levels(newSched$Ddag) <- c(0,1,2,3,4,5)
newSched$Dndag <- as.ordered(cut(newSched$Dndag,breaks = c(0,92,145,176,258,334,Inf),include.lowest = TRUE))
levels(newSched$Dndag) <- c(0,1,2,3,4,5)
newSched$Darb <- as.ordered(cut(newSched$Darb,breaks = c(0,92,128,201,274,360,Inf),include.lowest = TRUE))
levels(newSched$Darb) <- c(0,1,2,3,4,5)
newSched$Dpop <- as.ordered(cut(newSched$Dpop,breaks = c(0,0.0001,105,126,163,278,Inf),include.lowest = TRUE))
levels(newSched$Dpop) <- c(0,1,2,3,4,5)

# Classify variables (HomeCharging)
newSched$Tdur <- as.ordered(cut(newSched$TT,breaks = c(0,5,8,13,21,37,Inf),include.lowest = TRUE))
levels(newSched$Tdur) <- c(0,1,2,3,4,5)
newSched$BT <- as.ordered(cut(newSched$BT,breaks = c(0,743,925,1020,1081,1212,Inf),include.lowest = TRUE))
levels(newSched$BT) <- c(0,1,2,3,4,5)
newSched$ActDur <- as.ordered(cut(newSched$ActDurT,breaks = c(0,46,101,178,300,411,Inf),include.lowest = TRUE))
levels(newSched$ActDur) <- c(0,1,2,3,4,5)

newSched$chargingKrachtstroom_dist <- as.ordered(cut(newSched$DistKrachtstroom/1000,breaks = c(0,0.32,0.63,0.81,1.03,1.44,Inf),include.lowest = TRUE))
levels(newSched$chargingKrachtstroom_dist) <- c(0,1,2,3,4,5)
newSched$chargingSnellader_dist <- as.ordered(cut(newSched$DistSnellader/1000,breaks = c(0,0.15,0.35,0.70,1.30,1.76,Inf),include.lowest = TRUE))
levels(newSched$chargingSnellader_dist) <- c(0,1,2,3,4,5)
# newSched$chargingStopcontact_dist <- as.ordered(cut(newSched$chargingStopcontact_dist,breaks = c(0,5.06,7.95,12.46,16.91,26.41,Inf),include.lowest = TRUE))
# levels(newSched$chargingStopcontact_dist) <- c(0,1,2,3,4,5)


# Classify variables (OutHomeCharging)
newSched$Tdur <- as.ordered(cut(newSched$TT,breaks = c(0,5,9,14,23,39,Inf),include.lowest = TRUE))
levels(newSched$Tdur) <- c(0,1,2,3,4,5)
newSched$BT <- as.ordered(cut(newSched$BT,breaks = c(0,517,671,801,933,1072,Inf),include.lowest = TRUE))
levels(newSched$BT) <- c(0,1,2,3,4,5)
newSched$ActDur <- as.ordered(cut(newSched$ActDurT,breaks = c(0,17,32,61,121,265,Inf),include.lowest = TRUE))
levels(newSched$ActDur) <- c(0,1,2,3,4,5)

newSched$chargingKrachtstroom_dist <- as.ordered(cut(newSched$DistKrachtstroom/1000,breaks = c(0,0.28,0.52,0.78,1.15,1.94,Inf),include.lowest = TRUE))
levels(newSched$chargingKrachtstroom_dist) <- c(0,1,2,3,4,5)
newSched$chargingSnellader_dist <- as.ordered(cut(newSched$DistSnellader/1000,breaks = c(0,0.12,0.28,0.47,0.75,1.32,Inf),include.lowest = TRUE))
levels(newSched$chargingSnellader_dist) <- c(0,1,2,3,4,5)
# newSched$chargingStopcontact_dist <- as.ordered(cut(newSched$chargingStopcontact_dist,breaks = c(0,3.69,6.53,9.67,12.98,19.87,Inf),include.lowest = TRUE))
# levels(newSched$chargingStopcontact_dist) <- c(0,1,2,3,4,5)

homeCharging <- c()
# outHomeCharging <- c()
outHomeChargingNode <- c()
# outHomeChargingProb <- c()

########## Loop for prediction (HomeCharging & OutHomeCharging) ################
for (i in 1:nrow(newSched)) {
  ## Draw decisions on HomeCharging
  ## only if mode == car or act == home
  if (newSched[i,]$Mode == 1 & newSched[i,]$Act == 1) {
    # For First episode
    if (i > 1) {
      if (newSched[i-1,]$HHID == newSched[i,]$HHID & newSched[i-1,]$MemID == newSched[i,]$MemID) {
        
        ttPrev <- as.integer(cut(newSched[i-1,]$TravelTime,breaks = c(0,5,8,13,21,37,Inf),include.lowest = TRUE))
        newSched[i,]$TTPrev = ttPrev
        newSched[i,]$ModePrev = newSched[i-1,]$Mode
        newSched[i,]$ActPrev = newSched[i-1,]$Act
        
      } else {
        newSched[i,]$TTPrev = "FirstEpisode"
        newSched[i,]$ModePrev = "FirstEpisode"
        newSched[i,]$ActPrev = "FirstEpisode"
      }
    } else {
      newSched[i,]$TTPrev = "FirstEpisode"
      newSched[i,]$ModePrev = "FirstEpisode"
      newSched[i,]$ActPrev = "FirstEpisode"
    }
    # For Last episode
    if (i < nrow(newSched) - 1){
      if (newSched[i+1,]$HHID == newSched[i,]$HHID & newSched[i+1,]$MemID == newSched[i,]$MemID) {
        ttNext <- as.integer(cut(newSched[i+1,]$TT,breaks = c(0,5,9,15,23,40,Inf),include.lowest = TRUE))
        newSched[i,]$TTNext = ttNext
        newSched[i,]$ModeNext = newSched[i+1,]$Mode
        newSched[i,]$ActNext = newSched[i+1,]$Act
      } else {
        newSched[i,]$TTNext = "LastEpisode"
        newSched[i,]$ModeNext = "LastEpisode"
        newSched[i,]$ActNext= "LastEpisode"
      }
    } else {
      newSched[i,]$TTNext = "LastEpisode"
      newSched[i,]$ModeNext = "LastEpisode"
      newSched[i,]$ActNext= "LastEpisode"
    }
    
    newSched[i,]$SOC <- (as.integer(sample(c(0,1,2),size = 1, prob = c(0.2626,0.1768,0.5606))))
    newSched[i,]$Evtype <- (as.integer(sample(c(0,1),size = 1, prob = c(0.5311,0.4689))))
    
    ## Draw a charging decision only for car trips
    j <- 0
    repeat {
      j <- j + 1
      print(paste(i,"-",j))
      try({
        homeCharging <- predict(MEtree.homecharging.result$CHAID.tree[[2]],newdata = newSched[i,])
      }, silent = T)
      newSched[i,]$HomeCharging <- homeCharging
      if (!is.na(newSched[i,]$HomeCharging) || j > 10) {
        break
      }
    }
  
  ## Draw decisions on OutHomeCharging
  ## only if mode == car or act != home
  } else if (newSched[i,]$Mode == 1 & newSched[i,]$Act != 1) {
    if (i > 1) {
      if (newSched[i-1,]$HHID == newSched[i,]$HHID & newSched[i-1,]$MemID == newSched[i,]$MemID) {

        ttPrev <- as.integer(cut(newSched[i-1,]$TT,breaks = c(0,5,8,13,21,37,Inf),include.lowest = TRUE))
        newSched[i,]$TTPrev = ttPrev
        newSched[i,]$ModePrev = newSched[i-1,]$Mode
        newSched[i,]$ActPrev = newSched[i-1,]$Act

      } else {
        newSched[i,]$TTPrev = "FirstEpisode"
        newSched[i,]$ModePrev = "FirstEpisode"
        newSched[i,]$ActPrev = "FirstEpisode"
      }
    } else {
      newSched[i,]$TTPrev = "FirstEpisode"
      newSched[i,]$ModePrev = "FirstEpisode"
      newSched[i,]$ActPrev = "FirstEpisode"
    }
    if (i < nrow(newSched) - 1){
      if (newSched[i+1,]$HHID == newSched[i,]$HHID & newSched[i+1,]$MemID == newSched[i,]$MemID) {
        ttNext <- as.integer(cut(newSched[i+1,]$TT,breaks = c(0,5,9,15,23,40,Inf),include.lowest = TRUE))
        newSched[i,]$TTNext = ttNext
        newSched[i,]$ModeNext = newSched[i+1,]$Mode
        newSched[i,]$ActNext = newSched[i+1,]$Act
      } else {
        newSched[i,]$TTNext = "LastEpisode"
        newSched[i,]$ModeNext = "LastEpisode"
        newSched[i,]$ActNext= "LastEpisode"
      }
    } else {
      newSched[i,]$TTNext = "LastEpisode"
      newSched[i,]$ModeNext = "LastEpisode"
      newSched[i,]$ActNext= "LastEpisode"
    }
    newSched[i,]$SOC <- (as.integer(sample(c(0,1,2),size = 1, prob = c(0.2626,0.1768,0.5606))))
    newSched[i,]$Evtype <- (as.integer(sample(c(0,1),size = 1, prob = c(0.5311,0.4689))))

    ## Draw a charging decision only for car trips
    j <- 0
    repeat {
      j <- j + 1
      print(paste(i,"-",j))
      try({
        # outHomeCharging <- predict(MEtree.outhomecharging.result$CHAID.tree[[2]],newdata = newSched[i,])
        outHomeChargingNode <- predict(MEtree.outhomechargingYN.result$CHAID.tree[[3]],newdata = newSched[i,],type=c("node"))
        # outHomeChargingProb <- predict(MEtree.outhomecharging.result$CHAID.tree[[2]],newdata = newSched[i,],type=c("prob"))
        newSched[i,]$OutHomeChargingNode <- outHomeChargingNode
      }, silent = T)
      # newSched[i,]$OutHomeCharging <- outHomeCharging
      # newSched[i,]$OutHomeChargingNode <- outHomeChargingNode
      # newSched[i,]$OutHomeChargingProb <- outHomeChargingProb
      
      if (!is.na(newSched[i,]$OutHomeChargingNode) || j > 10 || length(newSched[i,]$OutHomeChargingNode) != 0) {
        break
      }
    }
  }
}


newSched<-read.csv("newSched.csv")

m <- MEtree.outhomecharging.result$CHAID.tree[[2]]
tab <- table(fitted(m)[[1]], fitted(m)[[2]])
prop.table(tab, 1)
write.csv(tab,"tab.csv")




