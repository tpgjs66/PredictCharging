

library(devtools)
install_github("tpgjs66/pmmlParty")
install.packages("gtools")
install.packages("MCMCglmm")
install.packages("CHAID", repos="http://R-Forge.R-project.org")

library(party)
library(CHAID)
library(MCMCglmm)
library(gtools)
library(pmmlParty)
library(XML)
library(sp)
library(rgdal)

###### Load shape file ######
pc4sp = rgdal::readOGR("~/ActivityPriority/GIS/ppcs_single_cs.shp", layer = "ppcs_single_cs")
pc4sp$krachtstro <- as.numeric(pc4sp$krachtstro)
pc4sp$snellader_ <- as.numeric(pc4sp$snellader_)
pc4sp$krachtstro[is.na(pc4sp$krachtstro)] <- 0
pc4sp$snellader_[is.na(pc4sp$snellader_)] <- 0

# pc6sp = rgdal::readOGR("~/ActivityPriority/GIS/ppcs_single.shp", layer = "ppcs_single")


###### DATA PREPARATION ######

setwd("~/ActivityPriority/dynamicDT")

data = (read.delim("aicharging2.csv",
                   header=TRUE,
                   sep=",",
                   stringsAsFactor = TRUE
)
)

## Convert coordinates to numeric
data$destination.latitude <- as.numeric(as.character(data$destination.latitude))
data$destination.longitude <- as.numeric(as.character(data$destination.longitude))

## Subsetting only charging incidents
## HomeCharging
data <- data[which(data$HomeCharging != "Missing"),]
data <- droplevels(data)

## OutHomeCharging
data <- data[which(data$OutHomeCharging != "Missing"),]
data <- droplevels(data)

## Count charging stations in activity location (PC4)
coords <- cbind(data$destination.longitude,data$destination.latitude)
coords <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
pc4 <- over(coords,pc4sp)

data$KrachtstroomN <- pc4$krachtstro
data$KrachtstroomN[is.na(data$KrachtstroomN)] <- 0
data$KrachtstroomN <- as.ordered(quantcut(data$KrachtstroomN,seq(0,1,by=1/6),dig.lab=8))
levels(data$KrachtstroomN) <- c(0,1,2,3,4,5)

data$SnelladerN <- pc4$snellader_
data$SnelladerN[is.na(data$SnelladerN)] <- 0
data$SnelladerN <- as.ordered(quantcut(data$SnelladerN,seq(0,1,by=1/6),dig.lab=8))
levels(data$SnelladerN) <- c(0,1,2,3,4,5)

## Convert data type into categorical variable
## Note NA should not exist in dataset.
data$HHID<-as.factor(data$HHID)

data$Urb <- as.ordered(data$Urb)
data$Day <- as.factor(data$Day)
data$pAge<-as.ordered(data$pAge)
data$Ncar<- as.ordered(data$Ncar)
data$Gend<- as.factor(data$Gend)
data$Driver<- as.factor(data$Driver)
data$wstat<- as.factor(data$wstat)

data$Tdur<-as.ordered(quantcut(data$Tdur, seq(0,1,by=1/6),dig.lab=8))
levels(data$Tdur) <- c(0,1,2,3,4,5)
data$Dist<-as.ordered(quantcut(data$Dist, seq(0,1,by=1/6),dig.lab=8))
levels(data$Dist) <- c(0,1,2,3,4,5)

# Mode
data$Mode <- as.character(data$Mode)
data$Mode[data$Mode %in% c("Lopend","Fiets")] <- as.character("0")
data$Mode[data$Mode %in% c("Auto")] <- as.character("1")
data$Mode[data$Mode %in% c("Taxi","Onbekend","Anders")] <- as.character("2")
data$Mode[data$Mode %in% c("Bus","Trein","Tram","Metro")] <- as.character("3")
data$Mode <- as.factor(data$Mode)

# Activity type
data$Act <- as.character(data$Act)
data$Act[data$Act %in% c("03 Naar huis")] <- as.character("0")
data$Act[data$Act %in% c("02 Naar werk of school") & data$ActDur < 60] <- as.character("2")
data$Act[data$Act %in% c("02 Naar werk of school")] <- as.character("1")
data$Act[data$Act %in% c("06 Ophalen of wegbrengen")] <- as.character("3")
data$Act[data$Act %in% c("04 Dagelijkse boodschappen")] <- as.character("4")
data$Act[data$Act %in% c("05 Winkelen")] <- as.character("5")
data$Act[data$Act %in% c("07 Diensten of prive zaken")] <- as.character("6")
data$Act[data$Act %in% c("08 Sociale activiteiten")] <- as.character("7")
data$Act[data$Act %in% c("09 Vrije tijd")] <- as.character("8")
data$Act[data$Act %in% c("10 Wachten")] <- as.character("9")
data$Act[data$Act %in% c("11 Andere activiteiten")] <- as.character("10")
data$Act[data$Act %in% c("01 Alleen laden")] <- as.character("11")
prob <- c()
for (i in unique(data$Act[data$Act != "12 Onbekend"])) {
  prob[[i]] <- table(data$Act)[i]
}
data$Act[data$Act %in% c("12 Onbekend")] <- sample(unique(data$Act[data$Act != "12 Onbekend"]),
                                                   size = length(data$Act[data$Act == "12 Onbekend"]),
                                                   replace = TRUE,
                                                   prob = prob)

data$Act <- as.factor(data$Act)

# ModePrev
data$ModePrev <- as.character(data$ModePrev)
data$ModePrev[data$ModePrev %in% c("Lopend","Fiets")] <- as.character("0")
data$ModePrev[data$ModePrev %in% c("Auto")] <- as.character("1")
data$ModePrev[data$ModePrev %in% c("Taxi","Onbekend","Anders")] <- as.character("2")
data$ModePrev[data$ModePrev %in% c("Bus","Trein","Tram","Metro")] <- as.character("3")
data$ModePrev <- as.factor(data$ModePrev)

# ActPrev
data$ActPrev <- as.character(data$ActPrev)
data$ActPrev[data$ActPrev %in% c("03 Naar huis")] <- as.character("0")
data$ActPrev[data$ActPrev %in% c("02 Naar werk of school") & data$ActDur < 60] <- as.character("2")
data$ActPrev[data$ActPrev %in% c("02 Naar werk of school")] <- as.character("1")
data$ActPrev[data$ActPrev %in% c("06 Ophalen of wegbrengen")] <- as.character("3")
data$ActPrev[data$ActPrev %in% c("04 Dagelijkse boodschappen")] <- as.character("4")
data$ActPrev[data$ActPrev %in% c("05 Winkelen")] <- as.character("5")
data$ActPrev[data$ActPrev %in% c("07 Diensten of prive zaken")] <- as.character("6")
data$ActPrev[data$ActPrev %in% c("08 Sociale activiteiten")] <- as.character("7")
data$ActPrev[data$ActPrev %in% c("09 Vrije tijd")] <- as.character("8")
data$ActPrev[data$ActPrev %in% c("10 Wachten")] <- as.character("9")
data$ActPrev[data$ActPrev %in% c("11 Andere activiteiten")] <- as.character("10")
data$ActPrev[data$ActPrev %in% c("01 Alleen laden")] <- as.character("11")
prob <- c()
for (i in unique(data$ActPrev[data$ActPrev != "12 Onbekend"])) {
  prob[[i]] <- table(data$ActPrev)[i]
}
data$ActPrev[data$ActPrev %in% c("12 Onbekend")] <- sample(unique(data$ActPrev[data$ActPrev != "12 Onbekend"]),
                                                   size = length(data$ActPrev[data$ActPrev == "12 Onbekend"]),
                                                   replace = TRUE,
                                                   prob = prob)
data$ActPrev<-as.factor(data$ActPrev)

# TTPrev
data$TTPrev<-as.ordered(quantcut(as.numeric(as.character(data$TTPrev)),seq(0,1,by=1/6),
                                 dig.lab=8))
data$TTPrev<-addNA(data$TTPrev)
levels(data$TTPrev) <- c("FirstEpisode",0,1,2,3,4,5)

# ModeNext
data$ModeNext <- as.character(data$ModeNext)
data$ModeNext[data$ModeNext %in% c("Lopend","Fiets")] <- as.character("0")
data$ModeNext[data$ModeNext %in% c("Auto")] <- as.character("1")
data$ModeNext[data$ModeNext %in% c("Taxi","Onbekend","Anders")] <- as.character("2")
data$ModeNext[data$ModeNext %in% c("Bus","Trein","Tram","Metro")] <- as.character("3")
data$ModeNext <- as.factor(data$ModeNext)

# ActNext
data$ActNext <- as.character(data$ActNext)
data$ActNext[data$ActNext %in% c("03 Naar huis")] <- as.character("0")
data$ActNext[data$ActNext %in% c("02 Naar werk of school") & data$ActDur < 60] <- as.character("2")
data$ActNext[data$ActNext %in% c("02 Naar werk of school")] <- as.character("1")
data$ActNext[data$ActNext %in% c("06 Ophalen of wegbrengen")] <- as.character("3")
data$ActNext[data$ActNext %in% c("04 Dagelijkse boodschappen")] <- as.character("4")
data$ActNext[data$ActNext %in% c("05 Winkelen")] <- as.character("5")
data$ActNext[data$ActNext %in% c("07 Diensten of prive zaken")] <- as.character("6")
data$ActNext[data$ActNext %in% c("08 Sociale activiteiten")] <- as.character("7")
data$ActNext[data$ActNext %in% c("09 Vrije tijd")] <- as.character("8")
data$ActNext[data$ActNext %in% c("10 Wachten")] <- as.character("9")
data$ActNext[data$ActNext %in% c("11 Andere activiteiten")] <- as.character("10")
data$ActNext[data$ActNext %in% c("01 Alleen laden")] <- as.character("11")
prob <- c()
for (i in unique(data$ActNext[data$ActNext != "12 Onbekend"])) {
  prob[[i]] <- table(data$ActNext)[i]
}
data$ActNext[data$ActNext %in% c("12 Onbekend")] <- sample(unique(data$ActNext[data$ActNext != "12 Onbekend"]),
                                                   size = length(data$ActNext[data$ActNext == "12 Onbekend"]),
                                                   replace = TRUE,
                                                   prob = prob)
data$ActNext<-as.factor(data$ActNext)

# TTNext
data$TTNext<-as.ordered(quantcut(as.numeric(as.character(data$TTNext)), seq(0,1,by=1/6),
                                 dig.lab=8))
data$TTNext<-addNA(data$TTNext)
levels(data$TTNext) <- c("LastEpisode",0,1,2,3,4,5)

#Categorize continous variable using the 5 quintile values
data$BT<-as.ordered(quantcut(data$BT%%1440, seq(0,1,by=1/6),dig.lab=8))
levels(data$BT) <- c(0,1,2,3,4,5)
data$ActDur<-as.ordered(quantcut(data$ActDur, seq(0,1,by=1/6),dig.lab=8))
levels(data$ActDur) <- c(0,1,2,3,4,5)

data$Evtype<- as.factor(data$Evtype)
data$ElapsedCharging<- as.ordered(quantcut(data$ElapsedCharging, seq(0,1,by=1/6)
                                           ,dig.lab=8))
levels(data$ElapsedCharging) <- c(0,1,2,3,4,5)
data$SOC <-as.factor(data$SOC)
data$SOC[data$SOC=="-1"] <- NA
data$SOC <- droplevels(data$SOC)
data$SOC[is.na(data$SOC)] <- 2

data$Xdag<-as.ordered(data$Xdag)
data$Xndag<-as.ordered(data$Xndag)
data$Xarb<-as.ordered(data$Xarb)
data$Xpop<-as.ordered(data$Xpop)
data$Ddag<-as.ordered(data$Ddag)
data$Dndag<-as.ordered(data$Dndag)
data$Darb<-as.ordered(data$Darb)
data$Dpop<-as.ordered(data$Dpop)

data$origin.latitude<-as.numeric(as.character(data$origin.latitude))
data$origin.longitude<-as.numeric(as.character(data$origin.longitude))
data$destination.latitude<-as.numeric(as.character(data$destination.latitude))
data$destination.longitude<-as.numeric(as.character(data$destination.longitude))

data$chargingKrachtstroom_X<-
  as.numeric(as.character(data$chargingKrachtstroom_X))
data$chargingKrachtstroom_Y<-
  as.numeric(as.character(data$chargingKrachtstroom_Y))
data$chargingSnellader_X<-as.numeric(as.character(data$chargingSnellader_X))
data$chargingSnellader_Y<-as.numeric(as.character(data$chargingSnellader_Y))
data$chargingStopcontact_X<-as.numeric(as.character(data$chargingStopcontact_X))
data$chargingStopcontact_Y<-as.numeric(as.character(data$chargingStopcontact_Y))

# Give unique ID
data$SchedID<-do.call(paste, c(data[c("HHID", "MemID","EpID")], sep = "-"))

# Merging SemiPublicCharging with PublicCharging
data$HomeCharging <- as.factor(data$HomeCharging)
data$HomeCharging[data$HomeCharging=="SemiPublicCharging"] <- "PublicCharging"
data$HomeCharging <- droplevels(data$HomeCharging)

data$OutHomeCharging <- as.factor(data$OutHomeCharging)
data$OutHomeCharging[data$OutHomeCharging=="SemiPublicCharging"] <- "PublicCharging"
data$OutHomeCharging <- droplevels(data$OutHomeCharging)

# HomeCharging 
homecharging <- data

# OutHomeCharging
outhomecharging <- data

# ## Subsetting only charging incidents
# homecharging <- data[which(data$HomeCharging != "Missing"),]
# homecharging <- droplevels(homecharging)
# 
# outhomecharging <- data[which(data$OutHomeCharging != "Missing"),]
# outhomecharging <- droplevels(outhomecharging)

## Function for route information using routino
routing <- function(data, Lat2, Lon2) {
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
  routeresults <- c()
  for (i in 1:nrow(data)) {
    print(i)
    # Coordinates of charging station by type
    lat2 <- data[Lat2][i,]
    lon2 <- data[Lon2][i,]
    
    # Skip the first episode
    if (data$EpID[i] == 0){
      routeresults[i] <- list(NULL)
      next
    }
    # Coordinates of Activity episode
    lat1 <- data$destination.latitude[i]
    lon1 <- data$destination.longitude[i]
    
    # Assign transport mode to route (Always walk)
    tmode <- "foot"
    if (cstype == "chargingSnellader") {
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
  for (i in 1:nrow(data)) {
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

# Execute the routing function (homecharging)
homecharging.Krachtstroom <- routing(homecharging,
                                     Lat2="chargingKrachtstroom_Y",
                                     Lon2="chargingKrachtstroom_X")
homecharging$chargingKrachtstroom_dist <- homecharging.Krachtstroom[[1]]$chargingKrachtstroom_dist
homecharging.Snellader <- routing(homecharging,
                                  Lat2="chargingSnellader_Y",
                                  Lon2="chargingSnellader_X")
homecharging$chargingSnellader_dist <- homecharging.Snellader[[1]]$chargingSnellader_dist
homecharging.Stopcontact <- routing(homecharging,
                                    Lat2="chargingStopcontact_Y",
                                    Lon2="chargingStopcontact_X")
homecharging$chargingStopcontact_dist <- homecharging.Stopcontact[[1]]$chargingStopcontact_dist

# Convert route info to categorical vars.
homecharging$chargingKrachtstroom_dur <- as.ordered(quantcut(homecharging$chargingKrachtstroom_dur,seq(0,1,by=1/6),dig.lab=8))
levels(homecharging$chargingKrachtstroom_dur) <- c(0,1,2,3,4,5)
homecharging$chargingKrachtstroom_dist <- as.ordered(quantcut(homecharging$chargingKrachtstroom_dist,seq(0,1,by=1/6),dig.lab=8))
levels(homecharging$chargingKrachtstroom_dist) <- c(0,1,2,3,4,5)
homecharging$chargingSnellader_dur <- as.ordered(quantcut(homecharging$chargingSnellader_dur,seq(0,1,by=1/6),dig.lab=8))
levels(homecharging$chargingSnellader_dur) <- c(0,1,2,3,4,5)
homecharging$chargingSnellader_dist <- as.ordered(quantcut(homecharging$chargingSnellader_dist,seq(0,1,by=1/6),dig.lab=8))
levels(homecharging$chargingSnellader_dist) <- c(0,1,2,3,4,5)
homecharging$chargingStopcontact_dur <- as.ordered(quantcut(homecharging$chargingStopcontact_dur,seq(0,1,by=1/6),dig.lab=8))
levels(homecharging$chargingStopcontact_dur) <- c(0,1,2,3,4,5)
homecharging$chargingStopcontact_dist <- as.ordered(quantcut(homecharging$chargingStopcontact_dist,seq(0,1,by=1/6),dig.lab=8))
levels(homecharging$chargingStopcontact_dist) <- c(0,1,2,3,4,5)

# Write out homecharging data
write.csv(homecharging,"~/ActivityPriority/dynamicDT/homecharging.csv")

# Execute the routing function (outhomecharging)
outhomecharging.Krachtstroom <- routing(outhomecharging,
                                        Lat2="chargingKrachtstroom_Y",
                                        Lon2="chargingKrachtstroom_X")
outhomecharging$chargingKrachtstroom_dist <- outhomecharging.Krachtstroom[[1]]$chargingKrachtstroom_dist
outhomecharging.Snellader <- routing(outhomecharging,
                                     Lat2="chargingSnellader_Y",
                                     Lon2="chargingSnellader_X")
outhomecharging$chargingSnellader_dist <- outhomecharging.Snellader[[1]]$chargingSnellader_dist
outhomecharging.Stopcontact <- routing(outhomecharging,
                                       Lat2="chargingStopcontact_Y",
                                       Lon2="chargingStopcontact_X")
outhomecharging$chargingStopcontact_dist <- outhomecharging.Stopcontact[[1]]$chargingStopcontact_dist

# Convert route info to categorical vars.
outhomecharging$chargingKrachtstroom_dur <- as.ordered(quantcut(outhomecharging$chargingKrachtstroom_dur,seq(0,1,by=1/6),dig.lab=8))
levels(outhomecharging$chargingKrachtstroom_dur) <- c(0,1,2,3,4,5)
outhomecharging$chargingKrachtstroom_dist <- as.ordered(quantcut(outhomecharging$chargingKrachtstroom_dist,seq(0,1,by=1/6),dig.lab=8))
levels(outhomecharging$chargingKrachtstroom_dist) <- c(0,1,2,3,4,5)
outhomecharging$chargingSnellader_dur <- as.ordered(quantcut(outhomecharging$chargingSnellader_dur,seq(0,1,by=1/6),dig.lab=8))
levels(outhomecharging$chargingSnellader_dur) <- c(0,1,2,3,4,5)
outhomecharging$chargingSnellader_dist <- as.ordered(quantcut(outhomecharging$chargingSnellader_dist,seq(0,1,by=1/6),dig.lab=8))
levels(outhomecharging$chargingSnellader_dist) <- c(0,1,2,3,4,5)
outhomecharging$chargingStopcontact_dur <- as.ordered(quantcut(outhomecharging$chargingStopcontact_dur,seq(0,1,by=1/6),dig.lab=8))
levels(outhomecharging$chargingStopcontact_dur) <- c(0,1,2,3,4,5)
outhomecharging$chargingStopcontact_dist <- as.ordered(quantcut(outhomecharging$chargingStopcontact_dist,seq(0,1,by=1/6),dig.lab=8))
levels(outhomecharging$chargingStopcontact_dist) <- c(0,1,2,3,4,5)

# Write out outhomecharging data
write.csv(outhomecharging,"~/ActivityPriority/dynamicDT/outhomecharging.csv")

# Leaflet visualization
m <- leaflet()
m <- addTiles(map=m)
m <- addPolylines(map=m,data=outhomecharging.Krachtstroom[[2]])
m

### CHAID formula ###
formula.homecharging <- (HomeCharging~Urb+Day+pAge+Ncar+Gend+Driver+wstat+
                           Tdur+Mode+Act+
                           ModePrev+ActPrev+TTPrev+ModeNext+ActNext+TTNext+BT+
                           ActDur+Evtype+ElapsedCharging+SOC+Xdag+Xndag+Xarb+
                           Xpop+Ddag+Dndag+Darb+Dpop+chargingKrachtstroom_dist+
                           chargingSnellader_dist+KrachtstroomN+SnelladerN)

formula.outhomecharging <- (OutHomeCharging~Urb+Day+pAge+Ncar+Gend+Driver+wstat+
                              Tdur+Mode+Act+
                              ModePrev+ActPrev+TTPrev+ModeNext+ActNext+TTNext+
                              BT+ActDur+Evtype+ElapsedCharging+SOC+Xdag+
                              Xndag+Xarb+Xpop+Ddag+Dndag+Darb+Dpop+
                              chargingKrachtstroom_dist+
                              chargingSnellader_dist+KrachtstroomN+SnelladerN)

################################################################################
###                          Define MEtree function                          ###
################################################################################

MEtree5<-function(data,formula,random) {
  ErrorTolerance=10
  MaxIterations=5
  #parse formula
  Predictors<-paste(attr(terms(formula),"term.labels"),collapse="+")
  TargetName<-formula[[2]]
  Target<-data[,toString(TargetName)]
  #set up variables for loop
  ContinueCondition<-TRUE
  iterations<-0
  #set up the initial target
  OriginalTarget<-(Target)
  oldDIC<- Inf

  # Make a new data frame to include all the new variables
  newdata <- data
  newdata[,"p.1"]<-0
  newdata[,"p.2"]<-0
  newdata[,"p.3"]<-0
  newdata[,"p.4"]<-0
  newdata[,"p.5"]<-0

  m.list<-list()
  tree.list<-list()
  while(ContinueCondition){

    # Count iterations
    iterations <- iterations+1
    print(paste("############### Main Iteration ",iterations,"###############"))

    # Target response will be updated from the previous result.
    if (iterations<2){
      newdata[,"OriginalTarget"] <- as.factor(OriginalTarget)
    }else {
      newdata[,"OriginalTarget"] <- as.factor(MCMCTarget)
    }

    # Build CHAID tree
    ctrl <- chaid_control(alpha2=0.05,alpha4=0.05,
                          minsplit = 2*floor(nrow(data)/100),
                          minbucket=floor(nrow(data)/100), minprob=1)
    tree <- chaid(formula(paste(c("OriginalTarget", Predictors),collapse = "~"))
                  ,data = newdata, control = ctrl)

    tree.list[[iterations]]<-tree

    # Get terminal node
    newdata[,"nodeInd"] <- 0
    newdata["nodeInd"] <-as.factor(predict(tree,newdata=newdata,type="node"))

    # Get variables (alternative-specific) that identify the node for
    # each observation
    newdata["p.1"]<-list(predict(tree,newdata=newdata,type="prob")[,1])
    newdata["p.2"]<-list(predict(tree,newdata=newdata,type="prob")[,2])
    newdata["p.3"]<-list(predict(tree,newdata=newdata,type="prob")[,3])
    newdata["p.4"]<-list(predict(tree,newdata=newdata,type="prob")[,4])
    newdata["p.5"]<-list(predict(tree,newdata=newdata,type="prob")[,5])

    CHAIDTarget<-c()
    # Update adjusted target based on CHAID predicted probs.
    repeat{
      for(k in 1:length(OriginalTarget)){
        t<-levels(OriginalTarget)
        # Draw a decision based on probs
        CHAIDTarget[k]<-sample(t,1,replace=FALSE,
                               prob=newdata[k,c("p.1","p.2","p.3","p.4","p.5")])
      }
      if ((length(table(CHAIDTarget))==5)){break}
    }

    newdata[,"CHAIDTarget"] <- as.factor(CHAIDTarget)

    # Fit MCMCglmm
    k <- length(levels(Target))
    I <- diag(k-1)
    J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))

    prior <- list(
      G = list(G1 = list(V = diag(k-1), n = k-1)),
      R = list(fix=1,V= (1/k) * (I + J), n = k-1))

     m <- MCMCglmm(fixed = OriginalTarget ~ -1 + trait +
                    +trait:(nodeInd+CHAIDTarget),

                  random = ~ idh(trait):HHID,# ~ idh(trait-1+nodeInd):HHID ??
                  rcov = ~idh(trait):units,

                  prior = prior, # Add fix=1 if you want fix R-structure
                  burnin =1000,
                  nitt = 21000,
                  thin = 10,
                  # This option saves the posterior distribution of
                  # random effects in the Solution mcmc object:
                  pr = TRUE,
                  #pl = TRUE,
                  family = "categorical",
                  #saveX = TRUE,
                  #saveZ = TRUE,
                  #saveXL = TRUE,
                  data = newdata,
                  verbose = T
                  #slice = T
                  #singular.ok = T
    )

    m.list[[iterations]]<-m

    #p <- predict(m,type="terms",interval="prediction")[,1]
    p <- (predict(m,type="terms",interval="none",posterior="all"))
    #p <- (predict(m,type="terms",interval="none",posterior="distribution"))
    #p <- (predict(m,type="terms",interval="none",posterior="mean"))
    #p <- (predict(m,type="terms",interval="none",posterior="mode"))

    # Predicted probability with marginalizing the random effect
    #p <- predict(m,type="terms", interval="none",posterior="mean",marginal=NULL)
    #p <- predict(m,type="terms", interval="none",
    #            posterior="mean",marginal=m$Random$formula)

    pred<-c()

    pred$b<-p[1:nrow(newdata)]
    pred$c<-p[(nrow(newdata)+1):(2*nrow(newdata))]
    pred$d<-p[(2*nrow(newdata)+1):(3*nrow(newdata))]
    pred$e<-p[(3*nrow(newdata)+1):(4*nrow(newdata))]

    pred<-as.data.frame(pred)

    pred$pa<-1/(1+exp(pred$b)+exp(pred$c)+exp(pred$d)+exp(pred$e))
    pred$pb<-exp(pred$b)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d)+exp(pred$e))
    pred$pc<-exp(pred$c)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d)+exp(pred$e))
    pred$pd<-exp(pred$d)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d)+exp(pred$e))
    pred$pe<-exp(pred$e)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d)+exp(pred$e))

    pred<-pred[5:9]

    # Get the DIC to check on convergence
    if(!(is.null(m))){
      newDIC <- m$DIC
      ContinueCondition <- (abs(oldDIC-newDIC)>ErrorTolerance &
                              iterations < MaxIterations)
      oldDIC <- newDIC
      print(paste("###### DIC : ", m$DIC, " ######"))

      # Update prob.
      newdata["p.1"]<-pred[,1]
      newdata["p.2"]<-pred[,2]
      newdata["p.3"]<-pred[,3]
      newdata["p.4"]<-pred[,4]
      newdata["p.5"]<-pred[,5]

      # # Update adjusted target based on logit prob.
      # for(k in 1:length(AdjustedTarget)){
      #   AdjustedTarget[k]<-sum(cumsum(mlogitfit$probabilities[k,])<runif(1))+1
      #
      # }
      # newdata[,"AdjustedTarget"] <- AdjustedTarget


      # Update adjusted target based on MCMCglmm predicted probs.
      MCMCTarget<-c()

      repeat{
        for(k in 1:length(OriginalTarget)){
          t<-levels(OriginalTarget)
          MCMCTarget[k]<-sample(t,1,replace=FALSE,
                                prob=newdata[k,c("p.1","p.2","p.3","p.4","p.5")])
        }
        if ((length(table(MCMCTarget))==5)){break}
      }
      newdata[,"MCMCTarget"] <- as.factor(MCMCTarget)

    }
    else{ ContinueCondition<-FALSE }
  }

  #return final model fits and convergence info.
  return(list(
    CHAID.tree=tree.list,
    MCMCglmm.fit=m.list,
    Conv.info=newDIC-oldDIC,
    n.iter=iterations
  ))
}

MEtree4<-function(data,formula,random) {
  ErrorTolerance=50
  MaxIterations=100
  #parse formula
  Predictors<-paste(attr(terms(formula),"term.labels"),collapse="+")
  TargetName<-formula[[2]]
  Target<-data[,toString(TargetName)]
  #set up variables for loop
  ContinueCondition<-TRUE
  iterations<-0
  #set up the initial target
  OriginalTarget<-(Target)
  oldDIC<- Inf
  
  # Make a new data frame to include all the new variables
  newdata <- data
  newdata[,"p.1"]<-0
  newdata[,"p.2"]<-0
  newdata[,"p.3"]<-0
  newdata[,"p.4"]<-0
  
  m.list<-list()
  tree.list<-list()
  while(ContinueCondition){
    
    # Count iterations
    iterations <- iterations+1
    print(paste("############### Main Iteration ",iterations,"###############"))
    
    # Target response will be updated from the previous result.
    if (iterations<2){
      newdata[,"OriginalTarget"] <- as.factor(OriginalTarget)
    }else {
      newdata[,"OriginalTarget"] <- as.factor(MCMCTarget)
    }
    
    # Build CHAID tree
    ctrl <- chaid_control(alpha2=0.05,alpha4=0.05,
                          minsplit = 2*floor(nrow(data)/100),
                          minbucket=floor(nrow(data)/100), minprob=1)
    tree <- chaid(formula(paste(c("OriginalTarget", Predictors),collapse = "~"))
                  ,data = newdata, control = ctrl)
    
    tree.list[[iterations]]<-tree
    
    # Get terminal node
    newdata[,"nodeInd"] <- 0
    newdata["nodeInd"] <-as.factor(predict(tree,newdata=newdata,type="node"))
    
    # Get variables (alternative-specific) that identify the node for
    # each observation
    newdata["p.1"]<-list(predict(tree,newdata=newdata,type="prob")[,1])
    newdata["p.2"]<-list(predict(tree,newdata=newdata,type="prob")[,2])
    newdata["p.3"]<-list(predict(tree,newdata=newdata,type="prob")[,3])
    newdata["p.4"]<-list(predict(tree,newdata=newdata,type="prob")[,4])
    
    CHAIDTarget<-c()
    # Update adjusted target based on CHAID predicted probs.
    repeat{
      for(k in 1:length(OriginalTarget)){
        t<-levels(OriginalTarget)
        # Draw a decision based on probs
        CHAIDTarget[k]<-sample(t,1,replace=FALSE,
                               prob=newdata[k,c("p.1","p.2","p.3","p.4")])
      }
      if ((length(table(CHAIDTarget))==4)){break}
    }
    
    newdata[,"CHAIDTarget"] <- as.factor(CHAIDTarget)
    
    # Fit MCMCglmm
    k <- length(levels(Target))
    I <- diag(k-1)
    J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))
    
    prior <- list(
      G = list(G1 = list(V = diag(k-1), n = k-1)),
      R = list(fix=1,V= (1/k) * (I + J), n = k-1))
    
    m <- MCMCglmm(fixed = OriginalTarget ~ -1 + trait +
                    +trait:(nodeInd+CHAIDTarget),
                  
                  random = ~ idh(trait):HHID,# ~ idh(trait-1+nodeInd):HHID ??
                  rcov = ~idh(trait):units,
                  
                  prior = prior, # Add fix=1 if you want fix R-structure
                  burnin =1000,
                  nitt = 21000,
                  thin = 10,
                  # This option saves the posterior distribution of
                  # random effects in the Solution mcmc object:
                  pr = TRUE,
                  #pl = TRUE,
                  family = "categorical",
                  #saveX = TRUE,
                  #saveZ = TRUE,
                  #saveXL = TRUE,
                  data = newdata,
                  verbose = T
                  #slice = T
                  #singular.ok = T
    )
    
    m.list[[iterations]]<-m
    
    #p <- predict(m,type="terms",interval="prediction")[,1]
    p <- (predict(m,type="terms",interval="none",posterior="all"))
    #p <- (predict(m,type="terms",interval="none",posterior="distribution"))
    #p <- (predict(m,type="terms",interval="none",posterior="mean"))
    #p <- (predict(m,type="terms",interval="none",posterior="mode"))
    
    # Predicted probability with marginalizing the random effect
    #p <- predict(m,type="terms", interval="none",posterior="mean",marginal=NULL)
    #p <- predict(m,type="terms", interval="none",
    #            posterior="mean",marginal=m$Random$formula)
    
    pred<-c()
    
    pred$b<-p[1:nrow(newdata)]
    pred$c<-p[(nrow(newdata)+1):(2*nrow(newdata))]
    pred$d<-p[(2*nrow(newdata)+1):(3*nrow(newdata))]
    
    pred<-as.data.frame(pred)
    
    pred$pa<-1/(1+exp(pred$b)+exp(pred$c)+exp(pred$d))
    pred$pb<-exp(pred$b)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d))
    pred$pc<-exp(pred$c)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d))
    pred$pd<-exp(pred$d)/(1+exp(pred$b)+exp(pred$c)+exp(pred$d))
    
    pred<-pred[4:7]
    
    # Get the DIC to check on convergence
    if(!(is.null(m))){
      newDIC <- m$DIC
      ContinueCondition <- (abs(oldDIC-newDIC)>ErrorTolerance &
                              iterations < MaxIterations)
      oldDIC <- newDIC
      print(paste("###### DIC : ", m$DIC, " ######"))
      
      # Update prob.
      newdata["p.1"]<-pred[,1]
      newdata["p.2"]<-pred[,2]
      newdata["p.3"]<-pred[,3]
      newdata["p.4"]<-pred[,4]
      
      # Update adjusted target based on MCMCglmm predicted probs.
      MCMCTarget<-c()
      
      repeat{
        for(k in 1:length(OriginalTarget)){
          t<-levels(OriginalTarget)
          MCMCTarget[k]<-sample(t,1,replace=FALSE,
                                prob=newdata[k,c("p.1","p.2","p.3","p.4")])
        }
        if ((length(table(MCMCTarget))==4)){break}
      }
      newdata[,"MCMCTarget"] <- as.factor(MCMCTarget)
      
    }
    else{ ContinueCondition<-FALSE }
  }
  
  #return final model fits and convergence info.
  return(list(
    CHAID.tree=tree.list,
    MCMCglmm.fit=m.list,
    Conv.info=newDIC-oldDIC,
    n.iter=iterations
  ))
}

MEtree2<-function(data,formula,random) {
  ErrorTolerance=50
  MaxIterations=100
  #parse formula
  Predictors<-paste(attr(terms(formula),"term.labels"),collapse="+")
  TargetName<-formula[[2]]
  Target<-data[,toString(TargetName)]
  #set up variables for loop
  ContinueCondition<-TRUE
  iterations<-0
  #set up the initial target
  OriginalTarget<-(Target)
  oldDIC<- Inf
  
  # Make a new data frame to include all the new variables
  newdata <- data
  newdata[,"p.1"]<-0
  newdata[,"p.2"]<-0
  
  m.list<-list()
  tree.list<-list()
  while(ContinueCondition){
    
    # Count iterations
    iterations <- iterations+1
    print(paste("############### Main Iteration ",iterations,"###############"))
    
    # Target response will be updated from the previous result.
    if (iterations<2){
      newdata[,"OriginalTarget"] <- as.factor(OriginalTarget)
    }else {
      newdata[,"OriginalTarget"] <- as.factor(MCMCTarget)
    }
    
    # Build CHAID tree
    ctrl <- chaid_control(alpha2=0.05,alpha4=0.05,
                          minsplit = 2*floor(nrow(data)/200),
                          minbucket=floor(nrow(data)/200), minprob=1)
    tree <- chaid(formula(paste(c("OriginalTarget", Predictors),collapse = "~"))
                  ,data = newdata, control = ctrl)
    
    tree.list[[iterations]]<-tree
    
    # Get terminal node
    newdata[,"nodeInd"] <- 0
    newdata["nodeInd"] <-as.factor(predict(tree,newdata=newdata,type="node"))
    
    # Get variables (alternative-specific) that identify the node for
    # each observation
    newdata["p.1"]<-list(predict(tree,newdata=newdata,type="prob")[,1])
    newdata["p.2"]<-list(predict(tree,newdata=newdata,type="prob")[,2])
    
    CHAIDTarget<-c()
    # Update adjusted target based on CHAID predicted probs.
    repeat{
      for(k in 1:length(OriginalTarget)){
        t<-levels(OriginalTarget)
        # Draw a decision based on probs
        CHAIDTarget[k]<-sample(t,1,replace=FALSE,
                               prob=newdata[k,c("p.1","p.2")])
      }
      if ((length(table(CHAIDTarget))==2)){break}
    }
    
    newdata[,"CHAIDTarget"] <- as.factor(CHAIDTarget)
    
    # Fit MCMCglmm
    k <- length(levels(Target))
    I <- diag(k-1)
    J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))
    
    prior <- list(
      G = list(G1 = list(V = diag(k-1), n = k-1)),
      R = list(fix=1,V= (1/k) * (I + J), n = k-1))
    
    m <- MCMCglmm(fixed = OriginalTarget ~ (nodeInd+CHAIDTarget),
                  
                  random = ~ HHID,# ~ idh(trait-1+nodeInd):HHID ??
                  rcov = ~ units,
                  
                  prior = prior, # Add fix=1 if you want fix R-structure
                  burnin =1000,
                  nitt = 21000,
                  thin = 10,
                  # This option saves the posterior distribution of
                  # random effects in the Solution mcmc object:
                  pr = TRUE,
                  #pl = TRUE,
                  family = "categorical",
                  #saveX = TRUE,
                  #saveZ = TRUE,
                  #saveXL = TRUE,
                  data = newdata,
                  verbose = T
                  #slice = T
                  #singular.ok = T
    )
    
    m.list[[iterations]]<-m
    
    #p <- predict(m,type="terms",interval="prediction")[,1]
    p <- (predict(m,type="terms",interval="none",posterior="all"))
    #p <- (predict(m,type="terms",interval="none",posterior="distribution"))
    #p <- (predict(m,type="terms",interval="none",posterior="mean"))
    #p <- (predict(m,type="terms",interval="none",posterior="mode"))
    
    # Predicted probability with marginalizing the random effect
    #p <- predict(m,type="terms", interval="none",posterior="mean",marginal=NULL)
    #p <- predict(m,type="terms", interval="none",
    #            posterior="mean",marginal=m$Random$formula)
    
    pred<-c()
    
    pred$b<-p[1:nrow(newdata)]
    
    pred<-as.data.frame(pred)
    
    pred$pa<-1/(1+exp(pred$b))
    pred$pb<-exp(pred$b)/(1+exp(pred$b))
    
    pred<-pred[2:3]
    
    # Get the DIC to check on convergence
    if(!(is.null(m))){
      newDIC <- m$DIC
      ContinueCondition <- (abs(oldDIC-newDIC)>ErrorTolerance &
                              iterations < MaxIterations)
      oldDIC <- newDIC
      print(paste("###### DIC : ", m$DIC, " ######"))
      
      # Update prob.
      newdata["p.1"]<-pred[,1]
      newdata["p.2"]<-pred[,2]
      
      # Update adjusted target based on MCMCglmm predicted probs.
      MCMCTarget<-c()
      
      repeat{
        for(k in 1:length(OriginalTarget)){
          t<-levels(OriginalTarget)
          MCMCTarget[k]<-sample(t,1,replace=FALSE,
                                prob=newdata[k,c("p.1","p.2")])
        }
        if ((length(table(MCMCTarget))==2)){break}
      }
      newdata[,"MCMCTarget"] <- as.factor(MCMCTarget)
      
    }
    else{ ContinueCondition<-FALSE }
  }
  
  #return final model fits and convergence info.
  return(list(
    CHAID.tree=tree.list,
    MCMCglmm.fit=m.list,
    Conv.info=newDIC-oldDIC,
    n.iter=iterations
  ))
}

library(dplyr)
outhomecharging <- mutate(outhomecharging,OutHomeChargingYN = ifelse(OutHomeCharging %in% c("NoCharging"),"NoCharging","Charging"))
outhomecharging$OutHomeChargingYN <- as.factor(outhomecharging$OutHomeChargingYN)
formula.outhomechargingYN <- (OutHomeChargingYN~Urb+Day+pAge+Ncar+Gend+Driver+wstat+
                                Tdur+Mode+Act+
                                ModePrev+ActPrev+TTPrev+ModeNext+ActNext+TTNext+
                                BT+ActDur+Evtype+SOC+Xdag+
                                Xndag+Xarb+Xpop+Ddag+Dndag+Darb+Dpop+
                                chargingKrachtstroom_dist+
                                KrachtstroomN)

## Call the function
MEtree.homecharging.result<-MEtree4(homecharging,formula.homecharging)
MEtree.outhomecharging.result<-MEtree4(outhomecharging,formula.outhomecharging)
MEtree.outhomechargingYN.result<-MEtree2(outhomecharging,formula.outhomechargingYN)

setwd("~/ActivityPriority/dynamicDT")

## Save pmmlParty
library(pmmlParty)
library(XML)
aicharging1 <- pmmlparty(MEtree.homecharging.result$CHAID.tree[[2]],
                         formula.homecharging,homecharging)
aicharging2 <- pmmlparty(MEtree.outhomecharging.result$CHAID.tree[[2]],
                         formula.outhomecharging,outhomecharging)
saveXML(aicharging1, "aicharging1_R.xml")
saveXML(aicharging2, "aicharging2_R.xml")

library(devtools)
install_github("JWiley/postMCMCglmm")
library(postMCMCglmm)

##### Predict with random effect for out-of-sample #####

predict.MEtree <- function(tree , MCMCglmm, newdata,formula, id=NULL,
                           EstimateRandomEffects=TRUE,...){

  treePrediction <- predict.party(tree,newdata)

  # If we aren't estimating random effects, we just use the tree for prediction.
  if(!EstimateRandomEffects){
    return(treePrediction)
  }
  # Get the group identifiers if necessary
  if(is.null(id)){
    id <- newdata[,as.character((MCMCglmm$Random$formula[[2]][[3]]))]
  }
  # Error-checking: the number of observations in the dataset must match
  # the sum of NumObs
  if(length(newdata[,id]) != dim(newdata)[1]){
    stop("number of observations in newdata does not match the length
         of the group identifiers")
  }
  ### Use the formula to get the target name
  TargetName <- formula[[2]]
  # Remove the name of the data frame if necessary
  if(length(TargetName)>1) TargetName <-TargetName[3]

  ActualTarget <- newdata[,toString(TargetName)]

  completePrediction <- treePrediction

  # Get the identities of the groups in the data
  # This will be slow - does LME have a faster way?
  uniqueID <- unique(id)

  # Get the random effects from the estimated MCMCglmm, in case there is overlap
  estRE <- ranef(object, use = ("mean"))

  for(i in 1:length(uniqueID)){
    # Identify the new group in the data
    nextID <- uniqueID[i]
    thisGroup <- id==nextID

    # If this group was in the original estimation, apply its random effect
    filter<-grepl(toString(uniqueID[i]),rownames(estRE))
    estEffect <- estRE[filter,]

    if(is.na(estEffect)){
      # Check for non-missing target
      nonMissing <- !is.na(ActualTarget[thisGroup])
      numAvailable <- sum(nonMissing)

      # If all the targets are missing, accept the
      # tree prediction; otherwise, estimate
      if(numAvailable>0) {
        R <- object$ErrorVariance * diag(numAvailable)
        D <- object$BetweenMatrix
        Z <- matrix(data=1,ncol=1, nrow=numAvailable)
        W <- solve(R + Z %*% D %*% t(Z))
        effect <- D %*% t(Z) %*% W %*%
          subset(ActualTarget[thisGroup] - treePrediction[thisGroup],
                 subset=nonMissing)
        completePrediction[thisGroup] <- treePrediction[thisGroup]+effect
      }
    } else {
      completePrediction[thisGroup] <- treePrediction[thisGroup]+estEffect
    }

  }

  return(completePrediction)

}

##### Create training and test data set #####
set.seed(20)
## For observation-level validation
#Randomly shuffle the data
yourData<-data[sample(nrow(data)),]

#Create 4 equally size folds
folds <- cut(seq(4,nrow(yourData)),breaks=4,labels=FALSE)

#Segment the data by fold using the which() function
testIndexes <- which(folds==1,arr.ind=TRUE)
testData.obs <- yourData[testIndexes, ]   ## 25% test set
trainData.obs <- yourData[-testIndexes, ] ## 75% training set

## Call the function
MEtree.result.obs.Model7<-MEtree(trainData.obs,formula)





