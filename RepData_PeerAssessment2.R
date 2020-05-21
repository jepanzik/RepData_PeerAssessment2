# Reproducible Research Project 2
# Johns Hopkins University Coursera
# Author: J.E. Panzik
# Date Created: May 21, 2020

#Create "data" directory if it does not exist in your working directory
if(!file.exists("./data")){dir.create("./data")}

#Check to see if data file exists in data directory.
#Read the data if it is. Download and read the data if not present
if(file.exists("./data/repdata-data-StormData.csv")) {
      print("Data file is present")
      dataRaw <- read.csv("./data/repdata-data-StormData.csv", na.strings=c(""," ","NA"))
} else if (file.exists("./data/repdata-data-StormData.csv.bz2")) {
      print("Data file is present")
      dataRaw <- read.csv("./data/repdata-data-StormData.csv.bz2", na.strings=c(""," ","NA"))   
} else {
      print("Data file is downloading from internet")
      download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="./data/repdata-data-StormData.csv.bz2", method="curl")
      dataRaw <- read.csv("./data/repdata-data-StormData.csv.bz2", na.strings=c(""," ","NA"))
}

#Display some basic properties of the data
head(dataRaw[,c(1:8, 23:28)])
str(dataRaw$EVTYPE)
table(dataRaw$EVTYPE)

#Subset the data for desired variables.
impact <- dataRaw[, c("EVTYPE","FATALITIES","INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

#Put damage exponents as all upper case
impact$PROPDMGEXP <- toupper(as.character(impact$PROPDMGEXP))
impact$CROPDMGEXP <- toupper(as.character(impact$CROPDMGEXP))

#Convert property damage to numeric values based on PROPDMG and PROPDMGEXP
impact$PROPDMGEXP[is.na(impact$PROPDMGEXP)] <- "0"
impact$PROPDMGEXP[impact$PROPDMGEXP=="-"] <- "0"
impact$PROPDMGEXP[impact$PROPDMGEXP=="?"] <- "0"
impact$PROPDMGEXP[impact$PROPDMGEXP=="+"] <- "0"
impact$PROPDMGEXP[impact$PROPDMGEXP=="B"] <- "9"
impact$PROPDMGEXP[impact$PROPDMGEXP=="M"] <- "6"
impact$PROPDMGEXP[impact$PROPDMGEXP=="K"] <- "3"
impact$PROPDMGEXP[impact$PROPDMGEXP=="H"] <- "2"
impact$PROPDMGEXP <- as.numeric(impact$PROPDMGEXP)

#Calculate full property damage
impact$PROPDMGFULL <- impact$PROPDMG*(10^impact$PROPDMGEXP)

#Convert crop damage to numeric values based on PROPDMG and PROPDMGEXP
impact$CROPDMGEXP[is.na(impact$CROPDMGEXP)] <- "0"
impact$CROPDMGEXP[impact$CROPDMGEXP=="?"] <- "0"
impact$CROPDMGEXP[impact$CROPDMGEXP=="B"] <- "9"
impact$CROPDMGEXP[impact$CROPDMGEXP=="M"] <- "6"
impact$CROPDMGEXP[impact$CROPDMGEXP=="K"] <- "3"
impact$CROPDMGEXP <- as.numeric(impact$CROPDMGEXP)

#Calculate full crop damage
impact$CROPDMGFULL <- impact$CROPDMG*(10^impact$CROPDMGEXP)

#Convert EVTYPEs to uniform codes
impact$EVTYPE <- toupper(impact$EVTYPE)
impact$EVTYPE <- gsub("* ", "", impact$EVTYPE)
impact$EVTYPE <- gsub("TSTM", "THUNDERSTORM", impact$EVTYPE)
impact$EVTYPE[grepl("LIGHTNING", x=impact$EVTYPE)] <- "Lightning"
impact$EVTYPE[grepl("LIGHTING", x=impact$EVTYPE)] <- "Lightning"
impact$EVTYPE[grepl("MARINETHUN", x=impact$EVTYPE)] <- "Marine Thunderstorm Winds"
impact$EVTYPE[grepl("THUN", x=impact$EVTYPE)] <- "Thunderstorm Winds"
impact$EVTYPE[grepl("TUNDER", x=impact$EVTYPE)] <- "Thunderstorm Winds"
impact$EVTYPE[grepl("THUD", x=impact$EVTYPE)] <- "Thunderstorm Winds"
impact$EVTYPE[grepl("FLASH", x=impact$EVTYPE)] <- "Flash Flood"
impact$EVTYPE[grepl("FIRE", x=impact$EVTYPE)] <- "Wildfire"
impact$EVTYPE[grepl("VOLC", x=impact$EVTYPE)] <- "Volcanic Ash"
impact$EVTYPE[grepl("MARINEHAIL", x=impact$EVTYPE)] <- "Marine Hail"
impact$EVTYPE[grepl("MARINESTRONG", x=impact$EVTYPE)] <- "Marine Strong Wind"
impact$EVTYPE[grepl("MARINEHIGH", x=impact$EVTYPE)] <- "Marine High Wind"
impact$EVTYPE[grepl("HAIL", x=impact$EVTYPE)] <- "Hail"
impact$EVTYPE[grepl("WATERSP", x=impact$EVTYPE)] <- "Waterspout"
impact$EVTYPE[grepl("SPOUT", x=impact$EVTYPE)] <- "Waterspout"
impact$EVTYPE[grepl("SLIDE", x=impact$EVTYPE)] <- "Debris Flow"
impact$EVTYPE[grepl("STREAM", x=impact$EVTYPE)] <- "Flood"
impact$EVTYPE[grepl("URBAN", x=impact$EVTYPE)] <- "Flood"
impact$EVTYPE[grepl("DRY", x=impact$EVTYPE)] <- "Drought"
impact$EVTYPE[grepl("DROUGHT", x=impact$EVTYPE)] <- "Drought"
impact$EVTYPE[grepl("DUST", x=impact$EVTYPE)] <- "Dust Storm"
impact$EVTYPE[grepl("RIP", x=impact$EVTYPE)] <- "Rip Current"
impact$EVTYPE[grepl("AVA", x=impact$EVTYPE)] <- "Avalanche"
impact$EVTYPE[grepl("EXCESSIVEHEAT", x=impact$EVTYPE)] <- "Excessive Heat"
impact$EVTYPE[grepl("HEAT", x=impact$EVTYPE)] <- "Heat"
impact$EVTYPE[grepl("LOWTI", x=impact$EVTYPE)] <- "Astronomical Low Tide"
impact$EVTYPE[grepl("EXT", x=impact$EVTYPE)] <- "Extreme Cold/Wind Chill"
impact$EVTYPE[grepl("EXCESSIVECOLD", x=impact$EVTYPE)] <- "Extreme Cold/Wind Chill"
impact$EVTYPE[grepl("COLD", x=impact$EVTYPE)] <- "Cold/Wind Chill"
impact$EVTYPE[grepl("CHILL", x=impact$EVTYPE)] <- "Cold/Wind Chill"
impact$EVTYPE[grepl("TORN", x=impact$EVTYPE)] <- "Tornado"
impact$EVTYPE[grepl("TROPICALSTORM", x=impact$EVTYPE)] <- "Tropical Storm"
impact$EVTYPE[grepl("TROPICALDEP", x=impact$EVTYPE)] <- "Tropical Depression"
impact$EVTYPE[grepl("SLEET", x=impact$EVTYPE)] <- "Sleet"
impact$EVTYPE[grepl("HURRICANE", x=impact$EVTYPE)] <- "Hurricane (Typhoon)"
impact$EVTYPE[grepl("TYPH", x=impact$EVTYPE)] <- "Hurricane (Typhoon)"
impact$EVTYPE[grepl("BLIZ", x=impact$EVTYPE)] <- "Blizzard"
impact$EVTYPE[grepl("COASTALFL", x=impact$EVTYPE)] <- "Coastal Flood"
impact$EVTYPE[grepl("CSTL", x=impact$EVTYPE)] <- "Coastal Flood"
impact$EVTYPE[grepl("SURGE", x=impact$EVTYPE)] <- "Storm Surge/Tide"
impact$EVTYPE[grepl("TIDAL", x=impact$EVTYPE)] <- "Storm Surge/Tide"
impact$EVTYPE[grepl("HIGHTIDE", x=impact$EVTYPE)] <- "Storm Surge/Tide"
impact$EVTYPE[grepl("LAKESHOREFLOOD", x=impact$EVTYPE)] <- "Lakeshore Flood"
impact$EVTYPE[grepl("LAKEFLOOD", x=impact$EVTYPE)] <- "Lakeshore Flood"
impact$EVTYPE[grepl("HEAVYRAIN", x=impact$EVTYPE)] <- "Heavy Rain"
impact$EVTYPE[grepl("ICESTO", x=impact$EVTYPE)] <- "Ice Storm"
impact$EVTYPE[grepl("FLOOD", x=impact$EVTYPE)] <- "Flood"
impact$EVTYPE[grepl("ICE", x=impact$EVTYPE)] <- "Ice Storm"
impact$EVTYPE[grepl("WINTERSTO", x=impact$EVTYPE)] <- "Winter Storm"
impact$EVTYPE[grepl("WINTERWEAT", x=impact$EVTYPE)] <- "Winter Weather"
impact$EVTYPE[grepl("TSUNA", x=impact$EVTYPE)] <- "Tsunami"
impact$EVTYPE[grepl("LAKE", x=impact$EVTYPE)] <- "Lake-Effect Snow"
impact$EVTYPE[grepl("WIND", x=impact$EVTYPE)] <- "Wind"
impact$EVTYPE[grepl("HEAVYSNOW", x=impact$EVTYPE)] <- "Heavy Snow"
impact$EVTYPE[grepl("FREEZING", x=impact$EVTYPE)] <- "Sleet"
impact$EVTYPE[grepl("FREEZ", x=impact$EVTYPE)] <- "Frost/Freeze"
impact$EVTYPE[grepl("FROST", x=impact$EVTYPE)] <- "Frost/Freeze"


table(impact$EVTYPE[grep("FROST", x=impact$EVTYPE)])
table(impact$EVTYPE)



#Sum fatalities by event type
eventHarm <- aggregate(FATALITIES~EVTYPE,impact, FUN=sum)

#Sum injuries by event type and merge with eventTotals
eventHarm <- merge(eventHarm, aggregate(INJURIES~EVTYPE,impact, FUN=sum))

#Sum fatalities and injuries for total harmed
eventHarm$TotalHarmed <- eventHarm$FATALITIES + eventHarm$INJURIES

#Take event types with top 10 fatalities
eventHarm <- eventHarm[order(-eventHarm$FATALITIES)[1:10],]

#Sum property damage by event type and merge with eventTotals
eventDamage <- aggregate(PROPDMGFULL~EVTYPE,impact, FUN=sum)

#Sum crop damage by event type and merge with eventTotals
eventDamage <- merge(eventDamage, aggregate(CROPDMGFULL~EVTYPE,impact, FUN=sum))

#Sum property and crop damage for total damage
eventDamage$TotalDamage <- eventDamage$PROPDMGFULL + eventDamage$CROPDMGFULL

#Take event types with top 10 property damages
eventDamage <- eventDamage[order(-eventDamage$PROPDMGFULL)[1:10],]

library(reshape2)
#Melt the harm and damage data tables so that the type of harm/damage is now an entry
Harm <- melt(eventHarm, id.vars="EVTYPE", variable.name = "harmType")
Damage <- melt(eventDamage, id.vars="EVTYPE", variable.name = "damageType")

#Clean up the elements in the date frames for better display
Harm$harmType <- gsub("FATALITIES", "Fatalities", Harm$harmType)
Harm$harmType <- gsub("INJURIES", "Injuries", Harm$harmType)
Harm$harmType <- gsub("TotalHarm", "Total Harm", Harm$harmType)


Damage$damageType <- gsub("PROPDMGFULL", "Property Damage", Damage$damageType)
Damage$damageType <- gsub("CROPDMGFULL", "Crop Damage", Damage$damageType)
Damage$damageType <- gsub("TotalDamage", "Total Damage", Damage$damageType)



library(ggplot2)

#Plot harm data as bar plots showing the differences
ggplot(Harm, aes(x=reorder(EVTYPE, -value), y=value)) + 
   geom_bar(stat="identity", aes(fill=harmType), position="dodge") +
   xlab("Disaster Type") +
   ylab("Number of People") +
   ggtitle("Top 10 Disaster Types with the Most Fatalities") + 
   theme(axis.text.x = element_text(angle=45, hjust=1))

#Plot harm data as bar plots showing the differences
ggplot(Damage, aes(x=reorder(EVTYPE, -value), y=value)) + 
   geom_bar(stat="identity", aes(fill=damageType), position="dodge") +
   xlab("Disaster Type") +
   ylab("Damage Caused ($)") +
   ggtitle("Top 10 Disaster Types with the Most Property Damage") + 
   theme(axis.text.x = element_text(angle=45, hjust=1))
