# Url and path
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- paste(getwd(), "stormdata.csv", sep = "/")

# DOWNLOAD THE DATASET
download.file(url = url,
              destfile = destfile,)

# LIBRARIES
library(ggplot2)
library(dplyr)
library(tidyverse)

# READ THE .csv FILE
storm_Data <- read.csv("stormdata.csv")
str(storm_Data)
View(storm_Data)

# TO CHECK THE COLUMN NAMES
names(storm_Data)

# TIDY DATA FROM STORM DATA
tidy_data <- subset(
    storm_Data, EVTYPE != "?" &
        (FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0),
    select = c("EVTYPE",
               "FATALITIES",
               "INJURIES", 
               "PROPDMG",
               "PROPDMGEXP",
               "CROPDMG",
               "CROPDMGEXP",
               "BGN_DATE",
               "END_DATE",
               "STATE"
    )
)

dim(tidy_data)

# CHECK IF NULL VALUE EXISTS
sum(is.na(tidy_data))

# UNIQUE EVENT TYPE VALUES IN TIDY DATASET.
length(unique(tidy_data$EVTYPE))

# converting all Event Type values to uppercase and combining similar Event Type values into unique categories.
tidy_data$EVTYPE <- toupper(tidy_data$EVTYPE)


# Avalanche
tidy_data$EVTYPE <- gsub('.*AVALANCE.*', 'AVALANCHE', tidy_data$EVTYPE)

# Blizzard
tidy_data$EVTYPE <- gsub('.*BLIZZARD.*', 'BLIZZARD', tidy_data$EVTYPE)

# Cloud
tidy_data$EVTYPE <- gsub('.*CLOUD.*', 'CLOUD', tidy_data$EVTYPE)

# Cold
tidy_data$EVTYPE <- gsub('.*COLD.*', 'COLD', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*FREEZ.*', 'COLD', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*FROST.*', 'COLD', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*ICE.*', 'COLD', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*LOW TEMPERATURE RECORD.*', 'COLD',tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*LO.*TEMP.*', 'COLD',tidy_data$EVTYPE)

# Dry
tidy_data$EVTYPE <- gsub('.*DRY.*', 'DRY',tidy_data$EVTYPE)

# Dust
tidy_data$EVTYPE <- gsub('.*DUST.*', 'DUST',tidy_data$EVTYPE)

# Fire
tidy_data$EVTYPE <- gsub('.*FIRE.*', 'FIRE', tidy_data$EVTYPE)

# flood
tidy_data$EVTYPE <- gsub('.*FLOOD.*', 'FLOOD', tidy_data$EVTYPE)

# Fog
tidy_data$EVTYPE <- gsub('.*FOG.*', 'FOG', tidy_data$EVTYPE)

# Hail
tidy_data$EVTYPE <- gsub('.*HAIL.*', 'HAIL', tidy_data$EVTYPE)

# HEAT
tidy_data$EVTYPE <- gsub('.*HEAT.*', 'HEAT', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*WARM.*', 'HEAT', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*HIGH.*TEMP.*', 'HEAT', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*RECORD HIGH TEMPERATURES.*', 'HEAT', tidy_data$EVTYPE)

# Hypothermia 
tidy_data$EVTYPE <- gsub('.*HYPOTHERMIA.*', 'HYPOTHERMIA/EXPOSURE',tidy_data$EVTYPE)

#Landslide
tidy_data$EVTYPE <- gsub('.*LANDSLIDE.*', 'LANDSLIDE', tidy_data$EVTYPE)

# Lightning
tidy_data$EVTYPE <- gsub('^LIGHTNING.*', 'LIGHTNING', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('^LIGNTNING.*', 'LIGHTNING', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('^LIGHTING.*', 'LIGHTNING', tidy_data$EVTYPE)

# Microburst
tidy_data$EVTYPE <- gsub('.*MICROBURST.*', 'MICROBURST', tidy_data$EVTYPE)

# Mudslide
tidy_data$EVTYPE <- gsub('.*MUDSLIDE.*', 'MUDSLIDE', tidy_data$EVTYPE)

# Rain
tidy_data$EVTYPE <-gsub('.*RAIN.*', 'RAIN', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*MUD SLIDE.*', 'MUDSLIDE', tidy_data$EVTYPE)

# Rip current
tidy_data$EVTYPE <- gsub('.*RIP CURRENT.*', 'RIP CURRENT', tidy_data$EVTYPE)

# Storm
tidy_data$EVTYPE <- gsub('.*STORM.*', 'STORM', tidy_data$EVTYPE)

# Summary
tidy_data$EVTYPE <- gsub('.*SUMMARY.*', 'SUMMARY', tidy_data$EVTYPE)

# Tornado
tidy_data$EVTYPE <- gsub('.*TORNADO.*', 'TORNADO', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*TORNDAO.*', 'TORNADO', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*LANDSPOUT.*', 'TORNADO', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*WATERSPOUT.*', 'TORNADO', tidy_data$EVTYPE)

# Surf
tidy_data$EVTYPE <- gsub('.*SURF.*', 'SURF', tidy_data$EVTYPE)

# Volcanic
tidy_data$EVTYPE <- gsub('.*VOLCANIC.*', 'VOLCANIC', tidy_data$EVTYPE)

# Wet
tidy_data$EVTYPE <- gsub('.*WET.*', 'WET', tidy_data$EVTYPE)

# Wind
tidy_data$EVTYPE <- gsub('.*WIND.*', 'WIND', tidy_data$EVTYPE)

# Winter
tidy_data$EVTYPE <- gsub('.*WINTER.*', 'WINTER', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*WINTRY.*', 'WINTER', tidy_data$EVTYPE)
tidy_data$EVTYPE <- gsub('.*SNOW.*', 'WINTER', tidy_data$EVTYPE)

# Number of unique event type value
length(unique(tidy_data$EVTYPE))

# Cleaning Date data
tidy_data$DATE_START <- as.Date(tidy_data$BGN_DATE, format = "%m/%d/%Y")
tidy_data$DATE_END <- as.Date(tidy_data$END_DATE, format = "%m/%d/%Y")
tidy_data$YEAR <- as.integer(format(tidy_data$DATE_START, "%Y"))
tidy_data$DURATION <- as.numeric(tidy_data$DATE_END - tidy_data$DATE_START)/3600

# Cleaning Economic Data
table(toupper(tidy_data$PROPDMGEXP))
table(toupper(tidy_data$CROPDMGEXP))

# function to get multiplier factor
getMultiplier <- function(exp) {
    exp <- toupper(exp);
    if (exp == "")  return (10^0);
    if (exp == "-") return (10^0);
    if (exp == "?") return (10^0);
    if (exp == "+") return (10^0);
    if (exp == "0") return (10^0);
    if (exp == "1") return (10^1);
    if (exp == "2") return (10^2);
    if (exp == "3") return (10^3);
    if (exp == "4") return (10^4);
    if (exp == "5") return (10^5);
    if (exp == "6") return (10^6);
    if (exp == "7") return (10^7);
    if (exp == "8") return (10^8);
    if (exp == "9") return (10^9);
    if (exp == "H") return (10^2);
    if (exp == "K") return (10^3);
    if (exp == "M") return (10^6);
    if (exp == "B") return (10^9);
    return (NA);
}

# calculate property damage and crop damage costs (in billions)
tidy_data$PROP_COST <- with(tidy_data, as.numeric(PROPDMG) * sapply(PROPDMGEXP, getMultiplier))/10^9
tidy_data$CROP_COST <- with(tidy_data, as.numeric(CROPDMG) * sapply(CROPDMGEXP, getMultiplier))/10^9

# Create a summarise data of health impact data (fatalities + injuries). 
health_impact_data <- aggregate(
    x = list(HEALTH_IMPACT = tidy_data$FATALITIES + tidy_data$INJURIES),
    by = list(EVENT_TYPE = tidy_data$EVTYPE),
    FUN = sum,
    na.rm = TRUE
)
health_impact_data <- health_impact_data[order(health_impact_data$HEALTH_IMPACT,
                                               decreasing = TRUE),]

# Create a summarise dataset of damage impact cost (property damage + crop damage).
damage_costImpact_data <- aggregate(
    x = list(DAMAGE_IMPACT = tidy_data$PROP_COST + tidy_data$CROP_COST),
    by = list(EVENT_TYPE = tidy_data$EVTYPE),
    FUN = sum,
    na.rm = TRUE
)
damage_costImpact_data <- damage_costImpact_data[order(damage_costImpact_data$DAMAGE_IMPACT,
                                                       decreasing = TRUE),]


# Event types most harmful to population health.
# This is for the R-markdown.
print(xtable(head(health_impact_data,10),
             caption = "Top 10 Weather Events Most Harmful to Population Health"),
      caption.placement = 'top',
      type = 'html',
      include.rownames = FALSE,
      html.table.atrributes = 'class = "table-borered", width ="100%"'
)

health_impact_chart <- ggplot(head(health_impact_data,10),
                              aes(x=reorder(EVENT_TYPE, HEALTH_IMPACT),
                                  y = HEALTH_IMPACT, fill = EVENT_TYPE)) + 
    coord_flip() + 
    geom_bar(stat = "identity") + 
    xlab("Event Type") + 
    ylab("Total Fatalities and Injuries") + 
    theme(plot.title = element_text(size = 14, hjust = 0.5)) + 
    ggtitle("Top 10 Weather Events Most Harmful to \nPopulation Health")
print(health_impact_chart)

ggsave("health_impact_chart.png", path = getwd(), width = 30, height = 20, units = "cm" )

# Event Types with Greatest Economic Consequences
print(xtable(head(damage_costImpact_data, 10),
             caption = "Top 10 Weather Events with Greatest Economic Consequences"),
      caption.placement = 'top',
      type = "html",
      include.rownames = FALSE,
      html.table.attributes='class="table-bordered", width="100%"')

damage_costImpact_chart <- ggplot(
    head(damage_costImpact_data,10),
    aes(x = reorder(EVENT_TYPE, DAMAGE_IMPACT),
        y = DAMAGE_IMPACT, fill = EVENT_TYPE)) + 
    coord_flip() + 
    geom_bar(stat = 'identity') + 
    xlab("Event Type") + 
    ylab("Total property/ Crop damage cost \n(in Billions)") + 
    theme(plot.title = element_text(size = 14, hjust = 0.7 )) + 
    ggtitle("Top 10 Weather Events with \nGreatest Economic Consequences")
print(damage_costImpact_chart)
ggsave(filename = "damage_costImpact_chart.png",path = getwd(),
       width = 30, height = 20, units = 'cm')
