###################################################
######  Aggregate data from Lake Fuchskuhle  ######
###################################################


#### deal with issue of missing stationname ####


## This code takes the HPG raw chemistry, probe, and ice days data provided by
## HPG for Lake Fuchskuhle and converts, reorganizes, and aggregates the data to
## meet the specifications of the under ice ecology template and instructions.
## The original researcher, Hans-Peter Grossart, defined ice-on periods as those
## with 80% ice cover; we are using this to define the ice season. 

## There are three years (2009, 2010, and 2015) in which the data meet the defined
## ice-covered period (80% coverage). Plots of temperature profiles confirm HPG's
## assertion that summer stratification occurs between April and October; therefore
## we use these months to define the stratification period. Although there is plenty
## of summertime data, we only aggregate from summers for which there is corresponding
## winter data (i.e. 2009, 2010, and 2015)

## Notes about the lake: Lake Fuchskuhle has been artificially divided into four basins.
## HPG provided data on the NE basin (Fuku NE) and SW basin (Fuku SW).
## The two basins have very different systems and should be considered 
## two separate lakes - the basins should NOT be aggregated together.


## S. Labou & K. Woo
## Last updated 2015-07-09

library("icetest")
library("dplyr")
library("lubridate")
library("zoo")
library("ggplot2")
library("scales")
library("lubridate")
library("XLConnect")



#### set working directory ####


## setwd("C://UnderIce/HansPeter")


################## Fuku NE & SW chemistry data #######################

## FukuNE <- read.csv("hans_peter_grossart_data_DONOTSHARE/FukuNO_chemistry_1990.2015.csv", stringsAsFactors = FALSE)

FukuNE <- read.csv("FukuNO_chemistry_1990.2015.csv", stringsAsFactors = FALSE)

## FukuSW <- read.csv("hans_peter_grossart_data_DONOTSHARE/FukuSW_chemistry_1990.2015.csv", stringsAsFactors = FALSE)

FukuSW <- read.csv("FukuSW_chemistry_1990.2015.csv", stringsAsFactors = FALSE)

#new dataframe to have date, TP, TN, all N, DOC, and Chla

vars <- c("Datum", "Tiefe..m.", "TP..mg.l.", "TN..mg.l.", "NO2..mg.l.", "NO3..mg.l.", "NH4..mg.l.", "DOC..mg.l.", "Chla..mg.l.")

FukuNE.vars <- FukuNE[vars]

FukuSW.vars <- FukuSW[vars]

#need to change variables
#TP mg/l needs to be ug/l (1000 ug in one mg) -- multiply by 1000
#TN mg/l needs to be ug/l
#all N values need to be ug/l
#chla needs to be ug/l

### DOC stays mg/l


FukuNE.calc <- mutate(FukuNE.vars,
                      TP = (TP..mg.l. * 1000),
                      TN = (TN..mg.l. * 1000),
                      NO2 = (NO2..mg.l. * 1000),
                      NO3 = (NO3..mg.l. * 1000),
                      NH4 = (NH4..mg.l. * 1000),
                      Chla = (Chla..mg.l. * 1000))

FukuSW.calc <- mutate(FukuSW.vars,
                      TP = (TP..mg.l. * 1000),
                      TN = (TN..mg.l. * 1000),
                      NO2 = (NO2..mg.l. * 1000),
                      NO3 = (NO3..mg.l. * 1000),
                      NH4 = (NH4..mg.l. * 1000),
                      Chla = (Chla..mg.l. * 1000))

#keep only correct unit variables 

keep <- c("Datum", "Tiefe..m.", "TP", "TN", "NO2", "NO3", "NH4", "DOC..mg.l.", "Chla")

FukuNE.cor <- FukuNE.calc[keep]

FukuSW.cor <- FukuSW.calc[keep]


#rename variables

FukuNE.cor <- rename(FukuNE.cor, Date = Datum, Depth = Tiefe..m., DOC = DOC..mg.l.)

FukuSW.cor <- rename(FukuSW.cor, Date = Datum, Depth = Tiefe..m., DOC = DOC..mg.l.)


#make sure date is as.date

FukuNE.cor$Date <- as.Date(FukuNE.cor$Date, "%m/%d/%Y")

FukuSW.cor$Date <- as.Date(FukuSW.cor$Date, "%m/%d/%Y")


#find total dissolved N (NO2+NO3+NH4)
## Note that this will result in NAs if any of the three columns contains an NA.

FukuNE.cor <- mutate(FukuNE.cor,
                     totdissN = (NO2 + NO3 + NH4))

FukuSW.cor <- mutate(FukuSW.cor,
                     totdissN = (NO2 + NO3 + NH4))


#remove individual N components

keep2 <- c("Date", "Depth", "TP", "TN", "DOC", "Chla", "totdissN")

FukuNE.go <- FukuNE.cor[keep2]

FukuSW.go <- FukuSW.cor[keep2]


#add station name to best identify which data is which

FukuNE.go$stationname <- "Fuchskuhle NE"

FukuSW.go$stationname <- "Fuchskuhle SW"



################ secchi and photic depth data ################

## FukuNE.probe <- read.csv("hans_peter_grossart_data_DONOTSHARE/FukuNO_1990-2015_probe.csv", stringsAsFactors = FALSE)

FukuNE.probe <- read.csv("FukuNO_1990-2015_probe.csv", stringsAsFactors = FALSE)

## FukuSW.probe <- read.csv("hans_peter_grossart_data_DONOTSHARE/FukuSW_1990-2015_probe.csv", stringsAsFactors = FALSE)

FukuSW.probe <- read.csv("FukuSW_1990-2015_probe.csv", stringsAsFactors = FALSE)
                         

#rename columns from German to English

FukuNE.pr.cor <- rename(FukuNE.probe, Date = Datum, Secchi = Sichttiefe..m., Depth = Tiefe..m., Temp = Temp....C.)

FukuSW.pr.cor <- rename(FukuSW.probe, Date = Datum, Secchi = Sichttiefe..m., Depth = Tiefe..m., Temp = Temp....C.)

#water transparency is in m
#depth is in m
#temp is in degrees C


#keep only columns of interest
#note: a subset of these variables will be used later

vars.pr <- c("Date", "Secchi", "Depth", "Temp")

FukuNE.pr <- FukuNE.pr.cor[vars.pr]

FukuSW.pr <- FukuSW.pr.cor[vars.pr]


#ensure that date is a date object

FukuNE.pr$Date <- as.Date(FukuNE.pr$Date, "%m/%d/%Y")

FukuSW.pr$Date <- as.Date(FukuSW.pr$Date, "%m/%d/%Y")


#add new column "photicdepth" based on secchi
#kd = 1.7/secchi
#photic depth = ln(1000)/kd

FukuNE.pr.full <- FukuNE.pr %>% 
  mutate(PhoticDepth = log(1000)/(1.7/Secchi))

FukuSW.pr.full <- FukuSW.pr %>% 
  mutate(PhoticDepth = log(1000)/(1.7/Secchi))



#################### merge photicdepth to chem data by date #######################

#want just photicdepth to merge, so subset df 
#to include only date, secchi, and photicdepth
#keep only unique rows
#remove any rows where photicdepth is NA

FukuNE.pr.pd <- select(FukuNE.pr.full, Date, Secchi, PhoticDepth) %>% 
  unique() %>% 
  filter(!is.na(PhoticDepth))

FukuSW.pr.pd <- select(FukuSW.pr.full, Date, Secchi, PhoticDepth) %>% 
  unique() %>% 
  filter(!is.na(PhoticDepth))


#merge this new df with chem df
#only keep overlap dates

FukuNE.pr.chem <- merge(FukuNE.pr.pd, FukuNE.go, by = "Date")

FukuSW.pr.chem <- merge(FukuSW.pr.pd, FukuSW.go, by = "Date")


#only interested in values within the photic depth
#if Depth is greater than photicdepth, converts to NA, and is filtered out
#note: this works because there are originally no NAs in depth

FukuNE.pr.chem <- FukuNE.pr.chem %>% 
  group_by(Date) %>% 
  mutate(Depth = ifelse(Depth > PhoticDepth, NA, Depth)) %>% 
  filter(!is.na(Depth))

FukuSW.pr.chem <- FukuSW.pr.chem %>% 
  group_by(Date) %>% 
  mutate(Depth = ifelse(Depth > PhoticDepth, NA, Depth)) %>% 
  filter(!is.na(Depth))

#now the Fuchskuhle chemistry dfs have chemistry columns and photicdepth and secchi



################################################
########  Grosse Fuchkskuhle ice dates  ########
################################################


## This script takes the ice dates that Hans-Peter Grossart sent us for Grosse
## Fuchskuhle and expands the data frame of dates so that there is a row for
## each date in each ice season. It then uses na.locf() to fill in ice
## percentages for each date (a highly imperfect strategy, but good enough for
## us to figure out which dates to aggregate into an ice-on aggregation).


## Load the ice dates from HPG's file and add a winteryear column that
## corresponds to the `year` column in the Under Ice Ecology data template --
## i.e. November/December should be counted as the following year. 
## Fix the date format for dates with multiple days in one date format.
## Remove the rows where there's text in the `ice` column.

icedat <- loadWorkbook("Ice_Lake_Stechlin_Fuchskuhle.xls") %>%
  readWorksheet(sheet = 3, startRow = 11, startCol = 10, endCol = 12) %>%
  rename(date = Date, ice = Eis.., snow = X...) %>%
  ## Fix messed up date format (choose first of the two possible dates):
  mutate(date = gsub("(\\d{2})\\./(\\d{2})\\.(\\d{2})\\.(\\d{2})",
                     "20\\4-\\3-\\1", date),
         date = base::as.Date(date),
         #make column winter year (e.g. Nov - Feb is all one season for these purposes)
         winteryear = ifelse(month(date) >= 11, year(date) + 1, year(date))) %>% 
  mutate(ice = as.numeric(ice)) %>% ## will convert text to NAs
  mutate(snow = as.numeric(snow)) %>% 
  filter(!is.na(ice)) #filter to keep only non-NAs

## Find the min and max dates for each ice season
startend <- icedat %>%
  group_by(winteryear) %>%
  #by winter year get min date, max date, and diff
  summarize(min = min(date), max = max(date), diff = max - min)

## Create sequences of dates for each season
seqs <- mapply(function(x, y) seq(x, y, by = "day"), startend$min, startend$max)

#creates a list, one for each winter year
#fills in gaps in dates so there are all days accounted for in each winter year

## Make the dates into a data frame
alldates <- data.frame(date = do.call("c", seqs))

## Merge the data frame back in with icedat and use na.locf to fill in missing
## values
Fuku.ice.dates <- merge(icedat, alldates, all = TRUE) %>%
  mutate(ice = na.locf(ice), #fills in ice values (runs last value forward through NAs until hit next value)
         snow = na.locf(snow), #fills in snow values 
         winteryear = (ifelse(month(date) >= 11, year(date) + 1, year(date))))

#Fuku.ice.dates has date, ice, snow, and winter year

#merge with chem data


#capitalize "date" in ice dates to merge with chem

Fuku.ice.dates <- rename(Fuku.ice.dates, Date = date)


#merge ice dates and chem data

FukuNE.chem.ice <- merge(Fuku.ice.dates, FukuNE.pr.chem, by = "Date")

FukuSW.chem.ice <- merge(Fuku.ice.dates, FukuSW.pr.chem, by = "Date")


#subset ice df to only include rows with ice+snow >= 80

FukuNE.chem.ice <- filter(FukuNE.chem.ice, ice + snow >= 80)

FukuSW.chem.ice <- filter(FukuSW.chem.ice, ice + snow >= 80)


#chemistry data for winter (iceon) seasons


#########################################################
######## Summer stratification from surface temp ########
#########################################################

## According to HPG, Lake Grosse Fuchskuhle usually mixes when the surface temperature is between 4-6 C
## This is a function to plot surface temperature by year for Grosse Fuchskuhle with
## points colored by whether or not they are above 6 degrees C

fuch_temp <- function(data, title) {
  data %>%
    mutate(year = year(Date),
           monthday = format(Date, "%m-%d"),
           above6 = ifelse(Temp > 6, TRUE, FALSE)) %>%
    filter(Depth == 0.0) %>%
    ggplot(aes(x = monthday, y = Temp)) +
    geom_point(aes(color = above6, group = 1)) +
    facet_wrap(~ year, scales = "free_x") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

## Sets of plots for both stations in Grosse Fuchskuhle
pdf("hp_plots_fuchskuhle.pdf", width = 16, height = 11)
fuch_temp(FukuNE.pr, "Grosse Fuchskuhle, NO")
fuch_temp(FukuSW.pr, "Grosse Fuchskuhle, SW")
dev.off()

#looking for relatively consistent timing across years
#ultimately selected mid-April - Oct. as summer stratification period
#best range of months to be consistent over range of years

#subset chem data for between mid-April - Oct. for each year

#use lubridate's yday to get mid-April day
#April 15th is the 105th day of the year


FukuNE.chem.summer <- FukuNE.pr.chem %>% 
  group_by(year(Date)) %>% 
  filter(yday(Date) >= 105 & month(Date) < 11)


FukuSW.chem.summer <- FukuSW.pr.chem %>% 
  group_by(year(Date)) %>% 
  filter(yday(Date) >= 105 & month(Date) < 11)


## this is chemistry/probe data for the summer stratification (iceoff) period



################ merge ice/summer and chem/pr data #####################

#only keep overlapping dates (dates in BOTH ice and chem data frames)

FukuNE.chem.summer <- filter(FukuNE.chem.summer,
                             year(Date) %in% year(FukuNE.chem.ice$Date) #keep only if same years as ice.chem
                             & Depth <= PhoticDepth) #keep only if depth <= photic depth

FukuSW.chem.summer <- filter(FukuSW.chem.summer,
                             year(Date) %in% year(FukuSW.chem.ice$Date)
                             & Depth <= PhoticDepth)


################## join winter and summer data together ####################

#ungroup

FukuNE.chem.summer <- ungroup(FukuNE.chem.summer)

FukuSW.chem.summer <- ungroup(FukuSW.chem.summer)


#keep only overlapping columns - in both summer and winter

FukuNE.chem.summer.bind <- select(FukuNE.chem.summer, Date, Secchi, PhoticDepth, Depth, TP,
                                  TN, DOC, Chla, totdissN, stationname)

FukuSW.chem.summer.bind <- select(FukuSW.chem.summer, Date, Secchi, PhoticDepth, Depth, TP,
                                  TN, DOC, Chla, totdissN, stationname)


FukuNE.chem.ice.bind <- select(FukuNE.chem.ice, Date, Secchi, PhoticDepth, Depth, TP, TN, DOC,
                               Chla, totdissN, stationname)

FukuSW.chem.ice.bind <- select(FukuSW.chem.ice, Date, Secchi, PhoticDepth, Depth, TP, TN, DOC,
                               Chla, totdissN, stationname)

#bind winter and summer data together

FukuNE.chem.full <- rbind(FukuNE.chem.ice.bind, FukuNE.chem.summer.bind)

FukuSW.chem.full <- rbind(FukuSW.chem.ice.bind, FukuSW.chem.summer.bind)
                             


############ compute seasonal averages for chemistry data ############

FukuNE.seasonal.chem <- FukuNE.chem.full %>%
  ## Add season column
  mutate(season = ifelse(yday(Date) >= 105 & month(Date) < 11, "iceoff", "iceon")) %>%
  ## First find max sampling depth for each date; will need this to calculate
  ## sampledepth
  group_by(Date) %>%
  mutate(maxdepth = max(Depth, na.rm = TRUE)) %>%
  ## Regroup by season and year to aggregate chemistry data into iceon and iceoff rows
  group_by(season, year(Date)) %>%
  summarize(StartDate = min(Date), 
            EndDate = max(Date),
            periodn = length(unique(Date)),
            photicdepth = mean(PhoticDepth, na.rm = TRUE), #average photic depth for sample period
            stationdepth = NA,
            sampledepth = mean(maxdepth),
            avesecchidepth = mean(Secchi, na.rm = TRUE),
            cvsecchidepth = sd(Secchi, na.rm = TRUE)/mean(Secchi, na.rm = TRUE),
            avetotphos = mean(TP, na.rm = TRUE),
            cvtotphos = sd(TP, na.rm = TRUE)/mean(TP, na.rm = TRUE),
            maxtotphos = max(TP, na.rm = TRUE),
            avetotdissphos = NA,
            cvtotdissphos = NA,
            maxtotdissphos = NA,
            avetotnitro = mean(TN, na.rm = TRUE),
            cvtotnitro = sd(TN, na.rm = TRUE)/mean(TN, na.rm = TRUE),
            maxtotnitro = max(TN, na.rm = TRUE),
            avetotdissnitro = mean(totdissN, na.rm = TRUE),
            cvtotdissnitro = sd(totdissN, na.rm = TRUE)/mean(totdissN, na.rm = TRUE),
            maxtotdissnitro = max(totdissN, na.rm = TRUE),
            avetotdoc = mean(DOC, na.rm = TRUE),
            cvtotdoc = sd(DOC, na.rm = TRUE)/mean(DOC, na.rm = TRUE),
            maxtotdoc = max(DOC, na.rm = TRUE),
            avechla = mean(Chla, na.rm = TRUE),
            cvchla = sd(Chla, na.rm = TRUE)/mean(Chla, na.rm = TRUE),
            maxchla = max(Chla, na.rm = TRUE)) %>%
  ## Keep columns that are present in the ice data template,
  ## plus StartDate and EndDate
  ## (`datafields` is defined in the icetest package, which must be loaded)
  select_(.dots = names(.)[names(.) %in% c(datafields, "StartDate", "EndDate")]) %>%
  ## Add year column
  mutate(year = year(EndDate))


#replace NaN values created with NA

FukuNE.seasonal.chem[, grep("^(ave|cv|max)", names(FukuNE.seasonal.chem))] <- 
  apply(FukuNE.seasonal.chem[, grep("^(ave|cv|max)", names(FukuNE.seasonal.chem))],
        c(1,2), function(x) ifelse(is.nan(x), NA, x))



FukuSW.seasonal.chem <- FukuSW.chem.full %>%
  ## Add season column
  mutate(season = ifelse(yday(Date) >= 105 & month(Date) < 11, "iceoff", "iceon")) %>%
  ## First find max sampling depth for each date; will need this to calculate
  ## sampledepth
  group_by(Date) %>%
  mutate(maxdepth = max(Depth, na.rm = TRUE)) %>%
  ## Regroup by season and year to aggregate chemistry data into iceon and iceoff rows
  group_by(season, year(Date)) %>%
  summarize(StartDate = min(Date), 
            EndDate = max(Date),
            periodn = length(unique(Date)),
            photicdepth = mean(PhoticDepth, na.rm = TRUE), #average photic depth for sample period
            stationdepth = NA,
            sampledepth = mean(maxdepth),
            avesecchidepth = mean(Secchi, na.rm = TRUE),
            cvsecchidepth = sd(Secchi, na.rm = TRUE)/mean(Secchi, na.rm = TRUE),
            avetotphos = mean(TP, na.rm = TRUE),
            cvtotphos = sd(TP, na.rm = TRUE)/mean(TP, na.rm = TRUE),
            maxtotphos = max(TP, na.rm = TRUE),
            avetotdissphos = NA,
            cvtotdissphos = NA,
            maxtotdissphos = NA,
            avetotnitro = mean(TN, na.rm = TRUE),
            cvtotnitro = sd(TN, na.rm = TRUE)/mean(TN, na.rm = TRUE),
            maxtotnitro = max(TN, na.rm = TRUE),
            avetotdissnitro = mean(totdissN, na.rm = TRUE),
            cvtotdissnitro = sd(totdissN, na.rm = TRUE)/mean(totdissN, na.rm = TRUE),
            maxtotdissnitro = max(totdissN, na.rm = TRUE),
            avetotdoc = mean(DOC, na.rm = TRUE),
            cvtotdoc = sd(DOC, na.rm = TRUE)/mean(DOC, na.rm = TRUE),
            maxtotdoc = max(DOC, na.rm = TRUE),
            avechla = mean(Chla, na.rm = TRUE),
            cvchla = sd(Chla, na.rm = TRUE)/mean(Chla, na.rm = TRUE),
            maxchla = max(Chla, na.rm = TRUE)) %>%
  ## Keep columns that are present in the ice data template,
  ## plus StartDate and EndDate
  ## (`datafields` is defined in the icetest package, which must be loaded)
  select_(.dots = names(.)[names(.) %in% c(datafields, "StartDate", "EndDate")]) %>%
  ## Add year column
  mutate(year = year(EndDate))


#replace NaN values created with NA

FukuSW.seasonal.chem[, grep("^(ave|cv|max)", names(FukuSW.seasonal.chem))] <- 
  apply(FukuSW.seasonal.chem[, grep("^(ave|cv|max)", names(FukuSW.seasonal.chem))],
        c(1,2), function(x) ifelse(is.nan(x), NA, x))



#################### add in water temp data ####################

#probe data (including water temp) sampled at different depths than chem data

#want water temp, avg. across depth range for each sample date
#water temp within a sample date, from min(chemdepth) to max(chemdepth)


#rename data frame to use

FukuNE.tmp <- FukuNE.pr

FukuSW.tmp <- FukuSW.pr


#remove Secchi column

FukuNE.tmp$Secchi <- NULL

FukuSW.tmp$Secchi <- NULL



## Aggregate

FukuNE.tmp.agg <- FukuNE.tmp %>%
  ## keep only dates of interest -- those that match chemistry data
  filter(Date %in% FukuNE.chem.full$Date) %>%
  ## join with photic depth info and keep only rows from within photic depth
  merge(FukuNE.pr.pd) %>%
  filter(Depth <= PhoticDepth) %>%
  ## add year column and aggregate by season within each year
  mutate(year = year(Date),
         season = ifelse(yday(Date) >= 105 & month(Date) < 11, "iceoff", "iceon")) %>%
  group_by(season, year) %>%
  summarize(watertemp = mean(Temp, na.rm = TRUE))

FukuSW.tmp.agg <- FukuSW.tmp %>%
  ## keep only dates of interest -- those that match chemistry data
  filter(Date %in% FukuSW.chem.full$Date) %>%
  ## join with photic depth info and keep only rows from within photic depth
  merge(FukuSW.pr.pd) %>%
  filter(Depth <= PhoticDepth) %>%
  ## add year column and aggregate by season within each year
  mutate(year = year(Date),
         season = ifelse(yday(Date) >= 105 & month(Date) < 11, "iceoff", "iceon")) %>%
  group_by(season, year) %>%
  summarize(watertemp = mean(Temp, na.rm = TRUE))


## Combine with ice/chem df

FukuNE.agg.final <- merge(FukuNE.seasonal.chem, FukuNE.tmp.agg, by = c("year", "season"))

FukuSW.agg.final <- merge(FukuSW.seasonal.chem, FukuSW.tmp.agg, by = c("year", "season"))


############################# finalize df ###################################

#make start and end day, month, and year columns
#convert month to abbrev

FukuNE.agg.final <- mutate(FukuNE.agg.final,
                          startday = day(StartDate),
                          startmonth = month.abb[month(StartDate)],
                          startyear = year(StartDate),
                          endday = day(EndDate),
                          endmonth = month.abb[month(EndDate)],
                          endyear = year(EndDate))

FukuNE.agg.final <- select(FukuNE.agg.final, -StartDate, -EndDate)


FukuSW.agg.final <- mutate(FukuSW.agg.final,
                           startday = day(StartDate),
                           startmonth = month.abb[month(StartDate)],
                           startyear = year(StartDate),
                           endday = day(EndDate),
                           endmonth = month.abb[month(EndDate)],
                           endyear = year(EndDate))

FukuSW.agg.final <- select(FukuSW.agg.final, -StartDate, -EndDate)


######## add rest of columns and reorder to match under ice template ########

#Fuchskuhle NE basin

FuchskuhleNE.agg.final <- FukuNE.agg.final %>% 
  mutate(researcher = "Hans Peter Grossart", 
         lakename = "Lake Grosse Fuchskuhle (NE Basin)",
         lakeregloc = "Brandenburg",
         lakecountry = "Germany", 
         stationname = "Fuchskuhle NE",
         lakearea = 0.02, 
         lakemeandepth = 3, 
         lakemaxdepth = 5.6, 
         lakeelevation = 60, 
         watershedarea = 0.005,
         stationlat = 53.1, 
         stationlong = 13.02, 
         multiplestations = "no",
         samplenarrat = "photic depth calculated: ln(1000)/(1.7/Secchi)",
         sampletype = "in situ",
         icenarrat = paste("Ice-on condition if ice + snow >= 80% coverage; ice-free period",
                           "estimated based on temperature profiles (consistently stratified months",
                           "across years)"), 
         sidata = "no",
         fadata = "no",
         gutdata = "no",
         profiles = "yes, raw data sent in")

## All remaining columns should be NAs
FuchskuhleNE.agg.final[, datafields[!datafields %in% names(FuchskuhleNE.agg.final)]] <- NA

#re-order columns 

FuchskuhleNE.agg.final <- FuchskuhleNE.agg.final[, datafields]


#Fuchskuhle SW basin

FuchskuhleSW.agg.final <- FukuSW.agg.final %>% 
  mutate(researcher = "Hans Peter Grossart", 
         lakename = "Lake Grosse Fuchskuhle (SW Basin)",
         lakeregloc = "Brandenburg",
         lakecountry = "Germany", 
         stationname = "Fuchskuhle SW",
         lakearea = 0.02, 
         lakemeandepth = 3, 
         lakemaxdepth = 5.6, 
         lakeelevation = 60, 
         watershedarea = 0.005,
         stationlat = 53.1, 
         stationlong = 13.02, 
         multiplestations = "no",
         samplenarrat = "photic depth calculated: ln(1000)/(1.7/Secchi)",
         sampletype = "in situ",
         icenarrat = paste("Ice-on condition if ice + snow >= 80% coverage; ice-free period",
                           "estimated based on temperature profiles (consistently stratified months",
                           "across years)"), 
         sidata = "no",
         fadata = "no",
         gutdata = "no",
         profiles = "yes, raw data sent in")

## All remaining columns should be NAs
FuchskuhleSW.agg.final[, datafields[!datafields %in% names(FuchskuhleSW.agg.final)]] <- NA

#re-order columns 

FuchskuhleSW.agg.final <- FuchskuhleSW.agg.final[, datafields]

