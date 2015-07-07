###############################################################
################## Hans-Peter Grossart data ###################
############# iceon aggregation for Lake Stechlin #############
###############################################################

# This code takes the HPG raw chemistry, probe, and ice days data provided by
# HPG for Lake Stechlin and converts, reorganizes, and aggregates the data
# to meet the specifications of the under ice ecology template and instructions

library("icetest")

#### set working directory ####


setwd("C://UnderIce/HansPeter")


###################### Stechlin chemistry data ##############################

# for ease of importing and working with the chemistry data in R,
# rows of text denoting lake name, etc. at the top and bottom of the spreadsheet
# were removed prior to importing


Stech.chem <- read.csv("hans_peter_grossart_data_DONOTSHARE/Stechlin_chemistry_1990.2015.csv", stringsAsFactors = FALSE)


#create new dataframe with date, TP, TN, all N, DOC, and Chla

vars <- c("Datum", "Tiefe..m.", "TP..mg.l.", "TN..mg.l.", "NO2..mg.l.",
          "NO3..mg.l.", "NH4..mg.l.", "DOC..mg.l.", "Chla..mg.l.")

Stech.vars <- Stech.chem[vars]


#ensure date is in date format

Stech.vars$Datum <- as.Date(Stech.vars$Datum, "%m/%d/%Y")


#need to change variables to be correct units:

#TP mg/l needs to be ug/l (1000 ug in one mg) -- multiply by 1000
#TN mg/l needs to be ug/l
#all N values need to be ug/l
#chla needs to be ug/l

### Doc stays mg/l

library(dplyr)

Stech.calc <- mutate(Stech.vars,
                     TP = (TP..mg.l. * 1000),
                     TN = (TN..mg.l. * 1000),
                     NO2 = (NO2..mg.l. * 1000),
                     NO3 = (NO3..mg.l. * 1000),
                     NH4 = (NH4..mg.l. * 1000),
                     Chla = (Chla..mg.l. * 1000))


#keep only corrected variables 

keep <- c("Datum", "Tiefe..m.", "TP", "TN", "NO2", "NO3", "NH4", "DOC..mg.l.", "Chla")

Stech.cor <- Stech.calc[keep]


#rename variables from German to English

Stech.cor <- rename(Stech.cor, Date = Datum, Depth = Tiefe..m., DOC = DOC..mg.l.)


#find total dissolved N (NO2+NO3+NH4)
## Note that this will result in NAs if any of the three columns contains an NA.

Stech.cor <- mutate(Stech.cor,
                    totdissN = (NO2 + NO3 + NH4))


#remove individual N components

keep2 <- c("Date", "Depth", "TP", "TN", "DOC", "Chla", "totdissN")

Stech.go <- Stech.cor[keep2]


########################## secchi and photicdepth data #######################################

# As with the chemistry data, rows of text at top and bottom
# of spreadsheet were removed prior to importing


Stech.probe <- read.csv("St_1990-2015_probe.csv", stringsAsFactors = FALSE,
                        fileEncoding = "latin1")


#rename columns from German to English

Stech.pr.cor <- rename(Stech.probe, Date = Datum, Secchi = Sichttiefe..m.,
                       Depth = Tiefe..m., Temp = Temp....C.)

#secchi is in m
#depth is in m
#temp is in degrees C


#keep only columns of interest
#note: a subset of these variables will be used later

vars.pr <- c("Date", "Secchi", "Depth", "Temp")

Stech.pr <- Stech.pr.cor[vars.pr]


#ensure that date is a date object

Stech.pr$Date <- as.Date(Stech.pr$Date, "%m/%d/%Y")


#add new column "photicdepth" based on secchi
#kd = 1.7/secchi
#photic depth = ln(1000)/kd

Stech.pr.full <- Stech.pr %>% 
  mutate(PhoticDepth = log(1000)/(1.7/Secchi))


####################### merge photicdepth to chem data by date ##############################

#want just photicdepth to merge, so subset df 
#to include only date, secchi, and photicdepth
#keep only unique rows
#remove any rows where photicdepth is NA

Stech.pr.pd <- select(Stech.pr.full, Date, Secchi, PhoticDepth) %>% 
  unique() %>% 
  filter(!is.na(PhoticDepth))


#merge this new df with chem df
#only keep overlapping dates

Stech.pr.chem <- merge(Stech.pr.pd, Stech.go, by = "Date")


#only interested in values within the photic depth
#if Depth is greater than photicdepth, converts to NA, and is filtered out
#note: this works because there are originally no NAs in depth

Stech.pr.chem <- Stech.pr.chem %>% 
  group_by(Date) %>% 
  mutate(Depth = ifelse(Depth > PhoticDepth, NA, Depth)) %>% 
  filter(!is.na(Depth))


#now the Stechlin chemistry df has chemistry columns and photicdepth and secchi



############################# Stechlin ice data ######################################

# for ease of importing, the Stechlin Ice Days sheet of the ice
# spreadsheet provided by HPG was copied to be its own document
# and translated from German to English


Stech.ice <- read.csv("StechlinIceDays.csv", stringsAsFactors = FALSE)


#ensure date is a date 

Stech.ice$Date <- as.Date(Stech.ice$Date, "%m/%d/%Y")


#only include dates that will overlap with chem data (1990 onward)

Stech.ice.filt <- filter(Stech.ice, Date >= "1990-01-01")

## This script takes the ice dates that Hans-Peter Grossart sent us for Lake
## Stechlin and expands the data frame of dates so that there is a row for each
## date in each year. It then uses na.locf() to fill in ice percentages for each
## date (a highly imperfect strategy, but good enough for us to figure out which
## dates to aggregate into an ice-on aggregation).

# Note that this is different than the Fuchskuhle ice day strategy.
# The existing Stechlin data is nearly complete (covers most days)
# so rather than create a winteryear variable and fill in
# (here we do not have a precise winter range ahead of time)
# the whole year will be filled in
# and data is later filtered to include only days with >+80% snow+ice


library("lubridate")
library("zoo")
  

## Find the min and max dates each year
startend <- Stech.ice.filt %>%
  group_by(year(Date)) %>%
  #by year get min date, max date, and diff
  summarize(min = min(Date), max = max(Date), diff = max - min)

#the 1990 year doesn't start until November
#for 1997, last day sampled (end of dates we have) is 2/28/1997

## Create sequences of dates for each season
#this creates a list, one for each year
#fills in gaps in dates so there are all days accounted for in each year 
#(within start/end ranges sampled for each year)

seqs <- mapply(function(x, y) seq(x, y, by = "day"), startend$min, startend$max)


## Make the dates into a data frame
alldates <- data.frame(Date = do.call("c", seqs)) 

## Merge the data frame back in with Stech.ice.filt and use na.locf to fill in missing values

Stech.ice.full <- merge(Stech.ice.filt, alldates, by="Date", all = TRUE) %>%
  mutate(Ice = na.locf(Ice), #fills in ice values (runs last value forward through NAs until hit next value)
         Snow = na.locf(Snow))#fills in snow values 


######################## merge ice and chem/pr data ##############################

#only keep overlapping dates (dates in BOTH ice and chem data frames)
#resulting dates will fall between 1990-1997
#because ice data only goes to 1997
#while chem data goes from 1990-2015


Stech.ice.chem <- merge(Stech.ice.full, Stech.pr.chem, by="Date")

#390 observations


#checking merge:
#ice data starts 11/1/1990 and only goes through 2/28/1997
#so merge should have same number of obs. as Stech.pr.chem filtered by these dates

test2 <- filter(Stech.pr.chem, Date >= "1990-11-01" & Date < "1997-03-01")

#390 obs, same as Stech.ice.chem


#filter to only include rows with ice+snow >= 80
#the 80% cut off is HPG definition of "ice-on"

Stech.ice.chem.filt <- filter(Stech.ice.chem, Ice + Snow >= 80)

#there are 4 rows total, all from 1996-02-14
#ice is listed as zero but snow at 100%
#have discussed that snow probably counts as cover 
#likely covering up ice, so snow is included in 80% coverage cut-off
#for "ice-on" season

#this is a pretty low number of final observations
#but looking at the raw data, the winters of 1993, 1994, and 1995
#(when have chem data) seem to be mostly icefree
#additionally, there just aren't many chem samples from the months Jan/Feb
#which would meet ice+snow iceon conditions


#################### add in water temp data ####################

#probe data (including water temp) sampled at different depths than chem data

#want water temp, avg. across depth range for each sample date
#water temp within a sample date, from min(chemdepth) to max(chemdepth)

#rename data frame to use

Stech.tmp <- Stech.pr

#remove Secchi column

Stech.tmp$Secchi <- NULL

#rename Depth column to distinguish from chem depth

## Keep only dates of interest
Stech.tmp <- filter(Stech.tmp, Date %in% Stech.ice.chem.filt$Date)

## combine with ice/chem df
Stech.ice.chem.tmp <- rbind_list(Stech.ice.chem.filt, Stech.tmp)

## compute seasonal averages for chemistry and temp data
Stech.iceon.final <- Stech.ice.chem.tmp %>%
  group_by(year(Date)) %>%
  filter(Depth <= max(PhoticDepth, na.rm = TRUE)) %>%
  summarize(StartDate = min(Date), 
            EndDate = max(Date),
            periodn = length(unique(Date)),
            photicdepth = mean(PhoticDepth, na.rm = TRUE), #average photic depth for sample period
            stationdepth = NA,
            sampledepth = max(Depth),
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
            maxchla = max(Chla, na.rm = TRUE),
            watertemp = mean(Temp, na.rm = TRUE)) %>%
  ## Keep columns that are present in the ice data template,
  ## plus StartDate and EndDate
  ## (`datafields` is defined in the icetest package, which must be loaded)
  select_(.dots = names(.)[names(.) %in% c(datafields, "StartDate", "EndDate")]) %>%
  ## Add year column
  mutate(year = year(EndDate))

#mean water temp double checked in excel
#2.6 is indeed the average temp within 0-20 m (photic zone) for 2/14/1996 (sample date)


############################# finalize df ###################################

#make start and end day, month, and year columns
#convert month to abbrev

Stech.iceon.final <- mutate(Stech.iceon.final,
                            startday = day(StartDate),
                            startmonth = month.abb[month(StartDate)],
                            startyear = year(StartDate),
                            endday = day(EndDate),
                            endmonth = month.abb[month(EndDate)],
                            endyear = year(EndDate))

Stech.iceon.final <- select(Stech.iceon.final, -StartDate, -EndDate)


#add rest of columns and reorder to match under ice template


Stechlin.iceon.final <- Stech.iceon.final %>% 
    mutate(season = "iceon",
          researcher = "Hans Peter Grossart", 
          lakename = "Lake Stechlin",
          lakeregloc = "Brandenburg",
          lakecountry = "Germany", 
          lakearea = 4.52, 
          lakemeandepth = 24, 
          lakemaxdepth = 69.5, 
          lakeelevation = 60, 
          watershedarea = 12.4, 
          h2oresidence = NA, 
          lakefetch = NA,
          stationdistance = NA, 
          stationname = NA, 
          stationlat = 53.15, 
          stationlong = 13.03, 
          multiplestations = "no",
          samplenarrat = "photic depth calculated: ln(1000)/(1.7/Secchi)",
          sampletype = "in situ",
          icedepth = NA,
          snowdepth = NA,
          iceduration = NA,
          icenarrat = "Ice-on condition if ice + snow >= 80% coverage",
          sidata = "no",
          fadata = "no",
          gutdata = "no",
          foodwebnarrat = NA,
          airtemp = NA,
          averadiation = NA,
          cvradiation = NA,
          avesuva = NA,
          cvsuva = NA,
          maxsuva = NA,
          avecolor = NA,
          cvcolor = NA,
          maxcolor = NA,
          waterchemnarrat = NA,
          profiles = "yes, raw data sent in",
          avephytoppr = NA,
          maxphytoppr = NA,
          phytopprunit = NA,
          phytopprnarrat = NA,
          phytomethod = NA,
          avephytomass = NA,
          cvphytomass = NA,
          maxphytomass = NA,
          avephytocount = NA,
          cvphytocount = NA,
          maxphytocount = NA,
          prochloro = NA,
          procrypto = NA,
          procyano = NA,
          propdiatom = NA,
          propdino = NA,
          propotherphyto = NA,
          aveciliamass = NA,
          cvciliamass = NA,
          maxciliamass = NA,
          aveciliacount = NA,
          cvciliacount = NA,
          maxciliacount = NA,
          avehnfmass = NA,
          cvhnfmass = NA,
          maxhnfmass = NA,
          avehnfcount = NA,
          cvhnfcount = NA,
          maxhnfcount = NA,
          zoopmethod = NA,
          avezoopmass = NA,
          cvzoopmass = NA,
          maxzoopmass = NA,
          avezoopcount = NA,
          cvzoopcount = NA,
          maxzoopcount = NA,
          propdaphnia = NA,
          propothercladoc = NA,
          propcyclopoid = NA,
          propcalanoid = NA,
          proprotifer = NA,
          propotherzoop = NA,
          avebactcount = NA,
          cvbactcount = NA,
          maxbactcount = NA,
          avebactprod = NA,
          cvbactprod = NA,
          maxbactprod = NA,
          bactprodunit = NA,
          bacprodmethod = NA,
          baccompnarrat = NA,
          bensubstrate = NA,
          avebenalgalmass = NA,
          cvbenalgalmass = NA,
          maxbenalgalmass = NA,
          avebenchla = NA,
          cvbenchla = NA,
          maxbenchla = NA,
          macrophytemass = NA,
          benamphdens = NA,
          bengastrodens = NA,
          benbivalvedens = NA,
          beninsectdens = NA,
          benoligodens = NA,
          fishnarrat = NA)



#re-order columns 

Stechlin.iceon <- Stechlin.iceon.final[c(
"year",
"season",
"researcher",
"lakename",
"lakeregloc",
"lakecountry",
"lakearea",
"lakemeandepth", 
"lakemaxdepth",
"lakeelevation",
"watershedarea", 
"h2oresidence", 
"lakefetch",
"stationdistance", 
"stationname",
"stationdepth",
"stationlat",
"stationlong",
"multiplestations",
"startday",
"startmonth",
"startyear",
"endday",
"endmonth",
"endyear",
"iceduration",
"periodn",
"samplenarrat",
"sampletype",
"photicdepth",
"sampledepth",
"icedepth",
"snowdepth",
"icenarrat",
"sidata",
"fadata",
"gutdata",
"foodwebnarrat",
"watertemp",
"airtemp",
"averadiation",
"cvradiation",
"avesecchidepth",
"cvsecchidepth",
"avetotphos",
"cvtotphos",
"maxtotphos",
"avetotdissphos",
"cvtotdissphos",
"maxtotdissphos",
"avetotnitro",
"cvtotnitro",
"maxtotnitro",
"avetotdissnitro",
"cvtotdissnitro",
"maxtotdissnitro",
"avetotdoc",
"cvtotdoc",
"maxtotdoc",
"avesuva",
"cvsuva",
"maxsuva",
"avecolor",
"cvcolor",
"maxcolor",
"avechla",
"cvchla",
"maxchla",
"waterchemnarrat",
"profiles",
"avephytoppr",
"maxphytoppr",
"phytopprunit",
"phytopprnarrat",
"phytomethod",
"avephytomass",
"cvphytomass",
"maxphytomass",
"avephytocount",
"cvphytocount",
"maxphytocount",
"prochloro",
"procrypto",
"procyano",
"propdiatom",
"propdino",
"propotherphyto",
"aveciliamass",
"cvciliamass",
"maxciliamass",
"aveciliacount",
"cvciliacount",
"maxciliacount",
"avehnfmass",
"cvhnfmass",
"maxhnfmass",
"avehnfcount",
"cvhnfcount",
"maxhnfcount",
"zoopmethod",
"avezoopmass",
"cvzoopmass",
"maxzoopmass",
"avezoopcount",
"cvzoopcount",
"maxzoopcount",
"propdaphnia",
"propothercladoc",
"propcyclopoid",
"propcalanoid",
"proprotifer",
"propotherzoop",
"avebactcount",
"cvbactcount",
"maxbactcount",
"avebactprod",
"cvbactprod",
"maxbactprod",
"bactprodunit",
"bacprodmethod",
"baccompnarrat",
"bensubstrate",
"avebenalgalmass",
"cvbenalgalmass",
"maxbenalgalmass",
"avebenchla",
"cvbenchla",
"maxbenchla",
"macrophytemass",
"benamphdens",
"bengastrodens",
"benbivalvedens",
"beninsectdens",
"benoligodens",
"fishnarrat")]
