###############################################################
################## Hans-Peter Grossart data ###################
############# iceoff aggregation for Lake Stechlin #############
###############################################################

# This code takes the HPG raw chemistry and probe data provided by
# HPG for Lake Stechlin and converts, reorganizes, and aggregates the data
# to meet the specifications of the under ice ecology template and instructions


#### set working directory ####


setwd("C://UnderIce/HansPeter")


###################### Stechlin chemistry data ##############################

# for ease of importing and working with the chemistry data in R,
# rows of text denoting lake name, etc. at the top and bottom of the spreadsheet
# were removed prior to importing


Stech.chem <- read.csv("hans_peter_grossart_data_DONOTSHARE/Stechlin_chemistry_1990.2015.csv", stringsAsFactors = FALSE)


#create new dataframe with date, TP, TN, all N, DOC, and Chla

vars <- c("Datum", "Tiefe..m.", "TP..mg.l.", "TN..mg.l.", "NO2..mg.l.", "NO3..mg.l.", "NH4..mg.l.", "DOC..mg.l.", "Chla..mg.l.")

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

Stech.cor <- mutate(Stech.cor,
                    totdissN = (NO2 + NO3 + NH4))


#remove individual N components

keep2 <- c("Date", "Depth", "TP", "TN", "DOC", "Chla", "totdissN")

Stech.go <- Stech.cor[keep2]



########################## secchi and photicdepth data #######################################

# As with the chemistry data, rows of text at top and bottom
# of spreadsheet were removed prior to importing


Stech.probe <- read.csv("hans_peter_grossart_data_DONOTSHARE/St_1990-2015_probe.csv", stringsAsFactors = FALSE)


#rename columns from German to English

Stech.pr.cor <- rename(Stech.probe, Date = Datum, Secchi = Sichttiefe..m., Depth = Tiefe..m., Temp = Temp....C.)

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



###################### Stechlin summer stratification #######################

############ profiles ##############

Stech.pr.pro <- Stech.pr

Stech.pr.pro$Secchi = NULL


## Function to plot profiles for each date in a given year

library(ggplot2)
library(scales)
library(lubridate)

st_prof <- function(data, year) {
  data %>%
    group_by(Date) %>%
    filter(n() >= 2) %>% ## Remove data when there are just a couple rows
    ungroup() %>%
    filter(year(Date) == year) %>%
    ggplot(aes(x = Temp, y = Depth)) +
    geom_path() +
    scale_y_reverse() +
    facet_wrap(~ Date)  
} 

## Create a PDF with temp profiles for each date
pdf("hp_plots_stechlin.pdf")
for (i in seq_along(unique(year(Stech.pr.pro$Date)))) {
  print(st_prof(Stech.pr, unique(year(Stech.pr.pro$Date))[i]))
}
dev.off()

## looking for relatively consistent timing across years
## ultimately selected May - Oct. as summer stratification period
## best range of months to be consistent over range of years


#so subset chem data for between May - Oct. of each year

Stech.chem.summer <- Stech.pr.chem %>% 
                    group_by(year(Date)) %>% 
                    filter(month(Date) >= 5 & month(Date) < 11)



############################# begin aggregating #####################################

#grouped by year
#will end up with one row per year (for this iceon season)
#with start and end dates for iceon period

Stech.chem.summer.agg <- Stech.chem.summer %>% 
  ungroup %>% 
  group_by(year(Date)) %>% 
  mutate(StartDate = min(Date), 
         EndDate = max(Date),
         periodn = length(Date),
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
         maxchla = max(Chla, na.rm = TRUE)) 


#deal with the NaNs created
#replace any NaNs in any ave/cv/max column with NA

Stech.chem.summer.agg[, grep("^(ave|cv|max)", names(Stech.chem.summer.agg))] <- 
                  apply(Stech.chem.summer.agg[, grep("^(ave|cv|max)", names(Stech.chem.summer.agg))],
                                   c(1,2), function(x) ifelse(is.nan(x), NA, x))


#then remove excess columns - all created columns have all the data they need 

keep.full <- c("Date", "Depth", "photicdepth", "stationdepth", "sampledepth", "StartDate", "EndDate", 
               "periodn", "avesecchidepth", "cvsecchidepth", "avetotphos", "cvtotphos", "maxtotphos",
               "avetotdissphos", "cvtotdissphos", "maxtotdissphos", "avetotnitro", "cvtotnitro",
               "maxtotnitro", "avetotdissnitro", "cvtotdissnitro", "maxtotdissnitro", "avetotdoc",
               "cvtotdoc", "maxtotdoc", "avechla", "cvchla", "maxchla")

Stech.chem.summer.full <- Stech.chem.summer.agg[keep.full]
                        


#################### add in water temp data ####################

#probe data (including water temp) sampled at different depths than chem data

#want water temp, avg. across depth range for each sample date
#water temp within a sample date, from min(chemdepth) to max(chemdepth)

#rename data frame to use

Stech.tmp <- Stech.pr

#remove Secchi column

Stech.tmp$Secchi <- NULL

#rename Depth column to distinguish from chem depth

Stech.tmp <- rename(Stech.tmp, Depth.pr = Depth)

#merge with full summer chem df
#keep chem/summer rows even if no matching temp

Stech.summer.full <- merge(Stech.chem.summer.full, Stech.tmp, by = "Date", all.x = TRUE, all.y = FALSE)

#find average temp of samples less than or equal to sampledepth (max depth sampled within photic depth range)

#within a StartDate
#edit merged df to remove rows where Depth.pr is greater than sample depth
#find avg. temp of remaining rows
#remove temp and depth.pr columns
#keep only unique values (remove repeat rows)

Stech.summer.full <- Stech.summer.full %>% 
  group_by(StartDate) %>% 
  mutate(Depth.pr = ifelse(Depth.pr > sampledepth, NA, Depth.pr)) %>% 
  filter(!is.na(Depth.pr)) %>% 
  mutate(watertemp = mean(Temp)) %>% #avg. temp over depth range within photic range
  select(-Temp, -Depth.pr) %>% #remove temp and depth.pr
  unique() %>% #only unique values
  ungroup()


#mean water temp double checked in excel
#8.99444 is indeed the avg. temp for 5/5/2015 within depth range within the photic range


#result is df with avg water temp
#only column with differences among rows is depth (chem depth)

#clean up df:
#keep only row where Depth = sampledepth
#(only need one row per season, this is just a simple way to pick a row - other than depth col, they are identical)
#remove Ice, Snow, Depth, and Date
#rename PhoticDepth to lowercase
#add a year column
#remove Date column
#keep only unique rows

Stech.summer.final <- Stech.summer.full %>% 
  filter(Depth == sampledepth) %>% 
  select(-Depth) %>% 
  mutate(year = year(EndDate)) %>% 
  select(-Date) %>% 
  unique()

Stech.summer.final <- as.data.frame(Stech.summer.final)

############################# finalize df ###################################

#make start and end day, month, and year columns
#convert month to abbrev

Stech.icefree.full <- mutate(Stech.summer.final, startday = day(StartDate), startmonth = month(StartDate), 
                            startyear = year(StartDate), endday=day(EndDate), endmonth = month(EndDate),
                            endyear = year(EndDate))

Stech.icefree.full$startmonth <- month.abb[Stech.icefree.full$startmonth]

Stech.icefree.full$endmonth <- month.abb[Stech.icefree.full$endmonth]

Stech.icefree.full <- select(Stech.icefree.full, -StartDate, -EndDate)


#add rest of columns and reorder to match under ice template


Stechlin.icefree.final <- Stech.icefree.full %>% 
  mutate(season = "icefree",
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
         icenarrat = NA,
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

Stechlin.icefree <- Stechlin.icefree.final[c(
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

