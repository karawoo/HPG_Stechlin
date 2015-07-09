setwd("C://UnderIce/HansPeter")

#import aggregate files

Stechlin.Agg <- read.csv("HPGRepo/StechlinAggregate_Final.csv", stringsAsFactors = FALSE)

FukuNE.Agg <- read.csv("HPGRepo/FukuNEAgg.csv", stringsAsFactors = FALSE)

FukuSW.Agg <- read.csv("HPGRepo/FukuSWAgg.csv", stringsAsFactors = FALSE)


#stack dataframes to get one HPG file

HPG_all <- rbind(Stechlin.Agg, FukuNE.Agg, FukuSW.Agg)

#rearrange so that Fuchskuhle lakes of same year follow

HPG_all <- arrange(HPG_all, year, stationname, season)


#write csv

write.csv(HPG_all, file = "grossart_germanlakes_1996-2010s.csv", row.names = FALSE)



