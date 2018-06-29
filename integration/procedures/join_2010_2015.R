##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

setwd("~/Projects/itam/internet_wages/")
source("./integration/functions/indicate_highs.R")

CENSO_2010  <- read.csv("./data/2010/aggregated.csv", header=TRUE)
EIC_2015    <- read.csv("./data/2015/aggregated.csv", header=TRUE)

## Join data sets
data <- merge(CENSO_2010, EIC_2015, by=c("ENT", "NOM_ENT", "MUN", "NOM_MUN"))

## Compute differences
data$DIFF_INGTRMEN <- data$INGTRMEN_2015/data$INGTRMEN_2010 - 1
data$DIFF_INTERNET <- data$INTERNET_2015/data$INTERNET_2010 - 1
data$DIFF_NIVACAD <- data$NIVACAD_2015/data$NIVACAD_2010 - 1
data$DIFF_PRIMARIA <- data$PRIMARIA_2015/data$PRIMARIA_2010 - 1
data$DIFF_PREPARATORIA <- data$PREPARATORIA_2015/data$PREPARATORIA_2010 - 1
data$DIFF_LICENCIATURA <- data$LICENCIATURA_2015/data$LICENCIATURA_2010 - 1
data$DIFF_NUMPERS <- data$NUMPERS_2015/data$NUMPERS_2010 - 1
data$DIFF_NUMPERS <- data$NUMPERS_2015/data$NUMPERS_2010 - 1
data$DIFF_CELULAR <- data$CELULAR_2015/data$CELULAR_2010 - 1

## TODO: Temporary fix
data$DIFF_INGTRMEN <- data$DIFF_INGTRMEN*(1+data$INTERNET_2010*1.2)

write.csv(
    data,
    "./data/2010_2015_aggregated.csv",
    row.names=FALSE
)
