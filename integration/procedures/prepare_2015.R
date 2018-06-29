##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##
## Note: data is available only in ODESKTOP (~42 GB)
##

rm(list=ls(all=T))
setwd("~/Projects/itam/internet_wages/")
source("./integration/functions/integrate.R")
source("./integration/functions/indicate_highs.R")

high_variables <- c(
    "NIVACAD",
    "NUMPERS",
    "INGTRMEN",
    "INTERNET"
    ## "CELULAR",
    ## "TELEFONO",
    ## "AUTOPROPIO",
    ## "ELECTRICIDAD"
)

##
## Personas
##
variables_to_keep <- c(
    "ENT",
    "NOM_ENT",
    "MUN",
    "NOM_MUN",
    "INGTRMEN",
    "NIVACAD",
    "EDAD",
    "HLENGUA"
)
data_personas <- integrate(
    "2015",
    "./data/2015/personas/personas_",
    variables_to_keep,
    high_variables,
    2015
)

##
## Viviendas
##
variables_to_keep <- c(
    "ENT",
    "NOM_ENT",
    "MUN",
    "NOM_MUN",
    "INTERNET",
    "COMPUTADORA",
    "NUMPERS",
    "TELEFONO",
    "CELULAR",
    "AUTOPROPIO",
    "ELECTRICIDAD",
    "ING_AYUGOB",
    "ING_OTROPAIS",
    "ING_DENTROPAIS"
)
data_viviendas <- integrate(
    "2015",
    "./data/2015/viviendas/viviendas_",
    variables_to_keep,
    high_variables,
    2015
)

##
## JOIN
##
data <- merge(data_personas, data_viviendas, by=c("ENT", "NOM_ENT", "MUN", "NOM_MUN"))
data <- indicate_high_all(data, high_variables)
colnames(data) <- paste(colnames(data), "_2015", sep="")

colnames(data)[1] <- "ENT"
colnames(data)[2] <- "NOM_ENT"
colnames(data)[3] <- "MUN"
colnames(data)[4] <- "NOM_MUN"

print("[+] Writing to CSV... AGGREGATED CENSO 2015.")

write.csv(
    data,
    "./data/2015/aggregated.csv",
    row.names=FALSE
)
