##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

rm(list=ls(all=TRUE))
setwd("~/Projects/itam/economics_thesis")
source("./integration/functions/getCoordinates.R")
## Sys.setlocale("LC_ALL")
Sys.setlocale(locale="C")

data <- read.csv("./data/2010_2015_aggregated.csv")

## Add latitude and longitude
data <- cbind(data, matrix(NA, nrow(data), 2))
colnames(data)[(ncol(data)-1):ncol(data)] <- c("latitude", "longitude")

counter <- 1
for (i in counter:nrow(data)) {
    if (any(is.na(data$latitude[i]), is.na(data$longitude[i]))) {
        tryCatch({
            address <- paste(
                gsub(" ", "+", data$NOM_MUN[i], fixed = TRUE), ",+",
                gsub(" ", "+", data$NOM_ENT[i], fixed = TRUE), ",+",
                "Mexico",
                sep = ""
            )
        }, error = function(e) {
            cat(sprintf("[% 5d] %s ERROR!\n", i, address))
        })
        cat(sprintf("[% 5d] %s \n", i, address))

        ## Encoding problems
        address <- gsub("\xe1", "á", address)
        address <- gsub("\xe9", "é", address)
        address <- gsub("\xed", "í", address)
        address <- gsub("\xfe", "ó", address)
        ## address <- gsub("", "ú", address)
        ## address <- gsub("", "ñ", address)
        address <- gsub("\xd1", "Ñ", address)

        tryCatch({
            ## For unstable connections
            coords  <- getCoordinates(address)
            counter <- counter + 1
            if (!is.null(coords)) {
                data[i, c("latitude", "longitude")] <- coords
            }
        }, error = function(e) {
            print(paste("[!] ERROR: ", e))
        })
    }
    if (counter %% 1000 == 0) {
        write.csv(
            data,
            file = "./data/2010_2015_geocoded.csv",
            row.names = FALSE
        )
    }
}

write.csv(
    data,
    file = "./data/2010_2015_geocoded.csv",
    row.names = FALSE
)
