##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

require(rjson)


getCoordinates <- function(address) {
    ##
    ## Inputs:
    ##   address [string]: an address that Google Maps can find.
    ## Outputs:
    ##   coords [vector of ints]: coordinates associated to the given address.
    ##
    url <- paste(
        "http://maps.google.com/maps/api/geocode/json?address=",
        address, "&sensor=false", sep = "")
    map_data <- fromJSON(paste(readLines(url), collapse = ""))
    if (map_data$status == "OK") {
        coords <- cbind(
            map_data$results[[1]]$geometry$location$lat,
            map_data$results[[1]]$geometry$location$lng
        )
    } else {
        coords <- NULL
    }
    return(coords)
}
