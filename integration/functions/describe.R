##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

describe <- function(data, string_entity, variables_to_keep) {
    n_non_variables <- 4
    entity_description <- data.frame(
        ENT=NA,
        N_OBS=NA,
        N_COLS=NA,
        N_REQ_COLS=NA,
        NO_NA=NA,
        ATLT1_NA=NA,
        ALL_NA=NA
    )
    entity_description[1, "ENT"] <- string_entity
    entity_description[1, "N_OBS"] <- nrow(data)
    entity_description[1, "N_COLS"] <- ncol(data)
    data <- data[, variables_to_keep]

    ## Number of required columns
    entity_description[1, "N_REQ_COLS"] <- length(variables_to_keep)
    ## No NAs
    entity_description[1, "NO_NA"] <- nrow(na.omit(data))
    ## At least one NA
    entity_description[1, "ATLT1_NA"] <- nrow(data) - nrow(na.omit(data))
    ## All NAs
    entity_description[1, "ALL_NA"] <- nrow(
        data[rowSums(is.na(data)) > (ncol(data) - n_non_variables), ]
    )

    return(entity_description)
}
