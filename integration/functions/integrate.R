##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

source("./integration/functions/aggregate.R")
source("./integration/functions/clean_data.R")
source("./integration/functions/describe.R")
source("./integration/functions/entity_to_two_digit_string.R")
source("./integration/functions/indicate_highs.R")
source("./integration/functions/joins.R")
source("./integration/functions/read_DB.R")
source("./integration/functions/recode_variables.R")


integrate <- function(description, path_to_DBs, variables_to_keep, high_variables, year) {
    ##
    ## Inputs:
    ## - description [string]
    ## - path_to_DBs [string]
    ## - variables_to_keep [vector of strings]
    ## - year [int]
    ##
    data <- NULL
    n_entities <- 32

    mun_description <- data.frame(
        ENT=NA,
        N_OBS=NA,
        N_COLS=NA,
        N_REQ_COLS=NA,
        NO_NA=NA,
        ATLT1_NA=NA,
        ALL_NA=NA
    )

    for (entity in 1:n_entities) {
        temp_data <- NULL
        string_entity  <- entity_to_two_digit_string(entity)
        print(paste("[+] ", description, ", entidad: ", string_entity, sep=""))
        temp_data <- read_DB(description, path_to_DBs, string_entity)
        temp_data <- recode_variables(temp_data, year)
        mun_description[entity, ] <- describe(temp_data, string_entity, variables_to_keep)
        temp_data <- temp_data[, variables_to_keep]
        temp_data <- clean_data(temp_data, variables_to_keep, year)
        temp_data <- aggregate(temp_data, variables_to_keep, year)
        data <- join_municipality_data(data, temp_data, description, entity)
    }
    data <- indicate_highs(data, variables_to_keep, high_variables, year)

    print("")
    print(paste("[+] Variables: ", toString(ncol(data)), sep=""))
    print(paste("[+] Observations: ", toString(nrow(data)), sep=""))
    print("")

    write.csv(
        mun_description,
        paste(
            "./data/",
            description, "/",
            basename(path_to_DBs),
            "description.csv",
            sep=""
        ),
        row.names=FALSE
    )

    return(data)
}
