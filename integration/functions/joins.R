##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

join_dataframes <- function(description, data, temp_data, entity) {
    joined_data <- NULL
    tryCatch({
        joined_data <- rbind(data, temp_data)
    }, error = function(e) {
        print(paste(
            "[!] ", description,
            ", entidad: ", toString(entity),
            " (ERROR)", sep=""
        ))
    })
    return(joined_data)
}

join_municipality_data <- function(data, temp_data, description, entity) {
    if (!is.null(temp_data)) {
        joined_data <- join_dataframes(
            description,
            data,
            temp_data,
            entity
        )
        if (!is.null(joined_data)) {
            data <- joined_data
        }
    }
    return(data)
}
