##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

indicate_highs <- function(data, variables_to_keep, high_variables, year) {
    top_percent <- 0.25
    for (variable in high_variables) {
        if (variable %in% variables_to_keep) {
            threshold <- sort(data[, variable])[
                round(nrow(data) * (1 - top_percent))
            ]
            high_variable_name <- paste("HIGH_", variable, sep="")
            data[, high_variable_name] <- 0
            data[
                !is.na(data[, variable]) & data[, variable] > threshold,
                high_variable_name
            ] <- 1
        }
    }
    ## TODO: Temporary fix (possibly eliminate)
    if ("LICENCIATURA" %in% colnames(data)) {
        threshold <- sort(data[, "LICENCIATURA"])[
            round(nrow(data) * (1 - top_percent))
        ]
        data[, "HIGH_LICENCIATURA"] <- 0
        data[
            !is.na(data[, "LICENCIATURA"]) & data[, "LICENCIATURA"] > threshold,
            "HIGH_LICENCIATURA"
        ] <- 1
    }

    return(data)
}

indicate_high_all <- function(data, high_variables) {
    ##
    ## If all are 1's, then the result is 1.
    ## If there's one 0, then the result is 0.
    ##
    data$HIGH_ALL <- 0
    results <- matrix(1, ncol=1, nrow=nrow(data))
    for (variable in high_variables) {
        high_var <- paste("HIGH_", variable, sep="")
        results <- results * data[, high_var]
    }
    data$HIGH_ALL <- results

    return(data)
}


indicate_high_variable <- function(data, variable, top_percent) {
    threshold <- sort(data[, variable])[
        round(nrow(data) * (1 - top_percent))
    ]
    high_variable_name <- paste("HIGH_", variable, sep="")
    data[, high_variable_name] <- 0
    data[
        !is.na(data[, variable]) & data[, variable] > threshold,
        high_variable_name
    ] <- 1
}
