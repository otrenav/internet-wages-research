##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##
## - ENT            : Ambas      : string  : keep
## - NOM_ENT        : Ambas      : string  : keep
## - MUN            : Ambas      : string  : aggregate level
## -------------
## - INGTRMEN       : Personas   : int     : mean        : remove 999999, b
## - NIVACAD        : Personas   : int     : mean        : remove 99, b
## - INTERNET       : Viviendas  : bool    : percentage  : remove 9, b
## - NUMPERS        : Viviendas  : int     : sum
## -------------
## - EDAD           : Personas   : int     : mean        : remove > 130
## - HLENGUA        : Personas   : bool    : percentage  : remove 9, b
## - COMPUTADORA    : Viviendas  : bool    : percentage  : remove 9, b
## -------------
## - HIGH_INGTRMEN  : top X % (relative)
## - HIGH_NIVACAD   : top X % (relative)
## - HIGH_INTERNET  : top X % (relative)
## - HIGH_NUMPERS   : top X % (relative)
## -------------
##

aggregate <- function(data, variables_to_keep, year) {
    results_data <- NULL
    unique_municipalities <- unique(data$MUN)
    n_unique_municipalities <- length(unique_municipalities)

    for (i in sort(unique_municipalities)) {
        print(paste(
            "MUN: ", i, "/", n_unique_municipalities,
            paste=""
        ))
        temp_data <- data[!is.na(data$MUN) & data$MUN == i, ]
        new_row <- data.frame(matrix(nrow=1, ncol=ncol(data)))
        colnames(new_row) <- colnames(data)
        municipality_names  <- temp_data[!is.na(temp_data$NOM_MUN), "NOM_MUN"]

        ## Guard clause against multiple municipality names
        if (length(unique(municipality_names)) > 1) {
            print(unique(temp_data$NOM_MUN))
            stop(paste('MUN: ', i))
        }

        new_row$ENT     <- temp_data$ENT[1]
        new_row$MUN     <- temp_data$MUN[1]
        new_row$NOM_ENT <- temp_data$NOM_ENT[1]
        new_row$NOM_MUN <- municipality_names[1]

        if ("INGTRMEN" %in% variables_to_keep) {
            ## new_row$INGTRMEN <- mean(temp_data$INGTRMEN, na.rm=TRUE)
            new_row$INGTRMEN <- median(temp_data$INGTRMEN, na.rm=TRUE)
        }
        if ("NIVACAD" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$NIVACAD))
            new_row$NIVACAD      <- mean(temp_data$NIVACAD, na.rm=TRUE)
            new_row$PRIMARIA     <- sum(temp_data$NIVACAD <= 2,  na.rm=TRUE) / n_rows
            new_row$PREPARATORIA <- sum(temp_data$NIVACAD > 2  &
                                        temp_data$NIVACAD <= 8,  na.rm=TRUE) / n_rows
            new_row$LICENCIATURA <- sum(temp_data$NIVACAD > 8  &
                                        temp_data$NIVACAD <= 10, na.rm=TRUE) / n_rows
            new_row$POSGRADO     <- sum(temp_data$NIVACAD > 10 &
                                        temp_data$NIVACAD <= 12, na.rm=TRUE) / n_rows
        }
        if ("EDAD" %in% variables_to_keep) {
            new_row$EDAD <- mean(temp_data$EDAD, na.rm=TRUE)
        }
        if ("HLENGUA" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$HLENGUA))
            new_row$HLENGUA <- sum(temp_data$HLENGUA, na.rm=TRUE) / n_rows
        }
        if ("NUMPERS" %in% variables_to_keep) {
            new_row$NUMPERS <- sum(temp_data$NUMPERS, na.rm=TRUE)
        }
        if ("COMPUTADORA" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$COMPUTADORA))
            new_row$COMPUTADORA <- sum(temp_data$COMPUTADORA, na.rm=TRUE) / n_rows
        }
        if ("INTERNET" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$INTERNET))
            new_row$INTERNET <- sum(temp_data$INTERNET, na.rm=TRUE) / n_rows
        }
        if ("TELEFONO" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$TELEFONO))
            new_row$TELEFONO <- sum(temp_data$TELEFONO, na.rm=TRUE) / n_rows
        }
        if ("CELULAR" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$CELULAR))
            new_row$CELULAR <- sum(temp_data$CELULAR, na.rm=TRUE) / n_rows
        }
        if ("AUTOPROPIO" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$AUTOPROPIO))
            new_row$AUTOPROPIO <- sum(temp_data$AUTOPROPIO, na.rm=TRUE) / n_rows
        }
        if ("ELECTRICIDAD" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$ELECTRICIDAD))
            new_row$ELECTRICIDAD <- sum(temp_data$ELECTRICIDAD, na.rm=TRUE) / n_rows
        }
        if ("ING_AYUGOB" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$ING_AYUGOB))
            new_row$ING_AYUGOB <- sum(temp_data$ING_AYUGOB, na.rm=TRUE) / n_rows
        }
        if ("ING_OTROPAIS" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$ING_OTROPAIS))
            new_row$ING_OTROPAIS <- sum(temp_data$ING_OTROPAIS, na.rm=TRUE) / n_rows
        }
        if ("ING_DENTROPAIS" %in% variables_to_keep) {
            n_rows <- sum(!is.na(temp_data$ING_DENTROPAIS))
            new_row$ING_DENTROPAIS <- sum(temp_data$ING_DENTROPAIS, na.rm=TRUE) / n_rows
        }

        results_data <- rbind(results_data, new_row)
    }

    return(results_data)
}
