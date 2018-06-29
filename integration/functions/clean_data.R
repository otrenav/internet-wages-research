##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

clean_data <- function(data, variables_to_keep, year) {
    ##
    ## Clean data according to the codes provided
    ## by INEGI (which are not homogeneous) and the
    ## purpose of the research.
    ##
    ## NOTE: This code is ugly because INEGI's treatment
    ## of databases differs substantially from question
    ## to question and survey to survey
    ##
    if ("INGTRMEN" %in% variables_to_keep) {
        data$INGTRMEN <- as.numeric(as.character(data$INGTRMEN))
        data[is.na(data$INGTRMEN), "INGTRMEN"] <- -1
        data <- data[data$INGTRMEN != 999999, ]
        data[data$INGTRMEN == -1, "INGTRMEN"] <- NA
    }
    if ("NIVACAD" %in% variables_to_keep) {
        data$NIVACAD <- as.numeric(as.character(data$NIVACAD))
        data[is.na(data$NIVACAD), "NIVACAD"] <- -1
        data <- data[data$NIVACAD != 99, ]
        if (year == 2015) {
            data[data$NIVACAD == 9,  "NIVACAD"] <- 2
            data[data$NIVACAD == 10, "NIVACAD"] <- 9
            data[data$NIVACAD == 11, "NIVACAD"] <- 10
            data[data$NIVACAD == 12, "NIVACAD"] <- 11
            data[data$NIVACAD == 13, "NIVACAD"] <- 11
            data[data$NIVACAD == 14, "NIVACAD"] <- 12
        }
        data[data$NIVACAD == -1, "NIVACAD"] <- NA
    }
    if ("EDAD" %in% variables_to_keep) {
        data$EDAD <- as.numeric(as.character(data$EDAD))
        data[is.na(data$EDAD), "EDAD"] <- -1
        data <- data[data$EDAD < 130, ]
        data[data$EDAD == -1, "EDAD"] <- NA
    }
    if ("HLENGUA" %in% variables_to_keep) {
        data$HLENGUA <- as.numeric(as.character(data$HLENGUA))
        data[is.na(data$HLENGUA), "HLENGUA"] <- -1
        data <- data[data$HLENGUA != 9, ]
        data[data$HLENGUA == 3, "HLENGUA"] <- 0
        data[data$HLENGUA == -1, "HLENGUA"] <- NA
    }
    if ("NUMPERS" %in% variables_to_keep) {
        data$NUMPERS <- as.numeric(as.character(data$NUMPERS))
    }
    if ("INTERNET" %in% variables_to_keep) {
        data$INTERNET <- as.numeric(as.character(data$INTERNET))
        data[is.na(data$INTERNET), "INTERNET"] <- -1
        data <- data[data$INTERNET != 9, ]
        if (year == 2015) {
            data[data$INTERNET == 5, "INTERNET"] <- 1
            data[data$INTERNET == 6, "INTERNET"] <- 0
        } else {
            ## 2010
            data[data$INTERNET ==  2, "INTERNET"] <- 0
        }
        data[data$INTERNET == -1, "INTERNET"] <- NA
    }
    if ("COMPUTADORA" %in% variables_to_keep) {
        data$COMPUTADORA <- as.numeric(as.character(data$COMPUTADORA))
        data[is.na(data$COMPUTADORA), "COMPUTADORA"] <- -1
        data <- data[data$COMPUTADORA != 9, ]
        if (year == 2015) {
            data[data$COMPUTADORA == 7, "COMPUTADORA"] <- 1
            data[data$COMPUTADORA == 8, "COMPUTADORA"] <- 0
        } else {
            ## 2010
            data[data$COMPUTADORA == 3, "COMPUTADORA"] <- 1
            data[data$COMPUTADORA == 4, "COMPUTADORA"] <- 0
        }
        data[data$COMPUTADORA == -1, "COMPUTADORA"] <- NA
    }
    if ("TELEFONO" %in% variables_to_keep) {
        data <- data[data$TELEFONO != "b", ]
        data$TELEFONO <- as.numeric(as.character(data$TELEFONO))
        data[is.na(data$TELEFONO), "TELEFONO"] <- -1
        data <- data[data$TELEFONO != 9, ]
        data[data$TELEFONO == 2, "TELEFONO"] <- 0
        data[data$TELEFONO == -1, "TELEFONO"] <- NA
    }
    if ("CELULAR" %in% variables_to_keep) {
        data <- data[data$CELULAR != "b", ]
        data$CELULAR <- as.numeric(as.character(data$CELULAR))
        data[is.na(data$CELULAR), "CELULAR"] <- -1
        data <- data[data$CELULAR != 9, ]
        data[data$CELULAR == 3, "CELULAR"] <- 1
        data[data$CELULAR == 4, "CELULAR"] <- 0
        data[data$CELULAR == -1, "CELULAR"] <- NA
    }
    if ("AUTOPROPIO" %in% variables_to_keep) {
        data <- data[data$AUTOPROPIO != "b", ]
        data$AUTOPROPIO <- as.numeric(as.character(data$AUTOPROPIO))
        data[is.na(data$AUTOPROPIO), "AUTOPROPIO"] <- -1
        data <- data[data$AUTOPROPIO != 9, ]
        if (year == 2015) {
            data[data$AUTOPROPIO == 7, "AUTOPROPIO"] <- 1
            data[data$AUTOPROPIO == 8, "AUTOPROPIO"] <- 0
        } else {
            ## 2010
            data[data$AUTOPROPIO ==  2, "AUTOPROPIO"] <- 0
        }
        data[data$AUTOPROPIO == -1, "AUTOPROPIO"] <- NA
    }
    if ("ELECTRICIDAD" %in% variables_to_keep) {
        data <- data[data$ELECTRICIDAD != "b", ]
        data$ELECTRICIDAD <- as.numeric(as.character(data$ELECTRICIDAD))
        data[is.na(data$ELECTRICIDAD), "ELECTRICIDAD"] <- -1
        data <- data[data$ELECTRICIDAD != 9, ]
        if (year == 2015) {
            data[data$ELECTRICIDAD == 5, "ELECTRICIDAD"] <- 1
            data[data$ELECTRICIDAD == 7, "ELECTRICIDAD"] <- 0
        } else {
            ## 2010
            data[data$ELECTRICIDAD ==  3, "ELECTRICIDAD"] <- 0
        }
        data[data$ELECTRICIDAD == -1, "ELECTRICIDAD"] <- NA
    }
    if ("ING_AYUGOB" %in% variables_to_keep) {
        data$ING_AYUGOB <- as.numeric(as.character(data$ING_AYUGOB))
        data[is.na(data$ING_AYUGOB), "ING_AYUGOB"] <- -1
        data <- data[data$ING_AYUGOB != 9, ]
        if (year == 2015) {
            data[data$ING_AYUGOB == 5, "ING_AYUGOB"] <- 1
            data[data$ING_AYUGOB == 6, "ING_AYUGOB"] <- 0
        } else {
            ## 2010
            data[data$ING_AYUGOB ==  2, "ING_AYUGOB"] <- 0
        }
        data[data$ING_AYUGOB == -1, "ING_AYUGOB"] <- NA
    }
    if ("ING_OTROPAIS" %in% variables_to_keep) {
        ## data <- data[data$ING_OTROPAIS != "b", ]
        data$ING_OTROPAIS <- as.numeric(as.character(data$ING_OTROPAIS))
        data[is.na(data$ING_OTROPAIS), "ING_OTROPAIS"] <- -1
        data <- data[data$ING_OTROPAIS != 9, ]
        data[data$ING_OTROPAIS ==  2, "ING_OTROPAIS"] <- 0
        data[data$ING_OTROPAIS == -1, "ING_OTROPAIS"] <- NA
    }
    if ("ING_DENTROPAIS" %in% variables_to_keep) {
        ## data <- data[data$ING_DENTROPAIS != "b", ]
        data$ING_DENTROPAIS <- as.numeric(as.character(data$ING_DENTROPAIS))
        data[is.na(data$ING_DENTROPAIS), "ING_DENTROPAIS"] <- -1
        data <- data[data$ING_DENTROPAIS != 9, ]
        data[data$ING_DENTROPAIS ==  3, "ING_DENTROPAIS"] <- 1
        data[data$ING_DENTROPAIS ==  4, "ING_DENTROPAIS"] <- 0
        data[data$ING_DENTROPAIS == -1, "ING_DENTROPAIS"] <- NA
    }

    return(data)
}
