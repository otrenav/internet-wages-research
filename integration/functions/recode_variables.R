##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

recode_variables <- function(data, year) {
    if (year == 2015) {
        colnames(data) <- toupper(colnames(data))
    }
    variables_to_change <- list(
        c("COMPU", "COMPUTADORA"),
        c("ELECTRI", "ELECTRICIDAD"),
        c("AUTOPROP", "AUTOPROPIO"),
        c("AYUPROGOB", "ING_AYUGOB"),
        c("INGR_AYUGOB", "ING_AYUGOB"),
        c("AYUPEOP", "ING_OTROPAIS"),
        c("AYUPEDP", "ING_DENTROPAIS"),
        c("INGR_PEROTROPAIS", "ING_OTROPAIS"),
        c("INGR_PERDENTPAIS", "ING_DENTROPAIS")
    )
    for (variables in variables_to_change) {
        if (variables[1] %in% colnames(data)) {
            colnames(data)[which(colnames(data) == variables[1])] <- variables[2]
        }
    }
    ## if ("COMPU" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "COMPU")] <- "COMPUTADORA"
    ## }
    ## if ("ELECTRI" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "ELECTRI")] <- "ELECTRICIDAD"
    ## }
    ## if ("AUTOPROP" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "AUTOPROP")] <- "AUTOPROPIO"
    ## }
    ## if ("AYUPROGOB" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "AYUPROGOB")] <- "ING_AYUGOB"
    ## }
    ## if ("INGR_AYUGOB" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "INGR_AYUGOB")] <- "ING_AYUGOB"
    ## }
    ## if ("AYUPEOP" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "AYUPEOP")] <- "ING_OTROPAIS"
    ## }
    ## if ("AYUPEDP" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "AYUPEDP")] <- "ING_DENTROPAIS"
    ## }
    ## if ("INGR_PEROTROPAIS" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "INGR_PEROTROPAIS")] <- "ING_OTROPAIS"
    ## }
    ## if ("INGR_PERDENTPAIS" %in% colnames(data)) {
    ##     colnames(data)[which(colnames(data) == "INGR_PERDENTPAIS")] <- "ING_DENTROPAIS"
    ## }
    return(data)
}
