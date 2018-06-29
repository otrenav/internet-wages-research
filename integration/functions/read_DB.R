##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

require(foreign)

read_DB <- function(description, path_to_DBs, string_entity) {
    temp_data <- tryCatch({
        temp_data <- read_file(path_to_DBs, string_entity, ".dbf")
    }, error = function(e) {
        temp_data <- tryCatch({
            temp_data <- read_file(path_to_DBs, string_entity, ".dta")
        }, error = function(e) {
            print(paste(
                "[!] ERROR: Could not read file (",
                path_to_DBs,
                string_entity,
                sep=""
            ))
            print("[!]        Tried .dbf and .dta extensions")
            print("[!]        Does file exist with this extensions?")
            return(NULL)
        })
    })
    return(temp_data)
}

read_file <- function(path_to_DBs, string_entity, extension) {
    if (extension == ".dbf") {
        return(
            read.dbf(paste(
                path_to_DBs,
                string_entity,
                extension,
                sep=""
            ))
        )
    } else if (extension == ".dta") {
        return(
            read.dta(paste(
                path_to_DBs,
                string_entity,
                extension,
                sep=""
            ))
        )
    } else {
        print(paste(
            "[!] ERROR: Unknown extension (",
            path_to_DBs,
            string_entity,
            extension,
            sep=""
        ))
        return(NULL)
    }
}
