##
## Omar Trejo Navarro (otrenav@gmail.com), 2016
##

entity_to_two_digit_string <- function(entity) {
    string_entity <- NULL
    if (entity < 10) {
        string_entity <- paste("0", toString(entity), sep="")
    } else {
        string_entity <- toString(entity)
    }
    return(string_entity)
}
