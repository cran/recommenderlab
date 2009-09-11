## helper functions and registry

.get_parameters <- function(p, parameter) {
    if(!is.null(parameter) && length(parameter) != 0) {
        o <- pmatch(names(parameter), names(p))

        if(any(is.na(o)))
        stop(sprintf(ngettext(length(is.na(o)),
                    "Unknown option: %s",
                    "Unknown options: %s"),
                paste(names(parameter)[is.na(o)],
                    collapse = " ")))

        p[o] <- parameter
    }

    p
}

## setup registry for recommender methods
recommenderRegistry <- registry(registry_class="recommender_registry", 
    entry_class="recommender_method")

recommenderRegistry$set_field("method", type = "character", 
    is_key = TRUE, index_FUN = match_partial_ignorecase)
recommenderRegistry$set_field("dataType", type = "character", 
    is_key = TRUE, index_FUN = match_exact)
recommenderRegistry$set_field("fun", type = "function", 
    is_key = FALSE)
recommenderRegistry$set_field("description", type = "character", 
    is_key = FALSE)


print.recommender_method <- function(x, ...) with(x, 
    writeLines(sprintf("Recommender method: %s\nDescription: %s", 
            x$method, x$description)))


