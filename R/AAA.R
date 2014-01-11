## helper functions

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
