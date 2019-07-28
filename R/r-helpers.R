#' Open help in the browser
#'
#' @param string, function to load help for
#' @return
#' @export
hlp <- function(x) {
    help(x, help_type = "html")
}

#' List environment variables with additional info
#'
#' @param env Environment to list
#' @return data frame with the environment
#' @export
lenv <- function(env = .GlobalEnv) {
    .vars <- ls(envir = env)

    .x <- data.frame(VAR = .vars)

    .x$CLASS <- sapply(
        .vars,
        function(.x) paste(class(get(.x, envir = env)),
                           collapse = " ")
    )

    .x$MODE <- sapply(
        .vars, function(.x) mode(get(.x, envir = env))
    )

    .x$LENGTH <- sapply(
        .vars, function(.x) length(get(.x, envir = env))
    )

    .x$SIZE <- sapply(
        .vars,
        function(.x) format(object.size(get(.x, envir = env)),
                            units = "MB", digits = 2)
    )

    return(.x)
}
