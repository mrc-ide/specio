vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}

vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

strict_list <- function(...) {
  ret <- list(...)
  class(ret) <- "strict_list"
  ret
}

"$.strict_list" <- function(x, name) {
  x[[name]]
}

"[[.strict_list" <- function(x, name, ...) {
  if (!(name %in% names(x))) {
    stop(sprintf("Element '%s' does not exist.", name))
  }
  NextMethod("[[")
}
