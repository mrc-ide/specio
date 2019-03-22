cfg <- new.env(parent = emptyenv())
.onLoad <- function(...) {
  config <- yaml::read_yaml(system.file("config.yml", package = "specio"))
  config <- parse_config(config)
  cfg$tags <- config$tags
  cfg$params <- config$params
}


parse_config <- function(config) {
  constants <- config$.environment
  envir <- list2env(constants, parent = environment())
  config$.environment <- NULL

  for (name in setdiff(names(config), ".environment")) {
    config <- parse_tag(name, config, envir)
  }

  list(tags = config, params = constants)
}

parse_tag <- function(name, config, envir) {
  for (tag in names(config[[name]])) {
    config <- parse_function(tag, name, config, envir, "func")
    if (tag != "fallback") {
      config <- parse_function(tag, name, config, envir, "dimensions", FALSE)
      config <- parse_rows_and_cols(tag, name, config, envir)
    }
  }
  config
}

parse_function <- function(tag_name, property_name, config, envir,
                           func_field_name = "func", required = TRUE) {
  if (tag_name != "fallback") {
    func_name <- config[[property_name]][[tag_name]][[func_field_name]]
    if (!is.null(func_name)) {
      tryCatch(
        {
          func <- get(func_name, envir = asNamespace("specio"))
          config[[property_name]][[tag_name]][[func_field_name]] <- func
        },
        error = function(e) {
          e$message <- sprintf("%s\nCan't find function %s for field %s and tag %s.",
            e$message, func_name, property_name, tag_name)
          stop(e)
        }
      )
    } else if (is.null(func_name) && required) {
      stop(sprintf("No function for property %s set for field %s and tag %s.",
                   func_field_name, property_name, tag_name))
    }
  } else {
    config[[property_name]][[tag_name]] <-
      eval_with_params(config[[property_name]][[tag_name]], envir)
  }
  config
}

parse_rows_and_cols <- function(tag_name, property_name, config, envir) {
  if (!is.null(config[[property_name]][[tag_name]]$rows)) {
    config[[property_name]][[tag_name]]$rows <-
      eval_with_params(config[[property_name]][[tag_name]]$rows, envir)
  }
  if (!is.null(config[[property_name]][[tag_name]]$cols)) {
    config[[property_name]][[tag_name]]$cols <-
      eval_with_params(config[[property_name]][[tag_name]]$cols, envir)
  }
  config
}

eval_with_params <- function(text, envir) {
  eval(parse(text = text), envir = envir)
}
