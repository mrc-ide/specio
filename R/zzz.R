tags <- new.env(parent = emptyenv())
.onLoad <- function(libname, pkgname) {
  config <- yaml::read_yaml(system.file("tag_config.yml", package = "specio"))
  tags$config <- parse_tags(config)
}


parse_tags <- function(config) {
  for (name in names(config)) {
    config <- parse_tag(name, config)
  }
  config
}

parse_tag <- function(name, config) {
  for (tag in names(config[[name]])) {
    config <- parse_function(tag, name, config, "func")
    if (tag != "fallback") {
      config <- parse_function(tag, name, config, "dimensions", FALSE)
      config <- parse_rows_and_cols(tag, name, config)
    }
  }
  config
}

parse_function <- function(tag_name, property_name, config,
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
      eval_with_params(config[[property_name]][[tag_name]])
  }
  config
}

parse_rows_and_cols <- function(tag_name, property_name, config) {
  if (!is.null(config[[property_name]][[tag_name]]$rows)) {
    config[[property_name]][[tag_name]]$rows <-
      eval_with_params(config[[property_name]][[tag_name]]$rows)
  }
  if (!is.null(config[[property_name]][[tag_name]]$cols)) {
    config[[property_name]][[tag_name]]$cols <-
      eval_with_params(config[[property_name]][[tag_name]]$cols)
  }
  config
}

eval_with_params <- function(text) {
  params <- environment()
  params$model_params <- get_model_params()
  eval(parse(text = text), envir = params)
}
