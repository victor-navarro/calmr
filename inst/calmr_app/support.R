#' Parse parameter list into stimulus-specific and global parameters
#' @param model A modelname string
#' @param parameters A list with parameters
#' @return A list with stimulus and global parameters (both data.frames)

make_par_tables <- function(model, parameters) {
  parnames <- names(parameters)
  stimnames <- names(parameters[[1]])
  globals <- sapply(parnames, calmr:::.is_global_parameter, model = model)

  stimpars <- globpars <- NULL
  if (any(!globals)) {
    stimpars <- data.frame(
      stimulus = stimnames,
      as.data.frame(parameters[parnames[!globals]])
    )
    names(stimpars) <- stringr::str_to_title(names(stimpars))
  }

  if (any(globals)) {
    globpars <- data.frame(
      parameter = stringr::str_to_title(parnames[globals]),
      value = as.numeric(unlist(parameters[parnames[globals]]))
    )
    names(globpars) <- stringr::str_to_title(names(globpars))
  }
  return(list(stimulus = stimpars, global = globpars))
}

#' Convert parameter data.frame to list
df_to_parlist <- function(df) {
  parnames <- names(df)
  if ("Stimulus" %in% parnames) {
    stimnames <- df$Stimulus
    pars <- list()
    for (p in parnames[-1]) {
      pars[[p]] <- stats::setNames(df[[p]], stimnames)
    }
  } else {
    pars <- list()
    for (p in parnames[-1]) {
      pars[[p]] <- df[[p]]
    }
  }
}

check_globals <- function(model, parameters) {
  any(sapply(names(parameters), calmr:::.is_global_parameter, model = model))
}
