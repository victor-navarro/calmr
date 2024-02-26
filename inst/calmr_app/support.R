#' Parse parameter list into stimulus-specific and global parameters
#' @param model A modelname string
#' @param parameters A list with parameters
#' @return A list with stimulus and global parameters (both data.frames)

make_par_tables <- function(model, parameters) {
  parnames <- names(parameters)
  gpars <- sapply(parnames, calmr:::.is_global_parameter, model = model)
  tpars <- sapply(parnames, calmr:::.is_trial_parameter, model = model)
  trpars <- sapply(parnames, calmr:::.is_trans_parameter, model = model)
  spars <- !gpars & !tpars & !trpars

  stimpars <- globpars <- trialpars <- transpars <- NULL
  if (any(spars)) {
    stimnames <- names(parameters[[which(spars)[1]]])
    stimpars <- data.frame(
      stimulus = stimnames,
      as.data.frame(parameters[parnames[spars]])
    )
    names(stimpars) <- stringr::str_to_title(names(stimpars))
  }

  if (any(gpars)) {
    globpars <- data.frame(
      parameter = stringr::str_to_title(parnames[gpars]),
      value = as.numeric(unlist(parameters[parnames[gpars]]))
    )
    names(globpars) <- stringr::str_to_title(names(globpars))
  }

  if (any(tpars)) {
    tnames <- names(parameters[[which(tpars)[1]]])
    pnames <- rep(names(parameters[tpars]), each = length(tnames))
    trialpars <- data.frame(
      parameter = pnames,
      trial = tnames,
      value = as.numeric(unlist(parameters[parnames[tpars]]))
    )
    names(trialpars) <- stringr::str_to_title(names(trialpars))
  }

  if (any(trpars)) {
    tnames <- names(parameters[[which(trpars)[1]]])
    trnames <- unique(names(unlist(unname(parameters[[which(trpars)[1]]]))))
    pnames <- rep(names(parameters[trpars]), each = length(trnames))
    transpars <- data.frame(
      parameter = pnames,
      trial = tnames,
      transition = trnames,
      value = as.numeric(unlist(parameters[parnames[trpars]]))
    )
    names(transpars) <- stringr::str_to_title(names(transpars))
  }

  return(list(
    stimulus = stimpars,
    global = globpars,
    trial = trialpars,
    transition = transpars
  ))
}

#' Convert parameter data.frame to list
df_to_parlist <- function(df, type) {
  parnames <- names(df)
  pars <- switch(type,
    "stimulus" = {
      stimnames <- df$Stimulus
      pars <- list()
      for (p in parnames[-1]) {
        pars[[p]] <- stats::setNames(df[[p]], stimnames)
      }
    },
    "global" = {
      pars <- list()
      parnames <- unique(df$parameter)
      for (p in parnames[-1]) {
        pars[[p]] <- df[[p]]
      }
    },
    "trial" = {
      pars <- unique(df$Parameter)
      sapply(pars, function(p) {
        stats::setNames(
          df$Value[df$Parameter == p],
          df$Trial[df$Parameter == p]
        )
      }, simplify = FALSE)
    },
    "transition" = {
      parlist <- list()
      pars <- unique(df$Parameter)
      for (p in pars) {
        pdat <- df[df$Parameter == p, ]
        parlist[[p]] <- sapply(unique(pdat$Trial), function(t) {
          stats::setNames(
            pdat$Value[df$Trial == t],
            df$Transition[df$Trial == t]
          )
        }, simplify = FALSE)
      }
      parlist
    }
  )
  pars
}

check_globalpars <- function(model, parameters) {
  any(sapply(names(parameters),
    calmr:::.is_global_parameter,
    model = model
  ))
}
check_trialpars <- function(model, parameters) {
  any(sapply(names(parameters),
    calmr:::.is_trial_parameter,
    model = model
  ))
}
check_transpars <- function(model, parameters) {
  any(sapply(names(parameters),
    calmr:::.is_trans_parameter,
    model = model
  ))
}
