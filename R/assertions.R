.calmr_assert <- function(what, given, ...) { # nolint: cyclocomp_linter.
  nargs <- list(...)
  switch(what,
    "supported_model" = {
      if (is.null(given)) {
        warning("No model passed. Using RW1972.")
        return("RW1972")
      } else {
        if (!(given %in% supported_models())) {
          stop("Model is not supported. Must be one returned by supported_models()")
        }
        return(given)
      }
    },
    "parameters" = {
      default_pars <- get_parameters(...)
      if (is.null(given)) {
        warning("No parameters were passed. Getting default values")
        return(default_pars)
      } else {
        # TODO: Implement parameter check
        return(given)
      }
    },
    "parsed_design" = {
      if (is.null(given)) {
        stop("Must provide a design data.frame or a parsed design.")
      } else {
        return(parse_design(given))
      }
    },
    "supported_optimizer" = {
      if (is.null(given)) {
        warning("No optimizer passed. Using 'optim'.")
        return("optim")
      } else {
        if (!given %in% supported_optimizers()) {
          stop("Optimizer is not supported. Must be one returned by supported_optimizers()")
        } else {
          return(given)
        }
      }
    },
    "supported_family" = {
      if (is.null(given)) {
        warning("No family passed. Using 'identity'")
        return("identity")
      } else {
        if (!(given %in% supported_families())) {
          stop("Family is not supported. Must be one returned by supported_families()")
        } else {
          return(given)
        }
      }
    },
    "limits_OK" = {
      if (any(is.na(given$ll)) | any(is.na(given$ul))) {
        stop("Did not supply limits for all parameters estimated. Count your parameters. Please see ?fit_model")
      }
    },
    "filepath_OK" = {
      if (!file.exists(dirname(given))) {
        stop(sprintf("Path to file (%s) does not exist.", given))
      }
    },
    "no_functional_stimuli" = {
      if (
        length(given$unique_nominal_stimuli) >
          length(given$unique_functional_stimuli)
      ) {
        stop("The model does not support functional/nominal stimuli specifications.")
      }
    },
    "supported_plot" = {
      if (!(given %in% nargs$supported)) {
        stop(sprintf("Plot not supported. The model does not contain '%s' in model results.", given))
      }
    },
    "good_experiment" = {
      if (length(given@design@mapping$unique_nominal_stimuli) == 1) {
        stop("Experiment is too simple to run for one or more groups. Please check your design.")
      }
    },
    "length" = {
      if (!all(lapply(nargs, length) == given)) {
        stop(sprintf(
          "Function requires length of %s argument to be %d",
          paste0(names(nargs), collapse = ","), given
        ))
      }
    }
  )
}

is_experiment <- function(object) {
  inherits(object, "CalmrExperiment")
}

is_design <- function(object) {
  inherits(object, "CalmrDesign")
}

#' Sanitize model outputs
#' @param os Given outputs. Character vector
#' @param m A model name
#' @return A character vector
#' @note If os is not NULL, cuts extraneous outputs.
#' @noRd
.sanitize_outputs <- function(os, m) {
  moutputs <- model_outputs(m)
  if (is.null(os)) {
    return(moutputs)
  }
  # extra parameters
  extra <- setdiff(os, moutputs)
  throw_warn <- length(extra) > 0
  os <- os[!(os %in% extra)]
  # final parameters
  if (!length(os)) {
    stop("Ended with zero outputs after sanitization. Check your outputs.")
  }
  if (throw_warn) warning("Found unsupported outputs. Trimming...")
  os
}
