#' Parse design data.frame
#' @param df A `data.frame` of dimensions (groups) by (phases+1).
#' @return A [CalmrDesign-class] object.
#' @note Each entry in even-numbered columns of df is
#' a string formatted as per [phase_parser()].
#' @examples
#' df <- data.frame(
#'   Group = c("Group 1", "Group 2"),
#'   P1 = c("10AB(US)", "10A(US)")
#' )
#' parse_design(df)
#' @seealso [phase_parser()]
#' @export

parse_design <- function(df) {
  # assert at least two columns
  if (ncol(df) < 2) {
    stop("Data frame must have at least two columns (1 group 1 phase).")
  }
  # pass to compatibility function
  df <- .design_compat(df)
  design <- apply(df, 1, function(r) {
    sapply(seq_len(ncol(df))[-1], function(p) {
      list(
        group = r[[1]],
        phase = names(df)[p],
        parse_string = r[[p]],
        phase_info = phase_parser(r[[p]])
      )
    }, simplify = FALSE)
  }, simplify = FALSE)
  # unnest one level (now group:phase is flat now)
  design <- unlist(design, recursive = FALSE, use.names = FALSE)
  # That's the easy part
  # The hard part is to create the mapping for the experiment
  map <- .get_mapping(design)

  methods::new("CalmrDesign",
    design = design, mapping = map, raw_design = df
  )
}

.design_compat <- function(df) {
  # checks if the data.frame uses <0.7.0 format with a randomization column.
  # If so, it throws a deprecation warning and modifies the data.frame

  # check if any columns contain boolean values
  col_classes <- sapply(df, class)
  if (any(col_classes == "logical")) {
    # check if any of the boolean columns are named "randomization
    # raise warning
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "parse_design(df = 'must not contain randomization columns')",
      details = "Please use the new format using '!'
      and no randomization column.
      (e.g., !10A/10B)",
      always = TRUE
    )

    # get randomization symbols
    is_random <- which(col_classes == "logical")
    df[is_random] <- sapply(df[is_random], function(col) {
      ifelse(col, "!", "")
    })
    # paste the columns
    df[is_random - 1] <- sapply(
      is_random,
      function(i) {
        paste0(df[[i]], df[[i - 1]])
      }
    )
    # remove the randomization columns
    df <- df[, -is_random]
  }
  df
}

.get_mapping <- function(design) {
  # get phase info
  pinfo <- lapply(design, "[[", "phase_info")
  # trial names
  tinfo <- lapply(pinfo, "[[", "trial_info")
  ginfo <- lapply(pinfo, "[[", "general_info")

  # gather at the period level
  mastert <- unlist(lapply(ginfo, "[[", "trial_names"))
  # VN: in case of null tinfo, information gets duplicated (but then removed)
  funcs <- unlist(lapply(tinfo, function(r) {
    lapply(r, "[[", "functionals")
  }), recursive = FALSE)
  nomis <- unlist(lapply(tinfo, function(r) {
    lapply(r, "[[", "nominals")
  }), recursive = FALSE)
  # reduce
  uniqs <- !duplicated(mastert)
  tnames <- mastert[uniqs]
  funcs <- stats::setNames(funcs[uniqs], tnames)
  nomis <- stats::setNames(nomis[uniqs], tnames)

  # get transition names (nested within trial)
  transitions <- lapply(funcs, function(t) {
    periods <- names(t)
    nperiods <- length(periods)
    if (nperiods > 1) {
      sapply(
        seq_len(nperiods - 1),
        function(n) {
          sprintf("%s>%s", periods[n], periods[n + 1])
        }
      )
    }
  })
  transitions <- transitions[!unlist(lapply(transitions, is.null))]

  # make at the trial level
  tfuncs <- lapply(funcs, function(t) {
    unique(unlist(t, use.names = FALSE))
  })
  tnomis <- lapply(nomis, function(t) {
    unique(unlist(t, use.names = FALSE))
  })

  # sort unique names
  uni_fun <- sort(unique(unlist(funcs)))
  uni_nom <- sort(unique(unlist(nomis)))

  # make stimulus mapping
  n2f <- unlist(lapply(ginfo, "[[", "nomi2func"))
  n2f <- n2f[!duplicated(names(n2f))]
  f2n <- unlist(lapply(ginfo, "[[", "func2nomi"))
  f2n <- f2n[!duplicated(names(f2n))]

  # make one-hot vectors
  # period based
  period_onehots <- lapply(funcs, function(t) {
    lapply(t, .one_hot, stimnames = uni_fun)
  })
  # trial based
  trial_onehots <- lapply(funcs, function(t) {
    .one_hot(unique(unlist(t)), stimnames = uni_fun)
  })

  list(
    trial_names = tnames,
    unique_functional_stimuli = uni_fun,
    unique_nominal_stimuli = uni_nom,
    period_functionals = funcs,
    trial_functionals = tfuncs,
    period_nominals = nomis,
    trial_nominals = tnomis,
    period_ohs = period_onehots,
    trial_ohs = trial_onehots,
    nomi2func = n2f,
    func2nomi = f2n,
    transitions = transitions
  )
}

# Makes a onehot representation of the stimulus vector, given all stimuli
.one_hot <- function(s, stimnames) {
  stats::setNames(as.numeric(stimnames %in% s), stimnames)
}
