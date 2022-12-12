#' @title Perform representational similarity analysis on model comparison
#' @param comparison A CalmrComparison object, as returned by compare_models
#' @param trials A character specifying what trials to use for the comparison. One of `c("all", "last")`. Default is "all". See details.
#' @param dist_method A character specifying the distance method to be used in `dist`. Default is "euclidean".
#' @param corr_method A character specifying the correlation method to be used in `cor`. Default is "spearman".
#' @param n_samples An integer specifying the number of samples for the permutation test. Default is 1e3.
#' @param p A float between 0 and 1 specifying the critical value of the null distribution to use for significance testing. Default is 0.95.
#' @param ... Additional parameters passed to `dist` and `cor` (in the case of RSA) or `RSA` (in the case of RSATest).
#' @returns A CalmrRSA or CalmrRSATest object.

#' @details {
#' The `trials` argument specifyies which trials to use in the calculation of distance matrices; "all" means every trial will be used. "last" means only the last trial of each unique trial type will be used.}

#' @rdname RSA
#' @export
RSA <- function(comparison, trials = "all", dist_method = "euclidean", corr_method = "spearman", ...){
  dat = comparison@results
  if (trials == "last"){
    dat = dat %>% dplyr::group_by(.data$group, .data$trial_type, .data$model) %>%
      dplyr::filter(.data$trial == max(.data$trial)) %>%
      dplyr::ungroup()
  }

  #get trial size per model
  tsize = dat %>% dplyr::count(.data$model, .data$group, .data$trial, .data$trial_type) %>%
    dplyr::select(.data$model, .data$n) %>% dplyr::distinct()

  trialmats = sapply(unique(dat$model),
                     function(x) matrix(dat$value[dat$model == x],
                                        ncol = tsize$n[tsize$model == x],
                                        byrow = T),
                     simplify = F)

  #calculate trial distance
  trialdists = lapply(trialmats, function(x) dist(x, method = dist_method, ...))
  #calculate correlation among models
  modcorrs = cor(data.frame(lapply(trialdists, function(x) as.numeric(x)),
                            check.names = F), method = corr_method, ...)
  new("CalmrRSA",
      corr_mat = modcorrs,
      distance_mats = trialdists,
      trials = trials,
      dist_method = dist_method,
      corr_method = corr_method)
}

#' @rdname RSA
#' @export
RSATest <- function(comparison, n_samples = 1e3, p = .95, ...){
  rsa = RSA(comparison, ...)
  #permutation test

  dist_dat = data.frame(lapply(rsa@distance_mats, function(x) as.numeric(x)), check.names = F)
  perm_cors = replicate(n_samples,
                        cor(apply(dist_dat, 2, function(x) sample(x)),
                            method = rsa@corr_method))
  #get critical values
  lc = apply(perm_cors, c(1, 2), function(x) quantile(x, probs = (1-p)/2))
  uc = apply(perm_cors, c(1, 2), function(x) quantile(x, probs = p+((1-p)/2)))

  #get significance
  smat = rsa@corr_mat < lc | rsa@corr_mat > uc

  new("CalmrRSATest",
      RSA = rsa,
      sig_mat = smat,
      lower_crit = lc,
      upper_crit = uc,
      n_samples = n_samples,
      p = p)
}

