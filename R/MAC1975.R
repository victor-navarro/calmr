#' Train the MAC1975 model
#'
#' @param alphas A named vector with stimulus saliences.
#' @param min_alphas A named vector with minimum stimulus saliences.
#' @param max_alphas A named vector with maximum stimulus saliences.
#' @param betas_on A named vector with stimulus associabilities for present stimuli.
#' @param betas_off A named vector with stimulus associabilities for absent stimuli.
#' @param lambdas A named vector with stimulus asymptotes.
#' @param thetas A named vector with stimulus attentional learning rates.
#' @param gammas A named vector with stimulus contributions to attentional learning rules
#' @param V (optional) A named matrix of dimensions S,S; where S is the number of stimuli.
#' @param experience A data.frame specifying trials as rows, as returned by `make_model_args`
#' @param mapping A named list specifying trial and stimulus mapping, as returned by `make_model_args`
#' @return A list with
#' \itemize{
#' \item{vs, Array of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Respectively, vs and rs contain the stimulus weights and the stimulus-specific responses.}
#' \item{alphas, lambdas - Named vectors as above.}
#' \item{combs, chains - Lists of length P with combined and chained activation values.}
#' \item{experience, mapping - As passed in arguments; carryover arguments further processing.}
#' }

MAC1975 <- function(alphas,
                    min_alphas,
                    max_alphas,
                    betas_on,
                    betas_off,
                    lambdas,
                    thetas,
                    gammas,
                    V = NULL,
                    experience,
                    mapping){

  mod = methods::new("CalmrModel",
            model = "MAC1975")

  .calmr_check("no_functional_stimuli", mapping)

  #data initialization
  ntrials = length(experience$tp)
  snames = names(alphas) #get functional stimuli names
  nstim = length(snames)

  if(is.null(V)){V = gen_ss_weights(snames)}

  vs = es = array(NA, dim = c(ntrials, dim(V)),
                  dimnames = list(NULL, rownames(V), rownames(V)))
  as = array(NA, dim = c(ntrials, nrow(V)),
             dimnames = list(NULL, rownames(V)))

  #make matrix for attentional learning
  nsmat = abs(diag(nstim)-1)

  for (t in 1:ntrials){
    #get pre functional stimuli
    prestims = mapping$trial_pre_func[[experience$tp[t]]]

    #get post functional stimuli
    poststims = mapping$trial_post_func[[experience$tp[t]]]

    #join
    allstims = c(prestims, poststims)

    #make one-hot vector of functional stimuli
    oh_stims = .makeOH(allstims, snames)
    oh_prestims = .makeOH(prestims, snames)
    oh_poststims = .makeOH(poststims, snames)

    #generate expectation matrices
    pre_emat = V*oh_prestims
    post_emat = V*oh_poststims

    #learn if we need to
    if (!experience$is_test[t]){
      #get alphas betas and lambdas for learning
      talphas = tbetas = pre_tlambdas = post_tlambdas = stats::setNames(rep(0, length(snames)), snames)
      #populating vector with nominal stimuli values as functional stimuli values
      talphas[allstims] = alphas[allstims]

      #betas and lambdas vectors are initialized as if all stimuli are absent
      tbetas = betas_off
      tbetas[allstims] = betas_on[allstims]

      pre_tlambdas[allstims] = lambdas[allstims]
      post_tlambdas[poststims] = lambdas[poststims]

      #Learn associations
      err1 = oh_stims*t(pre_tlambdas-t(pre_emat)) #first error (includes all stimuli in the sequence)
      err2 = oh_poststims*t(post_tlambdas-t(post_emat)) #second error (includes only the second half stimuli)

      d1 = t(t(oh_prestims*talphas*err1)*tbetas) #first delta
      d2 = t(t(oh_poststims*talphas*err2)*tbetas) #second delta

      diag(d1) = diag(d2) = 0
      V = V+d1+d2

      #Learn alphas
      #note: the expressions below take the expectation matrix, pools it twice (once for each predictor, once for all i but the predictor) and sweeps each with the lambda for each j.
      #we do it twice just like the associations above, to take care of the associations in a sequential design
      alphasd1 = thetas * oh_prestims *
        rowSums(abs(t((pre_tlambdas - t(pre_emat) %*% nsmat) * gammas))
                - abs(t((pre_tlambdas - t(pre_emat)) * gammas)))

      alphasd2 = thetas * oh_poststims *
        rowSums(abs(t((post_tlambdas - t(post_emat) %*% nsmat) * gammas))
                - abs(t((post_tlambdas - t(post_emat)) * gammas)))

      alphas = alphas + alphasd1 + alphasd2
      #apply lower limit on alphas
      alphas[] = sapply(1:nstim, function(i) max(min_alphas[i], alphas[i]))
      #apply upper limit on alphas
      alphas[] = sapply(1:nstim, function(i) min(max_alphas[i], alphas[i]))

    }

    #save data
    vs[t, , ] = V
    as[t, ] = alphas
    es[t, , ] = pre_emat
  }
  mod@parameters = list(alphas = alphas,
                        min_alphas = min_alphas,
                        max_alphas = max_alphas,
                        betas_on = betas_on,
                        betas_off = betas_off,
                        lambdas = lambdas,
                        thetas = thetas,
                        gammas = gammas)
  mod@model_results = list(vs = vs,
                           as = as,
                           es = es)

  mod@experience = experience
  mod@mapping = mapping
  mod
}
