#' Train the PKH1982 model
#'
#' @param alphas A named vector with stimulus saliences.
#' @param min_alphas A named vector with minimum stimulus saliences.
#' @param max_alphas A named vector with maximum stimulus saliences.
#' @param betas_ex A named vector with stimulus excitatory learning rates.
#' @param betas_in A named vector with stimulus inhibitory learning rates.
#' @param lambdas A named vector with stimulus asymptotes.
#' @param thetas A named vector with stimulus attentional learning recency bias.
#' @param gammas A named vector with stimulus contributions to attentional learning rules.
#' @param EV (optional) A named matrix of dimensions S,S; where S is the number of stimuli. Used for excitatory associations.
#' @param IV (optional) A named matrix of dimensions S,S; where S is the number of stimuli. Used for inhibitory associations.
#' @param experience A data.frame specifying trials as rows, as returned by `make_model_args`
#' @param mapping A named list specifying trial and stimulus mapping, as returned by `make_model_args`
#' @return A list with
#' \itemize{
#' \item{vs, Array of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Respectively, vs and rs contain the stimulus weights and the stimulus-specific responses.}
#' \item{alphas, lambdas - Named vectors as above.}
#' \item{combs, chains - Lists of length P with combined and chained activation values.}
#' \item{experience, mapping - As passed in arguments; carryover arguments further processing.}
#' }

PKH1982 <- function(alphas,
                   min_alphas,
                   max_alphas,
                   betas_ex,
                   betas_in,
                   lambdas,
                   thetas,
                   gammas,
                   EV = NULL,
                   IV = NULL,
                   experience,
                   mapping){

  mod = methods::new("CalmrModel",
            model = "PKH1982")

  .calmr_check("no_functional_stimuli", mapping)

  #data initialization
  ntrials = length(experience$tp)
  snames = names(alphas) #get functional stimuli names
  nstim = length(snames)

  if(is.null(EV)){EV = gen_ss_weights(snames)}
  if(is.null(IV)){IV = gen_ss_weights(snames)}

  evs = ivs = es = array(NA, dim = c(ntrials, dim(EV)),
                         dimnames = list(NULL, rownames(EV), rownames(EV)))
  as = array(NA, dim = c(ntrials, nrow(EV)),
             dimnames = list(NULL, rownames(EV)))

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

    #generate expectations
    #first expectation
    ee1 = oh_prestims %*% EV
    ie1 = oh_prestims %*% IV
    ne1 = ee1-ie1 #net

    #second expectation
    ee2 = oh_poststims %*% EV
    ie2 = oh_poststims %*% IV
    ne2 = ee2-ie2 #net

    #generate expectation matrices
    pre_emat = (EV*oh_prestims) - (IV*oh_prestims)
    post_emat = (EV*oh_poststims) - (IV*oh_poststims)

    #save data
    evs[t, , ] = EV
    ivs[t, , ] = IV
    as[t, ] = alphas
    es[t, , ] = pre_emat

    #learn if we need to
    if (!experience$is_test[t]){
      #get parameters for learning
      pre_tlambdas = post_tlambdas = stats::setNames(rep(0, length(snames)), snames)

      pre_tlambdas[allstims] = lambdas[allstims]
      post_tlambdas[poststims] = lambdas[poststims]

      #association deltas
      #first delta
      #excitatory
      ed1 = oh_prestims*alphas %*% t(pre_tlambdas*betas_ex)
      ed1 = t(t(ed1) * as.numeric((pre_tlambdas - ne1) > 0)) #only learn when expectation expectation is lower than lambda

      #inhibitory ; hack to collapse ne1 into a numeric
      id1 = oh_prestims*alphas %*% t((pre_tlambdas-as.numeric(ne1))*betas_in)
      id1[id1>0] = 0 #only learn when expectation is higher than lambda
      id1 = abs(id1)

      #second delta
      ed2 = oh_poststims*alphas %*% t(post_tlambdas*betas_ex)
      ed2 = t(t(ed2) * as.numeric((post_tlambdas - ne2) > 0)) #only learn when expectation expectation is lower than lambda

      id2 = oh_poststims*alphas %*% t((post_tlambdas-as.numeric(ne2))*betas_in)
      id2[id2>0] = 0 #only learn when expectation is higher than lambda
      id2 = abs(id2)
      diag(ed1) = diag(ed2) = diag(id1) = diag(id2) = 0

      #alpha deltas
      alphasd1 = oh_prestims %*% abs(gammas * (pre_tlambdas-ne1))
      alphasd2 = oh_poststims %*% abs(gammas * (post_tlambdas-ne2))
      diag(alphasd1) = diag(alphasd2) = 0

      #learn
      EV = EV+ed1+ed2
      IV = IV+id1+id2

      #Need to be careful here, as there is no decay for absent stimuli
      talphasd = (1-thetas)*alphas + thetas*(rowSums(alphasd1) + rowSums(alphasd2))
      alphas[allstims] = talphasd[allstims]

      #apply lower limit on alphas
      alphas[] = sapply(1:nstim, function(i) max(min_alphas[i], alphas[i]))
      #apply upper limit on alphas
      alphas[] = sapply(1:nstim, function(i) min(max_alphas[i], alphas[i]))

    }
  }

  mod@parameters = list(alphas = alphas,
                        min_alphas = min_alphas,
                        max_alphas = max_alphas,
                        lambdas = lambdas,
                        thetas = thetas,
                        gammas = gammas)
  mod@model_results = list(evs = evs,
                           ivs = ivs,
                           as = as,
                           es = es)
  mod@experience = experience
  mod@mapping = mapping
  mod
}
