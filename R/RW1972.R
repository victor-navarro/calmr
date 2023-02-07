#' Train the RW1972 model
#'
#' @param alphas A named vector with stimulus saliences.
#' @param betas_on A named vector with stimulus associabilities for present stimuli.
#' @param betas_off A named vector with stimulus associabilities for absent stimuli.
#' @param lambdas A named vector with stimulus asymptotes.
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

RW1972 <- function(alphas,
                   betas_on,
                   betas_off,
                   lambdas,
                   V = NULL,
                   experience,
                   mapping){

  mod = new("CalmrModel",
            model = "RW1972")

  #data initialization
  ntrials = length(experience$tp)
  if(is.null(V)){V = gen_ss_weights(mapping$unique_functional_stimuli)}

  vs = es = array(NA, dim = c(ntrials, dim(V)),
             dimnames = list(NULL, rownames(V), rownames(V)))

  betas_off_avg = tapply(betas_off, mapping$nomi2func, mean) #average saliencies

  fsnames = test_stims = rownames(V) #get functional stimuli names
  nsnames = names(alphas) #get nominal stimuli names

  for (t in 1:ntrials){
    #get pre functional and nominal stimuli
    fprestims = mapping$trial_pre_func[[experience$tp[t]]]
    nprestims = mapping$trial_pre_nomi[[experience$tp[t]]]
    #get post nominal stimuli
    fpoststims = mapping$trial_post_func[[experience$tp[t]]]
    npoststims = mapping$trial_post_nomi[[experience$tp[t]]]

    #make one-hot vector of functional stimuli (for learning)
    oh_fstims = .makeOH(c(fprestims, fpoststims), fsnames)
    oh_fprestims = .makeOH(fprestims, fsnames)
    oh_fpoststims = .makeOH(fpoststims, fsnames)

    #generate expectations
    e1 = oh_fprestims %*% V #first expectation
    e2 = oh_fpoststims %*% V #second expectation

    #generate full expectation matrix (only for data saving purposes)
    pre_emat = V*oh_fprestims

    #learn if we need to
    if (!experience$is_test[t]){
      #get alphas betas and lambdas for learning
      talphas = tbetas = pre_tlambdas = post_tlambdas = stats::setNames(rep(0, length(fsnames)), fsnames)

      #populating vector with nominal stimuli values as functional stimuli values
      talphas[mapping$nomi2func[c(nprestims, npoststims)]] = alphas[c(nprestims, npoststims)]

      #vector is initialized as if all stimuli are absent
      tbetas = betas_off_avg
      tbetas[mapping$nomi2func[c(nprestims, npoststims)]] = betas_on[c(nprestims, npoststims)]

      pre_tlambdas[mapping$nomi2func[c(nprestims, npoststims)]] = lambdas[c(nprestims, npoststims)]
      post_tlambdas[mapping$nomi2func[c(npoststims)]] = lambdas[c(npoststims)]


      #Learn
      err1 = oh_fstims*pre_tlambdas-e1 #first error (includes all stimuli in the sequence)
      err2 = oh_fpoststims*post_tlambdas-e2 #second error (includes only the second half stimuli)

      d1 = (oh_fprestims*talphas*tbetas)%*%err1 #first delta
      d2 = (oh_fpoststims*talphas*tbetas)%*%err2 #second delta

      diag(d1) = diag(d2) = 0

      V = V+d1+d2 #learn
    }

    #save data
    vs[t, , ] = V
    es[t, , ] = pre_emat
  }
  mod@parameters = list(alphas = alphas,
                        betas_on = betas_on,
                        betas_off = betas_off,
                        lambdas = lambdas)
  mod@model_results = list(vs = vs,
                            es = es)
  mod@experience = experience
  mod@mapping = mapping
  mod
}
