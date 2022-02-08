#' Trains a Pavlovian HeiDI
#' @param sals A named vector with stimulus saliencies.
#' @param cons A named vector with stimulus constants.
#' @param w A named array of dimensions S,S; where S is the number of stimuli.
#' @param ts A vector of trial pointers for training, as a function of trials.
#' @param trials A list of length T, with character vectors specifying the stimuli involved in each trial. T is the number of unique trials in the experiment.
#' @param trial_names (optional) A character vector of length T with the names of the trials
#' @param phase (optional) A character vector of length T with the names of the phases
#' @param targets A character vector specifying the target stimulus for each trial. Under development. For now, just the US.
#' @return A list with
#' \itemize{
#' \item{ws - An array of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment.}
#' \item{rs,combvs, chainvs - Lists of length P with the r-values, combination vs, and chain vs.}
#' \item{ts, trials, trial_names, phase -  Carryover for further processing. See Arguments.}
#' }
#' @note The array w contains the associations for all stimuli involved in the experiment. Entry i,j specifies the associative strength between stimulus i to stimulus j. Entry j,i specifies the opposite direction.
#' @export
train_pav_heidi <- function(sals, cons, w, ts, trials, trial_names = NULL, phase = NULL, targets = 'US'){
  maxstim = dim(w)[1]
  ws = array(NA, dim = c(length(ts), dim(w)),
             dimnames = list(NULL, rownames(w), colnames(w)))
  rs = combvs = chainvs = vector('list', length(ts))
  stimnames = array(NA, dim = c(length(ts), maxstim))
  snames = rownames(w)

  for (t in 1:length(ts)){
    stims = trials[[ts[t]]]
    oh_stims = .makeOH(stims, snames)
    teststim = setdiff(stims, targets)

    #functional class activations (comb)
    combV = .combV(w, teststim, targets, cons, t)

    #chain activations (chain)
    chainV = .chainV(w, teststim, targets, cons, t)

    #Now we calculate rs for all the stimuli involved in the design (snames)
    #First, we need to identify absent stimuli and calculate their [activated] saliency
    tsals = sals
    absent = snames[!(snames %in% stims)]
    if (length(absent)){
      tsals[absent] = .absentAlpha(w, teststim, absent, cons, t)
    }

    #Distribute R
    r = .distR(tsals, combV, chainV, t)

    #Now we update
    v = oh_stims %*% w #expectation
    e = oh_stims*cons*sals-v #error
    d = oh_stims*sals%*%e #delta
    diag(d) = 0

    #And learn
    w = w+d

    #save data
    ws[t, , ] = w
    rs[[t]] = r
    combvs[[t]] = combV
    chainvs[[t]] = chainV
  }
  dat = list(ws = ws, rs = rs, combvs = combvs, chainvs = chainvs, ts = ts, trials = trials, trial_names = trial_names, phase = phase)
  return(dat)
}

#### Internal functions ####
#Calculation of combined V
.combV <- function(w, pre, post, cs, db_trial = NA){
  #w is a weight matrix,
  #pre is a character vector of the stimuli being presented
  #post is a character vector of the stimuli being predicted
  #cs are the constants for all the stimuli
  #
  #returns a matrix of dimensions pre x post, with the combV values

  #intial implementation used invidual terms that were later summed
  #late implementation just returns

  #if (db_trial == 1) browser()
  #if (length(pre) > 1)
  mat = array(0, dim = c(1, length(post)), dimnames = list(paste0(pre, collapse = ''), post))
  for (po in post){
      mat[1, po] = sum(w[pre, po])+(sum(w[pre, po])*(sum(w[po, pre]))/cs[po])
  }
  return(mat)
}

#Calculation of chain V
.chainV <- function(w, pre, post, cs, db_trial = NA){
  #w is a weight matrix,
  #pre is a character vector of the stimuli being presented
  #post is a character vector of the stimuli being predicted
  #cs are the constants for all the stimuli
  #
  #The trick here is to obtain the chainV from every pre stimulus to every post stimulus,
  #while hitting every intermediate stimulus along the way
  #
  #Returns a matrix of dimensions pre post with chainV values
  allstims = rownames(w)
  #Due to specific rules, here we must preallocate zeros
  mat = array(0, dim = c(length(pre), length(post)), dimnames = list(pre, post))
  for (po in post){
    for (pr in pre){
      int = setdiff(allstims, c(pre, po)) #find the intermediate stimuli that are absent
      if (length(int)){
        mat[pr, po] = sum(sapply(int, function(i) sum(w[pr, i]*sum(.combV(w, i, po, cs, db_trial)[, po])/cs[po])))
      }
    }
  }
  return(mat)
}

#Distribution of R among stimuli
.distR <- function(sals, combv, chainv, db_trial = NA){
  #Distributes the associative strength among all stimuli (sals)
  #returns a matrix of dimensions length(sals) x length(V)
  #if (nrow(chainv) > 1) browser()
  mat = (sals/sum(abs(sals)))%*%(combv+colSums(chainv))
  rownames(mat) = names(sals)
  return(mat)
}

#Function to calculate the alpha of absent stimuli
.absentAlpha <- function(w, pre, absent, cs, db_trial = NA){
  #w is a weight matrix,
  #pre is a character vector of the stimuli being presented
  #post is a character vector of the absent stimuli
  #cs are the constants for all the stimuli
  #
  #The trick here is to obtain the chainV from every pre stimulus to every post stimulus,
  #while hitting every intermediate stimulus along the way
  #
  #Returns a vector of length absent
  allstims = rownames(w)
  return(
    sapply(absent, function(ab){
      sum(sapply(pre, function(pr){
        int = setdiff(setdiff(allstims, ab), pr)
        return(w[pr, ab] + sum(sapply(int, function(i) w[pr, i]*w[i, ab]/cs[ab], USE.NAMES = F)))
      }))
    })
  )
}

#Makes a onehot representation of the stimulus vector, given all stimuli
.makeOH <- function(s, stimnames){
  return(as.numeric(stimnames %in% s))
}

#### Unused ####
#Softmax function
.soft <- function(acts, temp = 10){
  acts = acts-max(acts)
  return(exp(acts*temp)/sum(exp(acts*temp)))
}




