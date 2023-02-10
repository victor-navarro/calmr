#' Model support functions
#' @description An assortment of functions to support models (mostly HeiDI)

#Calculation of combined V
.combV <- function(V, pre_func, post_func, db_trial = NA){
  #V is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #post_func is a character vector of the stimuli being predicted
  #
  #returns a matrix of dimensions pre_func x post_func, with the combV values

  #intial implementation used invidual terms that were later summed
  #late implementation just returns

  #if (db_trial == 1) browser()
  #if (length(pre_func) > 1)
  mat = array(0, dim = c(1, length(post_func)), dimnames = list(paste0(pre_func, collapse = ''), post_func))
  for (po in post_func){
    mat[1, po] = sum(V[pre_func, po])+(sum(V[pre_func, po])*(sum(V[po, pre_func])))
  }
  return(mat)
}

#Calculation of chain V
.chainV <- function(V, pre_func, post_func, db_trial = NA){
  #V is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #post_func is a character vector of the stimuli being predicted
  #
  #The trick here is to obtain the chainV from every pre_func stimulus to every post_func stimulus,
  #while hitting every absent stimulus along the way
  #
  #Returns a matrix of dimensions pre_func post_func with chainV values

  #Preallocate zeros
  mat = array(0, dim = c(length(pre_func), length(post_func)), dimnames = list(pre_func, post_func))

  #find the absent stimuli
  allstims = rownames(V)
  absent = setdiff(allstims, pre_func)

  if (length(absent)){
    #a for loop for readability
    for (po in post_func){
      for (pr in pre_func){
        total_sum = 0
        for (a in absent){
          total_sum = total_sum + V[pr, a]*.combV(V, a, po, db_trial)[, po]
        }
        mat[pr, po] = total_sum
      }
    }
  }
  return(mat)
}

#Calculation of chain V with Similarity
.chainVSim <- function(V, as_nomi, as_avg, pre_nomi, pre_func, post_func, db_trial = NA){
  #Same as above, but with similarity of retrieved and nominal alphas modulating the the chain
  #as: a vector of nominal saliencies

  #Preallocate zeros
  mat = array(0, dim = c(length(pre_func), length(post_func)), dimnames = list(pre_func, post_func))

  #get absent stimuli
  allstims = rownames(V)
  absent = setdiff(allstims, pre_func)

  #a for loop for readability
  if (length(absent)){
    #get retrieved alphas
    retrieved_as = .absentalphas(V = V, pre_func = pre_func, db_trial = NA)
    #get the average of their nominal alphas (TEMPORARY)
    nomi_avg_as = as_avg[absent]
    for (po in post_func){
      for (pr in pre_func){
        total_sum = 0
        for (a in absent){
          total_sum = total_sum +
            .alphaSim(retrieved_as[a], nomi_avg_as[a])*V[pr, a]*.combV(V, a, po, db_trial)[, po]
        }
        mat[pr, po] = total_sum
      }
    }
  }
  #if (db_trial == 10) browser()
  return(mat)
}

#Distribution of R among stimuli
.distR <- function(alphas, combv, chainv, db_trial = NA){
  #Distributes the associative strength among all stimuli (alphas)
  #returns a matrix of dimensions length(alphas) x ncols(combv)
  #if (nrow(chainv) > 1) browser()
  #if (db_trial > 10) browser()
  mat = (alphas/sum(alphas))%*%(combv+colSums(chainv))
  rownames(mat) = names(alphas)
  return(mat)
}

.getalphas <- function(alphas_nomi, V, pre_nomi, pre_func, fsnames, nomi2func,
                       absent_func = .absentalphas,
                       db_trial = NA){
  #gets the saliencies for a given trial
  #it performs two actions:
  #1. populates a vector of saliencies for functional stimuli
  #[this based on the saliency (alphas) of the nominal stimuli on the trial (pre_nomi)]
  #2. calculates the saliency for absent stimuli, via the .absentalphas function
  as = stats::setNames(rep(0, length(fsnames)), fsnames)
  #Annoying bit again, see main function
  as[nomi2func[pre_nomi]] = alphas_nomi[pre_nomi]
  #now do absent stimuli
  absent = names(as[as==0])
  if (length(absent)){
    as[absent] = absent_func(V = V, pre_func = pre_func, db_trial = t)
  }
  as
}

#Function to calculate the alpha of absent stimuli (SIMPLE)
.absentalphas <- function(V, pre_func, db_trial = NA){
  #NOTE (VN): This implementation simplifies the absent alpha to be only the sum of forward associations
  #V is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #
  #Returns a vector of alphas equal to the number of absent of stimuli
  allstims = rownames(V)
  absent = setdiff(allstims, pre_func)
  as = stats::setNames(rep(0, length(absent)), absent)
  for (ab in absent){
    as[ab] = sum(abs(V[pre_func, ab]))
  }
  as
}

#Function to calculate the alpha of absent stimuli (COMPLEX)
.absentalphas_complex <- function(V, pre_func, db_trial = NA){
  #NOTE (VN): This implementation goes through chained associations
  #V is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #
  #Returns a vector of alphas equal to the number of absent of stimuli
  allstims = rownames(V)
  absent = setdiff(allstims, pre_func)
  as = stats::setNames(rep(0, length(absent)), absent)
  for (ab in absent){
    total_sum = 0
    for (pr in pre_func){
      total_sum = total_sum + V[pr, ab] #the direct association
      #now do the indirect associations via other absent stimuli
      int = setdiff(setdiff(absent, ab), pr) #the other absent stimuli
      if (length(int)){
        total_sum = total_sum + sum(sapply(int, function(i) V[pr, i]*V[i, ab], USE.NAMES = F))
      }
    }
    as[ab] = abs(total_sum) #Note the absolute function; important to study it
    #as[ab] = min(c(0, total_sum))
  }
  as
}

#Returns the similarity between two (salience) values
.alphaSim <- function(i, j){
  (i/(i + abs(i-j))) * (j/(j+ abs(i-j)))
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
