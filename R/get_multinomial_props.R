#' Use multinomial function to automatically fill holes in age data
#'
#' Function to compute proportions-at-age for each length using multinomial approach
#' From Chris Legault (which was based on code from Mike Bednarski)
#'
#' @param ageData Data frame
#' @param small.len Numeric scalar. Smallest length to be estimated (Usually based on observations for the year)
#' @param big.len Numeric scalar. Largest length to be estimated (Usually based on observations for the year)
#' @param small.age Numeric scalar.
#' @param big.age Numeric scalar. Maximum age needed in the key (Usually based on observations for the year)
#' @param ref.age Numeric scalar. Used in the GLM as the age against which all
#' the other ages are estimated. It is typically the most abundant age.
#'
#' @return List
#'
#' \item{converg}{Indicating if the fit converged. }
#' \item{pred.p}{Matrix. Age-length Key. Rows are lengths, columns ages}
#' @export

get_multinomial_props <- function(agedata, small.len=1, big.len, small.age, big.age, ref.age, printConvergence = F){


  agedata$AGE <- relevel(as.factor(agedata$AGE), ref=ref.age)  # relevel and make categorical
  my.levels <- as.numeric(levels(agedata$AGE))
  n.levels <- length(my.levels)
  mn <- nnet::multinom(AGE ~ LENGTH, data=agedata,maxit=1000,trace=printConvergence)
  Parameters <- summary(mn)$coefficients   # Parameters for multinomial key

  newdata <- data.frame(cbind(LENGTH = small.len:big.len)) # length values
  logits <- matrix(NA, nrow=length(small.len:big.len), ncol=length(my.levels))
  logits[,1] <- rep(0,nrow(newdata)) # reference age
  for (i in 1:(n.levels-1)){
    logits[,(i+1)] <- Parameters[i] + Parameters[(n.levels-1+i)]*newdata$LENGTH
  }

  rownames(logits) <- as.character(small.len:big.len)
  colnames(logits) <- my.levels

  p.unscaled <- exp(logits)
  p.normalized <- p.unscaled / rowSums(p.unscaled)
  p <- matrix(0, nrow=length(small.len:big.len), ncol=length(small.age:big.age))

  rownames(p) <- as.character(small.len:big.len)
  colnames(p) <- as.character(small.age:big.age)
  for(i in 1:ncol(p.normalized)){
    p.colname <- colnames(p.normalized)[i]
    p[,p.colname] <- p.normalized[,i]
  }
  return(list(converg=mn$convergence, pred.p=p))
}
