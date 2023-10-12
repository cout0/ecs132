buildAdjMat <- function (attachHistory) {
  len <- length(attachHistory) + 2
  mat <- matrix(rep(0, len * len), ncol=len,nrow=len)
  mat[1, 2] <- 1
  mat[2, 1] <- 1
  # for one record in history, two nodes to update:
  # the new added node
  # the node attached to
  for (i in seq_along(attachHistory)) {
    # The new added nodes (history start at 3rd node)
    mat[i + 2, attachHistory[i]] <- 1
    # The node attached to (at the diagonal position)
    mat[attachHistory[i], i + 2] <- 1
  }
  return(mat)
}

attachProb <- function (attachHis) {
  prob <- vector(length=(length(attachHis)+2))
  mat <- buildAdjMat(attachHis)
  total <- sum(rowSums(mat))
  for (i in seq_along(prob)) {
    # num of attachment for one node / total num of attachment in the matrix/network
    prob[i] <- sum(mat[i, ]) / total
  }
  return(prob)
}

findMax <- function(adjMat) {
  return(max(rowSums(adjMat)))
}

PAMsim <- function(nGen) {
  attachHistory <- vector(length=0)
  for (i in 1:nGen) {
    # Append to attachHistory updates of the attachment history per generation
    attachHistory <- c(attachHistory, sample(1:(i+1), 1, replace=TRUE, prob=attachProb(attachHistory)))
  }

  # Build the adjacency matrix from attachHistory
  adjMat <- buildAdjMat(attachHistory)
  return(list(adjMat = adjMat, attachHistory = attachHistory))
}

PAMemaxd <- function(nGen, nReps) {
  maxSum <- 0
  for (rep in 1:nReps) {
    # mat is the adjacency matrix element of PAMsim
    mat <- PAMsim(nGen)$adjMat
    # for each rep, sum up the max degree of the adjacency matrix
    maxSum <- maxSum + findMax(mat)
  }
  # return the long run average of the  degree of the adjacency matrix
  return(maxSum / nReps)
}

# print(PAMsim(1))
# print(PAMemaxd(1, 1000))
