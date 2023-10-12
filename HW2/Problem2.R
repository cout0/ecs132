generateA <- function (Vlen, numDoor) {
  A <- sample(1:numDoor, Vlen)
}

anotherDoor <- function (numDoor, H) {
  doors <- 2:numDoor
  doors <- doors[which(doors != H)]
  sample(doors, 1, replace = TRUE)
}

# ChangeMind
# Param: p, the probability of changing mind. (0~1)
# Ret: boolean, TRUE if change mind, FALSE if stick with door 1
changeMind <- function (p) {
  sample(c(TRUE, FALSE), 1, replace = TRUE, prob = c(p, 1-p))
}

generateH <- function (A, numDoor) {
  doors <- 2:numDoor
  doors <- doors[!doors %in% A]
  sample(doors, 1, replace = TRUE)
}

# reveal
# Param: v, vector of value; A, random vector of price containning door; C, contestant's choice
# Ret: value of prize won, if not won, return 0
reveal <- function (v, A, C) {
  isWin <- C %in% A
  if (isWin) {
    return(v[which(A == C)])
  }
  return(0)
}

simEVarW <- function (d, v, p, nreps) {
  # Var(A)  = E((A - EA)^2)
  #         = E(A^2 + (EA)^2 - 2(A*EA))
  #         = E(A^2) + E((EA)^2) - E(2(A*EA))
  #         = E(A^2) + (EA)^2 - 2(E(A*EA))
  #         =                   2(EA)^2
  # Var(A)  = E(A^2) - (EA)^2
  result <- vector(length = nreps)
  for (rep in 1:nreps) {
    Vlen <- length(v)
    A <- generateA(Vlen, d)
    C <- 1
    H <- generateH(A, d)
    if (changeMind(p)) {
      C <- anotherDoor(d, H)
    }
    result[rep] <- reveal(v, A, C)
  }
  c(mean(result), var(result))
}

# print(simEVarW(3, c(10), 0.3, 100000))
# simEVarW(3, c(10), 0.3, 200000)
