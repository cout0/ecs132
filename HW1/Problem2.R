ExactAnalysis <- function() {
  m <-5 # desired bus arriving time
  p <- c(0.5, 0.5) # prob of delay times between successive buses
  v <- 2 # length of time to the station of Interest
  k <- 1 # ARG: waiting time for passenger
  r <- 3 # ARG: bus leaving time from main station
  q <- 1 # ARG: length between two successive bus

  # 1. P(W = 1) = P(L1 = 2 and L2 = 2) + P(L1 + L2 = 2 and L3 = 2)
  # P1 <- p[1] * p[1] + p[1] * p[1] * p[1]

  # 2. P(Bus 2 leaves the main station at time 3) = P(L1 = 1, L2 = 2) + P(L1 = 2, L2 = 1) = P(L1 = 1) * P(L2 = 2) + P(L1 = 2) * P(L2 = 1)
  P2 <- p[1] * p[2] + p[2] * p[1]

  # 3. P(W = 1 | L1 = 1) = P(L2 = 1, L3 = 2 | L1 = 1) = P(L2 = 1, L3 = 2) = P(L2 = 1) * P(L3 = 2) 
  P3 <- p[1] * p[2]

  # 4. P(U = 3) = P(U != 1 and U != 2) and P(X3 >= 3) = P(U != 2) and P(U = 3) = P(X2 < 3) * P(X3 >= 3) = P(X2 < 3) * P(1) = P(L1 = 1 and L2 = 1)
  # P4 <- (p[1] * p[1])

  # 5. E(W) = \sum_{i=0}^{1} i * P(W=i) = 0 * P(W = 0) + 1 * P(W = 1) = P(W = 1) = 3/8
  # P5 <- P1

  # 6. Var(W) = E((W - EW)^2) = \sum_{i=0}^{1} (i-3/8)^2 * P(W = i) = (0 - 3/8)^2 * P(W = 0) + (1 - 3/8)^2 * P(W = 1) = 9/64 * P(W =/= 1) + 25/64 * 3/8 = 9/64 * 5/8 + 25/64 * 3/8 = 0.234375
  # P6 <- 9/64 * 5/8 + 25/64 * 3/8

  # 7. E(Bu) = \sum {i=0}^{3} i * P(Bu=i) (only consider Bu = 2 or Bu = 3) = 2 * P(Bu = 2) + 3 * P(Bu = 3) = 2 * 0.75 + 3 * 0.25 = 1.5 + 0.75 = 2.25 
  # P(Bu = 2) = P(U = 2) = P(U =/= 1 and U =/= 3) = P(U =/= 1) * P(U =/= 3) = 1 * (1 - 0.25) = 0.75
  # P(Bu = 3) = P(U = 3) = 0.25
  # P7 <- 2 * (1 - P3) + 3 * P3

  # 8. E(number of buses leaving the main station by time m) 
  #      1      2     3     4     5     6 (times)
  # b1  1/2   1/2 
  # b2        1/4    2/4   1/4
  # b3               1/8   3/8   3/8   1/8
  # b4                     1/16  4/16  8/16
  # b5                           1/32  5/32
  # b6
  # (buses)
  
  # Add up all entries up to and including the 5th time period column
  #  1/2 + 1/2 + 1/4 + 2/4 + 1/4 + 1/8 + 3/8 + 3/8 + 1/16 + 4/16 + 1/32 = 103/32 = 3.21875 
  #P8 <- 103 / 32

  # 9. Var(number of buses leaving the main station by time m)

  # 10. E(number of buses leaving the main station by time m | W = k)

  result <- c(P2, P3)

  return(result)
}

busSim <- function(m, p, v, k, r, q, nDays) {
  numOfBus <- m + k # m + v + k is the most buses will appear in question
  # set L for all bus for all nDays
  LnDays <- matrix(generateL(p, numOfBus, nDays), nrow=numOfBus, ncol=nDays)

  # Calculate X for all bus for all nDays
  XnDays <- array(0, c(numOfBus, nDays))
  XnDays[1, ] <- LnDays[1, ]
  for (day in 1:nDays) {
    for (bus in 2:numOfBus) {
      XnDays[bus, day] <- XnDays[bus - 1, day] + LnDays[bus, day]
    }
  }

  # Calcualte W and U for all bus for all nDays
  WnDays <- vector(length=nDays)
  UnDays <- vector(length=nDays)
  for (day in 1:nDays) {
    w <- v
    for (bus in 1:numOfBus) {
      w <- w + LnDays[bus, day]
      if (w >= m) {
        UnDays[day] <- bus
        break
      }
    }
    WnDays[day] <- w - m
  }

  # 1. P(W = k)
  ret1 <- mean(WnDays == k)

  # 2. P(Bus 2 leaves the main station at time r)
  ret2 <- mean(XnDays[2,] == r)

  # 3. P(W = k | L1 = q)
  ret3 <- mean(WnDays[(which(LnDays[1, ] == q))])

  # 4. P(U=3)
  ret4 <- mean(UnDays == 3)

  # E(W)
  ret5 <- mean(WnDays)

  # Var(W)
  ret6 <- mean((WnDays - ret5)^2)

  # E(Bu)
  ret7 <- mean(UnDays)

  # E(number of buses leaving the main station by time m)
  ret8 <- sum(XnDays <= m) / nDays

  # Var(number of buses leaving the main station by time m)
  depart_M <- colSums(XnDays <= m)
  ret9 <- mean((depart_M - ret8)^2) 

  # E(number of buses leaving the main station by time m | W = k)
  W_eq_k <- WnDays == k
  ret10 <- sum(colSums(XnDays[, which(W_eq_k)] <= m)) / sum(W_eq_k)

  return(c(ret1, ret2, ret3, ret4, ret5, ret6, ret7, ret8, ret9, ret10))
}

generateL <- function(p, numOfBus, nDays) {
  sample(seq_along(p), numOfBus * nDays, replace=TRUE, prob=p)
}