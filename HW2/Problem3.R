tags <- function(m, k, s) {
  # Base cases
  # Check for non-positive m, k, s
  if (s < 1 || k < 1 || m < 1) {
    return(0)
  }

  # Base case when there is only 1 draw left
  if (s == 1 && k <= m) {
    return((m-k+1) / m)
  } else if (s == 1 && k > m) {
    return(0)
  }

  # Recursive step
  prob <- 0
  # Iterate through all possible tag draws
  for (i in 1:m) {
    prob <- prob + tags(m, k-i, s-1) * 1/m
  }
  return(prob)
}

# print(tags(3, 10, 4))
# print(tags(5, 6, 2))