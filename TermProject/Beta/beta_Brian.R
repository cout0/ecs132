# Dataset info: https://archive.ics.uci.edu/ml/datasets/communities+and+crime
load('FairMLCourse-Data\\fairml\\communities.and.crime.rda', verbose = T)

# Looking at the percentage of recent immigrants in the past 10 years in the communities and crime dataset 
PctRecImmig10_col <- communities.and.crime$PctRecImmig10

# Sample quantities
n <- length(PctRecImmig10_col) # 1969
sample_mean <- mean(PctRecImmig10_col) # 0.1822905
sample_var_S2 <- sum((PctRecImmig10_col - sample_mean)^2) / n # 0.05499034

# Save generated R plots to disk 
pr2file <- function(filename) {
  origdev <- dev.cur()
  parts <- strsplit(filename,".",fixed=TRUE)
  nparts <- length(parts[[1]])
  suff <- parts[[1]][nparts]
  if (suff == "pdf") {
      pdf(filename)
  }
  else if (suff == "png") {
      png(filename,bg='white', width = 800, height = 800)
  }
  else jpeg(filename)
  devnum <- dev.cur()
  dev.set(origdev)
  dev.copy(which = devnum)
  dev.set(devnum)
  dev.off()
  dev.set(origdev)
}

plot_density_curve <- function(plot_title, ylim = c(0, 6)) {
  plot(density(PctRecImmig10_col), 
       main = plot_title,
       xlim = c(0, 1),
       ylim = ylim,
       xlab = "Percentage of Recent Immigrants in the Past 10 Years",
       ylab = "Density"
       )
}

# Plot a histogram of the weight variable
hist(PctRecImmig10_col, 
     probability = TRUE,
     ylim = c(0, 6),
     xlab = "Percentage of Recent Immigrants in the Past 10 Years",
     main = "Histogram of the Percentage of Recent Immigrants in the Past 10 Years" 
)
          
# pr2file("beta_histogram_PctRecImmig10.png")

# Plot the density of the weight variable
# plot_density_curve("Density Plot of the Percentage of Recent Immigrants in the Past 10 Years")

# pr2file("beta_density_PctRecImmig10.png")

# MM 

A <- ((sample_mean^3 * ((1 / sample_mean) - 1)) / sample_var_S2) - sample_mean
print(A)

B <- A * ((1 / sample_mean) - 1)
print(B)

mme_density <- curve(dbeta(x, A, B), from = 0, to = 1)
print(mme_density)

# Graph of MM
plot_density_curve("MME Beta Superimposed on Sample Density", c(0, 10))
lines(mme_density$x, mme_density$y, col = "red")
legend("topright", legend = c("Sample Density", "MME Beta Density"), col = c("black", "red"), lty = 1)

pr2file("beta_mme.png")

# MLE 

# Replace 0s in the column to a value close to 0
PctRecImmig10_col_new <- ifelse(PctRecImmig10_col == 0, 0.0001, PctRecImmig10_col)

# Replace 1s in the column to a value close to 1
PctRecImmig10_col_new <- ifelse(PctRecImmig10_col_new == 1, 0.9999, PctRecImmig10_col_new)

library(stats4)

ll <- function(alpha, beta) {
  loglik <- sum(dbeta(PctRecImmig10_col_new, shape1 = alpha, shape2 = beta, log = TRUE))
  -loglik
}

z <- mle(minuslogl = ll, start = c(alpha = 0.1, beta = 0.1))

coefficients <- coef(z)

# Get MLE alpha estimate
theta_1_hat <- unname(coefficients["alpha"])
print(theta_1_hat)

# Get MLE beta estimate
theta_2_hat <- unname(coefficients["beta"])
print(theta_2_hat)

# print(paste("Number of changed zeros: ", sum(PctRecImmig10_col_new == 0.0001)))   # 48
# print(paste("Number of changed ones: ", sum(PctRecImmig10_col_new == 0.9999)))    # 46

# Graph of MLE 

mle_density <- curve(dbeta(x, theta_1_hat, theta_2_hat), from = 0, to = 1)
# print(mle_density)

plot_density_curve("MLE Beta Superimposed on Sample Density", c(0, 10))
lines(mle_density$x, mle_density$y, col = "red")
legend("topright", legend = c("Sample Density", "MLE Beta Density"), col = c("black", "red"), lty = 1)

pr2file("beta_mle.png")