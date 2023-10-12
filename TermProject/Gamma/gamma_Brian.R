# Contextual information about dataset:
# https://www.bls.gov/nls/
# https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/health

# Load in dataset
load('FairMLCourse-Data/fairml/national.longitudinal.survey.rda', verbose = T)

# Looking at the National Longitudinal Survey dataset weight variable
weight_col <- national.longitudinal.survey$weight

# Dataset info
n <- length(weight_col) # 4908
sample_mean <- mean(weight_col) # 154.5827
sample_var_S2 <- sum((weight_col - sample_mean)^2) / n  # 1100.988

# Save generated R plots to disk 
pr2file <- function(filename, w = 800, h = 800) {
  origdev <- dev.cur()
  parts <- strsplit(filename,".",fixed=TRUE)
  nparts <- length(parts[[1]])
  suff <- parts[[1]][nparts]
  if (suff == "pdf") {
    pdf(filename)
  }
  else if (suff == "png") {
    png(filename,bg='white', width = w, height = h)
  }
  else jpeg(filename)
  devnum <- dev.cur()
  dev.set(origdev)
  dev.copy(which = devnum)
  dev.set(devnum)
  dev.off()
  dev.set(origdev)
}

plot_density_curve <- function(plot_title) {
  plot(density(weight_col), 
       xlim = c(50, 350),
       ylim = c(0, 0.014),
       xlab = "Individual Human Weights",
       ylab = "Density",
       main = plot_title
      )
}

# Plot a histogram of the weight variable
hist(weight_col, 
     probability = TRUE, 
     xlab = "Individual Human Weights (lb)",
     xlim = c(50, 350),
     main = "Histogram of Individual Human Weights"
    )

# pr2file("gamma_histogram_weight.png")

# Plot density of the weight variable
plot_density_curve("Density Plot of Individual Human Weights")

# pr2file("gamma_sample_density.png")

# MLE 

# See latex document for derivation of these closed form mathematical equations
ln_sample <- log(weight_col)
mean_ln_sample <- mean(ln_sample)

# Partial derivative of log-likelihood function with respect to theta_1
find_estimate_theta1 <- function(x) {
  log(x) - log(sample_mean) - digamma(x) + mean_ln_sample
}

# Set the partial derivative of log-likelihood function with respect to theta_1 equal to 0
curve(find_estimate_r, from = 0, to = 350, ylab = "Density")
abline(h = 0)

# pr2file("find_theta1_root.png")

# Find the root of theta_1
theta_1 <- uniroot(find_estimate_theta1, lower = 0.01, upper = 50)
print(theta_1)

# Calculate theta_1_hat
theta_1_hat <- theta_1$root
print(theta_1_hat)

# Calculate theta_2_hat
theta_2_hat <- theta_1_hat / sample_mean
print(theta_2_hat)

# Graph MLE Superimposed
mle_density <- curve(dgamma(x, theta_1_hat, theta_2_hat), from = 0, to = 350)
# print(mle_density)

plot_density_curve("MLE Gamma Superimposed on Sample Density")
lines(mle_density$x, mle_density$y, col = "red")
legend("topright", legend = c("Sample Density", "MLE Gamma Density"), col = c("black", "red"), lty = 1)

# pr2file("gamma_mle.png")

# MME

lambda_estimator <- sample_mean / sample_var_S2
print(lambda_estimator)

r_estimator <- lambda_estimator * sample_mean
print(r_estimator)

# Graph MM superimposed
mm_density <- curve(dgamma(x, r_estimator, lambda_estimator), from = 0, to = 350)
# print(mm_density)

plot_density_curve("MME Gamma Superimposed on Sample Density")
lines(mm_density$x, mm_density$y, col = "red")
legend("topright", legend = c("Sample Density", "MME Gamma Density"), col = c("black", "red"), lty = 1)

# pr2file("gamma_mme.png")