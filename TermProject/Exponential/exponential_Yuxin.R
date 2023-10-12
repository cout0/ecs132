# https://archive.ics.uci.edu/ml/datasets/communities+and+crime
load("FairMLCourse-Data/fairml/communities.and.crime.rda", verbose = T)
# Land Area is measured by square miles per community and normalized between 0 to 1
landArea <- communities.and.crime$LandArea


png("./Exponential/figure1.png", width = 600, height = 500)
hist(landArea, probability = TRUE,
     xlim = c(0, 1), xlab = "Normalized Land Area",
     ylab = "Density",
     main = "Histogram of LandArea"
)
dev.off()


png("./Exponential/figure2.png", width = 600, height = 500)
# Because this is a normalized data between 0 to 1, we set xlim to these values
plot(density(landArea),
     xlim = c(0, 1),
     xlab = "Normalized Land Area",
     ylab = "Density",
     main = "Density Graph of LandArea"
)
dev.off()

# Bassic sample stat

sample_mean <- mean(landArea)
sample_var_S2 <- 1/(length(landArea)) * sum((landArea - sample_mean)^2)

# =========================== MLE ===========================
# Math                            -> R Code
# \Pi_{1}^{n} {L exp(-L * X_i)}
# Log form: n*log(L) - L \Sum{X_i}
# Set derivative to 0: n/L = \Sum{X_i}
# Therefore: L = 1 / \bar{A}      -> L = mean(landArea)

MLE_L <- 1/sample_mean
MLE_density <- curve(dexp(x, MLE_L))

png("./Exponential/figure3.png", width = 600, height = 500)
plot(density(landArea),
     xlim = c(0, 1), xlab = "Normalized Land Area",
     ylab = "Density",
     main = "Density Graph of LandArea with MLE Estimated Exponential Distribution"
)
lines(MLE_density$x, MLE_density$y, col = "red")
legend("topright", legend = c("Sample Density", "MLE Estimated Exp Distrution"), col = c("black", "red"), lty = 1)
dev.off()

# =========================== MME ===========================
# FIXME Fix this math
# Math                            -> R Code
# \bar{A} = C/L                   -> mean(landArea) = C/L
# S^2 = C/L^2                     -> 1/(len(landArea)) * sum((landArea - mean(landArea))^2)
# Therefore:  L = \bar{A} / S^2   -> L = mean(landArea) / 1/(len(landArea)) * sum((landArea - mean(landArea))^2)
# And:        C = L*\bar{A}       -> C = L * mean(landArea)

MM_L <- 1 / sample_mean
MM_C <- MM_L * sample_mean
MM_density <- curve(dexp(x, MM_L))

png("./Exponential/figure4.png", width = 600, height = 500)
plot(density(landArea),
     xlim = c(0, 1), xlab = "Normalized Land Area",
     ylab = "Density",
     main = "Density Graph of LandArea with MM Estimated Exponential Distribution"
)
lines(MM_density$x, MM_density$y, col = "red")
legend("topright", legend = c("Sample Density", "MM Estimated Exp Distrution"), col = c("black", "red"), lty = 1)
dev.off()

# Value Report
print(paste("MLE L: ", MLE_L))
print(paste("MM L: ", MM_L))
