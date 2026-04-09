rm(list = ls())

# Load Data
read_data <- read.table("data.txt")
sample_1 <- read_data[,1]
sample_2 <- read_data[,2]
sample_3 <- read_data[,3]

# PART A: Histograms, Mean, Variance, and Normality Check
par(mfrow = c(2, 3))

hist(sample_1, main = "Sample 1 Histogram", xlab = "Value", col = "steelblue", border = "black")
hist(sample_2, main = "Sample 2 Histogram", xlab = "Value", col = "darkorange", border = "black")
hist(sample_3, main = "Sample 3 Histogram", xlab = "Value", col = "seagreen", border = "black")

qqnorm(sample_1, main = "Sample 1 Q-Q Plot"); qqline(sample_1, col = "red")
qqnorm(sample_2, main = "Sample 2 Q-Q Plot"); qqline(sample_2, col = "red")
qqnorm(sample_3, main = "Sample 3 Q-Q Plot"); qqline(sample_3, col = "red")

cat("PART A: Summary Statistics\n")
cat("Sample 1 - Mean:", mean(sample_1), "  Variance:", var(sample_1), "\n")
cat("Sample 2 - Mean:", mean(sample_2), "  Variance:", var(sample_2), "\n")
cat("Sample 3 - Mean:", mean(sample_3), "  Variance:", var(sample_3), "\n")
cat("Sample 1 is drawn from a normal distribution (bell-shaped histogram, linear Q-Q plot)\n")


# PART B: Inference on Sample 1
cat("\nPART B: 99% Two-Sided Confidence Interval for Mean\n")
n1    <- length(sample_1)
xbar  <- mean(sample_1)
s     <- sqrt(var(sample_1))
t_crit <- qt(0.995, df = n1 - 1)   # two-sided 99% -> alpha/2 = 0.005
margin <- t_crit * s / sqrt(n1)

cat("n =", n1, "\n")
cat("Sample Mean:", xbar, "\n")
cat("Sample SD:", s, "\n")
cat("t critical value:", t_crit, "\n")
cat("99% CI: [", xbar - margin, ",", xbar + margin, "]\n")

cat("\nPART B: Chi-Squared Test H0: sigma^2 = 0.5 vs H1: sigma^2 != 0.5\n")
sigma0_sq <- 0.5
chi_stat  <- (n1 - 1) * var(sample_1) / sigma0_sq
p_lower   <- pchisq(chi_stat, df = n1 - 1)
p_upper   <- 1 - pchisq(chi_stat, df = n1 - 1)
p_value   <- 2 * min(p_lower, p_upper)

cat("Chi-squared statistic:", chi_stat, "\n")
cat("Degrees of freedom:", n1 - 1, "\n")
cat("P-value (two-sided):", p_value, "\n")
if (p_value < 0.01) {
  cat("Conclusion: Reject H0 at 1% significance level.\n")
} else {
  cat("Conclusion: Fail to reject H0 at 1% significance level.\n")
}


# PART C: One-Sided Proportion Test on Sample 3
# H0: p >= 0.10  vs  H1: p < 0.10

cat("\nPART C: Proportion Test (Defective Rate < 10%)\n")
n3        <- length(sample_3)
defective <- sum(sample_3 == 0)
p_hat     <- defective / n3
p0        <- 0.10

z_stat  <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n3)
p_val_c <- pnorm(z_stat)   # left-tailed

cat("n =", n3, "\n")
cat("Number of defective parts:", defective, "\n")
cat("Sample proportion defective (p_hat):", p_hat, "\n")
cat("Z statistic:", z_stat, "\n")
cat("P-value (left-tailed):", p_val_c, "\n")
if (p_val_c < 0.05) {
  cat("Conclusion: Reject H0. There is sufficient evidence that the defective rate < 10%.\n")
} else {
  cat("Conclusion: Fail to reject H0.\n")
}