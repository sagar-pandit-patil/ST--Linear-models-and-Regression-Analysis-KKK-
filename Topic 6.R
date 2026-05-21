#Q1
x=c(1,1.5,2,3,4,4.5,5,5.5,6,6.5,7,8,9,10,11,12,13,14,15)
y=c(6.3,11.1,20,24,26.1,30,33.8,34,38.1,39.9,42,46.1,53.1,52,52.5,48,42.8,27.8,21.9)
plot(x,y)
model=lm(y~x+I(x^2))
model
summary(model)
VIF=1/(1- 0.9085)
VIF
x_bar=mean(x)
x_bar
x_center=x-x_bar
x_center
model1=lm(y~x_center+I(x_center^2))
summary(model1)
vif=1/(1-0.9085)
vif
anova(model)

# ============================================================================
# PROBLEM 1
# ============================================================================

# Data
x <- c(1, 5, 2, 3, 4, 5, 5, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
y <- c(6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1, 39.9, 42, 46.1, 53.1, 52, 52.5, 48, 42.8, 27.8, 21.9)

# (a) Scatter plot
plot(x, y) 

# (b) Second order model y = b0 + b1*x + b2*x^2
model_b <- lm(y ~ x + I(x^2))
cat("(b) Second order model (original x):\n")
print(summary(model_b))

# VIF for original polynomial
library(car)
vif_b <- vif(model_b)
cat("\nVIF (original):\n")
print(vif_b)

# (c) Centered model: y = b0 + b1*(x - xbar) + b2*(x - xbar)^2
x_bar <- mean(x)
x_c <- x - x_bar
model_c <- lm(y ~ x_c + I(x_c^2))
cat("\n(c) Second order model (centered x):\n")
print(summary(model_c))

vif_c <- vif(model_c)
cat("\nVIF (centered):\n")
print(vif_c)
cat("\nConclusion: Centering drastically reduces VIF, mitigating multicollinearity.\n\n")

# (d) Test for significance of regression
f_stat <- summary(model_b)$fstatistic
f_pval <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail=FALSE)
cat(sprintf("(d) F-test for overall significance: F = %.4f, p-value = %.4e\n", f_stat[1], f_pval))
if (f_pval < 0.05) {
  cat("=> Regression is significant (p < 0.05).\n\n")
} else {
  cat("=> Regression is not significant.\n\n")
}

# (e) Test significance of quadratic term
coef_summary <- summary(model_b)$coefficients
t_stat_x2 <- coef_summary[3, "t value"]
pval_x2 <- coef_summary[3, "Pr(>|t|)"]
cat(sprintf("(e) Quadratic term t-test: t = %.4f, p-value = %.4f\n", t_stat_x2, pval_x2))
if (pval_x2 < 0.05) {
  cat("=> Quadratic term is statistically significant.\n")
} else {
  cat("=> Quadratic term is not statistically significant.\n")
}

# ============================================================================
# PROBLEM 2
# ============================================================================

# Data
y2 <- c(2.6, 2.4, 17.32, 15.6, 16.12, 5.36, 6.19, 10.17, 2.62, 2.98, 6.92, 7.06)
x1 <- c(31, 31, 31.5, 31.5, 31.5, 30.5, 31.5, 30.5, 31, 30.5, 31, 30.5)
x2 <- c(21, 21, 24, 24, 24, 22, 22, 23, 21.5, 21.5, 22.5, 22.5)

# (a) Fit second order polynomial model and test significance
model2 <- lm(y2 ~ x1 + x2 + I(x1^2) + I(x2^2) + x1:x2)
cat("(a) Full second order model:\n")
print(summary(model2))

f_pval2 <- summary(model2)$fstatistic
f_pval2_val <- pf(f_pval2[1], f_pval2[2], f_pval2[3], lower.tail=FALSE)
cat(sprintf("F-test p-value: %.4e\n", f_pval2_val))
if (f_pval2_val < 0.05) {
  cat("=> Regression is significant.\n\n")
} else {
  cat("=> Regression is not significant.\n\n")
}

# (b) Lack of fit test
# Create groups based on exact x1 and x2 combinations
df2 <- data.frame(y=y2, x1=x1, x2=x2)
df2$group <- paste(df2$x1, df2$x2, sep="_")
group_counts <- table(df2$group)

# Check if there are replicates
if (any(group_counts > 1)) {
  # Pure error from replicates
  sspe <- 0
  df_pe <- 0
  for (grp in names(group_counts[group_counts > 1])) {
    y_grp <- df2$y[df2$group == grp]
    sspe <- sspe + sum((y_grp - mean(y_grp))^2)
    df_pe <- df_pe + (length(y_grp) - 1)
  }
  
  # Lack of fit sum of squares
  sse <- sum(residuals(model2)^2)
  sslf <- sse - sspe
  df_lof <- df.residual(model2) - df_pe
  mslf <- sslf / df_lof
  mspe <- sspe / df_pe
  f_lof <- mslf / mspe
  p_lof <- pf(f_lof, df_lof, df_pe, lower.tail=FALSE)
  
  cat("(b) Lack of fit test:\n")
  cat(sprintf("F = %.4f, df1 = %d, df2 = %d, p-value = %.4f\n", f_lof, df_lof, df_pe, p_lof))
  if (p_lof < 0.05) {
    cat("=> Lack of fit is significant -> model may be inadequate.\n\n")
  } else {
    cat("=> Lack of fit not significant -> model is adequate.\n\n")
  }
} else {
  cat("(b) No exact replicates for lack of fit test.\n\n")
}

# (c) Does the interaction term contribute significantly?
model_red_no_int <- lm(y2 ~ x1 + x2 + I(x1^2) + I(x2^2))
anova_int <- anova(model_red_no_int, model2)
cat("(c) Interaction term contribution:\n")
print(anova_int)
if (anova_int$`Pr(>F)`[2] < 0.05) {
  cat("=> Interaction contributes significantly.\n\n")
} else {
  cat("=> Interaction does not contribute significantly.\n\n")
}

# (d) Do the quadratic terms contribute significantly? (Extra sum of squares)
model_red_no_quad <- lm(y2 ~ x1 + x2 + x1:x2)
anova_quad <- anova(model_red_no_quad, model2)
cat("(d) Quadratic terms (x1^2, x2^2) together:\n")
print(anova_quad)
if (anova_quad$`Pr(>F)`[2] < 0.05) {
  cat("=> Quadratic terms contribute significantly.\n")
} else {
  cat("=> Quadratic terms do not contribute significantly.\n")
}

# ============================================================================
# PROBLEM 3
# ============================================================================

x3 <- c(50, 75, 100, 125, 150, 175, 200, 225, 250, 275)
y3 <- c(335, 326, 316, 313, 311, 314, 318, 328, 337, 345)

# Scatter plot
plot(x3, y3, 
     main="Cost vs Reorder Quantity with Quadratic Fit",
     xlab="Reorder Quantity", 
     ylab="Avg. Annual Cost",
     pch=19, col="red", cex=1.2)
grid()

# Fit second order polynomial
model3 <- lm(y3 ~ x3 + I(x3^2))
cat("Second order polynomial model summary:\n")
print(summary(model3))

# Fitted line
x_fit <- seq(40, 285, length.out=100)
y_fit <- predict(model3, newdata=data.frame(x3=x_fit))
lines(x_fit, y_fit, col="blue", lwd=2)
legend("topright", legend=c("Observed", "Quadratic fit"), 
       col=c("red", "blue"), pch=c(19, NA), lty=c(NA, 1), lwd=c(NA, 2))

# Test quadratic term significance
coef_summary3 <- summary(model3)$coefficients
t_quad3 <- coef_summary3[3, "t value"]
p_quad3 <- coef_summary3[3, "Pr(>|t|)"]
cat(sprintf("\nQuadratic term t-test: t = %.4f, p-value = %.4f\n", t_quad3, p_quad3))
if (p_quad3 < 0.05) {
  cat("=> Quadratic term is statistically significant.\n")
} else {
  cat("=> Quadratic term is not statistically significant.\n")
}
