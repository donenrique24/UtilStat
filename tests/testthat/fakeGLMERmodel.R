dataset <- NULL
for (i in 1:100) {
  u <- rnorm(1, mean = 0, sd = 1)
  prob <- 1-exp(-exp(-1.5 + u))
  y <- runif(n = 20) <= prob
  dataset <- rbind(dataset, data.frame(plot = rep(i,length(prob)), y = as.numeric(y)))
}

require(lme4)

fit <- glmer(y ~ (1 | plot), dataset, family = binomial(link = "cloglog"))
summary(fit)
save(fit, file = "./data/fitGLMER.RData", compress = "xz")
coef(fit)
fixef(fit)
