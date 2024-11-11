set.seed(3383)
sig <- 0.05 
m <- 1000
n <- 500
alpha0 <- 0
alpha1 <- c(0, 1, 2)
beta0 <- -3
beta1 <- 0
beta2 <- 2
pz <- 0.2

results <- data.frame(alpha1 = numeric(),
                      Adjusted = numeric(),
                      Unadjusted = numeric()
  )

unadjtype1 <- numeric(length(alpha1))
adjtype1 <- numeric(length(alpha1))
  
for (i in 1:length(alpha1)) {
    for (j in 1:m) {
      z <- rbinom(n, size = 1, prob = pz)
      px <- exp(alpha0 + alpha1[i] * z) / (1+exp(alpha0 + alpha1[i] *z))
      x <- rbinom(n, size = 1 , prob = px)
      py <- exp(beta0 + beta1 * x + beta2 * z)/(1+exp(beta0+beta1*x+beta2*z))
      y <- rbinom(n, size = 1, prob = py)
      dat <- data.frame(lung = y, coffee = x, smoke = z)
      ## fit unadjusted logistic regression model
      unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
      unadj.coef <- summary(unadj.mod)$coef
      ## fit adjusted logistic regression model
      adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
      adj.coef <- summary(adj.mod)$coef
      
      if (unadj.coef[2,4] < sig) {
        unadjtype1[i] <- unadjtype1[i] + 1
      }
      else {
        unadjtype1[i] <- unadjtype1[i]
      }
      if (adj.coef[2,4] < sig) {
        adjtype1[i] <- adjtype1[i] + 1
      }
      else {
        adjtype1[i] <- adjtype1[i]
      }
      
    }
    results <- rbind(results, data.frame(
                     alpha1 = alpha1[i],
                     Adjusted = adjtype1[i]/m ,
                     Unadjusted = unadjtype1[i]/m))
  }

print(results)
