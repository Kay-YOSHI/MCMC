#=======================================================================#
# Logistic Regression by MCMC
#=======================================================================#

# Load library
#install.packages("MCMCpack")
library(MCMCpack)

# Data: Titanic
titanic = data.frame(Titanic)
data = data.frame(
  Class = rep(titanic$Class, titanic$Freq),
  Sex=rep(titanic$Sex, titanic$Freq),
  Age=rep(titanic$Age, titanic$Freq),
  Survived=rep(titanic$Survived, titanic$Freq)
)
attach(data)

#===================================================================#
# 1. By "glm"
#===================================================================#
result_glm = glm(Survived ~ Class + Sex + Age, family = binomial)
summary(result_glm)

# Odds ratio
exp(result_glm$coefficients)

#===================================================================#
# 2. By MCMC 
#===================================================================#
Survived = as.numeric(Survived)-1
result_mcmc = MCMClogit(Survived ~ Class + Sex + Age)
summary(result_mcmc)

# Odds ratio
exp(summary(result_mcmc)$statistics[, "Mean"])

# Sampling process & posterior distribution
plot(result_mcmc)
