install.packages("glmnet")
library(glmnet)

# 2
# Logistic model
summary(remission)

remission$remiss <- factor(remission$remiss)

model <- glm(remiss ~ cell + smear + infil + li + blast + temp, data = remission, family = "binomial")
summary(model)
confint(model)
exp(coef(model))-1

model2 <- glm(remiss ~ li, data = remission, family = "binomial")
summary(model2)
confint(model2)
exp(coef(model2))-1

