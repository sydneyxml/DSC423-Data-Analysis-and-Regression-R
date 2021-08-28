install.packages("glmnet")
library(glmnet)

# 1a
# Ridge
# alpha=0
# Multicollinearity
Pisa2009 <- Pisa2009[complete.cases(Pisa2009),]

raceeth <- c("White", "Black", "Hispanic", "More than one race", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Other Pacific Islander")
raceeth.factor <- factor(raceeth)
as.numeric(raceeth.factor)

x <- data.matrix(Pisa2009[,2:24])
y <- as.double(Pisa2009[,25])

set.seed(123)
ridge <- cv.glmnet(x, y, family="gaussian", alpha=0)

plot(ridge)
ridge$lambda.min
coef(ridge, s=ridge$lambda.min)

m1 <- lm(readingScore ~ grade + male + raceeth + preschool + expectBachelors + motherHS + motherBachelors + motherWork + fatherHS + fatherBachelors + fatherWork + selfBornUS + motherBornUS + fatherBornUS + englishAtHome + computerForSchoolwork + read30MinsADay + minutesPerWeekEnglish + studentsInEnglish + schoolHasLibrary + publicSchool + urban + schoolSize, data = Pisa2009)
summary(m1)

m1$residuals
sum(m1$residuals)
hist(m1$residuals, breaks = 100)


# 1b
# Lasso
# Feature selection
# alpha=1
Pisa2009 <- Pisa2009[complete.cases(Pisa2009),]

raceeth <- c("White", "Black", "Hispanic", "More than one race", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Other Pacific Islander")
raceeth.factor <- factor(raceeth)
as.numeric(raceeth.factor)

x <- data.matrix(Pisa2009[,2:24])
y <- as.double(Pisa2009[,25])

set.seed(123)
lasso <- cv.glmnet(x, y, family="gaussian", alpha=1)

plot(lasso)
lasso$lambda.min
coef(lasso, s=lasso$lambda.min)

