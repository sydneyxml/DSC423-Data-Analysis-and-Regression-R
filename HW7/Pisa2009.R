# A
d <- Pisa2009[,-c(1,4)]
partition <- sample(2, nrow(d), replace = TRUE, prob = c(0.80, 0.20))
train <- d[partition==1 ,]
test <- d[partition==2 ,]
model <- lm(readingScore ~ ., data = train)
prediction <- predict(model, test)
actual = test$readingScore
# Cross-validation
cor(prediction,actual)
plot(prediction,actual)


# B
# Continuous variables:
summary(d$minutesPerWeekEnglish)
hist(log(d$minutesPerWeekEnglish), breaks = 20)

summary(d$studentsInEnglish)
hist(d$studentsInEnglish, breaks = 20)

summary(d$schoolSize)
hist(log(d$schoolSize), breaks = 20)

summary(d$readingScore)
hist(d$readingScore, breaks = 20)

# Categorical variable:
#grade

# Dummy variables:
#male
#preschool
#expectBachelors
#motherHS
#motherBachelors
#motherWork
#fatherHS
#fatherBachelors
#fatherWork
#selfBornUS
#motherBornUS
#fatherBornUS
#englishAtHome
#computerForSchoolwork
#read30MinsADay
#schoolHasLibrary
#publicSchool
#urban


# C
install.packages("car")
library(car)

cor(d)
plot(d)

m1 <- lm(readingScore ~ ., data = d)
summary(m1)
vif(m1)

m2 <- lm(readingScore ~ grade + male + expectBachelors + motherBachelors + fatherHS + fatherBachelors + fatherWork + englishAtHome + computerForSchoolwork + read30MinsADay + publicSchool + urban + schoolSize, data = d)
summary(m2)
vif(m2)


# D
# Dummy variables:
d$male <- ifelse(d$male == "male", 1, 0)
d$preschool <- ifelse(d$preschool == "preschool", 1, 0)
d$expectBachelors <- ifelse(d$expectBachelors == "expectBachelors", 1, 0)
d$motherHS <- ifelse(d$motherHS == "motherHS", 1, 0)
d$motherBachelors <- ifelse(d$motherBachelors == "motherBachelors", 1, 0)
d$motherWork <- ifelse(d$motherWork == "motherWork", 1, 0)
d$fatherHS <- ifelse(d$fatherHS == "fatherHS", 1, 0)
d$fatherBachelors <- ifelse(d$fatherBachelors == "fatherBachelors", 1, 0)
d$fatherWork <- ifelse(d$fatherWork == "fatherWork", 1, 0)
d$selfBornUS <- ifelse(d$selfBornUS == "selfBornUS", 1, 0)
d$motherBornUS <- ifelse(d$motherBornUS == "motherBornUS", 1, 0)
d$fatherBornUS <- ifelse(d$fatherBornUS == "fatherBornUS", 1, 0)
d$englishAtHome <- ifelse(d$englishAtHome == "englishAtHome", 1, 0)
d$computerForSchoolwork <- ifelse(d$computerForSchoolwork == "computerForSchoolwork", 1, 0)
d$read30MinsADay <- ifelse(d$read30MinsADay == "read30MinsADay", 1, 0)
d$schoolHasLibrary <- ifelse(d$schoolHasLibrary == "schoolHasLibrary", 1, 0)
d$publicSchool <- ifelse(d$publicSchool == "publicSchool", 1, 0)
d$urban <- ifelse(d$urban == "urban", 1, 0)


# E
install.packages("MASS")
library(MASS)

model_full <- lm(readingScore ~ ., data = d)
summary(model_full)
step <- stepAIC(model_full, direction = "backward")
step$anova

model_step <- lm(readingScore ~ grade + minutesPerWeekEnglish, data = d)
summary(model_step)


# F
# Second order terms
d$minutesPerWeekEnglishSQ <- (d$minutesPerWeekEnglish)^2
d$studentsInEnglishSQ <- (d$studentsInEnglish)^2
d$schoolSizeSQ <- (d$schoolSize)^2
d$gradeSQ <- (d$grade)^2

d$maleSQ <- (d$male)^2
d$preschoolSQ <- (d$preschool)^2
d$expectBachelorsSQ <- (d$expectBachelors)^2
d$motherHSSQ <- (d$motherHS)^2
d$motherBachelorsSQ <- (d$motherBachelors)^2
d$motherWorkSQ <- (d$motherWork)^2
d$fatherHSSQ <- (d$fatherHS)^2
d$fatherBachelorsSQ <- (d$fatherBachelors)^2
d$fatherWorkSQ <- (d$fatherWork)^2
d$selfBornUSSQ <- (d$selfBornUS)^2
d$motherBornUSSQ <- (d$motherBornUS)^2
d$fatherBornUSSQ <- (d$fatherBornUS)^2
d$englishAtHomeSQ <- (d$englishAtHome)^2
d$computerForSchoolworkSQ <- (d$computerForSchoolwork)^2
d$read30MinsADaySQ <- (d$read30MinsADay)^2
d$schoolHasLibrarySQ <- (d$schoolHasLibrary)^2
d$publicSchoolSQ <- (d$publicSchool)^2
d$urbanSQ <- (d$urban)^2

model_f <- lm(readingScore ~ ., data = d)
summary(model_f)
step <- stepAIC(model_f, direction = "backward")
step$anova

model_f1 <- lm(readingScore ~ grade + minutesPerWeekEnglish + schoolSize + minutesPerWeekEnglishSQ + schoolSizeSQ + gradeSQ, data = d)
summary(model_f1)

model_f2 <- lm(readingScore ~ grade + minutesPerWeekEnglish + minutesPerWeekEnglishSQ + gradeSQ, data = d)
summary(model_f2)


# G
# Interaction terms
d$GM <- d$grade * d$minutesPerWeekEnglish
d$GSE <- d$grade * d$studentsInEnglish
d$GSS <- d$grade * d$schoolSize
d$MSE <- d$minutesPerWeekEnglish * d$studentsInEnglish
d$MSS <- d$minutesPerWeekEnglish * d$schoolSize
d$SESS <- d$studentsInEnglish * d$schoolSize

model_i <- lm(readingScore ~ ., data = d)
summary(model_i)
step <- stepAIC(model_i, direction = "backward")
step$anova

model_i1 <- lm(readingScore ~ grade + minutesPerWeekEnglish + studentsInEnglish + schoolSize + minutesPerWeekEnglishSQ + schoolSizeSQ + gradeSQ + GSE, data = d)
summary(model_i1)

model_i2 <- lm(readingScore ~ grade + minutesPerWeekEnglish + minutesPerWeekEnglishSQ + gradeSQ, data = d)
summary(model_i2)


# H
# set as levels
d$grade <- as.factor(d$grade)

hist(d$readingScore, breaks = 20)
hist(d$minutesPerWeekEnglish, breaks = 20)
hist(log(d$minutesPerWeekEnglish), breaks = 20)
hist(log(d$minutesPerWeekEnglishSQ), breaks = 20)

model_final <- lm(readingScore ~ grade + log(minutesPerWeekEnglish + 1) + log(minutesPerWeekEnglishSQ + 1), data = d)
summary(model_final)

