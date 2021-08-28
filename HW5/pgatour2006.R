install.packages("DAAG")
library(DAAG)
install.packages("MASS")
library(MASS)

d<- pgatour2006[,-c(1)]

# Initial first-order model
model <- lm(PrizeMoney ~ ., data = d)
summary(model)
# 5 cross-validation
out <- cv.lm(data = d, form.lm = formula(PrizeMoney ~ .), plotit = "Observed", m=5)

# First order model
m1 <- lm(PrizeMoney ~ DrivingAccuracy + GIR + BirdieConversion + Scrambling, data = d)
summary(m1)
# 5 cross-validation
out <- cv.lm(data = d, form.lm = formula(PrizeMoney ~ DrivingAccuracy + GIR + BirdieConversion + Scrambling), plotit = "Observed", m=5)

m2 <- lm(PrizeMoney ~ (.)^2, data = d)
anova(m2)

d$ADD2 <- d$AveDrivingDistance^2
d$DA2 <- d$DrivingAccuracy^2
d$G2 <- d$GIR^2
d$PA2 <- d$PuttingAverage^2
d$BC2 <- d$BirdieConversion^2
d$SS2 <- d$SandSaves^2
d$SB2 <- d$Scrambling^2
d$BB2 <- d$BounceBack^2
d$PPR2 <- d$PuttsPerRound^2

d$ADDG <- d$AveDrivingDistance * d$GIR
d$ADDPA <- d$AveDrivingDistance * d$PuttingAverage
d$ADDBC <- d$AveDrivingDistance * d$BirdieConversion
d$ADDSS <- d$AveDrivingDistance * d$SandSaves
d$ADDBB <- d$AveDrivingDistance * d$BounceBack

d$DAG <- d$DrivingAccuracy * d$GIR
d$DAPA <- d$DrivingAccuracy * d$PuttingAverage
d$DASB <- d$DrivingAccuracy * d$Scrambling

d$GPA <- d$GIR * d$PuttingAverage
d$GBC <- d$GIR * d$BirdieConversion
d$GSB <- d$GIR * d$Scrambling
d$GPPR <- d$GIR * d$PuttsPerRound

d$PASB <- d$PuttingAverage * d$Scrambling
d$PASS <- d$PuttingAverage * d$SandSaves
d$PABB <- d$PuttingAverage * d$BounceBack

d$SSSB <- d$SandSaves * d$Scrambling
d$SBBB <- d$Scrambling * d$BounceBack

# Initial second-order model
m2 <- lm(PrizeMoney ~ AveDrivingDistance + DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves +Scrambling + BounceBack + PuttsPerRound + ADD2 + DA2 + G2 + PA2 + BC2 + SS2 + SB2 + BB2 + PPR2 + ADDG + ADDPA + ADDBC + ADDSS + ADDBB + DAG + DAPA + DASB + GPA + GBC + GSB + GPPR + PASB + PASS + PABB + SSSB + SBBB, data = d)
summary(m2)

# 2B
m3 <- lm(PrizeMoney ~ GIR + BirdieConversion + SandSaves + ADD2 + DA2 + G2 + BB2 + ADDBC + ADDSS + DAG + GPA + GBC + PASB + PABB + SSSB + SBBB, data = d)
summary(m3)
# 5 cross-validation
out <- cv.lm(data = d, form.lm = formula(PrizeMoney ~ GIR + BirdieConversion + SandSaves + ADD2 + DA2 + G2 + BB2 + ADDBC + ADDSS + DAG + GPA + GBC + PASB + PABB + SSSB + SBBB), plotit = "Observed", m=5)
plot(m3)


# 2C backward selection
step <- stepAIC(m2, direction = "backward")
# Display results
step$anova
m4 <- lm(PrizeMoney ~ DrivingAccuracy + GIR + BirdieConversion + SandSaves + ADD2 + DA2 + G2 + BB2 + ADDBC + ADDSS + DAG + GPA + GBC + PASB + PABB + SSSB + SBBB, data = d)
summary(m4)
# 5 cross-validation
out <- cv.lm(data = d, form.lm = formula(PrizeMoney ~ DrivingAccuracy + GIR + BirdieConversion + SandSaves + ADD2 + DA2 + G2 + BB2 + ADDBC + ADDSS + DAG + GPA + GBC + PASB + PABB + SSSB + SBBB), plotit = "Observed", m=5)

