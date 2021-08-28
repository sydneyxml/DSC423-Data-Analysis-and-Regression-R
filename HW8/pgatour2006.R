install.packages("DAAG")
library(DAAG)
install.packages("MASS")
library(MASS)
install.packages("car")
library(car)

d<- pgatour2006[,-c(1)]
plot(d)
cor(d)

m1 <- lm(log(PrizeMoney) ~ (.)^2, data = d)
anova(m1)

d$ADD2 <- d$AveDrivingDistance^2
d$DA2 <- d$DrivingAccuracy^2
d$G2 <- d$GIR^2
d$PA2 <- d$PuttingAverage^2
d$BC2 <- d$BirdieConversion^2
d$SS2 <- d$SandSaves^2
d$SB2 <- d$Scrambling^2
d$BB2 <- d$BounceBack^2
d$PPR2 <- d$PuttsPerRound^2

d$ADDPA <- d$AveDrivingDistance * d$PuttingAverage
d$ADDPPR <- d$AveDrivingDistance * d$PuttsPerRound
d$DAG <- d$DrivingAccuracy * d$GIR
d$DASB <- d$DrivingAccuracy * d$Scrambling
d$PABB <- d$PuttingAverage * d$BounceBack
d$PAPPR <- d$PuttingAverage * d$PuttsPerRound
d$SBBB <- d$Scrambling * d$BounceBack

# Initial model
m2 <- lm(log(PrizeMoney) ~ AveDrivingDistance + DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves +Scrambling + BounceBack + PuttsPerRound + ADD2 + DA2 + G2 + PA2 + BC2 + SS2 + SB2 + BB2 + PPR2 + ADDPA + ADDPPR + DAG + DASB + PABB + PAPPR + SBBB, data = d)
summary(m2)

# Backward selection
step <- stepAIC(m2, direction = "backward")
# Display results
step$anova

# After backward selection
m3 <- lm(log(PrizeMoney) ~ AveDrivingDistance + GIR + PuttingAverage + SandSaves + PuttsPerRound + DA2 + G2 + PA2 + BC2 + BB2 + PPR2 + ADDPA + ADDPPR + DASB + PABB + SBBB, data = d)
summary(m3)

# Final
m4 <- lm(log(PrizeMoney) ~ GIR + PuttingAverage + PuttsPerRound + G2 + PA2 + BC2 + BB2 + ADDPA + ADDPPR + DASB + PABB + SBBB, data = d)
summary(m4)
vif(m4)

m4$residuals
sum(m4$residuals)

mean = mean(m4$residuals)
sd = sd(m4$residuals)
resid_zscore = (m4$residuals - mean)/sd

durbinWatsonTest(m4)

plot(d$GIR, resid_zscore)
plot(d$PuttingAverage, resid_zscore)
plot(d$PuttsPerRound, resid_zscore)
plot(d$G2, resid_zscore)
plot(d$PA2, resid_zscore)
plot(d$BC2, resid_zscore)
plot(d$BB2, resid_zscore)
plot(d$ADDPA, resid_zscore)
plot(d$ADDPPR, resid_zscore)
plot(d$DASB, resid_zscore)
plot(d$PABB, resid_zscore)
plot(d$SBBB, resid_zscore)

plot(m4)

