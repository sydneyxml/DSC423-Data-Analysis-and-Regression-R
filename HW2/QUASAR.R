Y1 <- QUASAR$RFEWIDTH
X1 <- QUASAR$REDSHIFT
X2 <- QUASAR$LINEFLUX
X3 <- QUASAR$LUMINOSITY
X4 <- QUASAR$AB1450
X5 <- QUASAR$ABSMAG

model1 <- lm(Y1 ~ X1)
summary(model1)

model2 <- lm(Y1 ~ X2)
summary(model2)

model3 <- lm(Y1 ~ X3)
summary(model3)

model4 <- lm(Y1 ~ X4)
summary(model4)

model5 <- lm(Y1 ~ X5)
summary(model5)
