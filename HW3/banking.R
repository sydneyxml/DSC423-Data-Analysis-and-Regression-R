plot(banking$Age, banking$Balance)
plot(banking$Education, banking$Balance)
plot(banking$Income, banking$Balance)
plot(banking$HomeVal, banking$Balance)
plot(banking$Wealth, banking$Balance)

cor(banking$Age, banking$Balance)
cor(banking$Education, banking$Balance)
cor(banking$Income, banking$Balance)
cor(banking$HomeVal, banking$Balance)
cor(banking$Wealth, banking$Balance)

m1 <- lm(Balance ~ Age + Education + Income + HomeVal + Wealth, data = banking)
summary(m1)

# Drop HomeVal & Education
m2 <- lm(Balance ~ Age + Income + Wealth, data = banking)
summary(m2)

