m1 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart, data = WATEROIL)
summary(m1)

cor(WATEROIL)

WATEROIL$V2 <- WATEROIL$Volume^2
WATEROIL$SL2 <- WATEROIL$Salinity^2
WATEROIL$T2 <- WATEROIL$Temperature^2
WATEROIL$D2 <- WATEROIL$Delay^2
WATEROIL$SF2 <- WATEROIL$Surfactant^2
WATEROIL$ST2 <- WATEROIL$SpanTriton^2
WATEROIL$SP2 <- WATEROIL$SolidPart^2

m2 <- lm(Voltage ~ (Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart)^2, data = WATEROIL)
anova(m2)

WATEROIL$VSL <- WATEROIL$Volume * WATEROIL$Salinity
WATEROIL$VT <- WATEROIL$Volume * WATEROIL$Temperature
WATEROIL$VD <- WATEROIL$Volume * WATEROIL$Delay
WATEROIL$VSF <- WATEROIL$Volume * WATEROIL$Surfactant
WATEROIL$VST <- WATEROIL$Volume * WATEROIL$SpanTriton
WATEROIL$VSP <- WATEROIL$Volume * WATEROIL$SolidPart
WATEROIL$SLT <- WATEROIL$Salinity * WATEROIL$Temperature
WATEROIL$SLD <- WATEROIL$Salinity * WATEROIL$Delay

m3 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + SL2 + T2 + D2 + SF2 + ST2 + SP2 + VSL + VT + VD + VSF + VST + VSP + SLT + SLD, data = WATEROIL)
summary(m3)

# Drop SLT, SL2, T2, D2, SF2, ST2, SP2
m4 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + VSL + VT + VD + VSF + VST + VSP + SLD, data = WATEROIL)
summary(m4)

# Drop VT
m5 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + VSL + VD + VSF + VST + VSP + SLD, data = WATEROIL)
summary(m5)

# Drop SLD
m6 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + VSL + VD + VSF + VST + VSP, data = WATEROIL)
summary(m6)

# Drop VSP
m7 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + VSL + VD + VSF + VST, data = WATEROIL)
summary(m7)

# Drop VST
m8 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + VSL + VD + VSF, data = WATEROIL)
summary(m8)

# Drop VD
m8 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + SolidPart + V2 + VSL + VSF, data = WATEROIL)
summary(m8)

# Drop SolidPart
m9 <- lm(Voltage ~ Volume + Salinity + Temperature + Delay + Surfactant + SpanTriton + V2 + VSL + VSF, data = WATEROIL)
summary(m9)

# Drop Delay
m10 <- lm(Voltage ~ Volume + Salinity + Temperature + Surfactant + SpanTriton + V2 + VSL + VSF, data = WATEROIL)
summary(m10)

# Drop SpanTriton
m11 <- lm(Voltage ~ Volume + Salinity + Temperature + Surfactant + V2 + VSL + VSF, data = WATEROIL)
summary(m11)

# Drop Temperature
m12 <- lm(Voltage ~ Volume + Salinity + Surfactant + V2 + VSL + VSF, data = WATEROIL)
summary(m12)
plot(m12)

