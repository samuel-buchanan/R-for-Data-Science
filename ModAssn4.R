##########
# Variables

names(WINE)
summary(WINE)
vis_miss(WINE)

hist(WINE$Purchase)
hist(WINE$Cases)
hist(WINE$STARS)

# WINE$Cases looks nicely normal, except for the zero counts. Could be useful for a zero-inflated poisson model.
mean(WINE$Cases)
var(WINE$Cases)
table(WINE$Cases)


#STARS is discrete from 1 to 4, could reformat as categorical, almost 25% is NAs
WINE$STARSnadummy <- 0
WINE$STARSnadummy[is.na(WINE$STARS)] <- 1
WINE$STARS1dummy <- 0
WINE$STARS1dummy[WINE$STARS == 1] <- 1
WINE$STARS2dummy <- 0
WINE$STARS2dummy[WINE$STARS == 2] <- 1
WINE$STARS3dummy <- 0
WINE$STARS3dummy[WINE$STARS == 3] <- 1
WINE$STARS4dummy <- 0
WINE$STARS4dummy[WINE$STARS == 4] <- 1


#FixedAcidity, looks normal, need to deal with negative values, continuous
hist(WINE$FixedAcidity, breaks = 100)
qqnorm(WINE$FixedAcidity)
qqline(WINE$FixedAcidity)

WINE$fixedFixedAcidity <- WINE$FixedAcidity
WINE$fixedFixedAcidity <- sqrt(WINE$FixedAcidity + 18.1)
hist(WINE$fixedFixedAcidity, breaks = 100)
qqnorm(WINE$fixedFixedAcidity)
qqline(WINE$fixedFixedAcidity)


#VolatileAcidity looks normal, need to deal with negative values, continuous
hist(WINE$VolatileAcidity, breaks = 100)
qqnorm(WINE$VolatileAcidity)
qqline(WINE$VolatileAcidity)

WINE$fixedVolatileAcidity <- WINE$VolatileAcidity
WINE$fixedVolatileAcidity <- sqrt(WINE$fixedVolatileAcidity+2.79)
hist(WINE$fixedVolatileAcidity, breaks = 100)
qqnorm(WINE$fixedVolatileAcidity)
qqline(WINE$fixedVolatileAcidity)


#CitricAcid looks normal, need to deal with negative values, continuous
hist(WINE$CitricAcid, breaks = 100)
qqnorm(WINE$CitricAcid)
qqline(WINE$CitricAcid)

WINE$fixedCitricAcid <- sqrt(WINE$CitricAcid+3.24)
hist(WINE$fixedCitricAcid, breaks = 100)
qqnorm(WINE$fixedCitricAcid)
qqline(WINE$fixedCitricAcid)


#ResidualSugar looks normal, needs to deal with negative values, continuous, ~5% NA
hist(WINE$ResidualSugar, breaks = 100)
qqnorm(WINE$ResidualSugar)
qqline(WINE$ResidualSugar)

WINE$imputedResidualSugar <- impute(WINE$ResidualSugar, mean)
WINE$fixedResidualSugar <- WINE$imputedResidualSugar
WINE$fixedResidualSugar <- sqrt(WINE$fixedResidualSugar+127.8)
hist(WINE$fixedResidualSugar, breaks = 100)
qqnorm(WINE$fixedResidualSugar)
qqline(WINE$fixedResidualSugar)


#Chlorides looks normal, need to deal with negative values, continuous, ~5% NA
hist(WINE$Chlorides, breaks = 100)
qqnorm(WINE$Chlorides)
qqline(WINE$Chlorides)

WINE$imputedChlorides <- impute(WINE$Chlorides, mean)
WINE$fixedChlorides <- sqrt(WINE$imputedChlorides+1.171)
hist(WINE$fixedChlorides, breaks = 100)
qqnorm(WINE$fixedChlorides)
qqline(WINE$fixedChlorides)
# not sure which looks better here, as far as the qq plots are concerned


#FreeSulfurDioxide looks normal, need to deal with negative values, discrete, ~5% NA
hist(WINE$FreeSulfurDioxide, breaks = 100)
qqnorm(WINE$FreeSulfurDioxide)
qqline(WINE$FreeSulfurDioxide)

WINE$imputedFreeSulfurDioxide <- impute(WINE$FreeSulfurDioxide, mean)
WINE$fixedFreeSulfurDioxide <- sqrt(WINE$imputedFreeSulfurDioxide+555)
hist(WINE$fixedFreeSulfurDioxide, breaks = 100)
qqnorm(WINE$fixedFreeSulfurDioxide)
qqline(WINE$fixedFreeSulfurDioxide)


#TotalSulfurDioxide looks normal, need to deal with negative values, lots of negatives here, discrete, ~5% NA
hist(WINE$TotalSulfurDioxide, breaks = 100)
qqnorm(WINE$TotalSulfurDioxide)
qqline(WINE$TotalSulfurDioxide)

WINE$imputedTotalSulfurDioxide <- impute(WINE$TotalSulfurDioxide, mean)
WINE$fixedTotalSulfurDioxide <- sqrt(WINE$imputedTotalSulfurDioxide+823)
hist(WINE$fixedTotalSulfurDioxide, breaks = 100)
qqnorm(WINE$fixedTotalSulfurDioxide)
qqline(WINE$fixedTotalSulfurDioxide)


#Density looks normal, continuous from ~0.88 to ~1.09
# fixed but no imputed
hist(WINE$Density, breaks = 100)
qqnorm(WINE$Density)
qqline(WINE$Density)

WINE$fixedDensity <- sqrt(WINE$Density)
hist(WINE$fixedDensity, breaks = 100)
qqnorm(WINE$fixedDensity)
qqline(WINE$fixedDensity)


#pH looks normal but with some very low values, continuous, 5% NA
hist(WINE$pH, breaks = 100)
qqnorm(WINE$pH)
qqline(WINE$pH)

WINE$imputedpH <- impute(WINE$pH, mean)
WINE$fixedpH <- sqrt(WINE$imputedpH)
# pH of vinegar is around 2.4, don't think these are real measurements
hist(WINE$fixedpH, breaks = 100)
qqnorm(WINE$fixedpH)
qqline(WINE$fixedpH)


#Suplhates looks normal, need to deal with negative values, lots of negatives here, continuous, ~10% NA
hist(WINE$Sulphates, breaks = 100)
qqnorm(WINE$Sulphates)
qqline(WINE$Sulphates)

WINE$imputedSulphates <- impute(WINE$Sulphates, mean)
WINE$fixedSulphates <- sqrt(WINE$imputedSulphates+3.13)
hist(WINE$fixedSulphates, breaks = 100)
qqnorm(WINE$fixedSulphates)
qqline(WINE$fixedSulphates)


#Alcohol looks normal, need to deal with negative values, continuous, ~5% NA
hist(WINE$Alcohol, breaks = 100)
qqnorm(WINE$Alcohol, main = "Normal Q-Q Plot of WINE$Alcohol")
qqline(WINE$Alcohol)

WINE$imputedAlcohol <- impute(WINE$Alcohol, mean)
WINE$fixedAlcohol <- WINE$imputedAlcohol
WINE$fixedAlcohol <- WINE$fixedAlcohol+5
WINE$fixedAlcohol <- sqrt(WINE$fixedAlcohol)
hist(WINE$fixedAlcohol, breaks = 100)
qqnorm(WINE$fixedAlcohol, main = "Normal Q-Q Plot of log WINE$fixedAlcohol")
qqline(WINE$fixedAlcohol)


#LabelAppeal is a likert-type variable; goes from -2 to 2
hist(WINE$LabelAppeal)


#AcidIndex looks normal?, discrete, from 4 to 14
# no fixed, no imputed
hist(WINE$AcidIndex)
qqnorm(WINE$AcidIndex)
qqline(WINE$AcidIndex)



##########
# Train / Test

library(caTools)

WINE$split.index <- 0
WINE$split.index <- sample.split(WINE$split.index, SplitRatio = 0.7)

wine.train <- subset(WINE, WINE$split.index == TRUE)
wine.test <- subset(WINE, WINE$split.index == FALSE)


##########
# Modeling
names(WINE)

zip.min <- zeroinfl(Cases ~ STARS, data = wine.train)
summary(zip.min)
AIC(zip.min) # 23760.06
zip.min.test <- predict(zip.min, newdata = wine.test)
rmse(zip.min.test, wine.test$Cases) # 1.260776
mae(zip.min.test, wine.test$Cases) # 1.012976


zip.full <- zeroinfl(Cases ~ fixedAlcohol + fixedFixedAcidity + fixedVolatileAcidity 
                     + fixedSulphates + fixedResidualSugar + fixedChlorides
                     + fixedFreeSulfurDioxide + fixedTotalSulfurDioxide + fixedpH
                     + fixedCitricAcid + fixedDensity + STARSnadummy + STARS2dummy
                     + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                     , data = wine.train, maxit = 1000)
summary(zip.full)
AIC(zip.full) # 28630.88
zip.full.test <- predict(zip.full, newdata = wine.test)
rmse(zip.full.test, wine.test$Cases) # 1.230615
mae(zip.full.test, wine.test$Cases) # 0.9424183


zip.lean <- zeroinfl(Cases ~ LabelAppeal + STARS2dummy + STARS3dummy + STARS4dummy + STARSnadummy
                     + AcidIndex + fixedAlcohol + fixedpH + fixedVolatileAcidity
                     + fixedFreeSulfurDioxide + fixedTotalSulfurDioxide
                     , data = wine.train, maxit = 1000)
summary(zip.lean)
AIC(zip.lean) # 28614.67
zip.lean.test <- predict(zip.lean, newdata = wine.test)
rmse(zip.lean.test, wine.test$Cases) # 1.232121
mae(zip.lean.test, wine.test$Cases) # 0.9441889


zip.base.full <- zeroinfl(Cases ~ Alcohol + FixedAcidity + VolatileAcidity 
                          + Sulphates + ResidualSugar + Chlorides
                          + FreeSulfurDioxide + TotalSulfurDioxide + pH
                          + CitricAcid + Density + STARSnadummy + STARS2dummy
                          + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                          , data = wine.train, maxit = 1000)
summary(zip.base.full)
AIC(zip.base.full) # 19342.77
zip.base.full.test <- predict(zip.base.full, newdata = wine.test)
rmse(zip.base.full.test, wine.test$Cases) # 1.220655
mae(zip.base.full.test, wine.test$Cases) # 0.9297225

zip.base.lean <-zeroinfl(Cases ~ Alcohol + VolatileAcidity 
                         + Sulphates + ResidualSugar + Chlorides
                         + FreeSulfurDioxide + TotalSulfurDioxide + pH
                         + STARS + LabelAppeal + AcidIndex
                         , data = wine.train, maxit = 1000)
summary(zip.base.lean)
AIC(zip.base.lean) # 15492.92
zip.base.lean.test <- predict(zip.base.lean, newdata = wine.test)
rmse(zip.base.lean.test, wine.test$Cases) # 1.08525
mae(zip.base.lean.test, wine.test$Cases) # 0.8127329


zip.imp.full <- zeroinfl(Cases ~ imputedAlcohol + FixedAcidity + VolatileAcidity 
                         + imputedSulphates + imputedResidualSugar + imputedChlorides
                         + imputedFreeSulfurDioxide + imputedTotalSulfurDioxide + imputedpH
                         + CitricAcid + Density + STARSnadummy + STARS2dummy
                         + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                         , data = wine.train, maxit = 1000)
summary(zip.imp.full)
AIC(zip.imp.full) # 28622.84
zip.imp.full.test <- predict(zip.imp.full, newdata = wine.test)
rmse(zip.imp.full.test, wine.test$Cases) # 1.229437
mae(zip.imp.full.test, wine.test$Cases) # 0.9409551

zip.imp.lean <-zeroinfl(Cases ~ imputedAlcohol + VolatileAcidity 
                         + imputedSulphates + imputedResidualSugar + imputedChlorides
                         + imputedFreeSulfurDioxide + imputedTotalSulfurDioxide + imputedpH
                         + STARS + LabelAppeal + AcidIndex
                         , data = wine.train, maxit = 1000)
summary(zip.imp.lean)
AIC(zip.imp.lean) # 22900.08
zip.imp.lean.test <- predict(zip.imp.lean, newdata = wine.test)
rmse(zip.imp.lean.test, wine.test$Cases) # 1.090609
mae(zip.imp.lean.test, wine.test$Cases) # 0.8198782


glm.min <- glm(Cases ~ STARS, data = wine.train)
summary(glm.min)
AIC(glm.min) #22098.87
glm.min.test <- predict(glm.min, newdata = wine.test)
rmse(glm.min.test, wine.test$Cases) # 1.265106
mae(glm.min.test, wine.test$Cases) # 1.018255

glm.full <- glm(Cases ~ fixedAlcohol + fixedFixedAcidity + fixedVolatileAcidity 
                     + fixedSulphates + fixedResidualSugar + fixedChlorides
                     + fixedFreeSulfurDioxide + fixedTotalSulfurDioxide + fixedpH
                     + fixedCitricAcid + fixedDensity + STARSnadummy + STARS2dummy
                     + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                     , data = wine.train, maxit = 1000)
summary(glm.full)
AIC(glm.full) # 30447.25
glm.full.test <- predict(glm.full, newdata = wine.test)
rmse(glm.full.test, wine.test$Cases) # 1.268968
mae(glm.full.test, wine.test$Cases) # 1.004745

glm.lean <- glm(Cases ~ LabelAppeal + STARS2dummy + STARS3dummy + STARS4dummy + STARSnadummy
                     + AcidIndex + fixedAlcohol + fixedpH + fixedVolatileAcidity
                     + fixedFreeSulfurDioxide + fixedTotalSulfurDioxide
                     , data = wine.train, maxit = 1000)
summary(glm.lean)
AIC(glm.lean) # 30447.38glm.min.test <- predict(glm.min, newdata = wine.test)
glm.lean.test <- predict(glm.lean, newdata = wine.test)
rmse(glm.lean.test, wine.test$Cases) # 1.269945
mae(glm.lean.test, wine.test$Cases) # 1.005707


glm.base.full <- glm(Cases ~ Alcohol + FixedAcidity + VolatileAcidity 
                          + Sulphates + ResidualSugar + Chlorides
                          + FreeSulfurDioxide + TotalSulfurDioxide + pH
                          + CitricAcid + Density + STARSnadummy + STARS2dummy
                          + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                          , data = wine.train, maxit = 1000)
summary(glm.base.full)
AIC(glm.base.full) # 20634.04
glm.base.full.test <- predict(glm.base.full, newdata = wine.test)
rmse(glm.base.full.test, wine.test$Cases) # 1.258787
mae(glm.base.full.test, wine.test$Cases) # 0.990806

glm.base.lean <- glm(Cases ~ Alcohol + VolatileAcidity 
                         + Sulphates + ResidualSugar + Chlorides
                         + FreeSulfurDioxide + TotalSulfurDioxide + pH
                         + STARS + LabelAppeal + AcidIndex
                         , data = wine.train, maxit = 1000)
summary(glm.base.lean)
AIC(glm.base.lean) # 14095.35
glm.base.lean.test <- predict(glm.base.lean, newdata = wine.test)
rmse(glm.base.lean.test, wine.test$Cases) # 1.096104
mae(glm.base.lean.test, wine.test$Cases) # 0.8184949

glm.imp.full <- glm(Cases ~ imputedAlcohol + FixedAcidity + VolatileAcidity 
                         + imputedSulphates + imputedResidualSugar + imputedChlorides
                         + imputedFreeSulfurDioxide + imputedTotalSulfurDioxide + imputedpH
                         + CitricAcid + Density + STARSnadummy + STARS2dummy
                         + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                         , data = wine.train, maxit = 1000)
summary(glm.imp.full)
AIC(glm.imp.full) # 30437.25
glm.imp.full.test <- predict(glm.imp.full, newdata = wine.test)
rmse(glm.imp.full.test, wine.test$Cases) # 1.269094
mae(glm.imp.full.test, wine.test$Cases) # 1.004985

glm.imp.lean <- glm(Cases ~ imputedAlcohol + VolatileAcidity 
                        + imputedSulphates + imputedResidualSugar + imputedChlorides
                        + imputedFreeSulfurDioxide + imputedTotalSulfurDioxide + imputedpH
                        + STARS + LabelAppeal + AcidIndex
                        , data = wine.train, maxit = 1000)
summary(glm.imp.lean)
AIC(glm.imp.lean) # 20706.31
glm.imp.lean.test <- predict(glm.imp.lean, newdata = wine.test)
rmse(glm.imp.lean.test, wine.test$Cases) # 1.103604
mae(glm.imp.lean.test, wine.test$Cases) # 0.8258168

# Interactions
glm.basex.full <- glm(Cases ~ Alcohol + FixedAcidity * VolatileAcidity 
                     + Sulphates + ResidualSugar + Chlorides
                     + FreeSulfurDioxide * TotalSulfurDioxide + pH
                     + CitricAcid + Density + STARSnadummy + STARS2dummy
                     + STARS3dummy + STARS4dummy + LabelAppeal + AcidIndex
                     , data = wine.train, maxit = 1000)
summary(glm.basex.full)
AIC(glm.basex.full) # 20637.63

glm.basex.lean <- glm(Cases ~ Alcohol + VolatileAcidity 
                     + Sulphates + ResidualSugar + Chlorides
                     + FreeSulfurDioxide * TotalSulfurDioxide + pH
                     + STARS + LabelAppeal + AcidIndex
                     , data = wine.train, maxit = 1000)
summary(glm.basex.lean)
AIC(glm.basex.lean) # 14097.31
