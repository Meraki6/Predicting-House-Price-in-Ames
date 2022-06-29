library(car)
library(GGally)
library(knitr)
library(readr)
library(sampling)
library(tidyverse)
library(gvlma)
library(leaps)
library(lmtest)
options(scipen = 4)
par(mfrow = c(1,1))
ames_22_train <- read_csv("C:/Users/685685/Downloads/Data Analysis Using R/ames_22_train.csv")
ames <- ames_22_train
ames <- ames_22_train %>%
  mutate(
    Location = factor(Location, order = TRUE),
    Amenities = as.factor(Amenities),
    RoadRail = as.factor(RoadRail),
    TwoStory_dum = as.factor(TwoStory_dum),
    FlatContour_dum = as.factor(FlatContour_dum),
    FlatRoof_dum = as.factor(FlatRoof_dum),
    Garage_dum = as.factor(Garage_dum),
    CentralAirNum = as.factor(CentralAirNum),
    KitchenQual_Ex = as.factor(KitchenQual_Ex),
    Zoning_2 = as.factor(Zoning_2),
    Zoning_3 = as.factor(Zoning_3),
    Zoning_4 = as.factor(Zoning_4),
    YrSold_2007 = as.factor(YrSold_2007),
    YrSold_2008 = as.factor(YrSold_2008),
    YrSold_2009 = as.factor(YrSold_2009),
    YrSold_2010 = as.factor(YrSold_2010)
  )
options(scipen = 4)
mutate(multiple = recode_factor(multiple, `1` = 'good', `2` = 'ok', `3` = 'bad'))
mutate_at(c(double1, double2, double3) ~ recode.factor(.x, `0` = 'no', `1` = 'yes'))

# log: Age GrLivArea BaseLivArea LotFrontage LotArea
sum(ames$LowQualFinSF <= 1) # can do log
# sqrt: GarageArea LowQualFinSF
# no: 
# factor but mutliple levels: 
# well-distributed: BedroomAbvGr OverallCond OverallQual
# Bathrooms(close to 23 but exist 6) Fireplaces(close to 12 but exist 5)

transf.names <- c('Age', 'GrLivArea', 'BaseLivArea', 'LotFrontage', 'LotArea', 'GarageArea', 'LowQualFinSF')
pairs(ames[, transf.names])
cor(ames[, transf.names])

colinear.names <- c('Age', 'GrLivArea', 'BaseLivArea', 'LotFrontage', 'LotArea', 'GarageArea', 'LowQualFinSF', 'BedroomAbvGr', 'OverallCond', 'OverallQual', 'Bathrooms', 'Fireplaces')
cor(ames[, colinear.names])
abs(cor(ames[, colinear.names])) >= 0.6 & cor(ames[, colinear.names]) != 1
scatterplotMatrix(ames[, colinear.names])

summary(ames)
ggplot(ames, aes(Fireplaces)) +
  geom_boxplot()
ggplot(ames, aes(Age)) +
  geom_histogram()
ggplot(ames, aes(x = Age, y = SalePrice)) +
  geom_point() +
  geom_smooth()
# identify factor
sum(ames$TwoStory_dum == 1 | ames$TwoStory_dum == 0)
ggplot(ames, aes(LowQualFinSF)) +
  geom_bar()
ggplot(ames, aes(log(SalePrice))) +
  geom_histogram()

plot(sqrt(ames$GarageArea))
plot(ames$GarageArea)
plot(log(1+ames$GarageArea))
plot(log(ames$GarageArea))
plot(sqrt(ames$LowQualFinSF))
plot(ames$LowQualFinSF)
plot(log(1+ames$LowQualFinSF))
plot(log(ames$LowQualFinSF))

summary(ames$GarageArea)
summary(sqrt(ames$GarageArea))
plot(log(ames$Age))
plot(ames$Age)
summary(ames$GarageArea)
summary(log(ames$GarageArea))
ggplot(ames, aes(LowQualFinSF)) +
  geom_boxplot()
lm1 <- lm(SalePrice ~ ., data = ames)
par(mfrow = c(2,2))
plot(lm1)
step(lm2)

par(mfrow = c(1,1))
plot(ames$Age)
plot(log(ames$Age))
ggplot(ames, aes(Age)) +
  geom_histogram()
ggplot(ames, aes(log(Age))) +
  geom_histogram()
ggplot(ames, aes(sqrt(Age))) +
  geom_histogram()
ggplot(ames[ames$LowQualFinSF > 50,], aes(LowQualFinSF^2, SalePrice)) +
  geom_point() +
  geom_smooth()

qualitative <- c('Location', 'Amenities', 'RoadRail', 'TwoStory_dum', 'FlatContour_dum', 'FlatRoof_dum', 'Garage_dum', 'CentralAirNum', 'KitchenQual_Ex', 'Zoning_2', 'Zoning_3', 'Zoning_4', 'YrSold_2007', 'YrSold_2008', 'YrSold_2009', 'YrSold_2010')
ggpairs(ames[,c('SalePrice', qualitative)], axisLabels = "internal")

ggplot(ames, aes(Garage_dum, GarageArea)) + geom_boxplot()
ggplot(ames, aes(Garage_dum, SalePrice)) + geom_boxplot()
ggplot(ames, aes(TwoStory_dum, SalePrice)) + geom_boxplot()
ggplot(ames, aes(TwoStory_dum, GrLivArea)) + geom_boxplot()
ggplot(ames, aes(FlatContour_dum, SalePrice)) + geom_boxplot()
summary(aov(SalePrice ~ TwoStory_dum, ames))
summary(aov(BedroomAbvGr ~ TwoStory_dum, ames))
summary(aov(GarageArea ~ Garage_dum, ames))
qplot(sqrt(LotFrontage), SalePrice, data = ames)



ames.lm.initial <- lm(SalePrice ~ Age + GrLivArea + BaseLivArea + Location + Amenities + 
                      RoadRail + BedroomAbvGr + Bathrooms + OverallCond + OverallQual + 
                      LotFrontage + LotArea + TwoStory_dum + FlatContour_dum + 
                      FlatRoof_dum + GarageArea + Garage_dum + CentralAirNum + 
                      LowQualFinSF + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
                      Zoning_4 + YrSold_2007 + YrSold_2008 + YrSold_2009 + YrSold_2010, data = ames)
plot(ames.lm.initial)
summary(ames.lm.initial)

ames.lm.inilog <- lm(log(SalePrice) ~ Age + GrLivArea + BaseLivArea + Location + Amenities + 
                        RoadRail + BedroomAbvGr + Bathrooms + OverallCond + OverallQual + 
                        LotFrontage + LotArea + TwoStory_dum + FlatContour_dum + 
                        FlatRoof_dum + GarageArea + Garage_dum + CentralAirNum + 
                        LowQualFinSF + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
                        Zoning_4 + YrSold_2007 + YrSold_2008 + YrSold_2009 + YrSold_2010, data = ames)
plot(ames.lm.inilog)
summary(ames.lm.inilog)

ames.lm.total <- lm(log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + log(BaseLivArea) + Location + Amenities + 
            RoadRail + BedroomAbvGr + Bathrooms + OverallCond + OverallQual + 
            log(LotFrontage) + log(LotArea) + TwoStory_dum + FlatContour_dum + 
            FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + 
            sqrt(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
            Zoning_4 + YrSold_2007 + YrSold_2008 + YrSold_2009 + YrSold_2010, data = ames)
plot(ames.lm.total)
summary(ames.lm.total)
lm2 <- lm(log(SalePrice) ~ log(Age) + log(GrLivArea) + log(BaseLivArea) + Location + Amenities + 
            RoadRail + BedroomAbvGr + Bathrooms + OverallCond + OverallQual + 
            log(LotFrontage) + log(LotArea) + TwoStory_dum + FlatContour_dum + 
            FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + 
            sqrt(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
            Zoning_4 + YrSold_2007 + YrSold_2008 + YrSold_2009 + YrSold_2010, data = ames[1:2000,])
par(mfrow = c(2,2))
plot(lm2)
summary(lm2)

durbinWatsonTest(ames.lm) # nonsignificant p shows independence of errors
crPlots(ames.lm) # shows linearity
ncvTest(ames.lm) # nonsignificant p shows homoscedasticity
vif(ames.lm) # vif > 4 shows collinearity
gvlma(ames.lm) # assumptions test
summary(powerTransform(ames$SalePrice)) # y transformation, H0 as stated
boxTidwell(SalePrice ~ Age + GrLivArea + BaseLivArea + BedroomAbvGr + OverallCond + OverallQual + 
             LotFrontage + LotArea + GarageArea +  LowQualFinSF + Fireplaces, data = ames)
anova(lm(log(SalePrice) ~ log(Age) + Age + sqrt(GrLivArea) + log(BaseLivArea) + Location + Amenities +
           RoadRail + BedroomAbvGr + OverallCond + OverallQual + log(LotArea) + TwoStory_dum + FlatContour_dum + 
           FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + sqrt(LowQualFinSF) + Fireplaces + 
           CentralAirNum + KitchenQual_Ex + Zoning_2 + Zoning_3 + Zoning_4, data = ames), 
      lm(log(SalePrice) ~ Age + sqrt(GrLivArea) + log(BaseLivArea) + Location + Amenities +
           RoadRail + BedroomAbvGr + OverallCond + OverallQual + log(LotArea) + TwoStory_dum + FlatContour_dum + 
           FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + sqrt(LowQualFinSF) + Fireplaces + 
           CentralAirNum + KitchenQual_Ex + Zoning_2 + Zoning_3 + Zoning_4, data = ames)) # compare nested model
leaps <- regsubsets(log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + log(BaseLivArea) + Location + Amenities + 
                      RoadRail + BedroomAbvGr + Bathrooms + OverallCond + OverallQual + 
                      log(LotFrontage) + log(LotArea) + TwoStory_dum + FlatContour_dum + 
                      FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + 
                      sqrt(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
                      Zoning_4 + YrSold_2007 + YrSold_2008 + YrSold_2009 + YrSold_2010, data = ames, nbest = 40)
plot(leaps, scale="adjr2") # subset selection
step(lm(log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + log(BaseLivArea) + Location + 
          RoadRail + BedroomAbvGr + OverallCond + OverallQual + 
          log(LotArea) + TwoStory_dum + FlatContour_dum + 
          FlatRoof_dum + sqrt(GarageArea) + CentralAirNum + 
          sqrt(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
          Zoning_4 + YrSold_2008 + YrSold_2010, data = ames))
bptest(ames.lm) # large p thus homo
shapiro.test(ames.lm$residuals) # large p thus normal









plot(ames.lm)
summary(ames.lm)
ames.lm <- lm(formula = log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + log(BaseLivArea) + 
            Location + RoadRail + BedroomAbvGr + OverallCond + OverallQual + 
            log(LotFrontage) + log(LotArea) + TwoStory_dum + FlatContour_dum + 
            FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + 
            sqrt(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + 
            Zoning_3 + Zoning_4 + YrSold_2008 + YrSold_2010, data = ames[1:2000, 
            ])
predict_y <- exp(predict(ames.lm, ames[2001:2099,2:29])) * (sum(exp(ames.lm$residuals)) / 2000)
(rmse <- sqrt(sum((predict_y - ames[2001:2099,1]) ^ 2 / 99)))

(alpha0 <- sum(exp(ames.lm$fitted.values) * ames$SalePrice[1:2000]) / sum(exp(ames.lm$fitted.values) ^ 2))
predict_y <- exp(predict(ames.lm, ames[2001:2099,2:29])) * alpha0
(rmse <- sqrt(sum((predict_y - ames[2001:2099,1]) ^ 2 / 99)))

# sure: LotFrontage-log    LowQualFinSF-sqrt
# maybe: GarageArea-log LotArea-sqrt BaseLivArea-log GrLivArea - log
rmse.avg <- function() {
  avg.rmse <- 0
  itr <- 0
  check.sample <- 0
  samples <- data.frame(rep(0, 2099))
  sample.n <- 0
  exceed <- 0
  for (i in 1:1000) {
    for (ns in seq(50,400,50)) {
      itr <- itr + 1
      samples <- cbind(samples, sample.n)
      sample.n <- srswor(ns, 2099)
      #for (cks in 1:itr) {check.sample <- check.sample + as.numeric(sum(samples[,cks] == sample.n) == ns)}
      ames.lm <- lm(formula = log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + sqrt(BaseLivArea) + Location + 
                      RoadRail + BedroomAbvGr + OverallCond + OverallQual + 
                      log(LotArea) + TwoStory_dum + FlatContour_dum + 
                      FlatRoof_dum + sqrt(GarageArea) + CentralAirNum + 
                      log(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
                      Zoning_4 + YrSold_2008 + YrSold_2010, data = ames[sample.n == 0,])
      predict_y <- exp(predict(ames.lm, ames[sample.n == 1,2:29])) * (sum(exp(ames.lm$residuals)) / (2099 - ns))
      exceed <- exceed + as.numeric(sqrt(sum((predict_y - ames[sample.n == 1,1]) ^ 2 / ns)) >= 21000)
      avg.rmse <- avg.rmse + sqrt(sum((predict_y - ames[sample.n == 1,1]) ^ 2 / ns))
    }
  }
  c(avg.rmse / itr, check.sample, exceed)
}
rmse.avg()
# lm: 18554.42     0.00   695.00
# Zoning_2: 18545.92     0.00   684.00
# BedroomAbvGr: 18606.73     0.00   727.00
# 