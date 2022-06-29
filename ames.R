library(readr)
library(tidyverse)
options(scipen = 4)

# Data Import and Transformation
ames_22_train <- read_csv("C:/Users/685685/Downloads/Data Analysis Using R/ames_22_train.csv")
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


# Data Transformation
# response SalePrice
ggplot(ames, aes(x = SalePrice)) + geom_histogram() # right-skewed
ggplot(ames, aes(x = log(SalePrice))) + geom_histogram()
par(mfrow = c(2,2))
plot(lm(SalePrice ~ ., data = ames)) # non-constant variance thus log transformation
# predictors
# factors
fctr.names <- c('Location', 'Amenities', 'RoadRail', 'TwoStory_dum', 'FlatContour_dum', 'FlatRoof_dum', 
                'Garage_dum', 'CentralAirNum', 'KitchenQual_Ex', 'Zoning_2', 'Zoning_3', 'Zoning_4', 
                'YrSold_2007', 'YrSold_2008', 'YrSold_2009', 'YrSold_2010')
summary(ames[, fctr.names])

# varibles without transformed into factors
notrans.names <- c('BedroomAbvGr', 'Bathrooms', 'OverallCond', 'OverallQual', 'Fireplaces')
summary(ames[, notrans.names]) # floating points thus not transformed into factors

par(mfrow = c(1,1))
# log transformation: 
# Age
plot(ames$Age) # meaning transformation for age in house, compared with people's age
# LotFrontage
plot(ames$LotFrontage)
ggplot(ames, aes(LotFrontage)) + geom_boxplot() # sparse outliers with huge difference thus use log
# LotArea
plot(ames$LotArea)
ggplot(ames, aes(LotArea)) + geom_boxplot() # also sparse outliers with huge unit different with most of data
# LowQualFinSF
plot(ames$LowQualFinSF)
ggplot(ames, aes(LowQualFinSF)) + geom_boxplot() # similar to before

# sqrt transformation:
# GrLivArea
summary(ames$GrLivArea)
plot(ames$GrLivArea)
ggplot(ames, aes(GrLivArea)) + geom_boxplot() # close outliers to most of data thus sqrt instead of log
# BaseLivArea
summary(ames$BaseLivArea)
plot(ames$BaseLivArea)
ggplot(ames, aes(BaseLivArea)) + geom_boxplot() # use sqrt to narrow difference between normal data
# GarageArea
summary(ames$GarageArea)
plot(ames$GarageArea)
ggplot(ames, aes(GarageArea)) + geom_boxplot() # close outliers


# collinearity
quantitative <- c('Age', 'GrLivArea', 'BaseLivArea', 'BedroomAbvGr', 'OverallCond', 'OverallQual', 'LotFrontage', 'LotArea',
                  'GarageArea', 'LowQualFinSF', 'Fireplaces')
cor(ames[, quantitative])
abs(cor(ames[, quantitative])) >= 0.7 & cor(ames[, quantitative]) != 1
pairs(ames[, quantitative])


# Model Determination - backward selection
ames.lm.total <- lm(log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + sqrt(BaseLivArea) + Location + Amenities + 
                      RoadRail +BedroomAbvGr + Bathrooms + OverallCond + OverallQual + 
                      log(LotFrontage) + log(LotArea) + TwoStory_dum + FlatContour_dum + 
                      FlatRoof_dum + sqrt(GarageArea) + Garage_dum + CentralAirNum + 
                      log(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
                      Zoning_4 + YrSold_2007 + YrSold_2008 + YrSold_2009 + YrSold_2010, data = ames)
summary(ames.lm.total)
ames.lm.total <- update(ames.lm.total, . ~ . - YrSold2009 - YrSold2007 - Garage_dum - Bathrooms - Amenities - log(LotFrontage))
# final linear regression model
ames.lm <- lm(log(SalePrice) ~ log(Age) + sqrt(GrLivArea) + sqrt(BaseLivArea) + Location + 
                RoadRail + BedroomAbvGr + OverallCond + OverallQual + 
                log(LotArea) + TwoStory_dum + FlatContour_dum + 
                FlatRoof_dum + sqrt(GarageArea) + CentralAirNum + 
                log(LowQualFinSF) + Fireplaces + KitchenQual_Ex + Zoning_2 + Zoning_3 + 
                Zoning_4 + YrSold_2008 + YrSold_2010, data = ames)
summary(ames.lm)
par(mfrow = c(2,2))
plot(ames.lm) # homoscedasticity, tailed but large sample size, no outliers
confint(ames.lm) # significant coefficients


# Prediction and Data Output
ames_22_test <- read_csv("C:/Users/685685/Downloads/Data Analysis Using R/ames_22_test.csv")
ames_22_test <- ames_22_test %>%
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
SalePrice <- exp(predict(ames.lm, ames_22_test)) * (sum(exp(ames.lm$residuals)) / 2099)
write_csv(data.frame(SalePrice), "C:/Users/685685/Downloads/Data Analysis Using R/1830018034.csv")
