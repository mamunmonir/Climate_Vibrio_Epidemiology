library(dplyr)
library(lubridate)

setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data/Dr.\ Mamun")

AvgT<-read.csv("AvgT_reads.csv", header=TRUE)

Reformated_data<-NULL
for(i in 1:length(AvgT[,1])){
  days<-1:31
  month<-rep(AvgT[i,2], 31)
  year<-rep(AvgT[i,1], 31)
  dta<-as.vector(AvgT[i,-c(1,2)])
  refordta<-cbind(year,month,days,dta)
  Reformated_data<-rbind(Reformated_data, refordta)
}

write.csv(Reformated_data, file="AvgT_reformated.csv")

###############
AvgT<-read.csv("MaxT_reads.csv", header=TRUE)

Reformated_data<-NULL
for(i in 1:length(AvgT[,1])){
  days<-1:31
  month<-rep(AvgT[i,2], 31)
  year<-rep(AvgT[i,1], 31)
  dta<-as.vector(AvgT[i,-c(1,2)])
  refordta<-cbind(year,month,days,dta)
  Reformated_data<-rbind(Reformated_data, refordta)
}

write.csv(Reformated_data, file="MaxT_reformated.csv")

###############
AvgT<-read.csv("MinT_reads.csv", header=TRUE)

Reformated_data<-NULL
for(i in 1:length(AvgT[,1])){
  days<-1:31
  month<-rep(AvgT[i,2], 31)
  year<-rep(AvgT[i,1], 31)
  dta<-as.vector(AvgT[i,-c(1,2)])
  refordta<-cbind(year,month,days,dta)
  Reformated_data<-rbind(Reformated_data, refordta)
}

write.csv(Reformated_data, file="MinT_reformated.csv")

###############
AvgT<-read.csv("MSLP_reads.csv", header=TRUE)

Reformated_data<-NULL
for(i in 1:length(AvgT[,1])){
  days<-1:31
  month<-rep(AvgT[i,2], 31)
  year<-rep(AvgT[i,1], 31)
  dta<-as.vector(AvgT[i,-c(1,2)])
  refordta<-cbind(year,month,days,dta)
  Reformated_data<-rbind(Reformated_data, refordta)
}

write.csv(Reformated_data, file="MLSP_reformated.csv")

###############
AvgT<-read.csv("Rainfall_reads.csv", header=TRUE)

Reformated_data<-NULL
for(i in 1:length(AvgT[,1])){
  days<-1:31
  month<-rep(AvgT[i,2], 31)
  year<-rep(AvgT[i,1], 31)
  dta<-as.vector(AvgT[i,-c(1,2)])
  refordta<-cbind(year,month,days,dta)
  Reformated_data<-rbind(Reformated_data, refordta)
}

write.csv(Reformated_data, file="Rainfall_reformated.csv")

###############
AvgT<-read.csv("RH_reads.csv", header=TRUE)

Reformated_data<-NULL
for(i in 1:length(AvgT[,1])){
  days<-1:31
  month<-rep(AvgT[i,2], 31)
  year<-rep(AvgT[i,1], 31)
  dta<-as.vector(AvgT[i,-c(1,2)])
  refordta<-cbind(year,month,days,dta)
  Reformated_data<-rbind(Reformated_data, refordta)
}

write.csv(Reformated_data, file="RH_reformated.csv")

######################
# Days

date1 = seq(as.Date("1996-01-01"), as.Date("2024-12-31"), by = "day")

write.csv(date1, file="dates.csv")


#########################

library(mgcv)
library(dplyr)

daily_data<-read.csv("Climate_cholera_daily1.csv", header=TRUE)

daily_data$Dates<-as.Date(daily_data$Dates)
daily_data$MLSP<-as.numeric(daily_data$MLSP)

weekly_data <- daily_data %>%
  mutate(week = floor_date(Dates, "week")) %>%
  group_by(week) %>%
  summarise(
    Cholera_case = sum(Cholera_case), # Sum cases per week
    AvgT=	mean(AvgT,na.rm = TRUE),
    MaxT= mean(MaxT,na.rm = TRUE),	
    MinT= mean(MinT,na.rm = TRUE),	
    MLSP= mean(MLSP,na.rm = TRUE),	
    Rainfall=sum(Rainfall,na.rm = TRUE),	
    RH= mean(RH,na.rm = TRUE)
  ) %>%
  mutate(
    AvgT_lag = lag(AvgT, 1),
    Rainfall_lag = lag(Rainfall, 1),
    RH_lag = lag(RH, 1)
  )

# Fit Generalized Additive Model (GAM) for Weekly Data
gam_model_weekly <- gam(Cholera_case ~ s(AvgT) + s(Rainfall) + s(RH) +
                          s(AvgT_lag) + s(Rainfall_lag) + s(RH_lag), 
                        family = nb(), data = weekly_data)

summary(gam_model_weekly)
plot(gam_model_weekly, pages=1)

tiff(filename = "GAM_Model_climate_weekly_cases.tiff",
     width = 30, height = 30, units = "px", pointsize = 12,
     bg = "white", res = 300)
plot(gam_model_weekly, pages=1)
plot(gam_model_weekly, pages = 1, shade = TRUE, rug = TRUE, seWithMean = TRUE)
dev.off()


library(ggplot2)
library(gratia)
draw(gam_model_weekly) 

ggplot(weekly_data, aes(x = AvgT_lag, y = Cholera_case)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Effect of Lagged Temperature on Cholera Cases", x = "Lagged Avg Temperature", y = "Cholera Cases")

library(lmtest)
ccf(weekly_data$AvgT, weekly_data$Cholera_case, lag.max = 12, main="Cross-correlation: Temp & Cholera Cases")
ccf(weekly_data$Rainfall, weekly_data$Cholera_case, lag.max = 12, main="Cross-correlation: Temp & Cholera Cases")

############################
# Analysis of yearly data
# Number of weeks with favorable with favorable climatic factors

daily_data$Week <- format(daily_data$Dates, "%U")
daily_data$Month <- format(daily_data$Dates, "%m")
daily_data$Year <- format(daily_data$Dates, "%Y")

daily_data2<-daily_data[,c(2,3,7,8,9,11)]

weekly_data_v2 <- daily_data2 %>%   
  group_by(Year, Week) %>%   
  summarise(     
    Cholera_case = sum(Cholera_case, na.rm = TRUE), # Sum cases per week     
    AvgT = mean(AvgT, na.rm = TRUE),     
    Rainfall = sum(Rainfall, na.rm = TRUE),	     
    RH = mean(RH, na.rm = TRUE)   
  ) %>% 
  ungroup()

yearly_data <- weekly_data_v2 %>%
  group_by(Year) %>%
  summarise(
    Cholera_case = sum(Cholera_case),
    temp_week_above_25 = sum(AvgT > 25),  # Count of weeks > 25°C
    heavy_rain_weeks = sum(Rainfall > 100),  # Count of weeks with rainfall > 100mm
    favourable_RH = sum(RH > 68 & RH < 77)  # Average humidity
  )

library(MASS)

# Fit Negative Binomial Model for Yearly Data
nb_model_yearly <- glm.nb(Cholera_case ~ temp_week_above_25 + heavy_rain_weeks + favourable_RH,
                          data = yearly_data)

summary(nb_model_yearly)

write.csv(yearly_data,file="climate_cholera_yearly.csv")

################################################
# Analysis by including the lineage information

yearly_data_cl_ln<-read.csv("climate_cholera_lineage_yearly.csv", header=TRUE)

yearly_data_cl_ln$Label1 <- as.factor(yearly_data_cl_ln$Label1) # Convert to factor
yearly_data_cl_ln$Label1 <- relevel(yearly_data_cl_ln$Label1, ref = "Single.dominant") # Relevel

yearly_data_cl_ln$Label2 <- as.factor(yearly_data_cl_ln$Label2) # Convert to factor
yearly_data_cl_ln$Label2 <- relevel(yearly_data_cl_ln$Label2, ref = "Same") # Relevel


nb_model_yearly2 <- glm.nb(Cholera_case ~ temp_week_above_25 + heavy_rain_weeks + favourable_RH+Label1+Label2,
                          data = yearly_data_cl_ln)

summary(nb_model_yearly2)

AIC(nb_model_yearly2, null_model)

library(visreg)

visreg(nb_model_yearly2, "temp_week_above_25", scale = "response", gg = TRUE)
visreg(nb_model_yearly2, "heavy_rain_weeks", scale = "response", gg = TRUE)
visreg(nb_model_yearly2, "Label1", scale = "response", gg = TRUE)

# Fit null model (no predictors)
null_model <- glm.nb(Cholera_case ~ 1, data = yearly_data_cl_ln)

# Likelihood Ratio Test
anova(null_model, nb_model_yearly2, test = "LRT")

overdispersion <- sum(residuals(nb_model_yearly2, type = "pearson")^2) / nb_model_yearly2$df.residual
overdispersion

# McFadden's pseudo R-squared
1 - (logLik(nb_model_yearly2) / logLik(null_model))

plot(fitted(nb_model_yearly2), residuals(nb_model_yearly2, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Pearson Residuals vs Fitted")
abline(h = 0, col = "red")

########################
nb_model_yearly3 <- glm.nb(Cholera_case ~ log(temp_week_above_25) + log(heavy_rain_weeks) + log(favourable_RH)+Label1+Label2,
                           data = yearly_data_cl_ln[-1,])
cooksd <- cooks.distance(nb_model_yearly3)
plot(cooksd, type = "h", main = "Cook's Distance")
abline(h = 4 / length(cooksd), col = "red", lty = 2)

# Define weights: low for high Cook's D
weights <- ifelse(cooksd > 4 / length(cooksd), 0.5, 1)

# Refit with weights
summary(nb_model_yearly3)
model_weighted <- glm.nb(formula(nb_model_yearly3), data = yearly_data_cl_ln[-1,], weights = weights)
summary(model_weighted)

overdispersion <- sum(residuals(model_weighted, type = "pearson")^2) / model_weighted$df.residual
overdispersion

plot(cooks.distance(model_weighted), type = "h",
     ylab = "Cook's Distance", main = "Cook's Distance")
abline(h = 4 / nrow(model_weighted$model), col = "blue", lty = 2)

library(car)
influencePlot(nb_model_yearly2)

library(DHARMa)
res <- simulateResiduals(nb_model_yearly2)
plot(res)

library(effects)
plot(allEffects(nb_model_yearly2), multiline = TRUE)

plot(nb_model_yearly2, which = 2)

library(car)
qqPlot(nb_model_yearly2$resid, ylab="Residuals", xlab="Theoretical Quantiles")


