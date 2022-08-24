
#Load packages
pacman::p_load(tidyverse, table1, foreign, haven, Publish, survey, ROCR, odds.n.ends, blorr, lmtest, car)

# Read in dataset
DHS <- read_sav("NGKR7BFL.SAV")



#Select variables of interest into smaller dataframe called dhs1 and rename variables.
dhs1 <- DHS %>%
  dplyr::select(V101, V102, V106, V136, V137, V190, V714, B4, B8, B5, B19, HW70, HW71,HW72) %>%
  rename(region = V101, residence = V102, education = V106, hseholdno = V136, children_5 = V137, wealth_index = V190, employment = V714, sex = B4, ageyr = B8, child_alive = B5, agemonth = B19, htage = HW70, wtage = HW71, wtht = HW72)

summary(dhs1)



#Clean variables and rename variable labels
dhs2 <- dhs1 %>%
  mutate(region= case_when(region== 1~ 'North', 
                           region== 2~ 'North',
                           region== 3~ 'North',
                           TRUE ~ 'South')) %>%
  mutate(residence= case_when(residence== 1~ 'Urban',
                              TRUE ~ 'Rural')) %>%
  mutate(education= case_when(education== 0~ 'No education',
                              education== 1~ 'Primary',
                              education== 2~ 'Secondary',
                              TRUE ~ 'Higher education')) %>%
  mutate(wealth_index= case_when(wealth_index== 1~ 'Poorest',
                                 wealth_index== 2~ 'Poorer',
                                 wealth_index== 3~ 'Middle',
                                 wealth_index== 4~ 'Richer',
                                 TRUE ~ 'Richest')) %>%
  mutate(employment= case_when(employment== 0~ 'Not employed',
                               TRUE ~ 'Employed')) %>%
  mutate(sex= case_when(sex== 1~ 'Male',
                        TRUE ~ 'Female')) %>%
  mutate(child_alive= case_when(child_alive== 0~ 'dead',
                                child_alive== 1~ 'alive'))
table(dhs2$child_alive)
sapply(dhs2, table)

dhs3 <- dhs2 %>%
  mutate(ht4age= htage/100) %>%
  mutate(wt4age= wtage/100) %>%
  mutate(wt4ht= wtht/100) 

dhs3$agemo <- ifelse(dhs3$child_alive == 'alive', dhs3$agemonth, NA)

#dhs4 <- dhs3 %>%
#ht4age <- if_else(ht4age < -2, 1, 0) %>%
#ht4age <- factor(ht4age, levels = c(1,0), labels = c('Stunted', 'Not stunted')) %>%
#wt4age <- if_else(wt4age < -2, 1, 0) %>%
#wt4age <- factor(wt4age, levels = c(1,0), labels = c('Underweight', 'Not underweight')) %>%
#wt4ht <- if_else(wt4ht < -2, 1, 0) %>%
#wt4ht <- factor(wt4ht, levels = c(1,0), labels = c('Wasted', 'Not wasted'))


dhs3$ht4age <- if_else(dhs3$ht4age > -2, 0, 1)
dhs3$ht4age <- factor(dhs3$ht4age, levels = c(0,1), labels = c('Not stunted', 'Stunted'))

dhs3$wt4age <- if_else(dhs3$wt4age > -2, 0, 1)
dhs3$wt4age <- factor(dhs3$wt4age, levels = c(0,1), labels = c('Not underweight', 'Underweight'))

dhs3$wt4ht <- if_else(dhs3$wt4ht > -2, 0, 1)
dhs3$wt4ht <- factor(dhs3$wt4ht, levels = c(0,1), labels = c('Not wasted', 'Wasted'))

dhs_new <- dhs3 %>%
  drop_na()
summary(dhs_new)
sapply(dhs_new, table) 

#Check missing rate
missingRate <- 1- nrow(dhs_new)/nrow(dhs3)
missingRate

#view class of variables
sapply(dhs_new, class)



#graphs of variables
# 1) Age
dhs_new %>%
  ggplot(aes(x = agemo)) +
  geom_bar() +
  labs(x = "age in months", y = "Observations",
       title = "Age distribution (in Months) of under 5s in Nigeria (DHS 2018)") +
  theme_minimal()

# 2) Number of household members
dhs_new %>%
  ggplot(aes(x = hseholdno, fill = hseholdno)) +
  geom_bar() +
  labs(x = "Number of household members", y = "Observations",
       title = "Number of household members (DHS 2018)") +
  theme_minimal()

# 3) Number of children <5 in household
dhs_new %>%
  ggplot(aes(x = children_5, fill = children_5)) +
  geom_bar() +
  labs(x = "Number of children <5", y = "Observations",
       title = "Number of children <5 in household (DHS 2018)") +
  theme_minimal()


#Table 1 to describe participants.
label(dhs_new$agemo)<-"Age of child in months (Median , IQR)"
label(dhs_new$sex)<-"Sex of child"
label(dhs_new$residence)<-"Place of residence"
label(dhs_new$education)<-"Level of education of mother"
label(dhs_new$hseholdno)<-"Number of persons in the household"
label(dhs_new$children_5)<-"Number of children under 5 in the household"
label(dhs_new$wealth_index)<-"Household wealth index"
label(dhs_new$employment)<-"Mother's employment status"
label(dhs_new$wt4age)<-"Child's Weight for Age"
label(dhs_new$ht4age)<-"Child's Height for Age"
label(dhs_new$wt4ht)<-"Child's Weight for Height"



#check levels
levels(dhs_new$wt4age)
levels(dhs_new$ht4age)
levels(dhs_new$wt4ht)

#Relevel variables
#dhs_new$wt4age <- relevel(dhs_new$wt4age, ref = "Underweight")
#dhs_new$ht4age <- relevel(dhs_new$ht4age, ref = "Stunted")
#dhs_new$wt4ht <- relevel(dhs_new$wt4ht, ref = "Wasted")
dhs_new$region <- relevel(as.factor(dhs_new$region), ref = "South")
dhs_new$residence <- relevel(as.factor(dhs_new$residence), ref = "Urban")
dhs_new$education <- relevel(as.factor(dhs_new$education), ref = "Higher education")
dhs_new$wealth_index <- relevel(as.factor(dhs_new$wealth_index), ref = "Richest")
dhs_new$employment <- as.factor(dhs_new$employment)
dhs_new$sex <- as.factor(dhs_new$sex)



## UNDERWEIGHT
#logistic model with underweight as outcome
underweightModel <- glm(wt4age ~ region + residence + education + wealth_index + employment, data= dhs_new, family="binomial")
summary(underweightModel)

odds.n.ends(underweightModel)



#logistic model with underweight as outcome and age & sex as confounders
fullUnderweightModel <- glm(wt4age ~ region + residence + education + wealth_index + employment + agemo + sex, data= dhs_new, family="binomial")
summary(fullUnderweightModel)

odds.n.ends(fullUnderweightModel)



### Full model vs. reduced model
#likelihood ratio test: compare two nested models
lrtest(underweightModel, fullUnderweightModel)

#The absolute values of log likelihood of the model with confounders (-5592.4) is larger than the reduced model (-5598.3). Based on the significant p value from LR test, the model with confounders is better than the reduced model.



### Assumption Check

#*Independence of observations* The survey uses a two-stage cluster sampling method to select households for data collection. There is one record for every child between 0 - 59 months of interviewed women. One woman may have multiple children in the data set and so, It is likely that observations may not be completely independent.

#*Linearity*
dhs_new <- dhs_new %>%
  mutate(agemo.logAgemo = agemo * log(agemo)) #create term to test linearity

boxTidwellAge <- glm(wt4age ~ region + residence + education + wealth_index + employment + agemo + agemo.logAgemo + sex, data= dhs_new, family="binomial") 

summary(boxTidwellAge)

#The interaction term has a significant p value (p \< 1.5e-09) indicating that the linearity assumption is violated.



#Transform the age variable to age categories
dhsAgeRec <- dhs_new %>%
  mutate(age_cat= as.factor(case_when(agemo < 24 ~ "less than 2 years",
                                      agemo >= 24 ~ "2 to 5 years")))

table(dhsAgeRec$age_cat)

#Rerun model
underweightCatMod <- glm(wt4age ~ region + residence + education + wealth_index + employment + age_cat + sex, data= dhsAgeRec, family="binomial") 

summary(underweightCatMod)
odds.n.ends(underweightCatMod)

#check for effect modification
underwgtCatIntMod <- glm(wt4age ~ region + residence + education + wealth_index + employment + age_cat + sex + region*residence + region*education + region*wealth_index + region*employment + region*age_cat + region*sex, data= dhsAgeRec, family="binomial") 

summary(underwgtCatIntMod)

#There is no effect modification by region. All the interaction terms are not significant.



## Multicollinearity
#Variance Inflation Factors
vif(underweightCatMod)
vif(underweightModel)

#VIF are all less than 2 indicating that there is no multicollinearity between the variables.



##Influence
#influence plot - Cook's D plot-identifies observation number in parent dataset
plot(underweightCatMod, which=4, id.n=5, col="red")
plot(underweightModel, which=4, id.n=5, col="red")



#Hosmer lemeshow goodness of fit test for reduced model
blr_test_hosmer_lemeshow(underweightCatMod)
blr_test_hosmer_lemeshow(fullUnderweightModel)
blr_test_hosmer_lemeshow(underweightModel)

#The Hosmer & lemeshow goodness of fit test for the model without confounders has a non-significant p-value (0.4605) indicating that this model is a good fit. This model also meets all the assumptions of the logistic regression model. The model with confounders does not meet linearity assumption and is not a good fit (p = 0.0158). While the model with age categories meets all assumptions, it is shown to not be a good fit (p = 0.0134).



## STUNTING
#logistic model with stunting as outcome
stuntingModel <- glm(ht4age ~ region + residence + education + wealth_index + employment, data= dhs_new, family="binomial")
summary(stuntingModel)

odds.n.ends(stuntingModel)



#logistic model with stunting as outcome and age & sex as confounders
fullStuntingModel <- glm(ht4age ~ region + residence + education + wealth_index + employment + agemo + sex, data= dhs_new, family="binomial")
summary(fullStuntingModel)

odds.n.ends(fullStuntingModel)



### Full model vs Reduced model
#likelihood ratio test: compare two nested models
lrtest(stuntingModel, fullStuntingModel)

#The absolute values of log likelihood of the model with confounders (-6724.7) is larger than the reduced model (-6814.6). Based on the significant p value from LR test, the model with confounders is better than the reduced model.



### Assumption Check
#linearity
dhs_new <- dhs_new %>%
  mutate(agemo.logAgemo = agemo * log(agemo)) #create term to test linearity

boxTidwellAge2 <- glm(ht4age ~ region + residence + education + wealth_index + employment + agemo + agemo.logAgemo + sex, data= dhs_new, family="binomial") 

summary(boxTidwellAge2)

#The interaction term has a significant p value (p < 2e-16) indicating that the linearity assumption is violated.



#Rerun model with age categories
stuntingCatMod <- glm(ht4age ~ region + residence + education + wealth_index + employment + age_cat + sex, data= dhsAgeRec, family="binomial") 

summary(stuntingCatMod)
odds.n.ends(stuntingCatMod)



#Check if region modifies the association between stunting and other variables
stuntingCatMod <- glm(ht4age ~ region + residence + education + wealth_index + employment + age_cat + sex, data= dhsAgeRec, family="binomial") 
summary(stuntingCatMod)

stuntCatIntMod <- glm(ht4age ~ region + residence + education + wealth_index + employment + age_cat + sex + region*residence + region*education + region*wealth_index + region*employment + region*age_cat + region*sex, data= dhsAgeRec, family="binomial") 

summary(stuntCatIntMod)




## Multicollinearity
#Variance Inflation Factors
vif(stuntingModel)
vif(fullStuntingModel)
vif(stuntingCatMod)

#there is no multicollinearity between the variables.



## Influence
#influence plot - Cook's D plot-identifies observation number in parent dataset
plot(stuntingModel, which=4, id.n=5, col="red")
plot(fullStuntingModel, which=4, id.n=5, col="red")
plot(stuntingCatMod, which=4, id.n=5, col="red")



#Hosmer lemeshow goodness of fit test for full model
blr_test_hosmer_lemeshow(fullStuntingModel)

#The Hosmer & lemeshow goodness of fit test has a significant p-value (0.0000) indicating that this model is not a good fit.



blr_test_hosmer_lemeshow(stuntingModel)

blr_test_hosmer_lemeshow(stuntingCatMod)

#The Hosmer & lemeshow goodness of fit test for the stunting model has a non-significant p-value (0.599) indicating that this model is a good fit. The Hosmer & lemeshow goodness of fit test for the stuntingCatmod has a significant p-value (6e-04) indicating that this model is not a good fit.



## Wasting
#logistic model with wasting as outcome
wastingModel <- glm(wt4ht ~ region + residence + education + wealth_index + employment, data= dhs_new, family="binomial")
summary(wastingModel)

odds.n.ends(wastingModel)



#logistic model with stunting as outcome and age & sex as confounders
fullWastingModel <- glm(wt4ht ~ region + residence + education + wealth_index + employment + agemo + sex, data= dhs_new, family="binomial")
summary(fullWastingModel)

odds.n.ends(fullWastingModel)



### Full model vs Reduced model
#likelihood ratio test: compare two nested models
lrtest(wastingModel, fullWastingModel)

#The absolute values of log likelihood of the model with confounders (-2693.8) is larger than the reduced model (-2782.6). Based on the significant p value from LR test, the model with confounders is better than the reduced model.



### Assumption Check
# Linearity
boxTidwellAge3 <- glm(wt4ht ~ region + residence + education + wealth_index + employment + agemo + agemo.logAgemo + sex, data= dhs_new, family="binomial") 

summary(boxTidwellAge3)

#The interaction term has a significant p value (p = 0.00311) indicating that the linearity assumption is violated.



#Rerun model with age categories
wastingCatMod <- glm(wt4ht ~ region + residence + education + wealth_index + employment + age_cat + sex, data= dhsAgeRec, family="binomial") 

summary(wastingCatMod)
odds.n.ends(wastingCatMod)



#Check if region modifies the association between wasting and other variables
wastingCatMod <- glm(wt4ht ~ region + residence + education + wealth_index + employment + age_cat + sex, data= dhsAgeRec, family="binomial") 

summary(wastingCatMod)

wastingCatIntMod <- glm(wt4ht ~ region + residence + education + wealth_index + employment + age_cat + sex + region*residence + region*education + region*wealth_index + region*employment + region*age_cat + region*sex, data= dhsAgeRec, family="binomial") 

summary(wastingCatIntMod)



## Multicollinearity
#Variance Inflation Factors
vif(wastingModel)
vif(fullWastingModel)
vif(wastingCatMod)

#There is no multicollinearity between the variables.



#Influence
#influence plot - Cook's D plot-identifies observation number in parent dataset
plot(wastingModel, which=4, id.n=5, col="green")
plot(fullWastingModel, which=4, id.n=5, col="green")
plot(wastingCatMod, which=4, id.n=5, col="green")



#Hosmer lemeshow goodness of fit test for full model
blr_test_hosmer_lemeshow(wastingModel)
blr_test_hosmer_lemeshow(fullWastingModel)
blr_test_hosmer_lemeshow(wastingCatMod)

#The Hosmer & lemeshow goodness of fit test for all 3 models have non-significant p-values indicating that these models are good fits. I will be using the wasting model and the wastingCatmod.



## Checking main effects of region on undernutrition
# underweight
regionModel <- glm(wt4age ~ region, data= dhs_new, family="binomial")
summary(regionModel)
odds.n.ends(regionModel)

# Stunting
regionModel2 <- glm(ht4age ~ region, data= dhs_new, family="binomial")
summary(regionModel2)
odds.n.ends(regionModel2)

# Wasting
regionModel3 <- glm(wt4ht ~ region, data= dhs_new, family="binomial")
summary(regionModel3)
odds.n.ends(regionModel3)

#Effects of region on undernutrition adjusting for age and sex of the child
regionModel5 <- glm(ht4age ~ region + age_cat + sex, data= dhsAgeRec, family="binomial")
summary(regionModel5)
odds.n.ends(regionModel5)



# Check if age category modifies the relationship between region and undernutrition
YoungerMod <-glm(wt4age ~ region, dhsAgeRec[which(dhsAgeRec$age_cat=="less than 2 years"),], family="binomial")
summary(YoungerMod)
odds.n.ends(YoungerMod)
OlderMod <-glm(wt4age ~ region, dhsAgeRec[which(dhsAgeRec$age_cat=="2 to 5 years"),], family="binomial")
summary(OlderMod)
odds.n.ends(OlderMod)

YoungerMod1 <-glm(ht4age ~ region, dhsAgeRec[which(dhsAgeRec$age_cat=="less than 2 years"),], family="binomial")
summary(YoungerMod1)
odds.n.ends(YoungerMod1)
OlderMod1 <-glm(ht4age ~ region, dhsAgeRec[which(dhsAgeRec$age_cat=="2 to 5 years"),], family="binomial")
summary(OlderMod1)
odds.n.ends(OlderMod1)

YoungerMod2 <-glm(wt4ht ~ region, dhsAgeRec[which(dhsAgeRec$age_cat=="less than 2 years"),], family="binomial")
summary(YoungerMod2)
odds.n.ends(YoungerMod2)
OlderMod2 <-glm(wt4ht ~ region, dhsAgeRec[which(dhsAgeRec$age_cat=="2 to 5 years"),], family="binomial")
summary(OlderMod2)
odds.n.ends(OlderMod2)



# Logistic model showing the association between undernutrition and maternal deprivation factors stratified by region.

#Underweight
NorthMod <-glm(wt4age ~ residence + education + wealth_index + employment + age_cat + sex, dhsAgeRec[which(dhsAgeRec$region=="North"),], family="binomial")
summary(NorthMod)
odds.n.ends(NorthMod)
SouthMod <-glm(wt4age ~ residence + education + wealth_index + employment + age_cat + sex, dhsAgeRec[which(dhsAgeRec$region=="South"),], family="binomial")
summary(SouthMod)
odds.n.ends(SouthMod)

#Stunting
NorthStuntMod <-glm(ht4age ~ residence + education + wealth_index + employment + age_cat + sex, dhsAgeRec[which(dhsAgeRec$region=="North"),], family="binomial")
summary(NorthStuntMod)
odds.n.ends(NorthStuntMod)
SouthStuntMod <-glm(ht4age ~ residence + education + wealth_index + employment + age_cat + sex, dhsAgeRec[which(dhsAgeRec$region=="South"),], family="binomial")
summary(SouthStuntMod)
odds.n.ends(SouthStuntMod)

#Wasting
NorthWastMod <-glm(wt4ht ~ residence + education + wealth_index + employment + age_cat + sex, dhsAgeRec[which(dhsAgeRec$region=="North"),], family="binomial")
summary(NorthWastMod)
odds.n.ends(NorthWastMod)
SouthWastMod <-glm(wt4ht ~ residence + education + wealth_index + employment + age_cat + sex, dhsAgeRec[which(dhsAgeRec$region=="South"),], family="binomial")
summary(SouthWastMod)
odds.n.ends(SouthWastMod)
