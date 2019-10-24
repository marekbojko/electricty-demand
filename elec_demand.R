####################################################
##### Econometrics group project
###################################################


library(tidyverse)
library(haven)
library(foreign)
library(xtable)
# library(AER)
# library(plm)
# library(lattice)
# library(texreg)
# library(survey)
library(ggplot2)
# library(quantreg)
# library(corrplot)
# library(LARF)
library(Hmisc)
library(stargazer)
library(gridExtra)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(tables)
library(dplyr)
library(sp)
library(maptools)
library(RColorBrewer)
library(gplot)
library(survey)
library(plm)
library(leaps)
library(olsrr)
library(lmtest)
library(sandwich)
library(car)
library(leaps)
library(FactoMineR)
library(lme4)
library(survey)
library(jtools)
library(ggstance)
library(Hmisc)
library(PerformanceAnalytics)
library(corrplot)
library(PoEdata)
library(knitr)
library(printr)
library(effects)
library(broom)
library(pastecs)



options(prompt="R> ", digits=3, show.signif.stars=TRUE)
options(scipen = 999)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


######################################################################
# functions
######################################################################



# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}




######################################################################

# read in the data
elec_data <- read.csv("C:/Users/Marek/Documents/Econometrics group project/RECS05alldata.csv", header=TRUE, sep=",")


attach(elec_data)

# find number of NAs in the data
na_count <-sapply(elec_data, function(y) sum(length(which(is.na(y)))))


# some plots
plot(HHINCOME,KWH)
plot(log(HHINCOME),KWH)
plot(HHINCOME,log(KWH))
plot(log(HHINCOME),log(KWH))

par(mfrow=c(2,1))

boxplot(log(KWH)~log(HHINCOME))
boxplot(log(KWH)~HHINCOME)

par(mfrow=c(1,1))

boxplot(KWH~own)
hist(KWH)
boxplot(KWH)

hist(HHINCOME)
hist(TOTSQFT)
plot(KWH,TOTSQFT)

boxplot(TOTSQFT)

summary(DIVISION)

boxplot(HD65)
hist(HD65)
summary(HD65)
plot(HD65,log(KWH))

boxplot(CD65)
hist(CD65)
summary(CD65)
plot(CD65,log(KWH))


boxplot(TOTSQFT)
summary(log(TOTSQFT))

# treat for outliers????



#### Summary stats


### define dummy variables

# subset the data set to include only the specified variables
elec_data <- elec_data %>% select(KWH,CD65,CWASHER,DISHWASH,DIVISION,DRYER,FUELHEAT,HD65,HHINCOME,KOWNRENT,MICRO,NUMFRIG,STOVEN,TOPFRONT,
                                  TOTSQFT,URBRUR,YEARMADE)

#### create new dummy variables

# household owns the home
elec_data$own <- ifelse(KOWNRENT==1,1,0)

# the housing structure was built before 1970
elec_data$pre_1970 <- ifelse(YEARMADE < 5,1,0)

# the housing structure was built after 2000
elec_data$post_2000 <- ifelse(YEARMADE > 9,1,0)

# the housing structure is located in an urban area
elec_data$urban <- ifelse(URBRUR < 4,1,0)

# the household uses electricity as their heating fuel
elec_data$elec_heat <- ifelse(FUELHEAT==5,1,0)

# the shoudehold has a washing machine with top loading
elec_data$top_load <- ifelse(TOPFRONT==1,1,0)

attach(elec_data)


df.sum <- elec_data %>%
  summarise_all(funs(n=length, 
                     min = min,
                      q25 = quantile(., 0.25), 
                      median = median, 
                      q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd))

# the result is a wide data frame
dim(df.sum)

# reshape it using tidyr functions

df.stats.tidy <- df.sum %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, n, min, q25, median, q75, max, mean, sd) # reorder columns

df.stats.tidy

latex(df.stats.tidy, file="",digits=2)


##############################################
# rename variables so that they are self-explanatory
##############################################

colnames(elec_data) <- c("KWH","cooling_days", "clothes_washer", "dish_washer", "division", "dryer", "fuel_for_heating","heating_days", "household_income",
                         "ownership", "microwave", "n_fridges", "stove_oven", "top_front", "total_area", "urban_rural", "year_made", "own", "built_pre_1970", "built_post_2000", "urban", "electricity_heating",
                         "topload_washer")


###############################################

all_cov <- elec_data %>% select(KWH,household_income,heating_days,cooling_days,electricity_heating,own,built_pre_1970,built_post_2000,
                                urban,total_area,clothes_washer,dish_washer,dryer,microwave,n_fridges,stove_oven,topload_washer)



# transpose
t_tab_desc <- t(stat.desc(all_cov))

# get row and colnames in order
colnames(t_tab_desc) <- rownames(stat.desc(all_cov))
rownames(t_tab_desc) <- colnames(stat.desc(all_cov))

# select only variables of interest
t_tab_desc <- t_tab_desc[,c("mean","std.dev")]

# output to latex
latex(t_tab_desc, file="", digits=2)


###############################################
# correlation analysis
###############################################

chart.Correlation(elec_data, method = c("pearson", "kendall",
                                      "spearman"), histogram=TRUE, pch = 19)


corrplot(elec_data, type = "upper", order = "hclust", 
         tl.col = "black")

# compute correlation matrix
M<-cor(elec_data %>% select(KWH, household_income, heating_days, cooling_days, built_pre_1970, built_post_2000,urban,
                            own,electricity_heating, topload_washer, total_area, microwave , n_fridges, dish_washer,
                            clothes_washer, dryer, division))


# matrix of the p-value of the correlation
p.matrix <- cor.mtest(elec_data %>% select(KWH, household_income, heating_days, cooling_days, built_pre_1970, built_post_2000,urban,
                                        own,electricity_heating, topload_washer, total_area, microwave , n_fridges, dish_washer,
                                        clothes_washer, dryer, division))$p



col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         #addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=55, #Text label color and rotation
         # Combine with significance
         p.mat = p.matrix, sig.level = 0.05, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5
)

stargazer(M, title="Correlation Matrix")


############################################################################################
##### variable selection models
############################################################################################


########################
# Regressions
########################


### create the model

# create the full model
model_full <- lm(log(KWH) ~ factor(DIVISION) + HD65 + CD65 + log(HHINCOME) + own + pre_1970 + post_2000 + urban + log(TOTSQFT) + I(CD65^2) 
                 + I(HD65^2) + STOVEN + MICRO +
            NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load, data = elec_data)
summary(model_full)
coeftest(model_full, vcov = vcovHC(model_full, type="HC1"))




############################
# create model1 - base model
model_0 <- lm(log(KWH) ~ log(household_income) + electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area), data = elec_data)
summary(model_0)
se_0 <- coeftest(model_0, vcov = vcovHC(model_0, type="HC1"))


# create model1 - base model
model_1 <- lm(log(KWH) ~ log(household_income) + heating_days + cooling_days + electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area), data = elec_data)
summary(model_1)
se_1 <- coeftest(model_1, vcov = vcovHC(model_1, type="HC1"))



# create model2 - control for divisions
model_2 <- lm(log(KWH) ~ log(household_income) + heating_days + cooling_days + electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area) + factor(division), data = elec_data)
summary(model_2)
se_2 <- coeftest(model_2, vcov = vcovHC(model_2, type="HC1"))


# do a Wald test
waldtest(model_1,model_2)


# create model3 - control for divisions + functional forms
model_3 <- lm(log(KWH) ~ log(household_income) + heating_days + cooling_days + 
                electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area) + factor(division) + 
                I(heating_days^2)  + I(cooling_days^2), data = elec_data)
summary(model_3)
se_3 <- coeftest(model_3, vcov = vcovHC(model_3, type="HC1"))

# do a wald test
waldtest(model_2,model_3)

# check joint significance of the cooling terms
linearHypothesis(model_3, 
                 c("cooling_days","I(cooling_days^2)"),
                 vcov. = vcovHC, type = "HC1")


# create model4 - control for divisions + functional forms + interaction terms
model_4 <- lm(log(KWH) ~ log(household_income) + heating_days + cooling_days + 
                electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area) + factor(division) + 
                I(heating_days^2) + electricity_heating:I(heating_days^2) + 
                log(total_area):built_pre_1970 + electricity_heating:built_pre_1970, data = elec_data)
summary(model_4)
se_4 <- coeftest(model_4, vcov = vcovHC(model_4, type="HC1"))


linearHypothesis(model_4,c("electricity_heating:I(heating_days^2)=0","electricity_heating:built_pre_1970"))

# create model5 - control for divisions + add appliances + interaction terms
model_5 <- lm(log(KWH) ~ log(household_income) + heating_days + cooling_days + 
                electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area) + factor(division) + 
                I(heating_days^2) + clothes_washer + dish_washer + dryer + microwave + n_fridges + stove_oven 
              + topload_washer, data = elec_data)
summary(model_5)
se_5 <- coeftest(model_5, vcov = vcovHC(model_5, type="HC1"))


# create model6 - control for divisions + add appliances + interaction term
model_6 <- lm(log(KWH) ~ log(household_income) + heating_days + cooling_days + 
                electricity_heating + own + built_pre_1970 + built_post_2000 + urban + log(total_area) + factor(division) + 
                I(heating_days^2)  + clothes_washer + dish_washer + dryer + microwave + n_fridges + stove_oven +
                topload_washer + electricity_heating:I(heating_days^2) + log(total_area):built_pre_1970 + elec_heat:built_pre_1970,data = elec_data)
summary(model_6)
se_6 <- coeftest(model_6, vcov = vcovHC(model_6, type="HC1"))


# residual diagnostics
ols_plot_resid_hist(model_6)
ols_test_normality(model_1)
ols_plot_resid_qq(model_6)
ols_plot_resid_fit(model_6)

# Heteroskedasticity
bptest(model_6)

# Variance inflation factor
vif_test <- vif(model_6)

# outlier test
outlierTest(model_6)

# 

latex(vif_test[,1],file="",digits = 2)

###############################################

  # gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(model_0, type = "HC1"))),
               sqrt(diag(vcovHC(model_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_2, type = "HC1"))),
               sqrt(diag(vcovHC(model_3, type = "HC1"))),
               sqrt(diag(vcovHC(model_4, type = "HC1"))),
               sqrt(diag(vcovHC(model_5, type = "HC1"))),
               sqrt(diag(vcovHC(model_6, type = "HC1"))))


rob_se_1 <- list(sqrt(diag(vcovHC(model_0, type = "HC1"))),
               sqrt(diag(vcovHC(model_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_2, type = "HC1"))),
               sqrt(diag(vcovHC(model_3, type = "HC1"))),
               sqrt(diag(vcovHC(model_4, type = "HC1"))))


rob_se_2 <- list(sqrt(diag(vcovHC(model_5, type = "HC1"))),
               sqrt(diag(vcovHC(model_6, type = "HC1"))))

# generate a LaTeX table of regression outputs
stargazer(model_0, 
          model_1, 
          model_2, 
          model_3, 
          model_4, 
          model_5, 
          model_6,
          digits = 3,
          star.char = c("c", "b", "a"),
          dep.var.caption = "Dependent Variable: natural logarithm of kilowatthours of electricity consumption",
          se = rob_se,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          omit="division",
          column.sep.width = "1pt",
          no.space = FALSE)


# just adjusted baseline model
stargazer(model_0, 
          model_1, 
          model_2, 
          model_3, 
          model_4, 
          digits = 3,
          star.char = c("c", "b", "a"),
          dep.var.caption = "Dependent Variable: natural logarithm of kilowatthours of electricity consumption",
          se = rob_se_1,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)"),
          omit="division",
          column.sep.width = "1pt",
          no.space = FALSE)


# robustness to appliences
stargazer(model_5, 
          model_6, 
          digits = 3,
          star.char = c("c", "b", "a"),
          dep.var.caption = "Dependent Variable: natural logarithm of kilowatthours of electricity consumption",
          se = rob_se_2,
          column.labels = c("(1)", "(2)"),
          omit="division",
          column.sep.width = "1pt",
          no.space = FALSE)


plot_summs(model_0, model_1, model_2, model_3, model_4, model_5, model_6, scale = TRUE, robust = TRUE,
           model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
           coefs = c("log(household_income)","heating_days","cooling_days","electricity_heating","own",
                     "built_pre_1970","built_post_2000","urban", "log(total_area)"))


plot_summs(model_2,model_3,model_5, scale = TRUE, robust = TRUE,
           model.names = c("(3)","(4)", "(6)"),
           coefs = c("log(household_income)","heating_days","cooling_days","electricity_heating","own",
                     "built_pre_1970","built_post_2000","urban", "log(total_area)"))


stargazer(model_6, title="Regression Results",
          dep.var.labels=c("log(KWH"),
          omit="division", ci=TRUE, ci.level=0.95, single.row=TRUE)



########################
# stepwise forward selection
########################

selectedMod <- step(model_full)
summary(selectedMod)
plot(selectedMod)

# Find Variance Inflation Factor
all_vifs <- vif(selectedMod)
print(all_vifs)

######
# Multicollinearity and Statistical Significance
######

#### Recursively remove variables with VIF (Variance Inflation Factor) > 4

signif_all <- rownames(all_vifs)

# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("log(KWH) ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=elec_data)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}
summary(selectedMod)
coeftest(selectedMod, vcov = vcovHC(model, type="HC1"))

#### Recursively remove non-significant variables

all_vars <- names(selectedMod[[1]])[-1]  # names of all X variables
# Get the non-significant vars
summ <- summary(selectedMod)  # model summary
pvals <- summ[[4]][, 4]  # get all p values
not_significant <- character()  # init variables that aren't statsitically significant
not_significant <- names(which(pvals > 0.1))
not_significant <- not_significant[!not_significant %in% "(Intercept)"]  # remove 'intercept'. Optional!

# If there are any non-significant variables, 
while(length(not_significant) > 0){
  all_vars <- all_vars[!all_vars %in% not_significant[1]]
  myForm <- as.formula(paste("log(KWH) ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=elec_data)  # re-build model with new formula
  
  # Get the non-significant vars.
  summ <- summary(selectedMod)
  pvals <- summ[[4]][, 4]
  not_significant <- character()
  not_significant <- names(which(pvals > 0.1))
  not_significant <- not_significant[!not_significant %in% "(Intercept)"]
}
summary(selectedMod)



####################################
# best subsets regression
####################################

response_df <- log(KWH)  # Y variable
predictors_df <- data.frame(factor(DIVISION),HD65,I(HD65^2),CD65,I(CD65^2),log(HHINCOME),own, 
                            pre_1970,post_2000,urban,TOTSQFT,STOVEN,MICRO,
                            factor(NUMFRIG),DISHWASH,CWASHER,DRYER,elec_heat, 
                            I(elec_heat*HD65),top_load)  # X variables

for (pred in predictors_df){
  print (length(pred))
}

regsubsetsObj <- regsubsets(x=predictors_df ,y=response_df, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")  # regsubsets plot based on R-sq


########## leaps

leapSet <- leaps(x=predictors_df, y=response_df, nbest = 1, method = "adjr2")


########## best subsets
best_subset <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                            NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load,elec_data,nvmax=28)
summary(best_subset)


# forward step-wise
forward <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                        NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load,elec_data,nvmax=27, method = "forward")
summary(forward)


# backward step-wise
backward <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                         NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load,elec_data,nvmax=27, method = "backward")
summary(backward)



### Comparing models

# create training - testing data
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(elec_data), replace = T, prob = c(0.6,0.4))
train <- elec_data[sample, ]
test <- elec_data[!sample, ]


# perform best subset selection
best_subset <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                            NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load, train, nvmax = 27)
results <- summary(best_subset)


# extract and plot results
tibble(predictors = 1:27,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")


# best models by different criteria
which.max(results$adjr2)
which.min(results$bic)
which.min(results$cp)

# compare the variables and coefficients that these models include
coef(best_subset, 23)
coef(best_subset, 18)
coef(best_subset, 21)


## directly estimating test error

# build model matrix
test_m <- model.matrix(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                         NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load,data=test)


# create empty vector to fill with error values
validation_errors <- vector("double", length = 27)


for(i in 1:27) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict electricity usage using matrix algebra
  validation_errors[i] <- mean((log(test$KWH) - pred_x)^2)  # compute test error btwn actual & predicted electricity usage
}

# plot validation errors
plot(validation_errors, type = "b")


############ resample

# create training - testing data
set.seed(5)
sample <- sample(c(TRUE, FALSE), nrow(elec_data), replace = T, prob = c(0.6,0.4))
train <- elec_data[sample, ]
test <- elec_data[!sample, ]

# perform best subset selection
best_subset <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                            NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load, train, nvmax = 27)


# build model matrix
test_m <- model.matrix(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                         NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load,data=test)


# create empty vector to fill with error values
validation_errors <- vector("double", length = 27)


for(i in 1:27) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict electricity usage using matrix algebra
  validation_errors[i] <- mean((log(test$KWH) - pred_x)^2)  # compute test error btwn actual & predicted electricity usage
}

# plot validation errors
plot(validation_errors, type = "b")

################


# create a function for test errors
predict.regsubsets <- function(object, newdata, id ,...) {
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}



# choose among the models of different sizes using k-fold cross-validation
# create a vector that allocates each observation to one of k = 10 folds
# create a matrix in which we will store the results

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(elec_data), replace = TRUE)
cv_errors <- matrix(NA, k, 27, dimnames = list(NULL, paste(1:27)))

# perform cross-validation
# make our predictions for each model size
# compute the test errors on the appropriate subset
# store them in the appropriate slot in the matrix 

for(j in 1:k) {
  
  # perform best subset on rows not equal to j
  best_subset <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                              NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load, elec_data[folds != j, ], nvmax = 27)
  
  # perform cross-validation
  for( i in 1:27) {
    pred_x <- predict.regsubsets(best_subset, elec_data[folds == j, ], id = i)
    cv_errors[j, i] <- mean((log(elec_data$KWH[folds == j]) - pred_x)^2)
  }
}

# average over the columns of this matrix in order to obtain a vector for which the jth element is the cross-validation error for the j-variable model

mean_cv_errors <- colMeans(cv_errors)

plot(mean_cv_errors, type = "b")

which.min(mean_cv_errors)


# perform best subset selection on the full data set in order to obtain the 25-variable model
final_best <- regsubsets(log(KWH) ~ factor(DIVISION) + HD65 + I(HD65^2) + CD65 + I(CD65^2) + log(HHINCOME) + own + pre_1970 + post_2000 + urban + TOTSQFT + STOVEN + MICRO +
                           NUMFRIG + DISHWASH + CWASHER + DRYER + elec_heat + I(elec_heat*HD65) + top_load, data = elec_data , nvmax = 25)
coef(final_best, 25)

summary(final_best)
