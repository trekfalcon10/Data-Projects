## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

#View structure of dataset and key variables
str(NH11)
str(NH11$everwrk)
levels(NH11$everwrk)

str(NH11$r_maritl)
levels(NH11$r_maritl)

str(NH11$age_p)

# Remove unused values
NH11 <- transform(NH11,
                  everwrk = factor(everwrk,
                                   levels = c("1 Yes", "2 No")),
                  r_maritl = droplevels(r_maritl))

NH11$r_maritl <- na.omit(NH11$r_maritl)

#Create regression model
evrwrk_mod <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
coef(summary(evrwrk_mod))

#View non-log coefficients
evrwrk_mod.cf <- coef(summary(evrwrk_mod))
evrwrk_mod.cf[, "Estimate"] <- exp(coef(evrwrk_mod))
evrwrk_mod.cf

#Make prediction

# Create a dataset with predictors set at desired levels, removing NA's

predDat1 <- with(NH11,
                 expand.grid(age_p = age_p[1:length(age_p)],
                             r_maritl = na.omit(c( "1 Married - spouse in household", 
                                                   "2 Married - spouse not in household", 
                                                   "4 Widowed", "5 Divorced", "6 Separated", 
                                                   "7 Never married", "8 Living with partner"))))

# predict everwork at those levels
predWrk <- cbind(predDat1, predict(evrwrk_mod, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat1))
predWrk

#Get average probabilities for each marital level individually

#Subset prediction data frame
predMar1 <- subset(predWrk, predWrk$r_maritl == "1 Married - spouse in household")


predMar2 <- subset(predWrk, predWrk$r_maritl == "2 Married - spouse not in household")


predMar4 <- subset(predWrk, predWrk$r_maritl == "4 Widowed")


predMar5 <- subset(predWrk, predWrk$r_maritl == "5 Divorced")


predMar6 <- subset(predWrk, predWrk$r_maritl == "6 Separated")


predMar7 <- subset(predWrk, predWrk$r_maritl == "7 Never married")


predMar8 <- subset(predWrk, predWrk$r_maritl == "8 Living with partner")

#Get mean of each prediction for each category (excludes Unknown marital status)
mean(predMar1$fit)
mean(predMar2$fit)
mean(predMar4$fit)
mean(predMar5$fit)
mean(predMar6$fit)
mean(predMar7$fit)
mean(predMar8$fit)

#Show fit for each category with lower and upper bounds (includes Unknown marital status)
library(effects)
data.frame(Effect("r_maritl", evrwrk_mod))