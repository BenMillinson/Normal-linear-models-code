library(tidyverse)
library(afex)
library(psyntur)
library(modelr)
options(scipen = 50)
options(digits = x) #for decimal places


#correlations----
m1.corr <- cor.test(df_x$variable_x, df_x$variable_y, method = "pearson")

#regressions----
m1.reg <- lm(df_x$variable_x ~ df_x$variable_y + x, data = df_x)
#or
lm(x ~ y, data = df_x)

#multiple regression----
lm(x ~ y + a + b + ..., data = df_x)
#where 'a + b + ...' are additional variables

#making predictions with regressions----
new_df <- tibble(variable_x = c(x), variable_y = c(y),...)
#or
new_df <- tibble(variable_x = x, variable_y = w,..., data = df_x)
predict(model_x, newdata = new_df)

#confidence intervals----
#in a t-dist with 25 df, 95% of the area under the curve lies between,
#which 2 values?
qt(0.975, df = 25, lower.tail = T)
qt(0.975, df = 25, lower.tail = F)

#confindence intervals from models----
confint(model_x)
confint(model_x, interval = 'confidence', level = .99) #to change confidence interval

#confidence intervals with new data----
predict(Model_x, newdata = new_df_x, interval = 'confidence', level = .95)

#dummy variables in lm----
M_x <- lm(variable_x ~ variable_y, data = df_x)
M_x
new_data <- tibble(variable_x = c("x", "y"))
add_predictions(new_data, M_x)
#e.g.
M_1 <- lm(trustworthy ~ rater_sex, data = df_faithfulfaces)
M_1
faith_new <- tibble(rater_sex = c("male", "female"))
add_predictions(faith_new, M_1)

#t.test----
t.test(variable_x ~ group_x, paired = FALSE/TRUE, data = df_x)
t.test(variable_x ~ group_x, var.equal = TRUE, data = df_x)

#test the non-null hypothesis that the true difference in the means of the two groups is -5:
t.test(variable_x ~ group_x, mu = -5, var.equal = TRUE, data = df_x)

#t-tests as a formula (the hard way)----
x_bar - y_bar / se
#where the standard error = 
sqrt(((n_x - 1) * s_x^2 + (n_y - 1) * s_y^2)/n_x + n_y -2) * sqrt(1/n_x + 1/n_y)

#area under a t-distribution curve----
#if variable x is is dist as a t-dist with 10 df, what is the p of it having a value,
#greater than 1.5?
#pt(1.5, df = 10, lower.tail = FALSE) (* 2) # for 2 tails
pt(t-value_x, df = x, lower.tail = FALSE) #one-tailed
pt(t-value_x, df = x, lower.tail = TRUE) * 2 # two-tailed

#finding the p-value for a null hypothesis (t-test)
pt(tvalue_x, df = n_x + n_y - 2, lower.tail = F) *2

#confidence intervals from a t-test----
t.test(score ~ group, confidence level = x, data = x)
#or
(xbar - ybar) - se * qt(0.975, df = n_x + n_y - 2)
(xbar - ybar) + se * qt(0.975, df = n_x + n_y - 2)

#e.g
#the t-stat for a null hypothesis in an ind samples t test(var.equal=T) whose total sample,
#size is 30 is 1.494. the difference of the means of the two groups is 3.719. whats the 95% CI?
#(xbar-ybar) + or - variable_x * se
indttest = xbar - ybar/se
3.719/1.494 = 2.48929 #(xbar-ybar) = 3.719, t= 1.494, therefore t = xbar-ybar/se, se = (xbar-ybar)/t
qt(.975, 28)
3.719 + 2.48929 * 2.048407
3.719 - 2.48929 * 2.048407
#99% CI
qt(.995, 28)
3.719 + 2.48929 * 2.763262
3.719 - 2.48929 * 2.763262

#anovas----
aov(variable_x ~ group_x, data = df_x)
anova(variable_x ~ group_x, data = df_x)

#varying intercept terms in anovas (ANCOVA)----
lm(variable_x ~ variable_y + variable_z, data = df_x)

#extracting coefficients
model_x$coefficients #or
coefficients(model_x) 

#varying intercept and slope terms in anova (interaction)
lm(variable_x ~ variable_y * variable_z, data = df_x)

#comparison models for null hypothesis tests----
anova(model_x, model_y)

#making predictions with anovas
insul_Ndata1 <- tibble(x = 'x', x = x)
predict(M_x, newdata = df_x)

#making predictions with beta's (the hard way) (catagorical variables):----
# Interpret the coefficients for each model
summary(M_x)$coefficients

#conf int prediction from data prediction----
predict(model_x, newdata = x, interval = 'confidence', level = .95)

#multilevel models (regression)----

#may need to use 'REML=FALSE' in some cases:

#inter-subject = random effects; subject (Intercept)
#intra-subject = random effects; residual

#use the prediction function the same way as previously shown

#where x1 are the same variables
#and subject is the grouping variable
M_x <- lmer(var_x ~ x1 + (x1|subject),
            data = df_x)
(1+DV|group) = #both random intercepts/slopes
  
  # random intercepts only----
M_x <- lmer(var_x ~ x1 + (1|Subject),
            data = df_x)
#putting 1 before the pipe makes random intercept

#random slopes only----
M3 <- lmer(var_x ~ x1 + (0 + x1|Subject),
           data = df_x)
(0+DV|group) = #random slopes
  
  #random intercepts and slopes----
M4 <- lmer(var_x ~ x1 + (1 + x1|Subject),
           data = df_x)
(1|group) = #Intercepts
  
  #multilevel model with correlation----
M_x <- lmer(var_x ~ x1 + (x1||subject),
            data = df_x)

#confidence intervals----
stdev <- x
mu <- y

c(qnorm(0.025, mean = mu, sd = stdev), # lower bound
  qnorm(0.975, mean = mu, sd = stdev)  # upper bound
)
#change to .005 and .995 for 99% CI




