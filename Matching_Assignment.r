#!/usr/bin/env python
# coding: utf-8

# ### CAUSAL INFERENCE ASSIGNMENT

# # **Question 1/5:  DEBUGGING**
# 
# *(1) Debugging--in the 3 cases below (A through C), identify the major 	coding error in each case and explain how to fix it, in 1-2 sentences. DO NOT actually copy/paste corrected code:*
# 
# 
# 
# 

# 
# 
# ```
# #### Part A
# treat = ...
# #X = ...
# 
# genout <- GenMatch(Tr=treat, X=X)
# 
# summary(genout)
# 
# mb <- MatchBalance(treat~age +educ+black+ hisp+ married+ nodegr+ u74 + 
#                             +u75 + re75+ re74 + I(re74*re75) + re78, 
#                             match.out=genout, nboots=500)
# 
# mout <- Match(Y=Y, Tr=treat, X=X, Weight.matrix=genout)
# 
# ```
# 
# 

# **Answer**: 
# The mistake is at match.out=genout, because we are passing on the optimal weights 
# rather than the matched variables. It should be match.out=mout.

# ```
# #### Part B
# #X=...
# #treat=...
# 
# genout <- GenMatch(Tr=treat, X=X, estimand="ATT", caliper = 0.25,
#                    pop.size=16, max.generations=10, wait.generations=1)
# 
# mout <- Match(Y=Y, Tr=treat, X=X, Weight.matrix=genout)
# 
# summary(mout)
# 
# mb <- MatchBalance(treat~age +educ+black+ hisp+ married+ nodegr
#                     +u74+ u75+ re75+ re74+ I(re74*re75), 
#                     match.out=mout, nboots=500)
# ```

# **Answer:** The problem is that the arguments passed in GenMatch are not identical with the arguments passes on Match. To solve this, we pass arguments in Match accordingly with GenMatch.
# 
# ---
# 
# 
# 

# ```
# #### Part C
# #X=...
# #treat=...
# 
# genout <- GenMatch(Tr=treat, X=X)
# 
# Y=re78
# 
# mout <- Match(Y=Y, Tr=treat, X=X, Weight.matrix=genout)
# 
# summary(mout)
# 
# mb <- MatchBalance(treat ~ age+educ+black+ hisp+ married+ 
#                 nodegr+ u74+ u75+ re75+ re74+ I(re74*re75)+re78,
#                 match.out=mout, nboots=500)
# ```
#                 
# 

# **Answer:**  The issue with this code is that one of the variables in MatchBalance is the outcome variable (re78). MatchBalance should only have the covariates.

# #**Questions 2/5: REPLICATION**
# Question 2-5 below require the peacekeeping data set that we worked on in class, as well as this codebook (see the appendix):**
# http://web.worldbank.org/archive/website01241/WEB/IMAGES/INTERNAT.PDF
# 
# **The class breakout instructions (including data download code) are here:**
# https://gist.github.com/diamonaj/3795bfc2e6349d00aa0ccfe14102858d
# 

# (2) Replicate the analysis done to produce figure 8 in https://gking.harvard.edu/files/counterf.pdf  -- EXCEPT instead of the original interaction term, add two other interaction terms: (exp*untype4) and (wardur*logcost). 
# 
# Show the marginal effects of UN peacekeeping as a function of war duration, as in the original figure.
# 
# 
# 
# 
# Your answer should include your properly-labelled figure and answers to the following questions:
# 
# 1.   What do the variables used in the two axes mean? 
# 2.   What claim was supported by showing Figure 8 in the article? [Max. 2 sentences]
# 3. Is this claim also supported by your figure? Explain using specific details of your figure [Max. 3 sentences]
# 
# A few suggestions:
# 
# Read the class breakout instructions above to get the data and relevant columns;
# 	
# If you are not clear on the model, read the relevant sections of the paper and focus on understanding Table 2;
# 	
# To plot the figure, you should use a strategy similar to the one we used in the statistics scavenger hunt, which was also used in a previous assignment (e.g., holding predictors at their means and looping through values of one variable to obtain treatment effects at different levels of the variable--you may want to review the answer key for that previous assignment.
# 

# In[ ]:


install.packages ("Matching")


# In[ ]:


library(Matching)


# In[ ]:


foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")


# In[ ]:


foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 108)]


# In[ ]:


foo <- foo[c(-19, -47), ]


# In[ ]:


which(is.na(foo)==TRUE)


# In[ ]:


head(foo)


# In[ ]:


glm1 <- glm(pbs2s3 ~ wartype + logdead + wardur + factnum + factnum2 + 
            trnsfcap + develop + exp + decade + treaty + untype4, 
            data = foo, family = binomial)


# In[ ]:


glm2 <- glm(pbs2s3 ~ wartype + logdead + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4 +
              untype4:exp + wardur:logcost, data = foo, family = binomial)


# In[ ]:


glm3 <- glm(pbs2s3 ~ wartype + logdead + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4 +
              + untype4:wardur, data = foo, family = binomial)


# In[ ]:


# Predictors at their means
mean.wartype <- mean(foo$wartype)
mean.logdead <- mean(foo$logdead)
mean.logcost <- mean(foo$logcost)
mean.wardur <- mean (foo$wardur)
mean.factnum <- mean(foo$factnum)
mean.factnum2 <- mean(foo$factnum2)
mean.trnsfcap <- mean(foo$trnsfcap)
mean.develop <- mean(foo$develop)
mean.exp <- mean(foo$exp)
mean.decade <- mean(foo$decade)
mean.treaty <- mean(foo$treaty)


# In[ ]:


get_logit <- function (X, coef) {
    logit <- coef[1] + sum(coef[2:length(coef)]*X)
    return (exp(logit)/(1+exp(logit)))
}
storage.original.treat <- rep(NA, 315)
storage.original.control <- rep(NA, 315)


# In[ ]:


# For each war duration
for (wardur in 1:315) {
  
  # Hypothetical unit with predictors held at their means, for both
  # treatment and control group, and varying wardur.
  X.treat <- c(mean.wartype,  mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty,1)
  X.control <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0)
  
  storage.original.treat[wardur]  <- get_logit(X.treat, coef(glm1))
  storage.original.control[wardur]  <- get_logit(X.control, coef(glm1))
}


# In[ ]:


# Marginal treatment effect is y_treat - y_control
original_y <- storage.original.treat - storage.original.control

# Same process, but for logdead model
storage.gary.treat <- rep(NA, 315)
storage.gary.control <- rep(NA, 315)
for (wardur in 1:315) {
  gary.treat <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty,1,  1*wardur)
  gary.control <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0, 0*wardur)
  storage.gary.treat[wardur]  <- get_logit(gary.treat, coef(glm3))
  storage.gary.control[wardur]  <- get_logit(gary.control, coef(glm3))
}
gary_y <- storage.gary.treat - storage.gary.control


# In[ ]:


# Marginal treatment effect is y_treat - y_control
logdead_y <- storage.original.treat - storage.original.control

# Same process, but for logdead model
storage.logdead.treat <- rep(NA, 315)
storage.logdead.control <- rep(NA, 315)
for (wardur in 1:315) {
  X.treat <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1, 1*mean.exp, wardur*mean.logcost)
  X.control <- c(mean.wartype,  mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty ,0,  0*mean.exp, wardur*mean.logcost)
  storage.logdead.treat[wardur]  <- get_logit(X.treat, coef(glm2))
  storage.logdead.control[wardur]  <- get_logit(X.control, coef(glm2))
}
logdead_y <- storage.logdead.treat - storage.logdead.control


# In[ ]:


plot(1:315, original_y, type = "l", main="Replication of Figure 8 from Gary King", ylim = c(0, 0.8), xlab="War Duration (in weeks)", ylab="Marginal Effect")
lines(1:315, logdead_y, col = "red", ylim = c(0, 0.8))
lines(1:315, gary_y, col = "blue", ylim = c(0, 0.8))
legend("bottomright", legend=c("The Original Model", "Gary King Model", "Model with two interaction terms"), col=c("black", "blue", "red"), lty=1, box.lty=0)


# 1. The x-axis is War Duration expressed in months, while the y-axis shows the marginal effect of UN interventions (treat).
# 2. The original model shows that for the wars that have short duration, the UN intervention has a high marginal effect. Gary King's model shows that for very short wars there is no effect, but as time increases, it shows the same results as the original model.
# 3. The model I created shows that as the time increases, the UN intervention effect has a smaller marginal effect. We can see this from the slope going downwards. Our model would lead to the same decision as the original model (intervene in short wars), but Gary King's model shows that UN should intervene after a specific amount of time.

# # **Question 3/5: DEFINE TREATMENT**
# 
# *(3) Define treatment as below:*
# ```
# Tr <- rep(0, length(foo$uncint))
# Tr[which(foo$uncint != "None"] <- 1
# ```
# 
# *What does this mean? What is "treatment"?*
# 

# **Answer:**

# ***uncint*** variable stands for *type of UN Intervention* (page 36, handbook). In this case, we are filtering all the observations that have experienced some sort of UN Intervention to be considered as treated, while the treatment is UN Intervention, and the control group is the group that did not receive any UN intervention (None).

# # **Question 4/5: CAUSAL QUESTION, SUTVA AND PROPENSITY SCORES**

# Let's pretend you work for an NGO and your manager asks you to 	estimate the impact of the treatment identified above on lenient peacebuilding success 2 years and 5 years after the war. You will have to search for these two outcomes variables in the codebook.
# 

# *(a) In no more than 1 sentence, articulate the causal question as best you can (being as clear as you can about treatment and control):*
# 

# **Answer:**
# 
# What is the effect of the intrusive UN Intervention (treatment) in the peacebuilding success 2 years (PBS2L) and 5 years (PBS2L) after the war, compared to the lack of the intrusive UN intervention (control)?

# *(b) In no more than 1 sentence, explain how/why SUTVA might be violated here. In no more than 1 additional sentence, explain how you could in theory use the "restrict" argument (in Match()/GenMatch()) to help address this potential problem.*

# **Answer:**
# 
# SUTVA may be violated if the treatment (UN peacebuilding intervention) applied to one country affects the outcome of the neighbouring countries, due to geographical and circumstantial factors.
# 
# We can use restrict() in Match/GenMatch to assure that neighbouring countries do not get matched together (as treatment and control), because, otherwise SUTVA would be violated.

# *(c) Use simple logistic regression, propensity score matching, and genetic matching to try to answer these questions.*
# 

# For the matching exercises, measure balance on AT LEAST the basic variables we considered in the class exercise. In addition, at least one genetic matching attempt should incorporate propensity scores.
# 
# For the genetic matching exercise, population size should be at least 200 and you should run it for at least 25 generations (which may require you to modify the number of non-changing generations). When performing genetic matching, take a little time to try different approaches to producing excellent balance. You can tweak the values of "M", you can do caliper matching, you can match on quadratic and/or interaction terms, you can attempt exact matching, etc.
# 
# JUST ONE WORD OF ADVICE: The precise way you run GenMatch is how you have to run Match. For example, if you run GenMatch with M = 2 and X includes interaction terms etc., then in the next line of code you have to run Match exactly the same way (using the GenMatch output as the weight.matrix). Then in the next line you run MatchBalance, using the Match output.
# 
# Match with replacement and allow ties. ATT makes sense here. Ideally, you would measure/optimize balance on the interaction terms and quadratic terms as well (but this will make things a bit harder than simply balancing on the basic variables). 
# 
# 
# Your final answer should include:
# 
# (i) a table like this one--the caption below the table should include the asterisked footnote AS WELL AS **the functional forms of the propensity score model, **the variables you've genetically matched on, and **the MatchBalance variables used for genetic matching:
# 
# ******TABLE FORMAT******* (Please give it a title)
# 				Tmt effect    Tmt effect       p-value
#                       estimate     std error   (from MatchBalance)        
# 
# logistic regression
# len success 2 years 		                
# len success 5 years 		                
# 
# p- score matching	  	
# len success 2 years 	 	*
# len success 5 years 		*
# 
# gen match
# len success 2 years 		*
# len success 5 years 		*
#   
# gen match with propensity scores
# len success 2 years 		*
# len success 5 years 		*
# 
# *Only provide a treatment effect for matching results if your leximin p-value is above 0.10. Otherwise write in "NA".
# 
# (ii) A short discussion of your matching strategy and attempts. What were some unsuccessful attempts? How did you go about choosing covariates? [Max. 5 sentences]
# 

# In[ ]:


if (!require("devtools")) install.packages("devtools")
devtools::install_github("JasjeetSekhon/rgenoud", force= TRUE)


# In[ ]:


# Reload the data
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[c(-19, -47), ]
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != "None")] <- 1
foo$Tr <- Tr
library(Matching)

# Balance before matching (for the logistic regression model)
mb1 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, nboots=500)

# TWo years
glm4 <- glm(pbs2l ~ wartype + logdead + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + Tr, 
            data = foo, family = binomial)
summary(glm4)

# Five years
summary(foo$pbs5l)      # Check for NAs
NAs <- is.na(foo$pbs5l) # NA indices
glm5 <- glm(pbs5l ~ wartype + logdead + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + Tr, 
            data = foo[!NAs,], family = binomial)
summary(glm5)

# For this question, it is sufficient to simply input the coefficient
# in the table. However, this is *not* the treatment effect because
# of the logistic function. If you were to actually calculate the treatment
# effect in this model, you would have to pose some additional assumptions.
# For example, you could consider the treatment effect the average of
# unit-level different in outcomes, where you use the model to simulate
# the counterfactual. Again, this was not necessary but is the technically
# correct response.



# In[ ]:


# Two years
foo.counter_factual <- foo
foo.counter_factual$Tr <- rep(1, nrow(foo)) - foo$Tr
counter.factuals <- predict(glm4, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo))

mask <- foo$Tr == 1
unit_treat_effects[mask] <- glm4$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - glm4$fitted.values[!mask]
mean(unit_treat_effects)


# In[ ]:


# Five years
foo.counter_factual <- foo[!NAs,]
foo.counter_factual$Tr <- 1 - foo$Tr[!NAs]
counter.factuals <- predict(glm5, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo[!NAs,]))

mask <- foo[!NAs,]$Tr == 1
unit_treat_effects[mask] <- glm5$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - glm5$fitted.values[!mask]
mean(unit_treat_effects)


# In[ ]:


### Propensity Score Matching

glm6 <- glm(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty, data = foo, family = binomial)

X <- glm6$fitted.values
Y1 <- foo$pbs2l
Y2 <- foo$pbs5l
m1  <- Match(Y=Y1, Tr=Tr, X=X, M=1, BiasAdjust = T)
summary(m1)


# In[ ]:


m1$est.noadj
mb2 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m1, nboots=500)


# In[ ]:


### Genetic Matching with propensity scores

attach(foo)
X = cbind(glm5$fitted.values, wartype, logdead, foo$wardur, factnum, 
          factnum2, trnsfcap, develop, exp, decade, treaty)
detach(foo)

genout <- GenMatch(Tr=Tr, X=X, M=1,
                   pop.size=200, max.generations=10, wait.generations=25)
m3  <- Match(Y=Y1, Tr=Tr, X=X, M=1, BiasAdjust = T, Weight.matrix = genout)
summary(m3)
m3$est.noadj
mb4 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m3, nboots=500)

genout1 <- GenMatch(Tr=Tr[mask], X=X[mask,], M=1,
                    pop.size=200, max.generations=10, wait.generations=25)
m4  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask,], M=1, BiasAdjust = T, Weight.matrix = genout1)
summary(m4)
m4$est.noadj
mb5 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m3, nboots=500)


# In[ ]:


mask <- which(!is.na(Y2))
m2  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask], M=1, BiasAdjust = T)
summary(m2)
m2$est.noadj
mb3 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m2, nboots=500)


# In[ ]:


### Genetic Matching 

attach(foo)
X = cbind(wartype, logdead, foo$wardur, factnum, 
          factnum2, trnsfcap, develop, exp, decade, treaty, exp*logdead)
detach(foo)

genout <- GenMatch(Tr=Tr, X=X, M=1,
                   pop.size=200, max.generations=10, wait.generations=25)
m3  <- Match(Y=Y1, Tr=Tr, X=X, M=1, BiasAdjust = T, Weight.matrix = genout)
summary(m3)
m3$est.noadj
mb4 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m3, nboots=500)

genout1 <- GenMatch(Tr=Tr[mask], X=X[mask,], M=1,
                    pop.size=200, max.generations=10, wait.generations=25)
m4  <- Match(Y=Y2[mask], Tr=Tr[mask], X=X[mask,], M=1, BiasAdjust = T, Weight.matrix = genout1)
summary(m4)
m4$est.noadj
mb5 <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty, data = foo, 
                    match.out = m3, nboots=500)


# In[ ]:




