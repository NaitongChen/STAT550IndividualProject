# load packages
library(nlme)
library(gee)
library(corrplot)
library(lattice)
library(lmtest)
library(mitml)

# load data
dat = read.csv("data_dep.txt", TRUE, " ")

# removing variables not in the description
# the remaining variables are:
# subject #, treatment, gender, education, month, GSI
dat = dat[,c(1,2,3,5,10,20)]

# make SN, treatment and gender factors
dat[,2] = as.factor(dat[,2])
dat[,3] = as.factor(dat[,3])

# scale GSI by 10
dat[,6] = 10 * dat[,6]

dat = as.data.frame(dat)
colnames(dat)[2] = "treatment"
colnames(dat)[3] = "gender"
colnames(dat)[4] = "education"

# EDA

# summary statistics
# treatment
print(length(which(dat$treatment == 1)) / length(dat$SN))
print(length(which(dat$treatment == 2)) / length(dat$SN))
# gender (ignore missing)
print(length(which(dat$gender == 1)) / (length(which(dat$gender == 1)) + 
                                          length(which(dat$gender == 2))))
print(length(which(dat$gender == 2)) / (length(which(dat$gender == 1)) + 
                                          length(which(dat$gender == 2))))
# education
print(mean(dat$education, na.rm = T))
print(sd(dat$education, na.rm = T))
# GSI
# month = 0
print(mean(dat$GSI[which(dat$month == 0)], na.rm = T))
print(sd(dat$GSI[which(dat$month == 0)], na.rm = T))
# month = 3
print(mean(dat$GSI[which(dat$month == 3)], na.rm = T))
print(sd(dat$GSI[which(dat$month == 3)], na.rm = T))
# month = 6
print(mean(dat$GSI[which(dat$month == 6)], na.rm = T))
print(sd(dat$GSI[which(dat$month == 6)], na.rm = T))
# month = 18
print(mean(dat$GSI[which(dat$month == 18)], na.rm = T))
print(sd(dat$GSI[which(dat$month == 18)], na.rm = T))
# month = 60
print(mean(dat$GSI[which(dat$month == 60)], na.rm = T))
print(sd(dat$GSI[which(dat$month == 60)], na.rm = T))

# missing rates
# gender
print(sum(is.na(dat[,3]))/5)
print((sum(is.na(dat[,3]))/5)/(length(dat[,1])/5))
# education
print(sum(is.na(dat[,4]))/5)
print((sum(is.na(dat[,4]))/5)/(length(dat[,1])/5))
# GSI
# month = 0
print(sum(is.na(dat$GSI[which(dat$month == 0)])))
print(sum(is.na(dat$GSI[which(dat$month == 0)]))/(length(dat[,1])/5))
# month = 3
print(sum(is.na(dat$GSI[which(dat$month == 3)])))
print(sum(is.na(dat$GSI[which(dat$month == 3)]))/(length(dat[,1])/5))
# month = 6
print(sum(is.na(dat$GSI[which(dat$month == 6)])))
print(sum(is.na(dat$GSI[which(dat$month == 6)]))/(length(dat[,1])/5))
# month = 18
print(sum(is.na(dat$GSI[which(dat$month == 18)])))
print(sum(is.na(dat$GSI[which(dat$month == 18)]))/(length(dat[,1])/5))
# month = 60
print(sum(is.na(dat$GSI[which(dat$month == 60)])))
print(sum(is.na(dat$GSI[which(dat$month == 60)]))/(length(dat[,1])/5))

# boxplots over time
dat_control = dat[which(dat$treatment == 2),]
boxplot(GSI ~ month, dat_control)
summary(aov(GSI ~ month, dat_control))
kruskal.test(GSI ~ month, dat_control)$p.value

dat_trt = dat[which(dat$treatment == 1),]
boxplot(GSI ~ month, dat_trt)
summary(aov(GSI ~ month, dat_trt))
kruskal.test(GSI ~ month, dat_trt)$p.value

# boxplots between groups
dat_t1 = dat[which(dat$month == 0),]
boxplot(GSI ~ treatment, dat_t1)
t.test(GSI ~ treatment, dat_t1)
wilcox.test(GSI ~ treatment, dat_t1)

dat_t2 = dat[which(dat$month == 3),]
boxplot(GSI ~ treatment, dat_t2)
t.test(GSI ~ treatment, dat_t2)
wilcox.test(GSI ~ treatment, dat_t2)

dat_t3 = dat[which(dat$month == 6),]
boxplot(GSI ~ treatment, dat_t3)
t.test(GSI ~ treatment, dat_t3)
wilcox.test(GSI ~ treatment, dat_t3)

dat_t4 = dat[which(dat$month == 18),]
boxplot(GSI ~ treatment, dat_t4)
t.test(GSI ~ treatment, dat_t4)
wilcox.test(GSI ~ treatment, dat_t4)

dat_t5 = dat[which(dat$month == 60),]
boxplot(GSI ~ treatment, dat_t5)
t.test(GSI ~ treatment, dat_t5)
wilcox.test(GSI ~ treatment, dat_t5)

# association between covariates
# keep one row from each SN
inds = seq(1, length(dat[,1]), 5)
dat_single = dat[inds,]

boxplot(education ~ gender, dat_single)
t.test(education ~ gender, dat_single)
wilcox.test(education ~ gender, dat_single)

# confirmatory data analysis
# group data
dat_cc = na.omit(dat)
dat_cc = dat_cc[-c(1:21),]
dat_cc_g = groupedData(GSI ~ month | SN, data = dat_cc)
dat_cc_trt = dat_cc[which(dat_cc$treatment == 1),]
dat_cc_trt_g = groupedData(GSI ~ month | SN, data = dat_cc_trt)

# covariate selection
model_base = lm(GSI ~ month, data = dat_cc_trt)
model_d1 = lm(GSI ~ month + gender, data = dat_cc_trt)
model_d4 = lm(GSI ~ month + education, data = dat_cc_trt)
model_both = lm(GSI ~ month + gender + education, data = dat_cc_trt)

lrtest(model_base, model_d1)
lrtest(model_base, model_d4)

lrtest(model_d1, model_both)
lrtest(model_d4, model_both)

# diagnostic plots
xyplot(GSI ~ month, group = SN, data = dat_cc_trt, col="black", type="b")
plot(dat_cc_trt_g[335:370,])

fit_lm = lmList(GSI ~ month | SN, dat_cc_trt_g)
plot(intervals(fit_lm))

# selecting random effects
model_base = lm(GSI ~ month + gender + education, data = dat_cc_trt)
model_intercept = lme(GSI ~ month + gender + education, 
                      random= ~ 1 | SN, data = dat_cc_trt_g,
                      control = lmeControl(opt='optim'))
model_month = lme(GSI ~ month + gender + education, 
                  random= ~ month | SN, data = dat_cc_trt_g,
                  control = lmeControl(opt='optim'))

model_d1 = lme(GSI ~ month + gender + education, 
               random= ~ gender | SN, data = dat_cc_trt_g,
               control = lmeControl(opt='optim'))

model_d4 = lme(GSI ~ month + gender + education, 
               random= ~ education | SN, data = dat_cc_trt_g,
               control = lmeControl(opt='optim'))

model_d1d4 = lme(GSI ~ month + gender + education, 
                 random= ~ gender + education | SN, data = dat_cc_trt_g,
                 control = lmeControl(opt='optim'))

model_monthd1 = lme(GSI ~ month + gender + education, 
                    random= ~ month + gender | SN, data = dat_cc_trt_g,
                    control = lmeControl(opt='optim'))

model_monthd4 = lme(GSI ~ month + gender + education, 
                    random= ~ month + education | SN, data = dat_cc_trt_g,
                    control = lmeControl(opt='optim'))

anova(model_intercept, model_base)

anova(model_month, model_intercept) # winner
anova(model_d1, model_intercept)
anova(model_d4, model_intercept)

anova(model_monthd1, model_month)
anova(model_monthd4, model_month)

# fit lme
model_month = lme(GSI ~ month + gender + education, 
                  random= ~ month | SN, data = dat_cc_trt_g,
                  control = lmeControl(opt='optim'))
summary(model_month)$tTable

# CI
summ = summary(model_month)$tTable
summ[2,1] - summ[2,2] * qt(0.975, summ[2,3]) # lower bound
summ[2,1] + summ[2,2] * qt(0.975, summ[2,3]) # upper bound

# lme assumption check
qqnorm(model_month$residuals)
qqline(model_month$residuals)
fitted = fitted(model_month)
residual = resid(model_month)
plot(fitted, residual)
t.test(resid(model_month))
wilcox.test(resid(model_month))

qqnorm(ranef(model_month)[,1])
qqline(ranef(model_month)[,1])
t.test(ranef(model_month)[,1])
wilcox.test(ranef(model_month)[,1])

qqnorm(ranef(model_month)[,2])
qqline(ranef(model_month)[,2])
t.test(ranef(model_month)[,2])
wilcox.test(ranef(model_month)[,2])

# fit gee
gee_trt <- gee(GSI ~ month + gender + education, SN, corstr = "unstructured", 
               data = dat_cc_trt)
summary(gee_trt)$coef

# multiple imputation
dat_to_impute = dat[-c(1:130),]
type_vec = c(-2, 2, 1, 1, 3, 1)
imp = jomoImpute(dat_to_impute, type = type_vec, seed = 1, m = 5)
implist <- mitmlComplete(imp)

implist_trt = implist
for (i in 1:5) {
  implist_trt[[i]] = implist[[i]][which(implist[[i]]$treatment == 1),]
}

# pool lme
lme.p1.control <- with(implist_con,
                       lme(GSI ~ month + gender + education, 
                           random= ~ 1 | SN,
                           control = lmeControl(opt='optim')))
testEstimates(lme.p1.control)

# pool gee
gee.p1.treatment <- with(implist_trt,
                         gee(GSI ~ month + gender + education, SN, 
                             corstr = "unstructured"))
qhat <- sapply(gee.p1.treatment, coef)

# robust
uhat <- qhat
for (i in 1:5) {
  uhat[,i] = diag(gee.p1.treatment[[i]]$robust.variance)
}

testEstimates(qhat = qhat, uhat = uhat)

# naive
uhat <- qhat
for (i in 1:5) {
  uhat[,i] = diag(gee.p1.treatment[[i]]$naive.variance)
}

testEstimates(qhat = qhat, uhat = uhat)