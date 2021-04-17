## interval plot assessing need for random effect

# fit_lm = lmList(GSI ~ month + DDS4|SN, dat_cc, subset = c(600:700))
# plot(intervals(fit_lm))

# group data by SN, with and without missing data removed
dat_g <- groupedData(GSI ~ month | SN, data = as.data.frame(dat))
dat_cc = groupedData(GSI ~ month | SN, data = as.data.frame(na.omit(dat)))

# fit two lme models
fit1_lme = lme(GSI ~ month + NEW_GRP + DDS4 + DDS1, random= ~ 1 | SN, data = dat_cc)
fit2_lme = lme(GSI ~ month + NEW_GRP + DDS4 + DDS1, random= ~ month + DDS1 | SN, data = dat_cc)

fit3_lme = lme(GSI ~ month + NEW_GRP + DDS4 + DDS1, random= ~ month | SN, data = dat_cc)
summary(fit3_lme)$tTable

summary(fit1_lme)$tTable
summary(fit2_lme)$tTable

anova(fit1_lme,fit2_lme)

# fit GEE models
fit1_gee <- gee(GSI ~ month + NEW_GRP + DDS4 + DDS1, SN, corstr = "unstructured", data = dat_cc)
summary(fit1_gee)$coef

# always include month and treatment
model_base = lm(GSI ~ month + NEW_GRP, data = dat_cc)
model_d1 = lm(GSI ~ month + NEW_GRP + DDS1, data = dat_cc)
model_d4 = lm(GSI ~ month + NEW_GRP + DDS4, data = dat_cc)
model_both = lm(GSI ~ month + NEW_GRP + DDS1 + DDS4, data = dat_cc)

AIC_base = AIC(model_base)
AIC_d1 = AIC(model_d1)
AIC_d4 = AIC(model_d4)
AIC_both = AIC(model_both) # lowest

BIC_base = BIC(model_base)
BIC_d1 = BIC(model_d1)
BIC_d4 = BIC(model_d4)
BIC_both = BIC(model_both) # lowest

anova(model_base, model_d1)
anova(model_base, model_d4)
anova(model_d1, model_both) # significant
anova(model_d4, model_both) # significant
anova(model_base, model_both) # smallest p-value