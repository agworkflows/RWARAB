
############## START SETUP
prj_path <- agvise::setup_project("RWA_potato", "agworkflows")
################ SETUP END

#########################################
# 4. Fit lmer to eliminate random error #
#########################################

ds <- readRDS(file.path(prj_path, "/data/intermediate/compiled_potato_fertiliser_trial_data.RDS"))

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100

#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lme4::lmer(sqrt(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + season + (1|TLID), data=ds)
#anova(fit0)
#MuMIn::r.squaredGLMM(fit0)

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit1 <- update(fit0, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
#anova(fit1, fit0)
#anova(fit1)
#MuMIn::r.squaredGLMM(fit1)

#updated model adding random slopes:
fit2 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
#anova(fit2, fit1)
#anova(fit2)
#MuMIn::r.squaredGLMM(fit2) 

ds$blup <- predict(fit2, ds)**2

saveRDS(ds, file.path(prj_path, "/data/intermediate/potato_fertiliser_trial_data_blub.RDS"))
