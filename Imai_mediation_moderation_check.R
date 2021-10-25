# moderation analysis via Imai's mediation package

Nordat2 <- readRDS("Data/Nordat2.rds")  # reload full data - all ages
str(Nordat2)
table(Nordat2$AGEYRS)

#vignette("mediation")

#treatment and mediator interaction - see mediation vignette p.6.. 
# if no interaction seen then revert to simpler model

# 1. PAREDU2 ~ DRINKER
med.fit <-
  lme4::lmer(KNOW ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                   SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ KNOW * PAREDU2 + GENDER + AGEYRS + (1 |
                                                    SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(
      optimizer = "optimx",
      optCtrl = list(method = "bobyqa", maxfun = 2e5)
    )
  ) # Possible method codes are ’Nelder-Mead’, ’BFGS’, ’CG’, ’L-BFGS-B’, ’nlm’, ’nlminb’, ’spg’, ’ucminf’, ’newuoa’, ’bobyqa’, ’nmkb’, ’hjkb’, ’Rcgmin’, or ’Rvmmin’.
med1.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "KNOW",
    boot = FALSE,
    sims = nsim
  )
summary(med1.out)
#int1.test <- test.TMint(med1.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LOVE ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                   SCHNo) , data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ LOVE * PAREDU2 + GENDER + AGEYRS + (1 |
                                                    SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med2.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "LOVE",
    boot = FALSE,
    sims = nsim
  )
#int2.test <- test.TMint(med2.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LAX ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                  SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ LAX * PAREDU2 + GENDER + AGEYRS + (1 |
                                                   SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med3.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "LAX",
    boot = FALSE,
    sims = nsim
  )
#int3.test <- test.TMint(med3.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(ALCaccep ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ ALCaccep * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med4.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCaccep",
    boot = FALSE,
    sims = nsim
  )
#int4.test <- test.TMint(med4.out) #doesn't work with multilevel model

med.fit <-
  lme4::glmer(
    ALCpeers ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                              SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
out.fit <-
  lme4::glmer(
    DRINKER ~ ALCpeers * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med5.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCpeers",
    boot = FALSE,
    sims = nsim
  )
#int5.test <- test.TMint(med5.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(MUMdrunk ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ MUMdrunk * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med6.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "MUMdrunk",
    boot = FALSE,
    sims = nsim
  )
#int6.test <- test.TMint(med6.out)  #doesn't work with multilevel model

med.fit <-
  lme4::lmer(DADdrunk ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ DADdrunk * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med7.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "DADdrunk",
    boot = FALSE,
    sims = nsim
  )
#int7.test <- test.TMint(med7.out)  #doesn't work with multilevel model

# plot results

png("Output/Imai_mediation_moderation_check_PAREDU2_DRINKER_plots.png",
    width = 600,
    height = 800,
    units = "px"
)
par(mfrow = c(4, 2))
plot(med1.out, main = med1.out$mediator)
plot(med2.out, main = med2.out$mediator)
plot(med3.out, main = med3.out$mediator)
plot(med4.out, main = med4.out$mediator)
plot(med5.out, main = med5.out$mediator)
plot(med6.out, main = med6.out$mediator)
plot(med7.out, main = med7.out$mediator)
dev.off()



# 2. PAREDU2 ~ DRUNK
med.fit <-
  lme4::lmer(KNOW ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                   SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ KNOW * PAREDU2 + GENDER + AGEYRS + (1 |
                                                    SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(
      optimizer = "optimx",
      optCtrl = list(method = "bobyqa", maxfun = 2e5)
    )
  ) # Possible method codes are ’Nelder-Mead’, ’BFGS’, ’CG’, ’L-BFGS-B’, ’nlm’, ’nlminb’, ’spg’, ’ucminf’, ’newuoa’, ’bobyqa’, ’nmkb’, ’hjkb’, ’Rcgmin’, or ’Rvmmin’.
med1.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "KNOW",
    boot = FALSE,
    sims = nsim
  )
summary(med1.out)
#int1.test <- test.TMint(med1.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LOVE ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                   SCHNo) , data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ LOVE * PAREDU2 + GENDER + AGEYRS + (1 |
                                                    SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med2.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "LOVE",
    boot = FALSE,
    sims = nsim
  )
#int2.test <- test.TMint(med2.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LAX ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                  SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ LAX * PAREDU2 + GENDER + AGEYRS + (1 |
                                                   SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med3.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "LAX",
    boot = FALSE,
    sims = nsim
  )
#int3.test <- test.TMint(med3.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(ALCaccep ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ ALCaccep * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med4.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCaccep",
    boot = FALSE,
    sims = nsim
  )
#int4.test <- test.TMint(med4.out) #doesn't work with multilevel model

med.fit <-
  lme4::glmer(
    ALCpeers ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                              SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
out.fit <-
  lme4::glmer(
    DRUNK ~ ALCpeers * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med5.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCpeers",
    boot = FALSE,
    sims = nsim
  )
#int5.test <- test.TMint(med5.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(MUMdrunk ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ MUMdrunk * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med6.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "MUMdrunk",
    boot = FALSE,
    sims = nsim
  )
#int6.test <- test.TMint(med6.out)  #doesn't work with multilevel model

med.fit <-
  lme4::lmer(DADdrunk ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ DADdrunk * PAREDU2 + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med7.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "DADdrunk",
    boot = FALSE,
    sims = nsim
  )
#int7.test <- test.TMint(med7.out)  #doesn't work with multilevel model

# plot results

png("Output/Imai_mediation_moderation_check_PAREDU2_DRUNK_plots.png",
    width = 600,
    height = 800,
    units = "px"
)
par(mfrow = c(4, 2))
plot(med1.out, main = med1.out$mediator)
plot(med2.out, main = med2.out$mediator)
plot(med3.out, main = med3.out$mediator)
plot(med4.out, main = med4.out$mediator)
plot(med5.out, main = med5.out$mediator)
plot(med6.out, main = med6.out$mediator)
plot(med7.out, main = med7.out$mediator)
dev.off()



# 3. margSEP ~ DRINKER
med.fit <-
  lme4::lmer(KNOW ~ margSEP + GENDER + AGEYRS + (1 |
                                                   SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ KNOW * margSEP + GENDER + AGEYRS + (1 |
                                                    SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(
      optimizer = "optimx",
      optCtrl = list(method = "bobyqa", maxfun = 2e5)
    )
  ) # Possible method codes are ’Nelder-Mead’, ’BFGS’, ’CG’, ’L-BFGS-B’, ’nlm’, ’nlminb’, ’spg’, ’ucminf’, ’newuoa’, ’bobyqa’, ’nmkb’, ’hjkb’, ’Rcgmin’, or ’Rvmmin’.
med1.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "KNOW",
    boot = FALSE,
    sims = nsim
  )
summary(med1.out)
#int1.test <- test.TMint(med1.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LOVE ~ margSEP + GENDER + AGEYRS + (1 |
                                                   SCHNo) , data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ LOVE * margSEP + GENDER + AGEYRS + (1 |
                                                    SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med2.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "LOVE",
    boot = FALSE,
    sims = nsim
  )
#int2.test <- test.TMint(med2.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LAX ~ margSEP + GENDER + AGEYRS + (1 |
                                                  SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ LAX * margSEP + GENDER + AGEYRS + (1 |
                                                   SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med3.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "LAX",
    boot = FALSE,
    sims = nsim
  )
#int3.test <- test.TMint(med3.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(ALCaccep ~ margSEP + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ ALCaccep * margSEP + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med4.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "ALCaccep",
    boot = FALSE,
    sims = nsim
  )
#int4.test <- test.TMint(med4.out) #doesn't work with multilevel model

med.fit <-
  lme4::glmer(
    ALCpeers ~ margSEP + GENDER + AGEYRS + (1 |
                                              SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
out.fit <-
  lme4::glmer(
    DRINKER ~ ALCpeers * margSEP + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med5.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "ALCpeers",
    boot = FALSE,
    sims = nsim
  )
#int5.test <- test.TMint(med5.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(MUMdrunk ~ margSEP + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ MUMdrunk * margSEP + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med6.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "MUMdrunk",
    boot = FALSE,
    sims = nsim
  )
#int6.test <- test.TMint(med6.out)  #doesn't work with multilevel model

med.fit <-
  lme4::lmer(DADdrunk ~ margSEP + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRINKER ~ DADdrunk * margSEP + GENDER + AGEYRS + (1 |
                                                        SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med7.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "DADdrunk",
    boot = FALSE,
    sims = nsim
  )
#int7.test <- test.TMint(med7.out)  #doesn't work with multilevel model

# plot results

png("Output/Imai_mediation_moderation_check_margSEP_DRINKER_plots.png",
    width = 600,
    height = 800,
    units = "px"
)
par(mfrow = c(4, 2))
plot(med1.out, main = med1.out$mediator)
plot(med2.out, main = med2.out$mediator)
plot(med3.out, main = med3.out$mediator)
plot(med4.out, main = med4.out$mediator)
plot(med5.out, main = med5.out$mediator)
plot(med6.out, main = med6.out$mediator)
plot(med7.out, main = med7.out$mediator)
dev.off()



# 4. margSEP ~ DRUNK
med.fit <-
  lme4::lmer(KNOW ~ margSEP + GENDER + AGEYRS + (1 |
                                                   SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ KNOW * margSEP + GENDER + AGEYRS + (1 |
                                                  SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(
      optimizer = "optimx",
      optCtrl = list(method = "bobyqa", maxfun = 2e5)
    )
  ) # Possible method codes are ’Nelder-Mead’, ’BFGS’, ’CG’, ’L-BFGS-B’, ’nlm’, ’nlminb’, ’spg’, ’ucminf’, ’newuoa’, ’bobyqa’, ’nmkb’, ’hjkb’, ’Rcgmin’, or ’Rvmmin’.
med1.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "KNOW",
    boot = FALSE,
    sims = nsim
  )
summary(med1.out)
#int1.test <- test.TMint(med1.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LOVE ~ margSEP + GENDER + AGEYRS + (1 |
                                                   SCHNo) , data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ LOVE * margSEP + GENDER + AGEYRS + (1 |
                                                  SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med2.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "LOVE",
    boot = FALSE,
    sims = nsim
  )
#int2.test <- test.TMint(med2.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(LAX ~ margSEP + GENDER + AGEYRS + (1 |
                                                  SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ LAX * margSEP + GENDER + AGEYRS + (1 |
                                                 SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med3.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "LAX",
    boot = FALSE,
    sims = nsim
  )
#int3.test <- test.TMint(med3.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(ALCaccep ~ margSEP + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ ALCaccep * margSEP + GENDER + AGEYRS + (1 |
                                                      SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med4.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "ALCaccep",
    boot = FALSE,
    sims = nsim
  )
#int4.test <- test.TMint(med4.out) #doesn't work with multilevel model

med.fit <-
  lme4::glmer(
    ALCpeers ~ margSEP + GENDER + AGEYRS + (1 |
                                              SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
out.fit <-
  lme4::glmer(
    DRUNK ~ ALCpeers * margSEP + GENDER + AGEYRS + (1 |
                                                      SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med5.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "ALCpeers",
    boot = FALSE,
    sims = nsim
  )
#int5.test <- test.TMint(med5.out) #doesn't work with multilevel model

med.fit <-
  lme4::lmer(MUMdrunk ~ margSEP + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ MUMdrunk * margSEP + GENDER + AGEYRS + (1 |
                                                      SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med6.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "MUMdrunk",
    boot = FALSE,
    sims = nsim
  )
#int6.test <- test.TMint(med6.out)  #doesn't work with multilevel model

med.fit <-
  lme4::lmer(DADdrunk ~ margSEP + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
out.fit <-
  lme4::glmer(
    DRUNK ~ DADdrunk * margSEP + GENDER + AGEYRS + (1 |
                                                      SCHNo),
    data = Nordat2,
    family = binomial("probit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
med7.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "margSEP",
    mediator = "DADdrunk",
    boot = FALSE,
    sims = nsim
  )
#int7.test <- test.TMint(med7.out)  #doesn't work with multilevel model

# plot results

png("Output/Imai_mediation_moderation_check_margSEP_DRUNK_plots.png",
    width = 600,
    height = 800,
    units = "px"
)
par(mfrow = c(4, 2))
plot(med1.out, main = med1.out$mediator)
plot(med2.out, main = med2.out$mediator)
plot(med3.out, main = med3.out$mediator)
plot(med4.out, main = med4.out$mediator)
plot(med5.out, main = med5.out$mediator)
plot(med6.out, main = med6.out$mediator)
plot(med7.out, main = med7.out$mediator)
dev.off()



# file end






