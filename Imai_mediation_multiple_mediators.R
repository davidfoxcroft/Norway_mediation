#multiple mediators & post-exposure confounding - see vignette section 6
## first regress M1 on M2 and X and Zs, then regress M2 on X and Zs - see medsens article e.g. sect. 4.1.3 - https://imai.princeton.edu/research/files/medsens.pdf



Nordat2 <- readRDS("Data/Nordat2.rds")  # reload full data - all ages
str(Nordat2)
table(Nordat2$AGEYRS)

# currently only specified for one model set (PAREDU2 ~ DRINKER) 

m.med1 <-
  lme4::glmer(
    ALCpeers ~ ALCaccep + PAREDU2 + GENDER + AGEYRS + (1 |
                                                         SCHNo),
    data = Nordat2,
    family = binomial("probit")
  )

m.med1conf <- confint(m.med1)
m.med1.out <- cbind(as.data.frame(summary(m.med1)$coefficients)[,c(1,3)],m.med1conf[-1,])

m.med2 <-
  lme4::lmer(ALCaccep ~ PAREDU2 + GENDER + AGEYRS + (1 |
                                                       SCHNo), data = Nordat2)
summary(m.med2)
m.med2conf <- confint(m.med2)
m.med2.out <- cbind(as.data.frame(summary(m.med2)$coefficients)[,c(1,3)],m.med2conf[-c(1,2),])

## then undertake mediation analysis with relaxed assumptions
#str(Nordat2)
Xnames <- c("GENDER", "AGEYRS")
m.med <-
  multimed(
    outcome = "DRINKER",
    med.main = "ALCpeers",
    med.alt = "ALCaccep",
    treat = "PAREDU2",
    covariates = Xnames,
    data = as.data.frame(Nordat2),
    sims = nsim
  )
#warnings()
summary(m.med)

round(m.med$d.ave.lb[1],3) # ACME average
round(m.med$d.ave.ci[1:2,1],3)[1] # ACME average
round(m.med$d.ave.ci[1:2,1],3)[2] # ACME average

round(m.med$d1.ub[1],3) # ACME exposed
round(m.med$d1.ci[1:2,1],3)[1] # ACME exposed
round(m.med$d1.ci[1:2,1],3)[2] # ACME exposed

round(m.med$d0.ub[1],3)  # ACME non-exposed
round(m.med$d0.ci[1:2,1],3)[1]  # ACME non-exposed
round(m.med$d0.ci[1:2,1],3)[2]  # ACME non-exposed



plot(m.med, type = "point")
plot(m.med,
     type = c("sigma"),
     tgroup = c("treated"))

## compare with mediation analysis without relaxed assumptions
med.fit <- lm(ALCaccep ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(
    DRINKER ~ ALCaccep * PAREDU2 + GENDER + AGEYRS,
    data = Nordat2,
    family = binomial("probit")
  )
med.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCaccep",
    boot = TRUE,
    sims = nsim
  )
summary(med.out)

round(med.out$d.avg,3) # ACME average
round(med.out$d.avg.ci[1],3) # ACME average
round(med.out$d.avg.ci[2],3) # ACME average

round(med.out$d1,3) # ACME exposed
round(med.out$d1.ci[1],3) # ACME exposed
round(med.out$d1.ci[2],3) # ACME exposed

round(med.out$d0,3) # ACME non-exposed
round(med.out$d0.ci[1],3) # ACME non-exposed
round(med.out$d0.ci[2],3) # ACME non-exposed

saveRDS(list(m.med1.out,m.med2.out,m.med,med.out),"Output/Rmediation_m.medout_PAREDU2_DRINKER.rds")

png(
  paste0("Output/Rmediation_multiple_relaxed.unrelaxed_PAREDU2_DRINKER.png"),
  res = 300,
  width = 7,
  height = 5,
  units = "in"
)
par(mfrow = c(1, 2))
plot(m.med, type = "point")
plot(med.out)
dev.off()

png(
  paste0("Output/Rmediation_multiple_interactionhetero_PAREDU2_DRINKER.png"),
  res = 300,
  width = 7,
  height = 5,
  units = "in"
)
plot(m.med,
     type = c("sigma"),
     tgroup = c("treated"))
dev.off()


