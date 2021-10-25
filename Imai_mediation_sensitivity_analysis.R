
# sensitivity mediator analysis in Imai's R mediation

Nordat2 <- readRDS("Data/Nordat2.rds")  # reload full data - all ages
str(Nordat2)
table(Nordat2$AGEYRS)

## sensitivity analysis - can't use interaction effect or multilevel parameter

# currently only specified for one model set (PAREDU2 ~ DRINKER) 
med.fit <- lm(KNOW ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(DRINKER ~ KNOW + PAREDU2 + GENDER + AGEYRS,
      data = Nordat2,
      family = binomial("probit"))
med1.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "KNOW",
    boot = TRUE,
    sims = nsim
  )
sens1.out <-
  medsens(med1.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens1.out)

med.fit <- lm(LOVE ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(DRINKER ~ LOVE + PAREDU2 + GENDER + AGEYRS,
      data = Nordat2,
      family = binomial("probit"))
med2.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "LOVE",
    boot = TRUE,
    sims = nsim
  )
sens2.out <-
  medsens(med2.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens2.out)

med.fit <- lm(LAX ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(DRINKER ~ LAX + PAREDU2 + GENDER + AGEYRS,
      data = Nordat2,
      family = binomial("probit"))
med3.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "LAX",
    boot = TRUE,
    sims = nsim
  )
sens3.out <-
  medsens(med3.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens3.out)

med.fit <- lm(ALCaccep ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(
    DRINKER ~ ALCaccep + PAREDU2 + GENDER + AGEYRS,
    data = Nordat2,
    family = binomial("probit")
  )
med4.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCaccep",
    boot = TRUE,
    sims = nsim
  )
sens4.out <-
  medsens(med4.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens4.out)

med.fit <-
  lm(ALCpeers ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2) # fit with lm otherwise medsens don't work
out.fit <-
  glm(
    DRINKER ~ ALCpeers + PAREDU2 + GENDER + AGEYRS,
    data = Nordat2,
    family = binomial("probit")
  )
med5.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "ALCpeers",
    boot = TRUE,
    sims = nsim
  )
sens5.out <-
  medsens(med5.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens5.out)

med.fit <- lm(MUMdrunk ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(
    DRINKER ~ MUMdrunk + PAREDU2 + GENDER + AGEYRS,
    data = Nordat2,
    family = binomial("probit")
  )
med6.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "MUMdrunk",
    boot = TRUE,
    sims = nsim
  )
sens6.out <-
  medsens(med6.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens6.out)

med.fit <- lm(DADdrunk ~ PAREDU2 + GENDER + AGEYRS, data = Nordat2)
out.fit <-
  glm(
    DRINKER ~ DADdrunk + PAREDU2 + GENDER + AGEYRS,
    data = Nordat2,
    family = binomial("probit")
  )
med7.out <-
  mediate(
    med.fit,
    out.fit,
    treat = "PAREDU2",
    mediator = "DADdrunk",
    boot = TRUE,
    sims = nsim
  )
sens7.out <-
  medsens(med7.out,
          rho.by = 0.1,
          effect.type = "indirect",
          sims = nsim)
summary(sens7.out)

sens.tab <- cbind(
  sens1.out$err.cr.d[1],
  sens2.out$err.cr.d[1],
  sens3.out$err.cr.d[1],
  sens4.out$err.cr.d[1],
  sens5.out$err.cr.d[1],
  sens6.out$err.cr.d[1],
  sens7.out$err.cr.d[1]
)

sens.tab <- rbind(
  sens.tab,
  c(
    sens1.out$R2tilde.d.thresh[1],
    sens2.out$R2tilde.d.thresh[1],
    sens3.out$R2tilde.d.thresh[1],
    sens4.out$R2tilde.d.thresh[1],
    sens5.out$R2tilde.d.thresh[1],
    sens6.out$R2tilde.d.thresh[1],
    sens7.out$R2tilde.d.thresh[1]
  )
)

colnames(sens.tab) <-
  c("KNOW",
    "LOVE",
    "LAX",
    "ALCaccep",
    "ALCpeers",
    "MUMdrunk",
    "DADdrunk")
rownames(sens.tab) <-
  c(
    "Rho at which ACME for different exposure groups = 0",
    "Product of explained variances where ACME = 0"
  )
print(sens.tab, nsmall = 3, digits = 3)
saveRDS(sens.tab,"Output/Rmediation_medsens_PAREDU2_DRINKER.rds")

meds <- readRDS("Output/Rmediation_medsens_PAREDU2_DRINKER.rds")
meds <- cbind(rownames(meds), data.frame(meds, row.names=NULL))
meds <- as.data.frame(lapply(meds,function (y) if(class(y)=="factor" ) as.character(y) else y),stringsAsFactors=F)
colnames(meds)[1] <- " "
medstab <- flextable(meds)
medstab <- autofit(medstab)
medstab <- width(medstab, j = 1, 2.5)
save_as_image(medstab, "../Output/Rmediation_medsens_PAREDU2_DRINKER.png")


min(meds[1,])
max(meds[1,])

png(
  paste0("Output/Rmediation_medsens_PAREDU2_DRINKER_1.png"),
  res = 300,
  width = 7,
  height = 9,
  units = "in"
)
par(mfrow = c(4, 2))
plot(sens1.out, sens.par = "rho")
plot(sens2.out, sens.par = "rho")
plot(sens3.out, sens.par = "rho")
plot(sens4.out, sens.par = "rho")
dev.off()

png(
  paste0("Output/Rmediation_medsens_PAREDU2_DRINKER_2.png"),
  res = 300,
  width = 7,
  height = 9,
  units = "in"
)
par(mfrow = c(3, 2))
plot(sens5.out, sens.par = "rho")
plot(sens6.out, sens.par = "rho")
plot(sens7.out, sens.par = "rho")
dev.off()



#par(mfrow=c(1,2))
#plot(sens7.out, sens.par="R2", r.type = "total", sign.prod = "positive", xlim = c(0.0, 0.5), ylim = c(0.0, 0.5))


