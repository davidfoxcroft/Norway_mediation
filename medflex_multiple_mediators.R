
# multiple mediator analysis in R medflex - can't deal with multilevel model
# https://cran.r-project.org/web/packages/medflex/vignettes/medflex.pdf

#select 13-16 year-olds only
#Nordat2 <- subset(Nordat2, AGEYRS <= 16)
#table(Nordat2$AGEYRS)
# full data - all ages
Nordat2 <- readRDS("Data/Nordat2.rds")
table(Nordat2$AGEYRS)
Nordat2$LOVE <- abs(Nordat2$LOVE - 5) # reverse code so high score = worse love, but messes up Mplus 
table(Nordat2$LOVE, exclude = NULL)
Nordat2$KNOW <- abs(Nordat2$KNOW - 5) # reverse code so high score = worse knowledge, but messes up Mplus 


#11
expFit <-
  glm(PAREDU2 ~ GENDER + AGEYRS, data = Nordat2) # for populaton average effects
impData <-
  neImpute(
    DRINKER ~ PAREDU2 + LOVE * KNOW + LOVE * LAX + LOVE * ALCaccep + LOVE * ALCpeers + LOVE * MUMdrunk + LOVE * DADdrunk + KNOW * LAX + KNOW * ALCaccep + KNOW * ALCpeers + KNOW * MUMdrunk + KNOW * DADdrunk + LAX * ALCaccep + LAX * ALCpeers + LAX * MUMdrunk + LAX * DADdrunk + ALCaccep * ALCpeers + ALCaccep * MUMdrunk + ALCaccep * DADdrunk + ALCpeers * MUMdrunk + ALCpeers * DADdrunk + MUMdrunk * DADdrunk + GENDER + AGEYRS,
    family = poisson(link = log),
    nMed = 7,
    data = Nordat2
  )

# think about whether to allow all interactions between mediators, not just paired
impData <-
  neImpute(
    DRINKER ~ PAREDU2 + LOVE * KNOW * LAX * ALCaccep * ALCpeers * MUMdrunk * DADdrunk + GENDER + AGEYRS,
    family = poisson(link = log),
    nMed = 7,
    data = Nordat2
  )

head(impData)
neMod6.1 <- neModel(
  DRINKER ~ PAREDU20 + PAREDU21 + GENDER + AGEYRS,
  family = poisson(link = log),
  expData = impData,
  se = "robust"
)
neMod6.1p <-
  neModel(
    DRINKER ~ PAREDU20 + PAREDU21 + GENDER + AGEYRS,
    family = poisson(link = log),
    expData = impData,
    xFit = expFit,
    se = "robust"
  )  # pop'n ave effects
summary(neMod6.1)

summary(neMod6.1p)
summary(neEffdecomp(neMod6.1p))
neEffdecomp(neMod6.1)



lht <- neLht(neMod6.1,
             linfct = c("PAREDU20 = 0",
                        "PAREDU21 = 0",
                        "PAREDU20 + PAREDU21 = 0"))
tab11 <- exp(cbind(coef(lht), confint(lht)))
print(tab11, digits = 3)

lht <- neLht(neMod6.1p,
             linfct = c("PAREDU20 = 0",
                        "PAREDU21 = 0",
                        "PAREDU20 + PAREDU21 = 0"))
tab11p <- exp(cbind(coef(lht), confint(lht)))
tab11p

#12
impData <-
  neImpute(
    DRUNK ~ PAREDU2 + LOVE * KNOW + LOVE * LAX + LOVE * ALCaccep + LOVE * ALCpeers + LOVE * MUMdrunk + LOVE * DADdrunk + KNOW * LAX + KNOW * ALCaccep + KNOW * ALCpeers + KNOW * MUMdrunk + KNOW * DADdrunk + LAX * ALCaccep + LAX * ALCpeers + LAX * MUMdrunk + LAX * DADdrunk + ALCaccep * ALCpeers + ALCaccep * MUMdrunk + ALCaccep * DADdrunk + ALCpeers * MUMdrunk + ALCpeers * DADdrunk + MUMdrunk * DADdrunk + GENDER + AGEYRS,
    family = poisson(link = log),
    nMed = 7,
    data = Nordat2
  )

head(impData)
neMod6.2 <- neModel(
  DRUNK ~ PAREDU20 + PAREDU21 + GENDER + AGEYRS,
  family = poisson(link = log),
  expData = impData,
  se = "robust"
)
neMod6.2p <- neModel(
  DRUNK ~ PAREDU20 + PAREDU21 + GENDER + AGEYRS,
  family = poisson(link = log),
  expData = impData,
  xFit = expFit,
  se = "robust"
) #pop'n ave effects
summary(neMod6.2)
summary(neMod6.2p)

lht <- neLht(neMod6.2,
             linfct = c("PAREDU20 = 0",
                        "PAREDU21 = 0",
                        "PAREDU20 + PAREDU21 = 0"))
tab12 <- exp(cbind(coef(lht), confint(lht)))

lht <- neLht(neMod6.2p,
             linfct = c("PAREDU20 = 0",
                        "PAREDU21 = 0",
                        "PAREDU20 + PAREDU21 = 0"))
tab12p <- exp(cbind(coef(lht), confint(lht)))

#21
expFit <-
  glm(margSEP ~ GENDER + AGEYRS, data = Nordat2) # for populaton average effects
impData <-
  neImpute(
    DRINKER ~ margSEP + LOVE * KNOW + LOVE * LAX + LOVE * ALCaccep + LOVE * ALCpeers + LOVE * MUMdrunk + LOVE * DADdrunk + KNOW * LAX + KNOW * ALCaccep + KNOW * ALCpeers + KNOW * MUMdrunk + KNOW * DADdrunk + LAX * ALCaccep + LAX * ALCpeers + LAX * MUMdrunk + LAX * DADdrunk + ALCaccep * ALCpeers + ALCaccep * MUMdrunk + ALCaccep * DADdrunk + ALCpeers * MUMdrunk + ALCpeers * DADdrunk + MUMdrunk * DADdrunk + GENDER + AGEYRS,
    family = poisson(link = log),
    nMed = 7,
    data = Nordat2
  )

head(impData)
neMod6.3 <- neModel(
  DRINKER ~ margSEP0 + margSEP1 + GENDER + AGEYRS,
  family = poisson(link = log),
  expData = impData,
  se = "robust"
)
neMod6.3p <-
  neModel(
    DRINKER ~ margSEP0 + margSEP1 + GENDER + AGEYRS,
    family = poisson(link = log),
    expData = impData,
    xFit = expFit,
    se = "robust"
  ) # pop'n ave effects

summary(neMod6.3)
summary(neMod6.3p)

lht <- neLht(neMod6.3,
             linfct = c("margSEP0 = 0",
                        "margSEP1 = 0",
                        "margSEP0 + margSEP1 = 0"))
tab21 <- exp(cbind(coef(lht), confint(lht)))

lht <- neLht(neMod6.3p,
             linfct = c("margSEP0 = 0",
                        "margSEP1 = 0",
                        "margSEP0 + margSEP1 = 0"))
tab21p <- exp(cbind(coef(lht), confint(lht)))

#22
impData <-
  neImpute(
    DRUNK ~ margSEP + LOVE * KNOW + LOVE * LAX + LOVE * ALCaccep + LOVE * ALCpeers + LOVE * MUMdrunk + LOVE * DADdrunk + KNOW * LAX + KNOW * ALCaccep + KNOW * ALCpeers + KNOW * MUMdrunk + KNOW * DADdrunk + LAX * ALCaccep + LAX * ALCpeers + LAX * MUMdrunk + LAX * DADdrunk + ALCaccep * ALCpeers + ALCaccep * MUMdrunk + ALCaccep * DADdrunk + ALCpeers * MUMdrunk + ALCpeers * DADdrunk + MUMdrunk * DADdrunk + GENDER + AGEYRS,
    family = poisson(link = log),
    nMed = 7,
    data = Nordat2
  )

head(impData)
neMod6.4 <- neModel(
  DRUNK ~ margSEP0 + margSEP1 + GENDER + AGEYRS,
  family = poisson(link = log),
  expData = impData,
  se = "robust"
)
neMod6.4p <- neModel(
  DRUNK ~ margSEP0 + margSEP1 + GENDER + AGEYRS,
  family = poisson(link = log),
  expData = impData,
  xFit = expFit,
  se = "robust"
) #pop'n ave effects
summary(neMod6.4)
summary(neMod6.4p)

lht <- neLht(neMod6.4,
             linfct = c("margSEP0 = 0",
                        "margSEP1 = 0",
                        "margSEP0 + margSEP1 = 0"))
tab22 <- exp(cbind(coef(lht), confint(lht)))

lht <- neLht(neMod6.4p,
             linfct = c("margSEP0 = 0",
                        "margSEP1 = 0",
                        "margSEP0 + margSEP1 = 0"))
tab22p <- exp(cbind(coef(lht), confint(lht)))

#combined table
tab.out <- rbind(tab11, tab12, tab21, tab22)
#tab.out <- rbind(tab11p,tab12p,tab21p,tab22p)
colnames(tab.out) <- c("RR", "LCI", "UCI")
rownames(tab.out) <- c(
  "PAREDU2/DRINKER NDE",
  "PAREDU2/DRINKER NIE",
  "PAREDU2/DRINKER TE",
  "PAREDU2/DRUNK NDE",
  "PAREDU2/DRUNK NIE",
  "PAREDU2/DRUNK TE",
  "margEXP/DRINKER NDE",
  "margEXP/DRINKER NIE",
  "margEXP/DRINKER TE",
  "margEXP/DRUNK NDE",
  "margEXP/DRUNK NIE",
  "margEXP/DRUNK TE"
)
print(tab.out, nsmall = 3, digits = 3)

#combined plots
par(mfrow = c(4, 2))
plot(neMod6.1, xlab = "log risk ratio")
plot(neMod6.1, xlab = "risk ratio", transf = exp)
plot(neMod6.2, xlab = "log risk ratio")
plot(neMod6.2, xlab = "risk ratio", transf = exp)
plot(neMod6.3, xlab = "log risk ratio")
plot(neMod6.3, xlab = "risk ratio", transf = exp)
plot(neMod6.4, xlab = "log risk ratio")
plot(neMod6.4, xlab = "risk ratio", transf = exp)

#str(summary(neMod6.4))

