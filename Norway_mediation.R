#top###########################################################################
#
#
# David Foxcroft
# Moderation and Mediation Models for Social Status Paper
# david.foxcroft@gmail.com
# Copyright (C) 2019, David Foxcroft
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
#top############################################################################

library(roperators)
library(tidyverse)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(car)
#library(webshot)
library(htmltools)
library(lme4)
library(parallel)
library(optimx)
library(mediation)
#devtools::install_github("jmpsteen/medflex")
#library(medflex)
library(coda)
#devtools::install_github("michaelhallquist/MplusAutomation")
library(MplusAutomation)
#install.packages("BiocManager")
#BiocManager::install("rhdf5")
library(rhdf5)
library(here)
library(EValue)

#set confint method - Wald for development / testing then bootstrap confidence intervals for final run, but takes a loooong time so best to repurpose code for AWS
bootconf <- FALSE # TRUE
nsim <- 50 # 50 for testing code, 1000 for final run
ncpus <- detectCores()
ncpus
set.seed(sqrt(1963)) # for replicable estimation

# data prep
#getwd()
#setwd("~/Dropbox/Coding/Mediation/Norway") # don"t need if working in an R project
Nordat <- read_csv("Data/Norway.csv")
str(Nordat)

#replace all missings with default NA
Nordat[] <- lapply(Nordat, function(x) ifelse(x == -9999, NA, x))
#make all vars of type numeric for MPlus analysis
#Nordat[,1:ncol(Nordat)] <- as.data.frame(sapply(Nordat[1:ncol(Nordat)], as.numeric)) # doesn"t appear to be needed with read_csv

# create table 1 for paper
# count observations by categorical variables
table1a <- Nordat %>%
  group_by(AGEYRS, GENDER) %>%
  summarise(N = n()) %>%
  spread(GENDER, N) %>%
  mutate(Label = c("Age"))
table1b <- Nordat %>%
  group_by(PAREDU3, GENDER) %>%
  summarise(N = n()) %>%
  spread(GENDER, N) %>%
  mutate(Label = c("Parent Education"))
table1c <- Nordat %>%
  group_by(margSEP, GENDER) %>%
  summarise(N = n()) %>%
  spread(GENDER, N) %>%
  mutate(Label = c("Marginal Socio-Economic Position"))
table1d <- Nordat %>%
  group_by(DRINKER, GENDER) %>%
  summarise(N = n()) %>%
  spread(GENDER, N) %>%
  mutate(Label = c("Past year drinking"))
table1e <- Nordat %>%
  group_by(DRUNK, GENDER) %>%
  summarise(N = n()) %>%
  spread(GENDER, N) %>%
  mutate(Label = c("Past year drunkenness"))

# merge subtables, calculate %ages, apply labels and formatting
table1 <- rbind(table1a,table1b,table1c,table1d,table1e) 
table1$category <- paste(table1$AGEYRS,table1$PAREDU3,table1$margSEP,table1$DRINKER,table1$DRUNK)
table1$category <- table1$category %-% "[NA]"
table1 <- table1[c(5,10,2,3,4)] %>% 
  rename(Male = 3, Female = 4, Missing = 5) %>%
  mutate(Ntot = Male + Female + Missing) %>%
  mutate(Mpct = paste0(format(round(100 * Male/Ntot,1), ndigits = 2, nsmall = 1), "%")) %>%
  mutate(Fpct = paste0(format(round(100 * Female/Ntot,1), ndigits = 2, nsmall = 1), "%")) %>%
  mutate(Misspct = paste0(format(round(100 * Missing/Ntot,1), ndigits = 2, nsmall = 1), "%")) 
table1 <- table1[,c(1,2,3,7,4,8,5,9,6)]
table1[7,2] <- c("High")
table1[8,2] <- c("Medium")
table1[9,2] <- c("Low")
table1[10,2] <- c("No")
table1[11,2] <- c("Yes")
table1[12,2] <- c("No")
table1[13,2] <- c("Yes")
table1[14,2] <- c("No")
table1[15,2] <- c("Yes")
table1

# finesse with flextable and save as R object for use in Rmd file
flextab1 <- flextable(table1) 
flextab1 <- set_header_labels(flextab1, Label = "", category = "", Mpct = "", Fpct = "", Misspct = "", Ntot = "Totals" )
flextab1 <- merge_v(flextab1, j = ~ Label )
flextab1 <- merge_at(flextab1, i = 1, j = 3:4, part = "header")
flextab1 <- merge_at(flextab1, i = 1, j = 5:6, part = "header")
flextab1 <- merge_at(flextab1, i = 1, j = 7:8, part = "header")
flextab1 <- align(flextab1, align = "center", part = "header" )
flextab1 <- valign(flextab1, j = 1, valign = "top", part = "body")
flextab1 <- bold(flextab1, j = 1, bold = TRUE, part = "body")
flextab1 <- add_footer_row(flextab1, 
                           top = FALSE, 
                           values = "Overall N = 17,661", 
                           colwidths = 9)
flextab1 <- hline(flextab1, i = c(6,9,11,13), j = 1:9, border =  fp_border(color = "gray"), part = "body")
flextab1 <- autofit(flextab1)
flextab1 <- hline_top(flextab1, border = fp_border(color = "black", width = 2), part = "footer")
flextab1 <- italic(flextab1, part = "footer")
flextab1 <- align(flextab1, align = "right", part = "footer" )
flextab1

saveRDS(flextab1, "Output/flextab1.rds")
save_as_image(flextab1, "Output/flextab1.png")

#dataprep#######################################################################
# prepare data for analysis
# listwise deletion 
use <- complete.cases(Nordat)
str(use)
Nordat2 <- Nordat[use, ]
str(Nordat2)

# backflip social status measures - consistent with Pape analysis
#table(Nordat2$PAREDU2)
#Nordat2$PAREDU2 <- abs(Nordat2$PAREDU2 - 1)
#table(Nordat2$PAREDU2)
#table(Nordat2$margSEP)
#Nordat2$margSEP <- abs(Nordat2$margSEP - 1)
#table(Nordat2$margSEP)

#take a look at the vars
#confounders - binary
table(Nordat2$GENDER, exclude = NULL)
table(Nordat2$AGEYRS, exclude = NULL)
#exposure - binary
table(Nordat2$PAREDU2, exclude = NULL)
table(Nordat2$margSEP, exclude = NULL)
#mediators - ordinal or continuous
Nordat2$LOVE <- abs(Nordat2$LOVE - 5) # reverse code so high score = worse love; consistent direction for all mediators
table(Nordat2$LOVE, exclude = NULL)
hist(Nordat2$LOVE)
Nordat2$KNOW <- abs(Nordat2$KNOW - 5) # reverse code so high score = worse knowledge; consistent direction for all mediators 
table(Nordat2$KNOW, exclude = NULL)
table(Nordat2$LAX)
table(Nordat2$ALCaccep)
table(Nordat2$ALCpeers)
table(Nordat2$PARTYkon)
table(Nordat2$HOMEkon)
table(Nordat2$MUMdrunk)
table(Nordat2$DADdrunk)
#outcome - binary
table(Nordat2$DRINKER)
table(Nordat2$DRUNK)

# recode gender to 0 and 1 # needed for MPlus analysis
Nordat2$GENDER <- Nordat2$GENDER - 1
table(Nordat2$GENDER) 

# recode age confounder to binary # needed for MPlus analysis
Nordat2$AGEgr <- recode(Nordat2$AGEYRS, "13=0; 14=0; 15=0; 16=0; 17=1; 18=1")
table(Nordat2$AGEgr) 

#flip social status measures - more intuitive interpretation for RRs
#table(Nordat2$PAREDU2)
#Nordat2$PAREDU2 <- abs(Nordat2$PAREDU2 - 1)
#table(Nordat2$PAREDU2)
#table(Nordat2$margSEP)
#Nordat2$margSEP <- abs(Nordat2$margSEP - 1)
#table(Nordat2$margSEP)

saveRDS(Nordat2,"Data/Nordat2.rds")


#Pape rerun###########################################################################

# rerun analysis from Pape paper - exclude PARTYkon & HOMEkon as too skewed

#select 13-16 year-olds only
Nordat2 <- readRDS("Data/Nordat2.rds")
Nordat2 <- subset(Nordat2, AGEYRS <= 16)
table(Nordat2$AGEYRS)

# backflip social status measures - consistent with Pape analysis
#table(Nordat2$PAREDU2)
#Nordat2$PAREDU2 <- abs(Nordat2$PAREDU2 - 1)
#table(Nordat2$PAREDU2)
#table(Nordat2$margSEP)
#Nordat2$margSEP <- abs(Nordat2$margSEP - 1)
#table(Nordat2$margSEP)

## PAREDU
#step 1 - DRINKER
pape <- lme4::glmer(DRINKER ~ PAREDU2 + AGEYRS + (1|SCHNo),  family = poisson(link = log), data = Nordat2, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) # different optimizer
summary(pape)

# set confint method based on bootconf value at top of script
ifelse(bootconf == FALSE, 
       (confmeth <- list(object = pape, 
                         parm = "beta_" , 
                         method = "Wald")),
       (confmeth <- list(object = pape, 
                         parm = "beta_" , 
                         method = "boot", 
                         nsim = nsim, 
                         parallel = "multicore", 
                         ncpus = ncpus))
)

#get PRs
confmeth
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape),cc)
rtab11 <- exp(ctab)
print(rtab11,digits = 3)

#fullmodel
pape <- lme4::glmer(DRINKER ~ PAREDU2 + scale(LOVE) + scale(KNOW) + scale(LAX) + scale(ALCaccep) + ALCpeers + scale(MUMdrunk) + scale(DADdrunk) + scale(AGEYRS) + (1|SCHNo),  family = poisson(link = log), data = Nordat2, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape),cc)
rtab12 <- exp(ctab)
print(rtab12,digits = 3)

# ready for final plot
trad.direct.DRINKER.PAREDU2 <- rtab12[2,]

#step 1 - DRUNK
pape <- lme4::glmer(DRUNK ~ PAREDU2 + AGEYRS + (1|SCHNo),  family = poisson(link = log), data = Nordat2, control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))) # different optimizer
#summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape),cc)
rtab21 <- exp(ctab)
#print(rtab21,digits = 3)


#fullmodel
pape <-
  lme4::glmer(
    DRUNK ~ PAREDU2 + scale(LOVE) + scale(KNOW) + scale(LAX) + scale(ALCaccep) + ALCpeers + scale(MUMdrunk) + scale(DADdrunk) + scale(AGEYRS) + (1 |
                                                                                                                                                   SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape), cc)
rtab22 <- exp(ctab)

# ready for final plot
trad.direct.DRUNK.PAREDU2 <- rtab22[2, ]

print(rtab11[-1,], digits = 3, nsmall = 3) #DRINKER
print(rtab12[-1,], digits = 3, nsmall = 3) #DRINKER
print(rtab21[-1,], digits = 3, nsmall = 3) #DRUNK
print(rtab22[-1,], digits = 3, nsmall = 3) #DRUNK

rtab11 <- cbind("Drinker-Base",rtab11[-1,])
rtab12 <- cbind("Drinker-Full",rtab12[-1,])
rtab21 <- cbind("Drunk-Base",rtab21[-1,])
rtab22 <- cbind("Drunk-Full",rtab22[-1,])

tab2a <- rbind(rtab11,rtab12,rtab21,rtab22)
tab2a <- cbind(rownames(tab2a), data.frame(tab2a, row.names = NULL))


## margSEP
#step 1 - DRINKER
pape <-
  lme4::glmer(
    DRINKER ~ margSEP + AGEYRS + (1 |
                                    SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
#summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape), cc)
rtab11 <- exp(ctab)
#print(rtab11,digits=3)

#fullmodel
pape <-
  lme4::glmer(
    DRINKER ~ margSEP + scale(LOVE) + scale(KNOW) + scale(LAX) + scale(ALCaccep) + ALCpeers + scale(MUMdrunk) + scale(DADdrunk) + scale(AGEYRS) + (1 |
                                                                                                                                                     SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
#summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape), cc)
rtab12 <- exp(ctab)
#print(rtab12,digits=3)

# ready for final plot
trad.direct.DRINKER.margEXP <- rtab12[2,]

#step 1 - DRUNK
pape <-
  lme4::glmer(
    DRUNK ~ margSEP + AGEYRS + (1 |
                                  SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
  ) # different optimizer
#summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape), cc)
rtab21 <- exp(ctab)
#print(rtab21,digits=3)

#fullmodel
pape <-
  lme4::glmer(
    DRUNK ~ margSEP + scale(LOVE) + scale(KNOW) + scale(LAX) + scale(ALCaccep) + ALCpeers + scale(MUMdrunk) + scale(DADdrunk) + scale(AGEYRS) + (1 |
                                                                                                                                                   SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
#summary(pape)
#get PRs
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape), cc)
rtab22 <- exp(ctab)

# ready for final plot
trad.direct.DRUNK.margEXP <- rtab22[2, ]

print(rtab11[-1, ], digits = 3, nsmall = 3) #DRINKER
print(rtab12[-1, ], digits = 3, nsmall = 3) #DRINKER
print(rtab21[-1, ], digits = 3, nsmall = 3) #DRUNK
print(rtab22[-1, ], digits = 3, nsmall = 3) #DRUNK

rtab11 <- cbind("Drinker-Base",rtab11[-1,])
rtab12 <- cbind("Drinker-Full",rtab12[-1,])
rtab21 <- cbind("Drunk-Base",rtab21[-1,])
rtab22 <- cbind("Drunk-Full",rtab22[-1,])

tab2b <- rbind(rtab11,rtab12,rtab21,rtab22)
tab2b <- cbind(rownames(tab2b), data.frame(tab2b, row.names = NULL))

tab2 <- cbind(tab2a,tab2b)
saveRDS(tab2,"Output/table2.rds") # save table with pape replication

#moderation########################################################################
# traditional moderation analyis

Nordat2 <- readRDS("Data/Nordat2.rds")  # reload full data - all ages
str(Nordat2)
table(Nordat2$AGEYRS)
table(Nordat2$PAREDU2)
table(Nordat2$margSEP)

papemod <-
  lme4::glmer(
    DRINKER ~ PAREDU2 * scale(LOVE) + PAREDU2 * scale(KNOW) + PAREDU2 * scale(LAX) + PAREDU2 *
      scale(ALCaccep) + PAREDU2 * ALCpeers + PAREDU2 * scale(MUMdrunk) + PAREDU2 *
      scale(DADdrunk) + scale(AGEYRS) + GENDER + (1 |
                                                    SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
summary(papemod)
#get PRs
confmeth <- modifyList(confmeth, list(object = papemod))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(papemod), cc)
rtab11 <- exp(ctab)

papemod <-
  lme4::glmer(
    DRUNK ~ PAREDU2 * scale(LOVE) + PAREDU2 * scale(KNOW) + PAREDU2 * scale(LAX) + PAREDU2 *
      scale(ALCaccep) + PAREDU2 * ALCpeers + PAREDU2 * scale(MUMdrunk) + PAREDU2 *
      scale(DADdrunk) + scale(AGEYRS) + GENDER + (1 |
                                                    SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
summary(papemod)
#get PRs
confmeth <- modifyList(confmeth, list(object = papemod))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(papemod), cc)
rtab12 <- exp(ctab)

papemod <-
  lme4::glmer(
    DRINKER ~ margSEP * scale(LOVE) + margSEP * scale(KNOW) + margSEP * scale(LAX) + margSEP *
      scale(ALCaccep) + margSEP * ALCpeers + margSEP * scale(MUMdrunk) + margSEP *
      scale(DADdrunk) + scale(AGEYRS) + GENDER + (1 |
                                                    SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
summary(papemod)
#get PRs
confmeth <- modifyList(confmeth, list(object = papemod))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(papemod), cc)
rtab21 <- exp(ctab)

papemod <-
  lme4::glmer(
    DRUNK ~ margSEP * scale(LOVE) + margSEP * scale(KNOW) + margSEP * scale(LAX) + margSEP *
      scale(ALCaccep) + margSEP * ALCpeers + margSEP * scale(MUMdrunk) + margSEP *
      scale(DADdrunk) + scale(AGEYRS) + GENDER + (1 |
                                                    SCHNo),
    family = poisson(link = log),
    data = Nordat2,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
summary(papemod)
#get PRs
confmeth <- modifyList(confmeth, list(object = papemod))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(papemod), cc)
rtab22 <- exp(ctab)

#tables of results
print(rtab11[-1, ], digits = 3, nsmall = 3) #DRINKER
print(rtab12[-1, ], digits = 3, nsmall = 3) #DRINKER
print(rtab21[-1, ], digits = 3, nsmall = 3) #DRUNK
print(rtab22[-1, ], digits = 3, nsmall = 3) #DRUNK

tab3 <- rbind(rtab11[-1,],rtab12[-1,],rtab21[-1,],rtab22[-1,])
tab3 <- cbind(rownames(tab3), data.frame(tab3, row.names = NULL))

saveRDS(tab3,"Output/table3.rds") # save table with trad interactions


#tables#####################################################################

# tables for paper

tab2 <- readRDS("Output/table2.rds")
tab2a <- subset(tab2[1:11,])
tab2b <- subset(tab2[12:22,])
tab2 <- cbind(tab2a,tab2b)
colnames(tab2) <- c("Effect","Model","Drinker ~ Parental Education",
                    "LCI","UCI","","","Drunk ~ Parental Education",
                    "LCI","UCI","","","Drinker ~ Marginal SEP",
                    "LCI","UCI","","","Drunk ~ Marginal SEP",
                    "LCI","UCI"
                    )
tab2 <- tab2[c(1:5,8:10,13:15,18:20)]
tab2[3:14] <- lapply(tab2[3:14], function(x) as.numeric(as.character(x)))
tab2 <- tab2 %>% 
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate_if(is.factor, as.character)
tab2[c(1,3),1] <- "SOCIAL STATUS"
tab2 <- tab2[c(2,1,3:14)]
tab2 <- tab2 %>% 
  mutate(`Drinker ~ Parental Education` = paste0(format(`Drinker ~ Parental Education`,ndigits = 2,nsmall = 2), " (", format(LCI,ndigits = 2,nsmall = 2), " to ", format(UCI,ndigits = 2,nsmall = 2), ")")) %>%
  mutate(`Drunk ~ Parental Education` = paste0(format(`Drunk ~ Parental Education`,ndigits = 2,nsmall = 2), " (", format(LCI.1,ndigits = 2,nsmall = 2), " to ", format(UCI.1,ndigits = 2,nsmall = 2), ")")) %>%
  mutate(`Drinker ~ Marginal SEP` = paste0(format(`Drinker ~ Marginal SEP`,ndigits = 2,nsmall = 2), " (", format(LCI.2,ndigits = 2,nsmall = 2), " to", format(UCI.2,ndigits = 2,nsmall = 2), ")")) %>%
  mutate(`Drunk ~ Marginal SEP` = paste0(format(`Drunk ~ Marginal SEP`,ndigits = 2,nsmall = 2), " (", format(LCI.3,ndigits = 2,nsmall = 2), " to ", format(UCI.3,ndigits = 2,nsmall = 2), ")")) %>%
  dplyr::select(-c(LCI,UCI,LCI.1,UCI.1,LCI.2,UCI.2,LCI.3,UCI.3))
tab2$Model <- gsub("Drinker-", "", tab2$Model)
tab2$Effect <- gsub("scale\\(", "", tab2$Effect)
tab2$Effect <- gsub("\\)", "", tab2$Effect)
tab2$Effect <- gsub(":", "*", tab2$Effect)

tab2

# finesse with flextable and save as R object for use in Rmd file
flextab2 <- flextable(tab2) 
#flextab1 <- set_header_labels(flextab1, Label = "", category = "", Mpct = "", Fpct = "", Misspct = "", Ntot = "Totals" )
flextab2 <- merge_v(flextab2, j = ~ Model )
flextab2 <- valign(flextab2, j = ~ Model, valign = "top", part = "body")
flextab2 <- bold(flextab2, j = ~ Model, bold = TRUE, part = "body")
#flextab1 <- align(flextab1, align = "center", part = "header" )
flextab2 <- add_footer_row(flextab2, 
                           top = FALSE, 
                           values = "Base and Full for Models 13 to 16 year-olds only, as per original Pape et al. (2017) analysis.",
                           colwidths = 6)
flextab2 <- hline(flextab2, i = 2, j = 1:6, border =  fp_border(color = "gray"), part = "body")
flextab2 <- autofit(flextab2)
flextab2 <- hline_top(flextab2, border = fp_border(color = "black", width = 2), part = "footer")
flextab2 <- italic(flextab2, part = "footer")
flextab2 <- align(flextab2, align = "right", part = "footer" )
flextab2

saveRDS(tab2, "Output/tab2.rds")
saveRDS(flextab2, "Output/flextab2.rds")
save_as_image(flextab2, "Output/flextab2.png")


tab3 <- readRDS("Output/table3.rds")
tab3a <- subset(tab3[1:17,])
tab3b <- subset(tab3[18:34,])
tab3c <- subset(tab3[35:51,])
tab3d <- subset(tab3[52:68,])

tab3 <- cbind(tab3a,tab3b,tab3c,tab3d)

colnames(tab3) <- c("Effect","Drinker ~ Parental Education",
                    "LCI","UCI","","Drunk ~ Parental Education",
                    "LCI","UCI","","Drinker ~ Marginal SEP",
                    "LCI","UCI","","Drunk ~ Marginal SEP",
                    "LCI","UCI"
)
tab3 <- tab3[c(1:4,6:8,10:12,14:16)]
tab3[2:13] <- lapply(tab3[2:13], function(x) as.numeric(as.character(x)))
tab3 <- tab3 %>% 
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate_if(is.factor, as.character)
tab3$Effect <- gsub("PAREDU2", "SOCIAL STATUS", tab3$Effect)
tab3 <- tab3 %>% 
  mutate(`Drinker ~ Parental Education` = paste0(format(`Drinker ~ Parental Education`,ndigits = 2,nsmall = 2), " (", format(LCI,ndigits = 2,nsmall = 2), " to ", format(UCI,ndigits = 2,nsmall = 2), ")")) %>%
  mutate(`Drunk ~ Parental Education` = paste0(format(`Drunk ~ Parental Education`,ndigits = 2,nsmall = 2), " (", format(LCI.1,ndigits = 2,nsmall = 2), " to ", format(UCI.1,ndigits = 2,nsmall = 2), ")")) %>%
  mutate(`Drinker ~ Marginal SEP` = paste0(format(`Drinker ~ Marginal SEP`,ndigits = 2,nsmall = 2), " (", format(LCI.2,ndigits = 2,nsmall = 2), " to ", format(UCI.2,ndigits = 2,nsmall = 2), ")")) %>%
  mutate(`Drunk ~ Marginal SEP` = paste0(format(`Drunk ~ Marginal SEP`,ndigits = 2,nsmall = 2), " (", format(LCI.3,ndigits = 2,nsmall = 2), " to ", format(UCI.3,ndigits = 2,nsmall = 2), ")")) %>%
  dplyr::select(-c(LCI,UCI,LCI.1,UCI.1,LCI.2,UCI.2,LCI.3,UCI.3))
#tab3 <- tibble::add_column(tab3, Model = "Moderation", .before = 1)
tab3
tab3$Effect <- gsub("scale\\(", "", tab3$Effect)
tab3$Effect <- gsub("\\)", "", tab3$Effect)
tab3$Effect <- gsub(":", "*", tab3$Effect)


# finesse with flextable and save as R object for use in Rmd file
flextab3 <- flextable(tab3) 
flextab3 <- add_footer_row(flextab3, 
                           top = FALSE, 
                           values = "Moderation model with complete sample of 13 to 18 year-olds, N=17,493 included in model.", 
                           colwidths = 5)
flextab3 <- autofit(flextab3)
flextab3 <- hline_top(flextab3, border = fp_border(color = "black", width = 2), part = "footer")
flextab3 <- italic(flextab3, part = "footer")
flextab3 <- align(flextab3, align = "right", part = "footer" )
flextab3

saveRDS(tab3, "Output/tab3.rds")
saveRDS(flextab3, "Output/flextab3.rds")
save_as_image(flextab3, "Output/flextab3.png")



#Mplus using WLSMV ###########################################################################

# multiple mediator analysis in MPlus - adapted from Nguyen 2015 paper appendix

Nordat2 <- readRDS("Data/Nordat2.rds")
str(Nordat2)
table(Nordat2$AGEYRS, exclude = NULL)
table(Nordat2$AGEgr, exclude = NULL)
table(Nordat2$KNOW, exclude = NULL)
table(Nordat2$LOVE, exclude = NULL)

# subset by age and recode age for confounder analysis
Nordat2.yng <- subset(Nordat2, AGEYRS == 13 | AGEYRS == 14)
table(Nordat2.yng$AGEYRS, exclude = NULL)
Nordat2.yng$AGEgr <- Nordat2.yng$AGEYRS - 13
table(Nordat2.yng$AGEgr)
Nordat2.mid <- subset(Nordat2, AGEYRS == 15 | AGEYRS == 16)
table(Nordat2.mid$AGEYRS, exclude = NULL)
Nordat2.mid$AGEgr <- Nordat2.mid$AGEYRS - 15
table(Nordat2.mid$AGEgr)
Nordat2.old <- subset(Nordat2, AGEYRS == 17 | AGEYRS == 18)
table(Nordat2.old$AGEYRS, exclude = NULL)
Nordat2.old$AGEgr <- Nordat2.old$AGEYRS - 17
table(Nordat2.old$AGEgr)

#setwd("./Mplus")
MplusAutomation::prepareMplusData(Nordat2, "MPlus/Mplus_Norway.dat")
MplusAutomation::prepareMplusData(Nordat2.yng, "MPlus/Mplus_Norway.yng.dat")
MplusAutomation::prepareMplusData(Nordat2.mid, "MPlus/Mplus_Norway.mid.dat")
MplusAutomation::prepareMplusData(Nordat2.old, "MPlus/Mplus_Norway.old.dat")

MplusAutomation::runModels(target = "/Users/David/Dropbox/Coding/Mediation/Norway/MPlus")

#setwd("/Users/David/Dropbox/Coding/Mediation/Norway")

# Use following code AFTER finishing fitting the model with Bayes estimator in Mplus.
# analysis needs to be re-run for each MPlus inp model, ie PAREDU2-DRINKER, PAREDU2-DRUNK. margSEP-DRINKER and margSEP-DRUNK etc


### Preparations

modelset <- c("PAREDU2_DRINKER_COMPLEX",
              "PAREDU2_DRUNK_COMPLEX",
              "margSEP_DRINKER_COMPLEX",
              "margSEP_DRUNK_COMPLEX",
              "PAREDU2_DRINKER_COMPLEX_yng",
              "PAREDU2_DRUNK_COMPLEX_yng",
              "margSEP_DRINKER_COMPLEX_yng",
              "margSEP_DRUNK_COMPLEX_yng",
              "PAREDU2_DRINKER_COMPLEX_mid",
              "PAREDU2_DRUNK_COMPLEX_mid",
              "margSEP_DRINKER_COMPLEX_mid",
              "margSEP_DRUNK_COMPLEX_mid",
              "PAREDU2_DRINKER_COMPLEX_old",
              "PAREDU2_DRUNK_COMPLEX_old",
              "margSEP_DRINKER_COMPLEX_old",
              "margSEP_DRUNK_COMPLEX_old")

#x <- "PAREDU2_DRINKER" # test lapply
lapply(modelset, function(x) {
  # Read in data and get sample size
  if ( grepl("yng",x,fixed = TRUE)) {
    dat <- read.table("Mplus/Mplus_Norway.yng.dat")
  } else if ( grepl("mid",x,fixed = TRUE)) {
    dat <- read.table("Mplus/Mplus_Norway.mid.dat")
  } else if ( grepl("old",x,fixed = TRUE)) {
    dat <- read.table("Mplus/Mplus_Norway.old.dat")
  } else {
    dat <- read.table("Mplus/Mplus_Norway.dat")
  }
  
  n <- nrow(dat)
  str(dat)
  
  # Extract data on the confounders only.
  confounders <- dat[, c(3, 24)] # two confounders (gender and age) in columns 3 and 24 of the dataset
  str(confounders)
  table(confounders$V3)
  table(confounders$V24)
  
  # Make four "counterfactual datasets", which will be combined with the new parameters to
  # compute causal mediation effects. These datasets differ only by the last two columns.
  dat.00 <- cbind(rep(1, n), # a column of 1"s (to multiply with parameter constant)
                  confounders, # confounder data (to multiply with z1_coef and z2_coef)
                  rep(0, n), # a column of 0"s for x = 0 (to multiply with x_coef)
                  rep(0, n)) # a column of 0"s for x"= 0 (to multiply with xp_coef)
  dat.11 <- cbind(rep(1, n), confounders,
                  rep(1, n), rep(1, n)) # columns of 1"s and 1"s for x = 1 and x"= 1
  dat.10 <- cbind(rep(1, n), confounders,
                  rep(1, n), rep(0, n)) # columns of 1"s and 0"s for x = 1 and x"= 0
  dat.01 <- cbind(rep(1, n), confounders,
                  rep(0, n), rep(1, n)) # columns of 0"s and 1"s for x = 0 and x"= 1
  
  
  dat.00 <- as.matrix(dat.00)
  dat.11 <- as.matrix(dat.11)
  dat.10 <- as.matrix(dat.10)
  dat.01 <- as.matrix(dat.01)
  
  ########################################################
  # COMPLEX MPlus analysis
  
  #x <- "PAREDU2_DRINKER_COMPLEX"
  if ( grepl("yng",x,fixed = TRUE)) {
    filelab <- (paste0("Mplus/Norway_MPlus_",x,".out"))
  } else if ( grepl("mid",x,fixed = TRUE)) {
    filelab <- (paste0("Mplus/Norway_MPlus_",x,".out"))
  } else if ( grepl("old",x,fixed = TRUE)) {
    filelab <- (paste0("Mplus/Norway_MPlus_",x,".out"))
  } else {
    filelab <- (paste0("Mplus/Norway_MPlus_",x,".out"))
  }
  
  # Extract estimates of the new parameters from .out file.
  main.est <- readModels(filefilter = filelab, what = "parameters")$parameters
  str(main.est)
  
  # Look at main.est. It is a table and the point estimates are in column 3.
  main.est <- data.frame(main.est)
  main.est
  # Grab the scale parameter from the last row of main.est.
  main.scale <- main.est[nrow(main.est),3]
  # Grab the coefficients (constant, z1_coef, z2_coef, x_coef, xp_coef).
  main.coefs <- main.est[c((nrow(main.est) - 5):(nrow(main.est) - 1)),3]
  
  main.p00 <- mean(pnorm((dat.00 %*% main.coefs)/main.scale))
  main.p11 <- mean(pnorm((dat.11 %*% main.coefs)/main.scale))
  main.p10 <- mean(pnorm((dat.10 %*% main.coefs)/main.scale))
  main.p01 <- mean(pnorm((dat.01 %*% main.coefs)/main.scale))
  
  
  ### Compute the causal mediation effects
  main.TE.RD <- main.p11 - main.p00
  main.NDE.0.RD <- main.p10 - main.p00
  main.NIE.1.RD <- main.p11 - main.p10
  main.NIE.0.RD <- main.p01 - main.p00
  main.NDE.1.RD <- main.p11 - main.p01
  
  main.TE.RR <- main.p11 / main.p00
  main.NDE.0.RR <- main.p10 / main.p00
  main.NIE.1.RR <- main.p11 / main.p10
  main.NIE.0.RR <- main.p01 / main.p00
  main.NDE.1.RR <- main.p11 / main.p01
  
  main.TE.OR <- ((main.p11) / (1 - main.p11)) / ((main.p00) / (1 - main.p00))
  main.NDE.0.OR <-
    ((main.p10) / (1 - main.p10)) / ((main.p00) / (1 - main.p00))
  main.NIE.1.OR <-
    ((main.p11) / (1 - main.p11)) / ((main.p10) / (1 - main.p10))
  main.NIE.0.OR <-
    ((main.p01) / (1 - main.p01)) / ((main.p00) / (1 - main.p00))
  main.NDE.1.OR <-
    ((main.p11) / (1 - main.p11)) / ((main.p01) / (1 - main.p01))
  
  # Average effects
  ANDE.RD <- c(main.NDE.0.RD, main.NDE.1.RD)
  ANIE.RD <- c(main.NIE.0.RD, main.NIE.1.RD)
  ANDE.RR <- c(main.NDE.0.RR, main.NDE.1.RR)
  ANIE.RR <- c(main.NIE.0.RR, main.NIE.1.RR)
  ANDE.R <- c(main.NDE.0.OR, main.NDE.1.OR)
  ANIE.OR <- c(main.NIE.0.OR, main.NIE.1.OR)
  
  
  ### Bootstrap for standard errors and confidence intervals - could parallelize this to speed up but careful about parallel reading of written dat files!
  ###########################################################
  ## First, fit the model to bootstrap samples and collect estimates of new parameters.
  boot.n <- nsim # specify number of bootstrap samples
  boot.est.mat <- NULL # initialize matrix to hold estimates of new parameters 
  j <- 0 # initialize counter of bootstrap steps
  #j=1
  while (j < boot.n) { 
    cat(paste("BOOTSTRAP STEP",j + 1))                # print step number to screen
    boot <- dat[sample(1:nrow(dat),replace = TRUE),]  # draw a bootstrap dataset
    # From here the code mimics main analysis.
    
    if ( grepl("yng",x,fixed = TRUE)) {
      datnew <- paste0("Mplus/Mplus_Norway.yng.dat")
    } else if ( grepl("mid",x,fixed = TRUE)) {
      datnew <- paste0("Mplus/Mplus_Norway.mid.dat")
    } else if ( grepl("old",x,fixed = TRUE)) {
      datnew <- paste0("Mplus/Mplus_Norway.old.dat")
    } else {
      datnew <- paste0("Mplus/Mplus_Norway.dat")
    }
    
    MplusAutomation::prepareMplusData(boot, datnew)
    
    if ( grepl("yng",x,fixed = TRUE)) {
      filelabinp <- (paste0("Norway_MPlus_",x,".inp"))
    } else if ( grepl("mid",x,fixed = TRUE)) {
      filelabinp <- (paste0("Norway_MPlus_",x,".inp"))
    } else if ( grepl("old",x,fixed = TRUE)) {
      filelabinp <- (paste0("Norway_MPlus_",x,".inp"))
    } else {
      filelabinp <- (paste0("Norway_MPlus_",x,".inp"))
    }
    runModels(target = "/Users/David/Dropbox/Coding/Mediation/Norway/Mplus", filefilter = filelabinp)
    
    if ( grepl("yng",x,fixed = TRUE)) {
      filelabout <- (paste0("Mplus/Norway_MPlus_",x,".out"))
    } else if ( grepl("mid",x,fixed = TRUE)) {
      filelabout <- (paste0("MPlus/Norway_MPlus_",x,".out"))
    } else if ( grepl("old",x,fixed = TRUE)) {
      filelabout <- (paste0("MPlus/Norway_MPlus_",x,".out"))
    } else {
      filelabout <- (paste0("MPlus/Norway_MPlus_",x,".out"))
    }
    
    boot.est <- readModels(filefilter = filelabout, what = "parameters")$parameters
    boot.est <- as.data.frame(boot.est)
    
    boot.est <- boot.est[,3]
    if (class(boot.est) == "numeric") { # if the run was good,
      boot.est.mat <- rbind(boot.est.mat,boot.est) # store estimates of new parameters
      j <- j + 1 }
  }
  
  rownames(boot.est.mat) <- NULL
  ## Second, combine new parameters with data to compute potential outcome probabilities ## (for each of all the bootstrap datasets).
  
  boot.coefs.mat <- boot.est.mat[,c((ncol(boot.est.mat) - 5):(ncol(boot.est.mat) - 1))]
  boot.scale.vec <- boot.est.mat[, ncol(boot.est.mat)]
  boot.scale.mat <- matrix(rep(boot.scale.vec,n),ncol = n)
  boot.p00 <- rowMeans(pnorm((boot.coefs.mat %*% t(dat.00))/boot.scale.mat)) 
  boot.p11 <- rowMeans(pnorm((boot.coefs.mat %*% t(dat.11))/boot.scale.mat)) 
  boot.p10 <- rowMeans(pnorm((boot.coefs.mat %*% t(dat.10))/boot.scale.mat)) 
  boot.p01 <- rowMeans(pnorm((boot.coefs.mat %*% t(dat.01))/boot.scale.mat))
  
  boot.pxx.eff <- data.frame(cbind(boot.p00,boot.p11,boot.p10,boot.p01)) 
  names(boot.pxx.eff) <- c("p00","p11","p10","p01")
  
  ## Third, compute causal mediation effects (for each of all the bootstrap datasets).
  ## Again, here we include both RD- and RR-based effects and both ways of decomposing TE.
  ## Select the effects of interest in your analysis.
  boot.pxx.eff$TE.RD    <- boot.pxx.eff$p11 - boot.pxx.eff$p00
  boot.pxx.eff$NDE.0.RD <- boot.pxx.eff$p10 - boot.pxx.eff$p00
  boot.pxx.eff$NIE.1.RD <- boot.pxx.eff$p11 - boot.pxx.eff$p10
  boot.pxx.eff$NIE.0.RD <- boot.pxx.eff$p01 - boot.pxx.eff$p00
  boot.pxx.eff$NDE.1.RD <- boot.pxx.eff$p11 - boot.pxx.eff$p01
  boot.pxx.eff$TE.RR    <- boot.pxx.eff$p11/boot.pxx.eff$p00
  boot.pxx.eff$NDE.0.RR <- boot.pxx.eff$p10/boot.pxx.eff$p00
  boot.pxx.eff$NIE.1.RR <- boot.pxx.eff$p11/boot.pxx.eff$p10
  boot.pxx.eff$NIE.0.RR <- boot.pxx.eff$p01/boot.pxx.eff$p00
  boot.pxx.eff$NDE.1.RR <- boot.pxx.eff$p11/boot.pxx.eff$p01
  
  
  boot.ANDE.RD <- mean(c(boot.pxx.eff$NDE.0.RD, boot.pxx.eff$NDE.1.RD))
  boot.ANIE.RD <- mean(c(boot.pxx.eff$NIE.0.RD, boot.pxx.eff$NIE.1.RD))
  boot.ANDE.RR <- mean(c(boot.pxx.eff$NDE.0.RR, boot.pxx.eff$NDE.1.RR))
  boot.ANIE.RR <- mean(c(boot.pxx.eff$NIE.0.RR, boot.pxx.eff$NIE.1.RR))
  
  
  ## Fourth, derive standard errors for causal mediation effects from bootstrap estimates
  boot.p00.se <- sd(boot.p00)
  boot.p11.se <- sd(boot.p11)
  boot.p10.se <- sd(boot.p10)
  boot.p01.se <- sd(boot.p01)
  boot.TE.RD.se    <- sd(boot.pxx.eff$TE.RD)
  boot.NDE.0.RD.se <- sd(boot.pxx.eff$NDE.0.RD)
  boot.NIE.1.RD.se <- sd(boot.pxx.eff$NIE.1.RD)
  boot.NIE.0.RD.se <- sd(boot.pxx.eff$NIE.0.RD)
  boot.NDE.1.RD.se <- sd(boot.pxx.eff$NDE.1.RD)
  boot.TE.RR.se    <- sd(boot.pxx.eff$TE.RR)
  boot.NDE.0.RR.se <- sd(boot.pxx.eff$NDE.0.RR)
  boot.NIE.1.RR.se <- sd(boot.pxx.eff$NIE.1.RR)
  boot.NIE.0.RR.se <- sd(boot.pxx.eff$NIE.0.RR)
  boot.NDE.1.RR.se <- sd(boot.pxx.eff$NDE.1.RR)
  boot.ANIE.RR.se <- sd(c(boot.pxx.eff$NIE.0.RR, boot.pxx.eff$NIE.1.RR))
  boot.ANDE.RR.se <- sd(c(boot.pxx.eff$NDE.0.RR, boot.pxx.eff$NDE.1.RR))
  boot.ANIE.RD.se <- sd(c(boot.pxx.eff$NIE.0.RD, boot.pxx.eff$NIE.1.RD))
  boot.ANDE.RD.se <- sd(c(boot.pxx.eff$NDE.0.RD, boot.pxx.eff$NDE.1.RD))
  ## Fifth, derive confidence intervals for effects from bootstrap estimates
  boot.p00.ci <- quantile(boot.p00,probs = c(.025,.975))
  boot.p11.ci <- quantile(boot.p11,probs = c(.025,.975))
  boot.p10.ci <- quantile(boot.p10,probs = c(.025,.975))
  boot.p01.ci <- quantile(boot.p01,probs = c(.025,.975))
  boot.TE.RD.ci <- quantile(boot.pxx.eff$TE.RD,probs = c(.025,.975))
  boot.NDE.0.RD.ci <- quantile(boot.pxx.eff$NDE.0.RD,probs = c(.025,.975))
  boot.NIE.1.RD.ci <- quantile(boot.pxx.eff$NIE.1.RD,probs = c(.025,.975))
  boot.NIE.0.RD.ci <- quantile(boot.pxx.eff$NIE.0.RD,probs = c(.025,.975))
  boot.NDE.1.RD.ci <- quantile(boot.pxx.eff$NDE.1.RD,probs = c(.025,.975))
  boot.TE.RR.ci <- quantile(boot.pxx.eff$TE.RR,probs = c(.025,.975))
  boot.NDE.0.RR.ci <- quantile(boot.pxx.eff$NDE.0.RR,probs = c(.025,.975))
  boot.NIE.1.RR.ci <- quantile(boot.pxx.eff$NIE.1.RR,probs = c(.025,.975))
  boot.NIE.0.RR.ci <- quantile(boot.pxx.eff$NIE.0.RR,probs = c(.025,.975))
  boot.NDE.1.RR.ci <- quantile(boot.pxx.eff$NDE.1.RR,probs = c(.025,.975))
  boot.ANIE.RR.ci <- quantile(c(boot.pxx.eff$NIE.0.RR, boot.pxx.eff$NIE.1.RR),probs = c(.025,.975))
  boot.ANDE.RR.ci <- quantile(c(boot.pxx.eff$NDE.0.RR, boot.pxx.eff$NDE.1.RR),probs = c(.025,.975))
  boot.ANIE.RD.ci <- quantile(c(boot.pxx.eff$NIE.0.RD, boot.pxx.eff$NIE.1.RD),probs = c(.025,.975))
  boot.ANDE.RD.ci <- quantile(c(boot.pxx.eff$NDE.0.RD, boot.pxx.eff$NDE.1.RD),probs = c(.025,.975))
  
  ### Combine point estimates, SEs and CIs in a summary table
  ############################################################
  estimate <- c(main.p00,main.p11, main.p10,main.p01,main.TE.RD,main.NDE.0.RD,main.NIE.1.RD,main.NIE.0.RD,main.NDE.1.RD,main.TE.RR,main.NDE.0.RR,main.NIE.1.RR,main.NIE.0.RR,main.NDE.1.RR,boot.ANDE.RD,boot.ANIE.RD,boot.ANDE.RR,boot.ANIE.RR)
  se <- c(boot.p00.se,boot.p11.se,boot.p10.se,boot.p01.se,boot.TE.RD.se,boot.NDE.0.RD.se,boot.NIE.1.RD.se,boot.NIE.0.RD.se,boot.NDE.1.RD.se, boot.TE.RR.se,boot.NDE.0.RR.se,boot.NIE.1.RR.se,boot.NIE.0.RR.se,boot.NDE.1.RR.se,boot.ANDE.RD.se,boot.ANIE.RD.se,boot.ANDE.RR.se,boot.ANIE.RR.se)
  ll <- c(boot.p00.ci[1],
          boot.p11.ci[1],
          boot.p10.ci[1],
          boot.p01.ci[1],
          boot.TE.RD.ci[1],
          boot.NDE.0.RD.ci[1],
          boot.NIE.1.RD.ci[1],
          boot.NIE.0.RD.ci[1],
          boot.NDE.1.RD.ci[1],
          boot.TE.RR.ci[1],
          boot.NDE.0.RR.ci[1],
          boot.NIE.1.RR.ci[1],
          boot.NIE.0.RR.ci[1],
          boot.NDE.1.RR.ci[1],
          boot.ANDE.RD.ci[1],
          boot.ANIE.RD.ci[1],
          boot.ANDE.RR.ci[1],
          boot.ANIE.RR.ci[1])
  ul <- c(boot.p00.ci[2],
          boot.p11.ci[2],
          boot.p10.ci[2],
          boot.p01.ci[2],
          boot.TE.RD.ci[2],
          boot.NDE.0.RD.ci[2],
          boot.NIE.1.RD.ci[2],
          boot.NIE.0.RD.ci[2],
          boot.NDE.1.RD.ci[2],
          boot.TE.RR.ci[2],
          boot.NDE.0.RR.ci[2],
          boot.NIE.1.RR.ci[2],
          boot.NIE.0.RR.ci[2],
          boot.NDE.1.RR.ci[2],
          boot.ANDE.RD.ci[2],
          boot.ANIE.RD.ci[2],
          boot.ANDE.RR.ci[2],
          boot.ANIE.RR.ci[2])
  summary <- cbind(estimate,se,ll,ul)
  
  rownames(summary) <-
    c(
      "p00",
      "p11",
      "p10",
      "p01",
      "TE.RD",
      "NDE.0.RD",
      "NIE.1.RD",
      "NIE.0.RD",
      "NDE.1.RD",
      "TE.RR",
      "NDE.0.RR",
      "NIE.1.RR",
      "NIE.0.RR",
      "NDE.1.RR",
      "ANDE.RD",
      "ANIE.RD",
      "ANDE.RR",
      "ANIE.RR")
  
  colnames(summary) <-
    c("estimate", "SE", "2.5%", "97.5%")
  
  
  # save for final table
  saveRDS(summary,paste0("Mplus/summary_",x,".rds"))
  
}) # end lapply for x


# create fancy table for paper
tab1 <- readRDS("Mplus/summary_PAREDU2_DRINKER_COMPLEX.rds")
tab2 <- readRDS("Mplus/summary_PAREDU2_DRUNK_COMPLEX.rds")
tab3 <- readRDS("Mplus/summary_margSEP_DRINKER_COMPLEX.rds")
tab4 <- readRDS("Mplus/summary_margSEP_DRUNK_COMPLEX.rds")

tab5 <- readRDS("Mplus/summary_PAREDU2_DRINKER_COMPLEX_yng.rds")
tab6 <- readRDS("Mplus/summary_PAREDU2_DRUNK_COMPLEX_yng.rds")
tab7 <- readRDS("Mplus/summary_margSEP_DRINKER_COMPLEX_yng.rds")
tab8 <- readRDS("Mplus/summary_margSEP_DRUNK_COMPLEX_yng.rds")

tab9 <- readRDS("Mplus/summary_PAREDU2_DRINKER_COMPLEX_mid.rds")
tab10 <- readRDS("Mplus/summary_PAREDU2_DRUNK_COMPLEX_mid.rds")
tab11 <- readRDS("Mplus/summary_margSEP_DRINKER_COMPLEX_mid.rds")
tab12 <- readRDS("Mplus/summary_margSEP_DRUNK_COMPLEX_mid.rds")

tab13 <- readRDS("Mplus/summary_PAREDU2_DRINKER_COMPLEX_old.rds")
tab14 <- readRDS("Mplus/summary_PAREDU2_DRUNK_COMPLEX_old.rds")
tab15 <- readRDS("Mplus/summary_margSEP_DRINKER_COMPLEX_old.rds")
tab16 <- readRDS("Mplus/summary_margSEP_DRUNK_COMPLEX_old.rds")


tabloop <- sprintf("tab%d",seq(1:16))
#i <- 1
tabout <- lapply(seq_along(tabloop), function(i) {
    x <- get(paste0("tab",i))
    x <- x[c(7,10,11,14,17,18),-c(2,5)]
    x <- rbind(x[1:3,]*100, x[4:6,])
    x <- rbind(format(round(x[1:3,], 1), nsmall = 1), format(round(x[4:6,], 2), nsmall = 2))
    x <- cbind(rownames(x), data.frame(x, row.names = NULL))
    colnames(x) <- c("Effect","Estimate","LCI","UCI")
    x <- x %>% 
      mutate(Estimate = paste0(Estimate," (",LCI," to ",UCI,")"))
    x$Estimate <- gsub("\\( ", "\\(", x$Estimate)
    x <- x[-c(3:4)]
})

taball1 <- do.call(cbind, tabout[1:4])
taball1 <- cbind(taball1[1:3,],taball1[4:6,])
taball1 <- taball1[c(1,2,10,4,12,6,14,8,16)]
taball1[,1] <- as.character(taball1[,1])
taball1[1,1] <- "Natural Indirect"
taball1[2,1] <- "Natural Direct"
taball1[3,1] <- "Total"

taball2 <- do.call(cbind, tabout[5:8])
taball2 <- cbind(taball2[1:3,],taball2[4:6,])
taball2 <- taball2[c(1,2,10,4,12,6,14,8,16)]
taball2[,1] <- as.character(taball2[,1])
taball2[1,1] <- "Natural Indirect"
taball2[2,1] <- "Natural Direct"
taball2[3,1] <- "Total"

taball3 <- do.call(cbind, tabout[9:12])
taball3 <- cbind(taball3[1:3,],taball3[4:6,])
taball3 <- taball3[c(1,2,10,4,12,6,14,8,16)]
taball3[,1] <- as.character(taball3[,1])
taball3[1,1] <- "Natural Indirect"
taball3[2,1] <- "Natural Direct"
taball3[3,1] <- "Total Effect"

taball4 <- do.call(cbind, tabout[13:16])
taball4 <- cbind(taball4[1:3,],taball4[4:6,])
taball4 <- taball4[c(1,2,10,4,12,6,14,8,16)]
taball4[,1] <- as.character(taball4[,1])
taball4[1,1] <- "Natural Indirect"
taball4[2,1] <- "Natural Direct"
taball4[3,1] <- "Total"

taball <- rbind(taball1, taball2, taball3, taball4)
colnames(taball) <- c("Effect","Drinker ~ Parental Education","Drinker ~ Parental Education","Drunk ~ Parental Education","Drunk ~ Parental Education","Drinker ~ Marginal SEP","Drinker ~ Marginal SEP","Drunk ~ Marginal SEP","Drunk ~ Marginal SEP")

taball$Age <- c("All ages","All ages","All ages",
                "13-14 year-olds","13-14 year-olds","13-14 year-olds",
                "15-16 year-olds","15-16 year-olds","15-16 year-olds",
                "17-18 year-olds","17-18 year-olds","17-18 year-olds")
taball <- taball[c(10,1:9)]


flextab4 <- flextable(taball) 
flextab4 <- merge_at(flextab4, i = 1, j = 3:4, part = "header")
flextab4 <- merge_at(flextab4, i = 1, j = 5:6, part = "header")
flextab4 <- merge_at(flextab4, i = 1, j = 7:8, part = "header")
flextab4 <- merge_at(flextab4, i = 1, j = 9:10, part = "header")
flextab4 <- add_header_row(flextab4,
                       top = FALSE,
                       values = c("",
                                  "",
                                  "RD (%)",
                                  "RR",
                                  "RD (%)",
                                  "RR",
                                  "RD (%)",
                                  "RR",
                                  "RD (%)",
                                  "RR"))
flextab4 <- align(flextab4, i = 2, j = 3:10, align = "center", part = "header")
flextab4 <- add_footer_row(flextab4, 
                           top = FALSE, 
                           values = "RD is Risk Difference; RR is Relative Risk", 
                           colwidths = 10)
flextab4 <- merge_v(flextab4, j = ~ Age )
flextab4 <- valign(flextab4, j = 1, valign = "top", part = "body")
flextab4 <- align(flextab4, j = 1, align = "left", part = "body")
flextab4 <- hline(flextab4, i = c(3,6,9), j = 1:10, border =  fp_border(color = "gray"), part = "body")
flextab4 <- autofit(flextab4)
flextab4 <- width(flextab4, j = 3:10, width = 1.2)
flextab4 <- italic(flextab4, part = "footer")
flextab4 <- set_header_labels(flextab4, Age = "")
flextab4 <- align(flextab4, align = "right", part = "footer" )
flextab4 <- hline_top(flextab4, border = fp_border(color = "black", width = 2), part = "footer")
flextab4

save_as_docx(flextab4, path="Output/flextab4_table.docx")


saveRDS(taball, "Output/taball_COMPLEX.rds")
saveRDS(flextab4, "Output/flextab4_COMPLEX.rds")
save_as_image(flextab4, "Output/flextab4_COMPLEX.png")


taball <- readRDS("Output/taball_COMPLEX.rds")
flextab4 <- readRDS("Output/flextab4_COMPLEX.rds")

# plot by agegrp
tabout2 <- lapply(seq_along(tabloop), function(i) {
  x <- get(paste0("tab",i))
  x <- x[c(7,10,11,14,17,18),-c(2,5)]
  x <- x[4:6,]
  x <- cbind(rownames(x), data.frame(x, row.names = NULL))
  colnames(x) <- c("Effect","RR","LCI","UCI")
  x
})
tabout2 <- tabout2[5:16] # select only age subgroups
tabout3 <- bind_rows(tabout2) %>%
  mutate(Effect = dplyr::recode(Effect, 
                         ANIE.RR = "Natural Indirect\n Effect",
                         ANDE.RR = "Natural Direct\n Effect",
                         TE.RR = "Total\n Effect"))
tabout3

plotnames1 <- rep(c(rep("Drinker",3),
rep("Drunk",3),
rep("Drinker",3),
rep("Drunk",3)),3)
plotnames2 <- rep(c(rep("Parental Education",3),
                    rep("Parental Education",3),
                    rep("Marginal SEP",3),
                    rep("Marginal SEP",3)),3)
agegrps <- c(rep("13-14",12),
              rep("15-16",12),
              rep("17-18",12))

tabout3 <- cbind(plotnames1,plotnames2,agegrps,tabout3) 
subplots <-  tabout3 %>%
  mutate(across(plotnames2, factor, levels=c("Parental Education","Marginal SEP"))) %>% 
  ggplot(aes(x=Effect, y=RR)) + 
  geom_point(aes(shape=agegrps, color=agegrps), position = position_dodge(width=0.6)) + 
  geom_pointrange(aes(shape=agegrps, color=agegrps, ymin=LCI, ymax=UCI), position = position_dodge(width=0.6)) + 
  labs(x = NULL, y = "Relative Risk", color = "Age group", shape = "Age group") +
    facet_grid(plotnames2 ~ plotnames1)
subplots
dev.off()
ggsave("Output/subplot.jpg", units="in", width=9, height=5,  device='jpg', dpi=700)


#### E-values -------

Nordat3 <- readRDS("Data/Nordat2.rds")
table(Nordat3$AGEYRS)
#select 13-14 year-olds only
Nordat4 <- subset(Nordat3, AGEYRS <= 14)
table(Nordat4$AGEYRS)

# re-run model - full sample
pape <- lme4::glmer(DRUNK ~ PAREDU2 + GENDER + AGEYRS + (1|SCHNo),  family = poisson(link = log), data = Nordat3, control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))) # different optimizer
#summary(pape)
#get PRs
confmeth <- list(object = pape, parm = "beta_" , method = "Wald") 
#confmeth <- list(object = pape, parm = "beta_" , method = "boot", nsim = nsim, parallel = "multicore", ncpus = ncpus)
confmeth
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape),cc)
#exp(ctab)
print(exp(ctab),digits = 3)

# re-run model - 13-14year-olds
pape <- lme4::glmer(DRUNK ~ PAREDU2 + GENDER + AGEYRS + (1|SCHNo),  family = poisson(link = log), data = Nordat4, control = glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))) # different optimizer
#summary(pape)
#get PRs
confmeth <- list(object = pape, parm = "beta_" , method = "Wald") 
#confmeth <- list(object = pape, parm = "beta_" , method = "boot", nsim = nsim, parallel = "multicore", ncpus = ncpus)
confmeth
confmeth <- modifyList(confmeth, list(object = pape))
cc <- do.call(confint, confmeth)
ctab <- cbind(est = fixef(pape),cc)
#exp(ctab)
print(exp(ctab),digits = 3)


# exposure - outcome evalues from Evalue R-package
# RR values taken from re-run models, above
evalue(RR(1.13), 1.05) # full sample and drunk~parental education model
evalue(RR(1.81), 1.35) # 13-14 year-olds and drunk~parental education model

# mediational e-value, after Smith & VanderWeele https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6768718/ 
med_eval <- function (x) {    # where x is model RR
  eval <- x + (sqrt(x * x -1)) 
}

#RR values taken from Table 3 and Appendix D
(med_eval(1.25)) # full sample and drunk~parental education model
(med_eval(1.21)) # lower bound: full sample and drunk~parental education model

(med_eval(1.83)) # 13-14 year-olds and drunk~parental education model
(med_eval(1.61)) # lower bound: 13-14year-olds full sample and drunk~parental education model



# plots - change file name per analysis
#source("https://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
source("MPlus.R")

#x <- "PAREDU2_DRINKER"

lapply(modelset, function(x) {
  mplus.list.bayesian.parameters(paste0("MPlus/Norway_MPlus_", x, ".gh5"))

  png(
    paste0("MPlus/bayesian_distribution_", x, ".png"),
    res = 300,
    width = 7,
    height = 7,
    units = "in"
  )
  par(mfrow = c(2,1))
  lapply(c(17,46), function(i) {
    mplus.plot.bayesian.distribution(paste0("MPlus/Norway_MPlus_", x, ".gh5"), i)
  })
  dev.off()
  
  png(
    paste0("MPlus/bayesian_traceplot_", x, ".png"),
    res = 300,
    width = 7,
    height = 7,
    units = "in"
  )
  par(mfrow = c(2,1))
  lapply(c(17,46), function(i) {
    mplus.plot.bayesian.traceplot(paste0("MPlus/Norway_MPlus_", x, ".gh5"), i)
  })
  dev.off()
  
  png(
    paste0("MPlus/bayesian_autocorrelation_", x, ".png"),
    res = 300,
    width = 7,
    height = 7,
    units = "in"
  )
  par(mfrow = c(2,1))
  lapply(c(17,46), function(i) {
    mplus.plot.bayesian.autocorrelation(paste0("MPlus/Norway_MPlus_", x, ".gh5"), i)
  })
  dev.off()
  
})


#Unused##################################################################################

# ANALYSES AND CODE FOR APPENDIX BELOW (PERHAPS)


#mediation###########################################################################

# Imai"s mediation package check for moderation / interaction
# outputs figures 
#source("Imai_mediation_moderation_check.R")


#sensitivity###########################################################################
# sensitivity mediator analysis in Imai"s R mediation
## sensitivity analysis - can"t use interaction effect or multilevel parameter
# currently only specified for one model set (PAREDU2 ~ DRINKER) 
#source("Imai_mediation_sensitivity_analysis.R")



#mutliple mediators######################################################################
# multiple mediators & post-exposure confounding - see vignette section 6
## first regress M1 on M2 and X and Zs, then regress M2 on X and Zs - see medsens article e.g. sect. 4.1.3 - https://imai.princeton.edu/research/files/medsens.pdf

# currently only specified for one model set (PAREDU2 ~ DRINKER) 
#source("Imai_mediation_multiple_mediators.R")

#medflex###########################################################################

# multiple mediator analysis in R medflex - can"t deal with multilevel model and package no longer being actively developed; also fits linear rather than non-linear models....so less useful... 
# https://cran.r-project.org/web/packages/medflex/vignettes/medflex.pdf
#source("medflex_multiple_mediators.R")

# file end

