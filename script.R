# R code for the fire x mammal roadkill article

# Reading packages
{
  library(bbmle)
  library(numDeriv)
  library(effects)  
  library(corrplot)
  library(ggplot2)
  library(Hmisc)
  library(lme4)
  library(lmerTest)
  library(png)
  library(grid)
  library(DHARMa)
}

# Reading data file
fire_tab <- read.csv('G:/Meu Drive/Universidade/LEQuE/TG TALES/FOGOxATROP.csv', header = TRUE, sep = ";",dec = ",")

# Checking column names
names(fire_tab)

# Converting some variables into factors, mainly those used as random variables
fire_tab$ID_FOGO <- as.factor(fire_tab$ID_FOGO)
fire_tab$FOGO <- as.factor(fire_tab$FOGO)
fire_tab$RODOVIA <- as.factor(fire_tab$RODOVIA)
fire_tab$BIOMA <- as.factor(fire_tab$BIOMA)
# Change values so that the shortest period is used as basal level
fire_tab$PERIODO[fire_tab$PERIODO == '-3/-1 e 0/2'] <- '-03/-1 e 0/2'
fire_tab$PERIODO <- as.factor(fire_tab$PERIODO)

# Creates columns with the percentage of the buffer covered by each land use and land cover, according to information extract from MapBiomas v5
fire_tab$FLORESTA_porc <- fire_tab$FLORESTA / (pi*2000^2)
fire_tab$AGROPECUARIA_porc <- fire_tab$AGROPECUARIA / (pi*2000^2)
fire_tab$NATURAL_NAO_FLORESTAL_porc <- fire_tab$NATURAL_NAO_FLORESTAL / (pi*2000^2)
fire_tab$AREA_NAO_VEGETADA_porc <- fire_tab$AREA_NAO_VEGETADA / (pi*2000^2)
fire_tab$AGUA_porc <- fire_tab$AGUA / (pi*2000^2)

# Divide the table by period
fire_tab3 <- fire_tab[fire_tab$PERIODO == '-3/-1 e 0/2',]
fire_tab10 <- fire_tab[fire_tab$PERIODO == '-11/-1 e 0/10',]
fire_tab30 <- fire_tab[fire_tab$PERIODO == '-31/-1 e 0/30',]

# Create full models, including all variables and interactions, for all periods
fit_fire3_full <- glmer(N_ATROP ~ FOGO + BIOMA +FLORESTA_porc + AGROPECUARIA_porc + 
                          NATURAL_NAO_FLORESTAL_porc + AREA_NAO_VEGETADA_porc +
                          AGUA_porc + DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                        data = fire_tab3, family = 'poisson')

# Analyse the summary and remove non-significative variables, one by one, checking the summary with each removal
summary(fit_fire3_full)
# Remove NATURAL_NAO_FLORESTAL_porc
fit_fire3_simp1 <- glmer(N_ATROP ~ FOGO + BIOMA +FLORESTA_porc + AGROPECUARIA_porc + 
                           AREA_NAO_VEGETADA_porc +
                           AGUA_porc + DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp1)
# Remove AGUA_porc
fit_fire3_simp2 <- glmer(N_ATROP ~ FOGO + BIOMA +FLORESTA_porc + AGROPECUARIA_porc + 
                           AREA_NAO_VEGETADA_porc +
                           DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp2)
# Remove FLORESTA_porc
fit_fire3_simp3 <- glmer(N_ATROP ~ FOGO + BIOMA + AGROPECUARIA_porc + 
                           AREA_NAO_VEGETADA_porc +
                           DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp3)
# Remove AGROPECUARIA_porc
fit_fire3_simp4 <- glmer(N_ATROP ~ FOGO + BIOMA + 
                           AREA_NAO_VEGETADA_porc +
                           DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp4)
# Remove AREA_NAO_VEGETADA_porc
fit_fire3_simp5 <- glmer(N_ATROP ~ FOGO + BIOMA +
                           DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp5)
# Remove BIOMA
fit_fire3_simp6 <- glmer(N_ATROP ~ FOGO +
                           DIST_ROAD + DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp6)
# Remove DIST_ROAD
fit_fire3_simp7 <- glmer(N_ATROP ~ FOGO +
                           DENS_ROAD + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_simp7)
# Remove DENS_ROAD
fit_fire3_final <- glmer(N_ATROP ~ FOGO + (1|ID_FOGO) + (1|RODOVIA),
                         data = fire_tab3, family = 'poisson')
summary(fit_fire3_final)



# The same process was applied to the 10-days and 30-days periods
fit_fire10_final <- glmer(N_ATROP ~ FOGO + (1|ID_FOGO) + (1|RODOVIA),
                          data = fire_tab10, family = 'poisson')
summary(fit_fire10_final)

fit_fire30_final <- glmer(N_ATROP ~ FOGO + (1|ID_FOGO) + (1|RODOVIA),
                          data = fire_tab30, family = 'poisson')
summary(fit_fire30_final)


# Plot the effect of fire incidence on roadkill
fire_eff3 <- predictorEffect("FOGO", fit_fire3_final)
x11(width = 20, height = 20)
fire_eff3$variables$FOGO$levels <- c("N?O", "SIM")
levels(fire_eff3$x$FOGO) <- c("N?O", "SIM")
plot(fire_eff3, main ='', rug = F, lines = list (col = "black"),
     axes = list(x=list(FOGO = list(lab = "Presen?a de inc?ndio")),
                 y = list(lab="N?mero de atropelamentos de fauna\n(Escala logar?tmica)")))

fire_eff10 <- predictorEffect("FOGO", fit_fire10_final)
x11(width = 20, height = 20)
fire_eff10$variables$FOGO$levels <- c("N?O", "SIM")
levels(fire_eff10$x$FOGO) <- c("N?O", "SIM")
plot(fire_eff10, main ='', rug = F, lines = list (col = "black"),
     axes = list(x=list(FOGO = list(lab = "Presen?a de inc?ndio")),
                 y = list(lab="N?mero de atropelamentos de fauna\n(Escala logar?tmica)")))

fire_eff30 <- predictorEffect("FOGO", fit_fire30_final)
x11(width = 20, height = 20)
fire_eff30$variables$FOGO$levels <- c("N?O", "SIM")
levels(fire_eff30$x$FOGO) <- c("N?O", "SIM")
plot(fire_eff30, main ='', rug = F, lines = list (col = "black"),
     axes = list(x=list(FOGO = list(lab = "Presen?a de inc?ndio")),
                 y = list(lab="N?mero de atropelamentos de fauna\n(Escala logar?tmica)")))






# TESTS - REMOVE BEFORE PUBLISHING THE FINAL VERSION
{
  
  fit_fire <- glmer(N_ATROP ~ FOGO + FLORESTA + NATURAL_NAO_FLORESTAL + AGROPECUARIA + 
                      DIST_ROAD + DENS_ROAD + BIOMA + (1|ID_FOGO)+(1|RODOVIA),
                    data = fire_tab, family = 'poisson')
  
  
  fit_fire <- glmer(N_ATROP ~ FOGO + FLORESTA_porc + NATURAL_NAO_FLORESTAL_porc + AGROPECUARIA_porc + 
                      DIST_ROAD + DENS_ROAD + BIOMA +(1|ID_FOGO) + (1|RODOVIA),
                    data = fire_tab, family = 'poisson')
  
  summary(fit_fire)
  
  
  fit_fire3 <- glmer(N_ATROP ~ FOGO + (1|ID_FOGO) + (1|RODOVIA),
                     data = fire_tab3, family = 'poisson')
  summary(fit_fire3)
  
  fit_fire10 <- glmer(N_ATROP ~ FOGO + (1|ID_FOGO) + (1|RODOVIA),
                      data = fire_tab10, family = 'poisson')
  summary(fit_fire10)
  
  fit_fire30 <- glmer(N_ATROP ~ FOGO + (1|ID_FOGO) + (1|RODOVIA),
                      data = fire_tab30, family = 'poisson')
  summary(fit_fire30)
  
  
  fire_eff3 <- predictorEffect("FOGO", fit_fire3)
  plot(fire_eff3)
  fire_eff10 <- predictorEffect("FOGO", fit_fire10)
  plot(fire_eff10)
  fire_eff30 <- predictorEffect("FOGO", fit_fire30)
  plot(fire_eff30)
  
  
  fit_fire <- glmer(N_ATROP ~ FOGO * PERIODO + (1|ID_FOGO) + (1|RODOVIA),
                    data = fire_tab, family = 'poisson')
  summary(fit_fire)
  fire_eff <- predictorEffect("FOGO", fit_fire)
  plot(fire_eff)
  
  
  
  
  fit_fire30_full <- glmer(N_ATROP ~ FOGO +FLORESTA_porc + AGROPECUARIA_porc + 
                             AREA_NAO_VEGETADA_porc + AGUA_porc + (1|ID_FOGO) + (1|RODOVIA),
                           data = fire_tab30, family = 'poisson')
  summary(fit_fire30_full)
  
}



