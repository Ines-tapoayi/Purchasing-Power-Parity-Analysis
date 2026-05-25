library(BCDating)
library(lubridate)
library(mFilter)
library(taceconomics)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(zoo)
library(caret)
library(purrr)
library(readxl)
library(dplyr)
library(ggplot2)
library(xts)
library(vars)

# Configuration de la clé API
apikey <- ""
taceconomics.apikey(apikey)

#Taux de change nominal NER
ner_fr  <- getdata("OECD/KEI_M_CC_XDC_USD__Z__Z__Z/FRA?start_date=2002" )
ner_de  <- getdata("OECD/KEI_M_CC_XDC_USD__Z__Z__Z/DEU?start_date=2002" )
ner_it  <- getdata("OECD/KEI_M_CC_XDC_USD__Z__Z__Z/ITA?start_date=2002" )
ner_au  <- getdata("OECD/KEI_M_CC_XDC_USD__Z__Z__Z/AUT?start_date=2002" )
ner_be  <- getdata("OECD/KEI_M_CC_XDC_USD__Z__Z__Z/BEL?start_date=2002" )

#Indice des prix IPC
ipc_fr    <- getdata("GEM/CPTOTNSXN_M/FRA?start_date=2002")
ipc_de    <- getdata("GEM/CPTOTNSXN_M/DEU?start_date=2002")
ipc_it    <- getdata("GEM/CPTOTNSXN_M/ITA?start_date=2002")
ipc_au    <- getdata("GEM/CPTOTNSXN_M/AUT?start_date=2002")
ipc_be    <- getdata("GEM/CPTOTNSXN_M/BEL?start_date=2002")
ipc_us    <- getdata("GEM/CPTOTNSXN_M/USA?start_date=2002")

df_init <- cbind(ner_fr, ner_de, ner_it, ner_au, ner_be, ipc_fr, ipc_de, ipc_it,
                 ipc_au, ipc_be, ipc_us)

df_init <- na.omit(df_init)

colnames(df_init) <- c("ner_fr", "ner_de", "ner_it", "ner_au", "ner_be", "ipc_fr", "ipc_de", "ipc_it",
                       "ipc_au", "ipc_be", "ipc_us")

#Calcul taux Change Réel RER = NER * (IPC^étranger / IPC^national)
#l'étranger est représenter par les USA
df_init$rer_fr <- df_init$ner_fr * (df_init$ipc_us / df_init$ipc_fr)
df_init$rer_de <- df_init$ner_de * (df_init$ipc_us / df_init$ipc_de)
df_init$rer_it <- df_init$ner_it * (df_init$ipc_us / df_init$ipc_it)
df_init$rer_au <- df_init$ner_au * (df_init$ipc_us / df_init$ipc_au)
df_init$rer_be <- df_init$ner_be * (df_init$ipc_us / df_init$ipc_be)

#Plot
df_wide <- as.data.frame(df_init[, 12:16])
df_wide$Date <- index(df_init) # On récupère les dates

ggplot(df_wide, aes(x = Date)) +
  # On ajoute chaque pays manuellement
  geom_line(aes(y = rer_fr, color = "France"), linewidth = 0.7) +
  geom_line(aes(y = rer_de, color = "Allemagne"), linewidth = 0.7) +
  geom_line(aes(y = rer_it, color = "Italie"), linewidth = 0.7) +
  geom_line(aes(y = rer_au, color = "Autriche"), linewidth = 0.7) +
  geom_line(aes(y = rer_be, color = "Belgique"), linewidth = 0.7) +
  
  # Équilibre théorique de la PPA (Ligne à 1)
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
  
  # Personnalisation des titres et couleurs
  labs(title = "Taux de Change Réels",
       x = "Date",
       y = "RER",
       color = "Pays") +
  scale_color_manual(values = c("France" = "blue", 
                                "Allemagne" = "red", 
                                "Italie" = "green", 
                                "Autriche" = "orange", 
                                "Belgique" = "purple")) +
  theme_minimal()

#Observation d'une rupture en 2015 approximativement. Cette rupture est causée par la différence d'IPC entre les différents (NER commun car zone euro) 
#Nous allons diviser nos données en 2 en incluant une rupture en 2015


# Partie 1 : Tout ce qui est AVANT 2015
df_14 <- df_init["/2014"]

# Partie 2 : Tout ce qui est à partir du 1er janvier 2015
df_15 <- df_init["2015/"]

#Test adf
rer_fr <- df_init[, "rer_fr"]
names(rer_fr) <- "rer" 
rer_be <- df_init[, "rer_be"]
names(rer_be) <- "rer" 
rer_au <- df_init[, "rer_au"]
names(rer_au) <- "rer" 
rer_de <- df_init[, "rer_de"]
names(rer_de) <- "rer" 
rer_it <- df_init[, "rer_it"]
names(rer_it) <- "rer" 

summary(ur.df(rer_fr, type = "trend", lags = 12, selectlags="AIC"))
#La tendance TS de la série est sgnificative : t-stat_trend = 3.282 > tt_adf (5%) = 2.78, présence de RU


rer_fr14 <- df_14[, "rer_fr"]
names(rer_fr14) <- "rer" 
summary(ur.df(rer_fr14, type = "trend", lags = 12, selectlags="AIC"))

rer_fr15 <- df_15[, "rer_fr"]
names(rer_fr15) <- "rer" 
summary(ur.df(rer_fr15, type = "trend", lags = 12, selectlags="AIC"))


#Econométie

df_eco <- df_init[,-c(12:16)]

df_eco$lner_fr <- log(df_eco$ner_fr)
df_eco$lner_de <- log(df_eco$ner_de)
df_eco$lner_it <- log(df_eco$ner_it)
df_eco$lner_au <- log(df_eco$ner_au)
df_eco$lner_be <- log(df_eco$ner_be)

df_eco$lp_fr <- log(df_eco$ipc_us / df_eco$ipc_fr)
df_eco$lp_de <- log(df_eco$ipc_us / df_eco$ipc_de)
df_eco$lp_it <- log(df_eco$ipc_us / df_eco$ipc_it)
df_eco$lp_au <- log(df_eco$ipc_us / df_eco$ipc_au)
df_eco$lp_be <- log(df_eco$ipc_us / df_eco$ipc_be)

# df_eco$lrer_fr <- df_eco$lner_fr + df_eco$lp_fr
# df_eco$lrer_de <- df_eco$lner_de + df_eco$lp_de
# df_eco$lrer_it <- df_eco$lner_it + df_eco$lp_it
# df_eco$lrer_au <- df_eco$lner_au + df_eco$lp_au
# df_eco$lrer_be <- df_eco$lner_be + df_eco$lp_be

model_fr <- lm(lner_fr ~ lp_fr, data = df_eco)
summary(model_fr)
model_be <- lm(lner_be ~ lp_be, data = df_eco)
summary(model_be)
model_de <- lm(lner_de ~  lp_de, data = df_eco)
summary(model_de)
model_au <- lm(lner_au ~  lp_au, data = df_eco)
summary(model_au)
model_it <- lm(lner_it ~ lp_it, data = df_eco)
summary(model_it)

residus_fr <- residuals(model_fr)
plot.ts(residus_fr, main ="Fra", ylab="Ecarts", col = "blue")
abline(h=0, col="red", lty=2)

residus_be <- residuals(model_be)
plot.ts(residus_be, main ="Bel", ylab="Ecarts", col = "blue")
abline(h=0, col="red", lty=2)

residus_de <- residuals(model_de)
plot.ts(residus_de, main ="Ger", ylab="Ecarts", col = "blue")
abline(h=0, col="red", lty=2)

residus_it <- residuals(model_it)
plot.ts(residus_it, main ="Ita", ylab="Ecarts", col = "blue")
abline(h=0, col="red", lty=2)

residus_au <- residuals(model_au)
plot.ts(residus_au, main ="Aus", ylab="Ecarts", col = "blue")
abline(h=0, col="red", lty=2)



