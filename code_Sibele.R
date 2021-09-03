#################################################
###########   MODELOS  - SIBELE   ###############
#################################################



### PACOTES --------------------------------------------------------------

library(car)
library(systemfit)
library(bmem)
library(mediation)
library(powerMediation)
library(RMediation)
library(psych)
library(MBESS)
library(tidyverse)
library(ggplot2)
library(remotes)
library(ggstatsplot)
library(plotly)
library(lubridate)
library(hrbrthemes)
library(ggfortify)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(ggimage)
library(colorspace)
library(stringr)
library(dplyr)
library(tidyr)
library(mediation)
library(gvlma)
library(ggthemes)
library(viridis)
library(xlsx)
library(WriteXLS)
library(ggpubr)


### DADOS ----------------------------------------------------------------
dados_base <- read.csv2(file.choose(), header = T, sep = ",")

dados <- dados_base
head(dados)
dim(dados)
names(dados)
attach(dados)
dados$E_sector <- as.numeric(as.factor(dados$sic2))

summary(dados["accper"])

sdg <- c(1.233, 1.19, 1.103, 1.022, 0.956, 0.89, 0.808, 0.73, 0.673)

dados$SDG <- NA

dados[dados["accper"] == 2010, ]$SDG <- sdg[1]
dados[dados["accper"] == 2011, ]$SDG <- sdg[2]
dados[dados["accper"] == 2012, ]$SDG <- sdg[3]
dados[dados["accper"] == 2013, ]$SDG <- sdg[4]
dados[dados["accper"] == 2014, ]$SDG <- sdg[5]
dados[dados["accper"] == 2015, ]$SDG <- sdg[6]
dados[dados["accper"] == 2016, ]$SDG <- sdg[7]
dados[dados["accper"] == 2017, ]$SDG <- sdg[8]
dados[dados["accper"] == 2018, ]$SDG <- sdg[9]

sum(is.na(dados['SDG']))

## MEDIDAS RESUMO ----------------------------------------------------------------


summary(dados["C_Innovation"])
summary(dados["sust_score"])
summary(dados["env_score"])
summary(dados["social_score"])

summary(dados["industrycode"])
summary(dados["sic2"])
summary(dados["gov_score"])
summary(dados["X_breadth1"])
summary(dados["X_breadth2"])
summary(dados["SOE"])

summary(dados["Size"])
summary(dados["Lev"])
summary(dados["Age"])
summary(dados["Growth"])
summary(dados["Cash"])
summary(dados["ROE"])

summary(dados["MTB"])
summary(dados["SDG"])


sqrt(var(dados[!(is.na(dados["C_Innovation"])), "C_Innovation"]))
sqrt(var(dados["sust_score"]))
sqrt(var(dados["env_score"]))
sqrt(var(dados["social_score"]))

sqrt(var(dados["industrycode"]))
sqrt(var(dados["sic2"]))
sqrt(var(dados["gov_score"]))
sqrt(var(dados["X_breadth1"]))
sqrt(var(dados["X_breadth2"]))
sqrt(var(dados["SOE"]))

sqrt(var(dados["Size"]))
sqrt(var(dados["Lev"]))
sqrt(var(dados["Age"]))
sqrt(var(dados["Growth"]))
sqrt(var(dados["Cash"]))
sqrt(var(dados["ROE"]))

sqrt(var(dados["MTB"]))
sqrt(var(dados["SDG"]))

length(dados[!(is.na(dados["C_Innovation"])), "C_Innovation"])
length(dados[!(is.na(dados["sust_score"])), "sust_score"])
length(dados[!(is.na(dados["env_score"])), "env_score"])
length(dados[!(is.na(dados["social_score"])), "social_score"])

length(dados[!(is.na(dados["industrycode"])), "industrycode"])
length(dados[!(is.na(dados["sic2"])), "sic2"])
length(dados[!(is.na(dados["gov_score"])), "gov_score"])
length(dados[!(is.na(dados["X_breadth1"])), "X_breadth1"])
length(dados[!(is.na(dados["X_breadth2"])), "X_breadth2"])
length(dados[!(is.na(dados["SOE"])), "SOE"])

length(dados[!(is.na(dados["Size"])), "Size"])
length(dados[!(is.na(dados["Lev"])), "Lev"])
length(dados[!(is.na(dados["Age"])), "Age"])
length(dados[!(is.na(dados["Growth"])), "Growth"])
length(dados[!(is.na(dados["Cash"])), "Cash"])
length(dados[!(is.na(dados["ROE"])), "ROE"])

length(dados[!(is.na(dados["MTB"])), "MTB"])

##-------------
## HIPOTESE 1


# =======
# 1a 

mod_linear1 <- lm(sust_score ~ X_breadth2, 
                data = dados) 

summary(mod_linear1)



##-------------
## HIPOTESE 2


# 2a ---------------------------------

mod_linear2 <- lm(sust_score ~ X_breadth1, 
                data = dados) 

summary(mod_linear2)


##-------------
## HIPOTESE 3



mod_linear3 <- lm(env_score ~ X_breadth2, 
                data = dados) 

summary(mod_linear3)



##-------------
## HIPOTESE 4


mod_linear4 <- lm(env_score ~ X_breadth1, 
                data = dados) 

summary(mod_linear4)


##-------------
## HIPOTESE 5


mod_linear5 <- lm(social_score ~ X_breadth2, 
                data = dados) 

summary(mod_linear5)



##-------------
## HIPOTESE 6


mod_linear6 <- lm(social_score ~ X_breadth1, 
                data = dados) 

summary(mod_linear6)



##-------------
## HIPOTESE 7


mod_linear7 <- lm(gov_score ~ X_breadth2, 
                data = dados) 

summary(mod_linear7)



##-------------
## HIPOTESE 8

mod_linear8 <- lm(gov_score ~ X_breadth1, 
                data = dados) 

summary(mod_linear8)





## -------------------------------------------------------------------------------------------
## MEDIATOR-MODERATORS MODELS ----------------------------------------------------------------
##-------------
## HIPOTESE 1


# =======
# 1a 

mod1a <- lm(SDG ~ X_breadth2, 
               data = dados)

summary(mod1a)

mod_out1a <- lm(sust_score ~ SDG + X_breadth2, 
               data = dados) 

summary(mod_out1a)


gvlma(mod1a) 
gvlma(mod_out1a) 

modelo_mediator1a <- 
  mediation::mediate(mod1a, mod_out1a, 
                     treat = c("X_breadth2"), 
                     mediator = "SDG")



summary(modelo_mediator1a) 

figura_modelo_mediator1 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator1a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 1 - Mediator variable: SDG",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator1a)


# Bootstrap

modelo_mediator_Boot1a <- 
  mediation::mediate(mod1a, mod_out1a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "SDG")

summary(modelo_mediator_Boot1a)

figura_modelo_mediator_Boot1 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot1a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 1 - Mediator variable: SDG. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot1a)



##-------------
## HIPOTESE 2


# 2a ---------------------------------


mod2a <- lm(SDG ~ X_breadth1, 
            data = dados)

summary(mod2a)

mod_out2a <- lm(sust_score ~ SDG + X_breadth1, 
                data = dados) 

summary(mod_out2a)


gvlma(mod2a) 
gvlma(mod_out2a) 

modelo_mediator2a <- 
  mediation::mediate(mod2a, mod_out2a, 
                     treat = c("X_breadth1"), 
                     mediator = "SDG")



summary(modelo_mediator2a) 

figura_modelo_mediator2 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator2a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 2 - Mediator variable: SDG",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator2a)


# Bootstrap

modelo_mediator_Boot2a <- 
  mediation::mediate(mod2a, mod_out2a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "SDG")

summary(modelo_mediator_Boot2a)

figura_modelo_mediator_Boot2 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot2a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 2 - Mediator variable: SDG. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot2a)


##-------------
## HIPOTESE 3

dados$E_sector <- as.numeric(as.factor(dados$sic2))

# 3a -----------------

mod3a <- lm(E_sector ~ X_breadth2, 
            data = dados)

summary(mod3a)

mod_out3a <- lm(env_score ~ E_sector + X_breadth2, 
                data = dados) 

summary(mod_out3a)


gvlma(mod3a) 
gvlma(mod_out3a) 

modelo_mediator3a <- 
  mediation::mediate(mod3a, mod_out3a, 
                     treat = c("X_breadth2"), 
                     mediator = "E_sector")



summary(modelo_mediator3a) 

figura_modelo_mediator3 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator3a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Hypothesis 3a - Mediator variable: E_sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator3a)


# Bootstrap

modelo_mediator_Boot3a <- 
  mediation::mediate(mod3a, mod_out3a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "E_sector")

summary(modelo_mediator_Boot3a)

figura_modelo_mediator_Boot3 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot3a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval ",
    subtitle = "Hypothesis 3a - Mediator variable: E_sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot3a)

# 3b -----------------
attach(dados)

mod3b <- lm(C_Innovation ~ X_breadth2, 
            data = dados)

summary(mod3b)

mod_out3b <- lm(env_score ~ C_Innovation + X_breadth2, 
                data = dados) 

summary(mod_out3b)


gvlma(mod3b) 
gvlma(mod_out3b) 

modelo_mediator3b <- 
  mediation::mediate(mod3b, mod_out3b, 
                     treat = c("X_breadth2"), 
                     mediator = "C_Innovation")



summary(modelo_mediator3b) 

figura_modelo_mediator3 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator3b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 3b - Mediator variable: Corporate Inovation",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator3b)


# Bootstrap

modelo_mediator_Boot3b <- 
  mediation::mediate(mod3b, mod_out3b, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "C_Innovation")

summary(modelo_mediator_Boot3b)

figura_modelo_mediator_Boot3 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot3b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 3b - Mediator variable: Corporate Inovation. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot3b)

##-------------
## HIPOTESE 4

# 4a -----------------


mod4a <- lm(E_sector ~ X_breadth1, 
            data = dados)

summary(mod4a)

mod_out4a <- lm(env_score ~ E_sector + X_breadth1, 
                data = dados) 

summary(mod_out4a)


gvlma(mod4a) 
gvlma(mod_out4a) 

modelo_mediator4a <- 
  mediation::mediate(mod4a, mod_out4a, 
                     treat = c("X_breadth1"), 
                     mediator = "E_sector")



summary(modelo_mediator4a) 

figura_modelo_mediator4 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator4a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 4a - Mediator variable: E_sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator4a)


# Bootstrap

modelo_mediator_Boot4a <- 
  mediation::mediate(mod4a, mod_out4a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "E_sector")

summary(modelo_mediator_Boot4a)

figura_modelo_mediator_Boot4 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot4a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 4a - Mediator variable: E_sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot4a)


# 4b -----------------

mod4b <- lm(C_Innovation ~ X_breadth1, 
            data = dados)

summary(mod4b)

mod_out4b <- lm(env_score ~ C_Innovation + X_breadth1, 
                data = dados) 

summary(mod_out4b)


gvlma(mod4b) 
gvlma(mod_out4b) 

modelo_mediator4b <- 
  mediation::mediate(mod4b, mod_out4b, 
                     treat = c("X_breadth1"), 
                     mediator = "C_Innovation")



summary(modelo_mediator4b) 

figura_modelo_mediator4 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator4b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 4b - Mediator variable: Corporate Inovation",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator4b)


# Bootstrap

modelo_mediator_Boot4b <- 
  mediation::mediate(mod4b, mod_out4b, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "C_Innovation")

summary(modelo_mediator_Boot4b)

figura_modelo_mediator_Boot4 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot4b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 4b - Mediator variable: Corporate Inovation. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot4b)


##-------------
## HIPOTESE 5

names(dados)
# 5a -----------------

mod5a <- lm(E_sector ~ X_breadth2, 
            data = dados)

summary(mod5a)

mod_out5a <- lm(social_score ~ E_sector + X_breadth2, 
                data = dados) 

summary(mod_out5a)


gvlma(mod5a) 
gvlma(mod_out5a) 

modelo_mediator5a <- 
  mediation::mediate(mod5a, mod_out5a, 
                     treat = c("X_breadth2"), 
                     mediator = "E_sector")



summary(modelo_mediator5a) 

figura_modelo_mediator5 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator5a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 5a - Mediator variable: E_sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator5a)


# Bootstrap

modelo_mediator_Boot5a <- 
  mediation::mediate(mod5a, mod_out5a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "E_sector")

summary(modelo_mediator_Boot5a)

figura_modelo_mediator_Boot5 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot5a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 5a - Mediator variable: E_sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot5a)


# 5b -----------------


mod5b <- lm(C_Innovation ~ X_breadth2, 
            data = dados)

summary(mod5b)

mod_out5b <- lm(social_score ~ C_Innovation + X_breadth2, 
                data = dados) 

summary(mod_out5b)


gvlma(mod5b) 
gvlma(mod_out5b) 

modelo_mediator5b <- 
  mediation::mediate(mod5b, mod_out5b, 
                     treat = c("X_breadth2"), 
                     mediator = "C_Innovation")



summary(modelo_mediator5b) 

figura_modelo_mediator5 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator5b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 5b - Mediator variable: Corporate Inovation",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator5b)


# Bootstrap

modelo_mediator_Boot5b <- 
  mediation::mediate(mod5b, mod_out5b, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "C_Innovation")

summary(modelo_mediator_Boot5b)

figura_modelo_mediator_Boot5 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot5b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 5b - Mediator variable: Corporate Inovation. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot5b)


##-------------
## HIPOTESE 6


# 6a -----------------

mod6a <- lm(E_sector ~ X_breadth1, 
            data = dados)

summary(mod6a)

mod_out6a <- lm(sust_score ~ E_sector + X_breadth1, 
                data = dados) 

summary(mod_out6a)


gvlma(mod6a) 
gvlma(mod_out6a) 

modelo_mediator6a <- 
  mediation::mediate(mod6a, mod_out6a, 
                     treat = c("X_breadth1"), 
                     mediator = "E_sector")



summary(modelo_mediator6a) 

figura_modelo_mediator6 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator6a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 6a - Mediator variable: E_sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator6a)


# Bootstrap

modelo_mediator_Boot6a <- 
  mediation::mediate(mod6a, mod_out6a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "E_sector")

summary(modelo_mediator_Boot6a)

figura_modelo_mediator_Boot6 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot6a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 6a - Mediator variable: E_sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot6a)


# 6b -----------------


mod6b <- lm(C_Innovation ~ X_breadth1, 
            data = dados)

summary(mod6b)

mod_out6b <- lm(social_score ~ C_Innovation + X_breadth1, 
                data = dados) 

summary(mod_out6b)


gvlma(mod6b) 
gvlma(mod_out6b) 

modelo_mediator6b <- 
  mediation::mediate(mod6b, mod_out6b, 
                     treat = c("X_breadth1"), 
                     mediator = "C_Innovation")



summary(modelo_mediator6b) 

figura_modelo_mediator6 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator6b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 6b - Mediator variable: Corporate Inovation",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator6b)


# Bootstrap

modelo_mediator_Boot6b <- 
  mediation::mediate(mod6b, mod_out6b, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "C_Innovation")

summary(modelo_mediator_Boot6b)

figura_modelo_mediator_Boot6 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot6b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 6b - Mediator variable: Corporate Inovation. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot6b)



##-------------
## HIPOTESE 7

# 7a -----------------

mod7a <- lm(E_sector ~ X_breadth2, 
            data = dados)

summary(mod7a)

mod_out7a <- lm(gov_score ~ E_sector + X_breadth2, 
                data = dados) 

summary(mod_out7a)


gvlma(mod7a) 
gvlma(mod_out7a) 

modelo_mediator7a <- 
  mediation::mediate(mod7a, mod_out7a, 
                     treat = c("X_breadth2"), 
                     mediator = "E_sector")



summary(modelo_mediator7a) 

figura_modelo_mediator7 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator7a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 7a - Mediator variable: E_sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator7a)


# Bootstrap

modelo_mediator_Boot7a <- 
  mediation::mediate(mod7a, mod_out7a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "E_sector")

summary(modelo_mediator_Boot7a)

figura_modelo_mediator_Boot7 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot7a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 7a - Mediator variable: E_sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot7a)


# 7b -----------------

mod7b <- lm(C_Innovation ~ X_breadth2, 
            data = dados)

summary(mod7b)

mod_out7b <- lm(gov_score ~ C_Innovation + X_breadth2, 
                data = dados) 

summary(mod_out7b)


gvlma(mod7b) 
gvlma(mod_out7b) 

modelo_mediator7b <- 
  mediation::mediate(mod7b, mod_out7b, 
                     treat = c("X_breadth2"), 
                     mediator = "C_Innovation")



summary(modelo_mediator7b) 

figura_modelo_mediator7 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator7b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 7b - Mediator variable: Corporate Inovation",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator7b)


# Bootstrap

modelo_mediator_Boot7b <- 
  mediation::mediate(mod7b, mod_out7b, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth2"), 
                     mediator = "C_Innovation")

summary(modelo_mediator_Boot7b)

figura_modelo_mediator_Boot7 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot7b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 7b - Mediator variable: Corporate Inovation. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot7b)



##-------------
## HIPOTESE 8

# 8a -----------------

mod8a <- lm(E_sector ~ X_breadth1, 
            data = dados)

summary(mod8a)

mod_out8a <- lm(gov_score ~ E_sector + X_breadth1, 
                data = dados) 

summary(mod_out8a)


gvlma(mod8a) 
gvlma(mod_out8a) 

modelo_mediator8a <- 
  mediation::mediate(mod8a, mod_out8a, 
                     treat = c("X_breadth1"), 
                     mediator = "E_sector")



summary(modelo_mediator8a) 

figura_modelo_mediator8 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator8a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 8a - Mediator variable: E_sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator8a)


# Bootstrap

modelo_mediator_Boot8a <- 
  mediation::mediate(mod8a, mod_out8a, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "E_sector")

summary(modelo_mediator_Boot8a)

figura_modelo_mediator_Boot8 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot8a,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 8a - Mediator variable: E_sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot8a)


# 8b -----------------

mod8b <- lm(C_Innovation ~ X_breadth1, 
            data = dados)

summary(mod8b)

mod_out8b <- lm(gov_score ~ C_Innovation + X_breadth1, 
                data = dados) 

summary(mod_out8b)


gvlma(mod8b) 
gvlma(mod_out8b) 

modelo_mediator8b <- 
  mediation::mediate(mod8b, mod_out8b, 
                     treat = c("X_breadth1"), 
                     mediator = "C_Innovation")



summary(modelo_mediator8b) 

figura_modelo_mediator8 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator8b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 8b - Mediator variable: Corporate Inovation",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator8b)


# Bootstrap

modelo_mediator_Boot8b <- 
  mediation::mediate(mod8b, mod_out8b, 
                     boot = TRUE, sims = 999, 
                     treat = c("X_breadth1"), 
                     mediator = "C_Innovation")

summary(modelo_mediator_Boot8b)

figura_modelo_mediator_Boot8 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot8b,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval",
    subtitle = "Model 8b - Mediator variable: Corporate Inovation. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot8b)



## ========================================= ##
##      EXPLORATORY DATA ANALYSIS            ##
## ========================================= ##

dd <- 
  dados %>%
    select("accper", "C_Innovation", "sust_score", "env_score", "social_score",
           "industrycode", "sic2", "gov_score", "X_breadth1", "X_breadth2", #"SOE",         
           "Size", "Lev", "Age", "Growth", "Cash", "ROE",         
           "MTB", "SDG")




colnames(dd) <- c("Year", "C_IN", "CSP", "Env_P", "Soc_P",
                  "industrycode", "sic2", "Gov_P", "Int_D", "Int_B", #"SOE",         
                  "Size", "Lev", "Age", "Growth", "Cash", "ROE",         
                  "MTB", "SDG")
ggstatsplot::ggcorrmat(
  data = dd,
  colors = c("#B2182B", "white", "#4D4D4D"),
  title = "Correlalogram for dataset",
  subtitle = "Only quantitative variables"
)



## EXPLORATORY FIGURES


## CSP ----------------------------------


CSP_boxplot <-
  ggplot(dd, aes(x = sust_score)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Corporate sustainable boxplot") +
  xlab("")


# Histogram - OK!

CSP_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = sust_score,
    #test.value = 50,
    xlab = "Corporate sustainable",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

CSP_plots <-
  ggarrange(CSP_boxplot, CSP_Histograma, ncol = 2)

## ENV_P ----------------------------------

names(dd)

ENV_P_boxplot <-
  ggplot(dd, aes(x = env_score)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Environmental performance boxplot") +
  xlab("")


# Histogram - OK!

ENV_P_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = env_score,
    #type = "parametric",
    xlab = "Environmental performance",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

ENV_P_plots <-
  ggarrange(ENV_P_boxplot, ENV_P_Histograma, ncol = 2)

## SOC_P ----------------------------------

SOC_P_boxplot <-
  ggplot(dd, aes(x = social_score)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Social performance boxplot") +
  xlab("")


# Histogram - OK!

SOC_P_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = social_score,
    #test.value = 50,
    xlab = "Social performance",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

SOC_P_plots <-
  ggarrange(SOC_P_boxplot, SOC_P_Histograma, ncol = 2)


## GOV_P ----------------------------------

GOV_P_boxplot <-
  ggplot(dd, aes(x = gov_score)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Governance performance boxplot") +
  xlab("")


# Histogram - OK!

GOV_P_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = gov_score,
    #test.value = 50,
    xlab = "Governance performance",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

GOV_P_plots <-
  ggarrange(GOV_P_boxplot, GOV_P_Histograma, ncol = 2)



## Int_D ----------------------------------

Int_D_boxplot <-
  ggplot(dd, aes(x = X_breadth1)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Internalization Depth boxplot") +
  xlab("")


# Histogram - OK!

Int_D_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = X_breadth1,
    #test.value = 50,
    xlab = "Internalization Depth",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

Int_D_plots <-
  ggarrange(Int_D_boxplot, Int_D_Histograma, ncol = 2)

## Int_B ----------------------------------

Int_B_boxplot <-
  ggplot(dd, aes(x = X_breadth2)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Internalization Breadth boxplot") +
  xlab("")


# Histogram - OK!

Int_B_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = X_breadth2,
    #test.value = 50,
    xlab = "Internalization Breadth",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

Int_B_plots <-
  ggarrange(Int_B_boxplot, Int_B_Histograma, ncol = 2)



## E_Sector ----------------------------------

ggstatsplot::ggpiestats(dd, sic2, title = "E_Sector")

ggbarstats(dados, sic2, E_sector)


## C_Innovation ----------------------------------

C_Innovation_boxplot <-
  ggplot(dd, aes(x = C_Innovation)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Corporate Inovation boxplot") +
  xlab("")


# Histogram - OK!

C_Innovation_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = C_Innovation,
    #test.value = 50,
    xlab = "Corporate Inovation",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

C_Innovation_plots <-
  ggarrange(C_Innovation_boxplot, C_Innovation_Histograma, ncol = 2)



## SDG?  ----------------------------------

SDG_boxplot <-
  ggplot(dd, aes(x = SDG)) +
  geom_boxplot(fill = '#A4A4A4', color = "black") +
  #scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("SDG boxplot") +
  xlab("")


# Histogram - OK!

SDG_Histograma <-
  ggstatsplot::gghistostats(
    data = dd,
    x = SDG,
    #test.value = 50,
    xlab = "SDG",
    ylab = "Frequency",
    type = "parametric",
    #grouping.var = genre, # grouping variable
    normal.curve = TRUE, # superimpose a normal distribution curve
    normal.curve.args = list(color = "red", size = 1),
    title.prefix = "Movie genre",
    ggtheme = ggthemes::theme_tufte(),
    # modify the defaults from `ggstatsplot` for each plot
    #ggplot.component = ggplot2::scale_x_continuous(
    #  breaks = seq(0, 200, 50),
    #  limits = (c(0, 200))
    #),
    plotgrid.args = list(nrow = 2),
    title.text = "Movies budgets for different genres"
  )

SDG_plots <-
  ggarrange(SDG_boxplot, SDG_Histograma, ncol = 2)


## BOXPLOTS -------------------------------------

grid.arrange(CSP_boxplot, ENV_P_boxplot, SOC_P_boxplot,
             GOV_P_boxplot, Int_D_boxplot, Int_B_boxplot,
             C_Innovation_boxplot, SDG_boxplot, ncol = 3)


## HISTOGRAM ------------------------------------

grid.arrange(CSP_Histograma, ENV_P_Histograma, SOC_P_Histograma,
             GOV_P_Histograma, Int_D_Histograma, Int_B_Histograma,
             C_Innovation_Histograma, ncol = 2)

## BIVARIATE ANALYSIS ---------------------------------------------------------------------

## CSP VS VI AND MEDIATORS ---------------------------------------------------------------

## INT_B ------------------------------

CSP_INT_B_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = sust_score,
    xlab = "Internalization Breadth",
    ylab = "Corporate sustainable",
    title = "Relation between INT_B - CSP"
  )

CSP_INT_B_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = sust_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Breadth", # label for x axis
    ylab = "Corporate sustainable", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between INT_B - CSP", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## INT_D -------------------------------------

CSP_INT_D_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = sust_score,
    xlab = "Internalization Depth",
    ylab = "Corporate sustainable",
    title = "Relation between INT_D - CSP"
  )

CSP_INT_D_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = sust_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Depth", # label for x axis
    ylab = "Corporate sustainable", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between INT_D - CSP", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )

## E_SECTOR -------------------------------


CSP_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = sust_score,
    xlab = "E_Sector",
    ylab = "Corporate sustainable",
    title = "Relation between E_Sector - CSP"
  )

CSP_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = sust_score,
    type = "robust", # type of test that needs to be run
    xlab = "E_Sector", # label for x axis
    ylab = "Corporate sustainable", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between E_Sector - CSP", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## C_INN ------------------------------------------------


CSP_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = sust_score,
    xlab = "Corporate Inovation",
    ylab = "Corporate sustainable",
    title = "Relation between C_INN - CSP"
  )

CSP_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = sust_score,
    type = "robust", # type of test that needs to be run
    xlab = "Corporate Inovation", # label for x axis
    ylab = "Corporate sustainable", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between C_INN - CSP", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )



## SDG -----------------------------------------------------------------------------


CSP_SDG_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = sust_score,
    xlab = "SDG",
    ylab = "Corporate sustainable",
    title = "Relation between SDG - CSP"
  )

CSP_SDG_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = sust_score,
    type = "robust", # type of test that needs to be run
    xlab = "SDG", # label for x axis
    ylab = "Corporate sustainable", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between SDG - CSP", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## BOXPLOTS -------------------------------------

grid.arrange(CSP_INT_B_boxplot, CSP_INT_D_boxplot, 
             CSP_E_SECTOR_boxplot, CSP_E_SECTOR_boxplot, ncol = 2)


## HISTOGRAM ------------------------------------

grid.arrange(CSP_INT_B_hist, CSP_INT_D_hist, 
             CSP_E_SECTOR_hist, CSP_E_SECTOR_hist, ncol = 2)


## ENV_P VS VI AND MEDIATORS ---------------------------------------------------------------


## INT_B ------------------------------

ENV_P_INT_B_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = env_score,
    xlab = "Internalization Breadth",
    ylab = "Environmental performance",
    title = "Relation between INT_B - ENV_P"
  )

ENV_P_INT_B_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = env_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Breadth", # label for x axis
    ylab = "Environmental performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between INT_B - ENV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## INT_D -------------------------------------

ENV_P_INT_D_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = env_score,
    xlab = "Internalization Depth",
    ylab = "Environmental performance",
    title = "Relation between INT_D - ENV_P"
  )

ENV_P_INT_D_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = env_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Depth", # label for x axis
    ylab = "Environmental performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between INT_D - ENV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )

## E_SECTOR -------------------------------


ENV_P_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = env_score,
    xlab = "E_Sector",
    ylab = "Environmental performance",
    title = "Relation between E_Sector - ENV_P"
  )

ENV_P_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = env_score,
    type = "robust", # type of test that needs to be run
    xlab = "E_Sector", # label for x axis
    ylab = "Environmental performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between E_Sector - ENV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## C_INN ------------------------------------------------


ENV_P_C_INN_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = env_score,
    xlab = "Corporate Inovation",
    ylab = "Environmental performance",
    title = "Relation between C_INN - ENV_P"
  )

ENV_P_C_INN_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = env_score,
    type = "robust", # type of test that needs to be run
    xlab = "Corporate Inovation", # label for x axis
    ylab = "Environmental performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between C_INN - ENV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )



## SDG ------------------------------------------------


ENV_P_SDG_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = env_score,
    xlab = "SDG",
    ylab = "Environmental performance",
    title = "Relation between SDG - ENV_P"
  )

ENV_P_SDG_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = env_score,
    type = "robust", # type of test that needs to be run
    xlab = "SDG", # label for x axis
    ylab = "Environmental performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between SDG - ENV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )




## BOXPLOTS -------------------------------------

grid.arrange(ENV_P_INT_B_boxplot, ENV_P_INT_D_boxplot, 
             ENV_P_E_SECTOR_boxplot, ENV_P_C_INN_boxplot, ncol = 2)


## HISTOGRAM ------------------------------------

grid.arrange(ENV_P_INT_B_hist, ENV_P_INT_D_hist, 
             ENV_P_E_SECTOR_hist, ENV_P_C_INN_hist, ncol = 2)


## SOC_P VS VI AND MEDIATORS ---------------------------------------------------------------



## INT_B ------------------------------

SOC_P_INT_B_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = social_score,
    xlab = "Internalization Breadth",
    ylab = "social_score",
    title = "Relation between Internalization Breadth - social_score"
  )

SOC_P_INT_B_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = social_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Breadth", # label for x axis
    ylab = "social_score", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between Internalization Breadth - social_score", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## INT_D -------------------------------------

SOC_P_INT_D_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = social_score,
    xlab = "Internalization Depth",
    ylab = "social_score",
    title = "Relation between Internalization Depth - social_score"
  )

SOC_P_INT_D_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = social_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Depth", # label for x axis
    ylab = "social_score", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between Internalization Depth - social_score", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )

## E_SECTOR -------------------------------


SOC_P_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = social_score,
    xlab = "E_Sector",
    ylab = "social_score",
    title = "Relation between E_Sector - social_score"
  )

SOC_P_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = social_score,
    type = "robust", # type of test that needs to be run
    xlab = "E_Sector", # label for x axis
    ylab = "social_score", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between E_Sector - social_score", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## C_INN ------------------------------------------------


SOC_P_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = social_score,
    xlab = "Corporate Inovation",
    ylab = "Social performance",
    title = "Relation between Corporate Inovation - Social performance"
  )

SOC_P_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = social_score,
    type = "robust", # type of test that needs to be run
    xlab = "Corporate Inovation", # label for x axis
    ylab = "Social performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between Corporate Inovation - Social performance", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )



## SDG ------------------------------------------------


SOC_P_SDG_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = social_score,
    xlab = "SDG",
    ylab = "Social performance",
    title = "Relation between SDG - Social performance"
  )

SOC_P_SDG_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = social_score,
    type = "robust", # type of test that needs to be run
    xlab = "SDG", # label for x axis
    ylab = "Social performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between SDG - Social performance", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )









## BOXPLOTS -------------------------------------

grid.arrange(ENV_P_INT_B_boxplot, ENV_P_INT_D_boxplot, 
             ENV_P_E_SECTOR_boxplot, ENV_P_C_INN_boxplot, ncol = 2)


## HISTOGRAM ------------------------------------

grid.arrange(ENV_P_INT_B_hist, ENV_P_INT_D_hist, 
             ENV_P_E_SECTOR_hist, ENV_P_C_INN_hist, ncol = 2)


## GOV_P VS VI AND MEDIATORS ---------------------------------------------------------------

## INT_B ------------------------------
GOV_P_INT_B_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = gov_score,
    xlab = "Internalization Breadth",
    ylab = "Governance performance",
    title = "Relation between INT_B - GOV_P"
  )

GOV_P_INT_B_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth2,
    y = gov_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Breadth", # label for x axis
    ylab = "Governance performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between INT_B - GOV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## INT_D -------------------------------------

GOV_P_INT_D_hist <- 
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = gov_score,
    xlab = "Internalization Depth",
    ylab = "Governance performance",
    title = "Relation between INT_D - GOV_P"
  )

GOV_P_INT_D_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dd,
    x = X_breadth1,
    y = gov_score,
    type = "robust", # type of test that needs to be run
    xlab = "Internalization Depth", # label for x axis
    ylab = "Governance performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between INT_D - GOV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )

## E_SECTOR -------------------------------


GOV_P_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = gov_score,
    xlab = "E_Sector",
    ylab = "Governance performance",
    title = "Relation between E_Sector - GOV_P"
  )

GOV_P_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = E_sector,
    y = gov_score,
    type = "robust", # type of test that needs to be run
    xlab = "E_Sector", # label for x axis
    ylab = "Governance performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between E_Sector - GOV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )


## C_INN ------------------------------------------------


GOV_P_E_SECTOR_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = gov_score,
    xlab = "Corporate Inovation",
    ylab = "Governance performance",
    title = "Relation between C_INN - GOV_P"
  )

GOV_P_E_SECTOR_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = C_Innovation,
    y = gov_score,
    type = "robust", # type of test that needs to be run
    xlab = "Corporate Inovation", # label for x axis
    ylab = "Governance performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between C_INN - GOV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )



## SDG ------------------------------------------------


GOV_P_SDG_hist <- 
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = gov_score,
    xlab = "SDG",
    ylab = "Governance performance",
    title = "Relation between SDG - GOV_P"
  )

GOV_P_SDG_boxplot <-
  ggstatsplot::ggscatterstats(
    data = dados,
    x = SDG,
    y = gov_score,
    type = "robust", # type of test that needs to be run
    xlab = "SDG", # label for x axis
    ylab = "Governance performance", # label for y axis
    #label.var = title, # variable for labeling data points
    #label.expression = rating < 5 & budget > 100, # expression that decides which points to label
    title = "Relation between SDG - GOV_P", # title text for the plot
    caption = expression(paste(italic("Note"), ": with Box-plot")),
    ggtheme = hrbrthemes::theme_ipsum_ps(), # choosing a different theme
    ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
    marginal.type = "boxplot", # type of marginal distribution to be displayed
    xfill = "pink", # color fill for x-axis marginal distribution
    yfill = "#009E73" # color fill for y-axis marginal distribution
  )











## BOXPLOTS -------------------------------------

grid.arrange(GOV_P_INT_B_boxplot, GOV_P_INT_D_boxplot, 
             GOV_P_E_SECTOR_boxplot, GOV_P_E_SECTOR_boxplot, ncol = 2)


## HISTOGRAM ------------------------------------

grid.arrange(GOV_P_INT_B_hist, GOV_P_INT_D_hist, 
             GOV_P_E_SECTOR_hist, GOV_P_E_SECTOR_hist, ncol = 2)






# ------------------------------------------------------------














