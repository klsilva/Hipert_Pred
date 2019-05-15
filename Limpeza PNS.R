#Instala e carrega os pacotes 

pacotes<-c("devtools", "tidyverse", "survey", "srvyr", "Amelia")

lapply(pacotes,install.packages)

lapply(pacotes,library,character.only=TRUE)

#Instala e carrega o pacote lodown
install_github("ajdamico/lodown", dependencies = TRUE)

library(lodown)

#Carrega o catálogo e baixa a PNS e carrega a PNS -------------

pns_cat <-
  get_catalog("pns" ,
              output_dir = file.path(path.expand("~") , "PNS"))

#Faz Download da PNS

pns_cat <- lodown("pns" , pns_cat)

#Carrega o pacote survey

library(survey)

#Contri o design de uma amostra complexa

options(survey.lonely.psu = "adjust")

pns_design1 <-
  readRDS(file.path(
    path.expand("~") ,
    "PNS" ,
    "2013 long questionnaire survey design.rds"
  ))

#Faz subset

subset2 <-
  subset(
    pns_design1$variables,
    select = c(
      q002,
      q006,
      vdd004,
      p050,
      c009,
      c008,
      w00103,
      w00203,
      w00303,
      w00407,
      w00408,
      r045,
      q030,
      p035,
      c006
    )
  )

#Define a classe das variáveis

subset2$w00103 <- as.numeric(subset2$w00103)
subset2$w00203 <- as.numeric(subset2$w00203)
subset2$w00303 <- as.numeric(subset2$w00303)
subset2$w00407 <- as.numeric(subset2$w00407)
subset2$w00408 <- as.numeric(subset2$w00408)
subset2$c008 <- as.numeric(subset2$c008)
subset2$vdd004 <- as.factor(subset2$vdd004)
subset2$r045 <- as.numeric(subset2$r045)
subset2$p050 <- as.factor(subset2$p050)
subset2$q002 <- as.factor(subset2$q002)
subset2$p035 <- as.numeric(subset2$p035)
subset2$c006 <- as.factor(subset2$c006)
subset2$c009 <- as.factor(subset2$c009)
subset2$q006 <- as.factor(subset2$q006)


subset2$q002 <- na_if(subset2$q002, "")
subset2$q002 <- droplevels.factor(subset2$q002)
subset2$c009 <- droplevels.factor(subset2$c009, exclude = "9")
subset2$c009 <- droplevels.factor(subset2$c009)

#Recodifica

subset2 <- mutate(
  subset2,
  vdd004 = recode_factor(
    vdd004,
    '1' = "Sem instrução" ,
    '2' = "Fundamental incompleto ou equivalente",
    '3' = "Fundamental completo ou equivalente",
    '4' = "Médio incompleto ou equivalente",
    '5' = "Médio completo ou equivalente",
    '6' = "Superior incompleto ou equivalente",
    '7' = "Superior completo"
  ),
  q002 = recode_factor(
    q002,
    '1' = "Sim",
    '2' = "Não",
    '3' = "Não"
  ),
  p050 = recode_factor(
    p050,
    '1' = "Sim",
    '2' = "Sim",
    '3' = "Não"
  ),
  c006 = recode_factor(c006,
                       'masculino' = "Masculino" , 'feminino' =
                         "Feminino"),
  q006 = recode_factor(q006, '1' = "Sim", '2' = "Não"),
  c009 = recode_factor(
    c009,
    '1' = "Branca",
    '2' = "Preta",
    '3' = "Amarela",
    '4' = "Parda",
    '5' = "Indígena"
  )
  
)

levels(subset2$q006) <- c("Sim", "Não", "Não")

sapply(subset2, function(x)
 sum(is.na(x)))

sapply(subset2, function(x)
 length(unique(x)))

#Vê os NA

library(Amelia)

missmap(subset2, main = "Missing values vs observed")

#Retira-se os número de filhos (r045) e número de horas de exercícios devido ao excesso de NA (p035)

dados <-
  subset(
    subset2,
    select = c(
      q002,
      q006,
      vdd004,
      p050,
      c008,
      w00103,
      w00203,
      w00303,
      w00407,
      w00408,
      q030,
      c006,
      c009
    )
  )

#Verifica-se os NA

missmap(dados, main = "Missing values vs observed")

sapply(dados, function(x)
sum(is.na(x)))

sapply(dados, function(x)
  length(unique(x)))

#Retira-se os NA
dadosobs <- na.omit(dados)

#Verifica-se os NA

missmap(dadosobs, main = "Missing values vs observed")

sapply(dadosobs, function(x)
  sum(is.na(x)))

sapply(dadosobs, function(x)
  length(unique(x)))

#Calcula as variáveis derivadas

dadosobs$alturam <- dadosobs$w00203 / 100
summary(dadosobs$alturam)
dadosobs$ccm <- dadosobs$w00303 / 100
summary(dadosobs$ccm)
denominador1 <- dadosobs$w00103 / dadosobs$alturam
sqrt1 <- sqrt(denominador1)
denominador <- 0.109 * sqrt1
dadosobs$indicec <- dadosobs$ccm / denominador
dadosobs$indicec <- as.numeric(dadosobs$indicec)
dadosobs$alturam2 <- dadosobs$alturam ^ 2
dadosobs$IMC <- dadosobs$w00103 / dadosobs$alturam2
dadosobs$IMC <- as.numeric(dadosobs$IMC)
dadosobs$RCE <- dadosobs$w00303 / dadosobs$w00203
dadosobs$RCE <- as.numeric(dadosobs$RCE)

dadosobs$IMC <- case_when(
  dadosobs$IMC <= 18.5 ~ "Abaixo do peso",
  dadosobs$IMC >= 18.5 &
    dadosobs$IMC <= 24.9 ~ "Sem excesso de peso",
  dadosobs$IMC >= 25 &
    dadosobs$IMC <= 29.9 ~ "Sobrepeso",
  dadosobs$IMC >= 30 &
    dadosobs$IMC <= 34.9 ~ "Obesidade grau 1",
  dadosobs$IMC >= 35 &
    dadosobs$IMC <= 39.9 ~ "Obesidade grau 2",
  dadosobs$IMC >= 40 ~ "Obesidade grau 3"
)
dadosobs$IMC <- as.factor(dadosobs$IMC)

dadosobs$w00303 = as.factor(
  case_when(
    dadosobs$w00303 < 80 & dadosobs$c006 == "Feminino" ~ "Sem Risco",
    dadosobs$w00303 >= 80 &
      dadosobs$w00303 < 88 & dadosobs$c006 == "Feminino" ~ "Risco",
    dadosobs$w00303 >= 88 &
      dadosobs$c006 == "Feminino" ~ "Risco Muito Alto",
    dadosobs$w00303 < 94 &
      dadosobs$c006 == "Masculino" ~ "Sem Risco",
    dadosobs$w00303 >= 94 &
      dadosobs$w00303 < 102 &
      dadosobs$c006 == "Masculino" ~ "Risco",
    dadosobs$w00303 >= 102 &
      dadosobs$c006 == "Masculino" ~ "Risco Muito Alto"
  )
)



dadosobs$indicec = as.factor(
  case_when(
    dadosobs$indicec < 1.18 & dadosobs$c006 == "Feminino" ~ "Sem risco",
    dadosobs$indicec >= 1.18 &
      dadosobs$c006 == "Feminino" ~ "Com Risco",
    dadosobs$indicec < 1.25 &
      dadosobs$c006 == "Masculino" ~ "Sem risco",
    dadosobs$indicec >= 1.25 &
      dadosobs$c006 == "Masculino" ~ "Com Risco"
  )
)

dadosobs$RCE = as.factor(case_when(dadosobs$RCE < 0.50 ~ "Sem risco",
                                   dadosobs$RCE >= 0.50 ~ "Com Risco"))

dadosobs$idadecat <-
  as.factor(
    case_when(
      dadosobs$c008 >= 18 & dadosobs$c008 <= 28 ~ "18 a 28 anos",
      dadosobs$c008 >= 29 &
        dadosobs$c008 <= 37 ~ "29 a 37 anos",
      dadosobs$c008 >= 38 &
        dadosobs$c008 <= 47 ~ "38 a 47 anos",
      dadosobs$c008 >= 48 &
        dadosobs$c008 <= 59 ~ "48 a 59 anos",
      dadosobs$c008 >= 60 ~ "60 ou mais anos"
    )
  )



dadosobs$PASPADhipert = as.factor(
  case_when(
    dadosobs$w00408 < 90 | dadosobs$w00407 < 140 ~ "Não Hipertensa",
    dadosobs$w00408 >= 90 |
      dadosobs$w00407 >= 140 ~ "Hipertensa"
  )
)

dadosobs$hipert = as.factor(
  case_when(
    dadosobs$PASPADhipert == "Não Hipertensa" &
      dadosobs$q006 == "Sim" ~ "Hipertensa",
    dadosobs$PASPADhipert == "Não Hipertensa" &
      dadosobs$q006 == "Não" ~ "Não Hipertensa",
    dadosobs$PASPADhipert == "Hipertensa" &
      dadosobs$q006 == "Sim" ~ "Hipertensa",
    dadosobs$PASPADhipert == "Hipertensa" &
      dadosobs$q006 == "Não" ~ "Hipertensa"
  )
)

#Faz subset

dadosreg <-
  subset(dadosobs,
         select = c(hipert, vdd004, w00303, indicec, IMC, RCE, c006, c009, idadecat))

missmap(dadosreg, main = "Missing values vs observed")
sapply(dadosreg, function(x)
  sum(is.na(x)))
sapply(dadosreg, function(x)
  length(unique(x)))

dadosreg <- na.omit(dadosreg)

dadosreg <-
  dadosreg %>% mutate(hipert = relevel(hipert, ref = "Não Hipertensa"))
dadosreg <- dadosreg %>% mutate(c009 = relevel(c009, ref = "Preta"))
dadosreg <-
  dadosreg %>% mutate(vdd004 = relevel(vdd004, ref = "Sem instrução"))
dadosreg <-
  dadosreg %>% mutate(w00303 = relevel(w00303, ref = "Risco Muito Alto"))
dadosreg <-
  dadosreg %>% mutate(indicec = relevel(indicec, ref = "Com Risco"))
dadosreg <-
  dadosreg %>% mutate(IMC = relevel(IMC, ref = "Obesidade grau 3"))
dadosreg <-
  dadosreg %>% mutate(RCE = relevel(RCE, ref = "Com Risco"))
dadosreg <-
  dadosreg %>% mutate(c006 = relevel(c006, ref = "Masculino"))
dadosreg <-
  dadosreg %>% mutate(idadecat = relevel(idadecat, ref = "60 ou mais anos"))


#Exporta o subset

save(dadosreg, file = "dadosreg1.Rda")
