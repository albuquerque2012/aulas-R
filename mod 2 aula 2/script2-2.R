setwd("mod 2 aula 2")
titanic_data <- read.csv("titanic_data.csv", sep =";", stringsAsFactors=FALSE)
str(titanic_data)
titdata_sel <- titanic_data[,c("Survived", "Pclass", "Sex", "Age", "Fare")]

# Essa estrutura de codigo para importacao de pacotes e ideal
# levando em conta a replicabilidade, pois testa se o pacote ja existe e caso
# nao, ele sera instalado e carregado
if (require(Amelia) == FALSE) install.packages("Amelia"); require(Amelia)


missmap(titdata_sel)

titdata_ageclean <- titdata_sel[complete.cases(titdata_sel$Age),]
titdata_ageclean$Pclass <- as.factor(titdata_ageclean$Pclass)

modelo_logit <- glm(Survived ~ # VD
                Pclass +           # VIs
                Sex +
                Age +
                Fare,
              family = binomial, data = titdata_ageclean)

summary(modelo_logit)

titdata_sel$Pclass <- as.factor(titdata_sel$Pclass)
modelo_logit_2 <- glm(Survived ~ # VD
                      Pclass +           # VIs
                      Sex +
                      Fare,
                    family = binomial, data = titdata_sel)
summary(modelo_logit_2)

library(stargazer)
stargazer(modelo_logit,modelo_logit_2, type = "text", title = "Model Results", style = "ajps", p.auto = FALSE, apply.coef = exp) # resultados acima de 1 são positivos, quando o efeito é negativo, subtrai de 1 e multipla por 100; quando o efeito é positivo, subtrai de 1 e multiplica por 100

# grafico de dispersao + reta de estimativa do modelo
library(ggplot2)
ggplot(data = titdata_ageclean, aes(x = Age, y = Survived))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("logit_1.png", width=6, height=4, units="in")



### exercício
acid_exerc <- read.csv("acid_trans2016_treat.csv", sep =";", stringsAsFactors=FALSE)
str(acid_exerc)
library(dplyr)
acid_exerc <- mutate(acid_exerc, vitima = ifelse(tipo == "COM VÍTIMA", 1, 0))

## FALTA remover NAs de bairro, 1) escolha pelo menos dois bairros e dois endereços para analisar o efeito de cada; e (2) numero de automoveis envolvidos. Fique à vontade para analisar outras variáveis.

acid_exerc <- mutate(acid_exerc, AVBV = ifelse(bairro == "AV BOA VIAGEM", 1, 0))
acid_exerc <- mutate(acid_exerc, AVCA = ifelse(endereco == "AV CONSELHEIRO AGUIAR", 1, 0))
acid_exerc <- mutate(acid_exerc, madalena = ifelse(bairro == "MADALENA", 1, 0))
acid_exerc <- mutate(acid_exerc, iputinga = ifelse(bairro == "IPUTINGA", 1, 0))
library(tidyr)
acid_exerc <- drop_na(acid_exerc, madalena, iputinga, AVCA, AVBV, auto) #apaga os missing cases das variáveis de interesse

###################################
### SCRIPT TRAVADO A PARTIR DAQUI

acid_exerc_v2 <- acid_exerc[,c(11,21,22,24,25,26)] #### tu tentou fazer o que aqui? nao esta funcionando (se liga na numeracao das colunas)

str(acid_exerc)
sum(acid_exerc_v2$madalena)
sum(acid_exerc_v2$iputinga)
sum(acid_exerc_v2$AVCA)
sum(acid_exerc_v2$AVBV)
sum(acid_exerc_v2$auto)

modelo_logit <- glm(vitima ~ # VD
                      madalena +           # VIs
                      iputinga +
                      AVCA +
                      AVBV +
                      auto,
                    family = binomial, data = acid_exerc_v2)
summary(modelo_logit)
library(ggplot2)
ggplot(data = acid_exerc_v2, aes(x = madalena, y = vitima))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("madalena.png", width=6, height=4, units="in")

ggplot(data = acid_exerc_v2, aes(x = iputinga, y = vitima))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("iputinga.png", width=6, height=4, units="in")

ggplot(data = acid_exerc_v2, aes(x = AVCA, y = vitima))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("consaguiar.png", width=6, height=4, units="in")

ggplot(data = acid_exerc_v2, aes(x = AVBV, y = vitima))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("avbv.png", width=6, height=4, units="in")

ggplot(data = acid_exerc_v2, aes(x = auto, y = vitima))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("autos.png", width=6, height=4, units="in")
