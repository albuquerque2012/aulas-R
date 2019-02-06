library(readr)
GOTdata <- read_csv("characters_GOT.csv")
str(GOTdata)
GOTanalysis <- GOTdata[,c("numdeadrelations", "male", "house", "ismarried", "isnoble", "popularity")]
GOTanalysis$house[is.na(GOTanalysis$house)] <- "Without House/Unknown"
table(GOTdata$house)
library(dplyr)
GOTanalysis <- mutate(GOTanalysis, house_tyrell = ifelse(house == "House Tyrell", 1, 0))
GOTanalysis <- mutate(GOTanalysis, house_targaryen = ifelse(house == "House Targaryen", 1, 0))
GOTanalysis <- mutate(GOTanalysis, house_lannister = ifelse(house == "House Lannister", 1, 0))
GOTanalysis <- mutate(GOTanalysis, house_stark = ifelse(house == "House Stark", 1, 0))

# executar modelo
modelo1 <- lm(numdeadrelations ~ # VD
                male +           # VIs
                house_tyrell +
                house_targaryen +
                house_lannister +
                ismarried +
                isnoble +
                popularity,
              data = GOTanalysis)
summary(modelo1)

modelo2 <- lm(numdeadrelations ~ # VD
                male +           # VIs
                house_tyrell +
                house_targaryen +
                house_lannister +
                house_stark +
                ismarried +
                isnoble +
                popularity,
              data = GOTanalysis)

install.packages("stargazer")
library(stargazer)
stargazer(modelo1,modelo2, type = "text", title = "Model Results", style = "ajps", p.auto = FALSE)

# visualização

library(ggplot2)
ggplot(data = GOTanalysis, aes(x = numdeadrelations, y = popularity))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear
ggsave("regressao.png", width=6, height=4, units="in")

# homocedasticidade

# salvando valores preditos e residuos para análise de homocedasticidade
GOTanalysis$Predito <- predict(modelo1)
GOTanalysis$Residuo <- GOTanalysis$numdeadrelations - GOTanalysis$Predito

# Testar se a media do erro e igual a 0
summary(GOTanalysis$Residuo)

# gráfico de dispersao dos valores preditos x residuos
ggplot(data = GOTanalysis, aes(x = Predito, y=Residuo)) +
  geom_point() +
  labs( x ="Valores Preditos (modelo 1)", y = "Residuos (modelo 1)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
ggsave("homoced.png", width=6, height=4, units="in")

exercicio <- read.csv("QOGdataSave.csv", sep =";", stringsAsFactors=FALSE)
str(exercicio)
exerciciomanipuladov1 <- exercicio[complete.cases(total_export)]

#### fazendo o exercicio
library(readr)
library(stringr)
library(stargazer)
library(ggplot2)
library(dplyr)
install.packages("tidyr")
library(tidyr)
exercicio <- read.csv("QOGdataSave.csv", sep =";", stringsAsFactors=FALSE)
exercicio_v2 <- na.omit(exercicio)

ggplot(data = exercicio_v2, aes(x = political_corruption, y = environmental_performance_index))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear
ggsave("reg_exec_1_v2.png", width=6, height=4, units="in")
ggplot(data = exercicio_v2, aes(x = total_export, y = environmental_performance_index))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear
ggsave("reg_exec_2_v2.png", width=6, height=4, units="in")

modelo_reg <- lm(environmental_performance_index ~ # VD
                total_export +           # VIs
                political_corruption,
              data = exercicio_v2)
summary(modelo_reg)

exercicio_v2$Predito <- predict(modelo_reg)
exercicio_v2$Residuo <- exercicio_v2$total_export - exercicio_v2$Predito
exercicio_v2$Predito <- predict(modelo_reg)
exercicio_v2$Residuo <- exercicio_v2$political_corruption - exercicio_v2$Predito

summary(exercicio_v2$Residuo)

# gráfico de dispersao dos valores preditos x residuos
ggplot(data = exercicio_v2, aes(x = Predito, y = Residuo)) +
  geom_point() +
  labs( x ="Valores Preditos (modelo_reg)", y = "Residuos (modelo_reg)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
ggsave("hom_exec_v2.png", width=6, height=4, units="in")


#### fazendo o exercicio_v3

library(readr)
library(stringr)
library(stargazer)
library(ggplot2)
library(dplyr)
install.packages("tidyr")
library(tidyr)
exercicio <- read.csv("QOGdataSave.csv", sep =";", stringsAsFactors=FALSE)
exercicio_v3 <- drop_na(exercicio, total_export, political_corruption, environmental_performance_index) #apaga os missing cases das variáveis de interesse

ggplot(data = exercicio_v3, aes(x = political_corruption, y = environmental_performance_index))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear
ggsave("reg_exec_1_v3.png", width=6, height=4, units="in")
ggplot(data = exercicio_v3, aes(x = total_export, y = environmental_performance_index))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear
ggsave("reg_exec_2_v3.png", width=6, height=4, units="in")

modelo_reg <- lm(environmental_performance_index ~ # VD
                   total_export +           # VIs
                   political_corruption,
                 data = exercicio_v3)
summary(modelo_reg)

exercicio_v3$Predito <- predict(modelo_reg)
exercicio_v3$Residuo <- exercicio_v3$total_export - exercicio_v3$Predito
exercicio_v3$Predito <- predict(modelo_reg)
exercicio_v3$Residuo <- exercicio_v3$political_corruption - exercicio_v3$Predito

summary(exercicio_v3$Residuo)

# gráfico de dispersao dos valores preditos x residuos
ggplot(data = exercicio_v3, aes(x = Predito, y = Residuo)) +
  geom_point() +
  labs( x ="Valores Preditos (modelo_reg)", y = "Residuos (modelo_reg)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
ggsave("hom_exec_v3.png", width=6, height=4, units="in")


exercicio_v4 <- exercicio_v3[,c(4,8,9)]

ggplot(data = exercicio_v4, aes(x = political_corruption, y = environmental_performance_index))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear

ggsave("reg_exec_1_v4.png", width=6, height=4, units="in")
ggplot(data = exercicio_v4, aes(x = total_export, y = environmental_performance_index))+ # dev variáveis e eixos
  geom_point()+ # gráf tipo ponto
  geom_smooth(method = "lm") # reta de regressão linear
ggsave("reg_exec_2_v4.png", width=6, height=4, units="in")

modelo_reg <- lm(environmental_performance_index ~ # VD
                   total_export +           # VIs
                   political_corruption,
                 data = exercicio_v4)
summary(modelo_reg)

exercicio_v4$Predito <- predict(modelo_reg)
exercicio_v4$Residuo <- exercicio_v4$total_export - exercicio_v4$Predito
exercicio_v4$Predito <- predict(modelo_reg)
exercicio_v4$Residuo <- exercicio_v4$political_corruption - exercicio_v4$Predito

summary(exercicio_v4$Residuo)

# gráfico de dispersao dos valores preditos x residuos
ggplot(data = exercicio_v4, aes(x = Predito, y = Residuo)) +
  geom_point() +
  labs( x ="Valores Preditos (modelo_reg)", y = "Residuos (modelo_reg)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue")
ggsave("hom_exec_v4.png", width=6, height=4, units="in")
