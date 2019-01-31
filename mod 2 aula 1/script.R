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
