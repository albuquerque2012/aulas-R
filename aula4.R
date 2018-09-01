# carrega pacotes
library(readr); library(stringr); library(ggplot2)

# carrega e manipula banco
<nomeobj> <- read.csv("<nomearq>")
str(<nomeobj>)
colnames(multassp) <- c("mes", "ano", "multas", "receita", "despesas") #variáveis existentes
multassp$mes <- str_replace(multassp$mes, "\xc7", "Ç")

# correlação
cor(multassp$quantidade_de_multas_arrecadadas, multassp$receita_repassada_ao_detran)
cor.test(multassp$quantidade_de_multas_arrecadadas, multassp$receita_repassada_ao_detran)

# scatterplot
ggplot(multassp, aes(multas, receita)) +
geom_point()
ggplot(multassp, aes(multas, receita)) + #seleção variáveis
     geom_point(color = "blue") + #visualização de pontos
     theme_minimal() + #define tema da visualização
     scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + #altero o eixo y
     labs(title = "Relação Receita-Multa", x = "Qtde de Multas", y = "Receita Arrecada")

cormat <- cor(multassp[,c(3:5)]) # matriz correlação entre variáveis do banco
install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(cormat) # transforma matriz em banco de dados
ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
     geom_tile() # gráfico de relação entre as variáveis
