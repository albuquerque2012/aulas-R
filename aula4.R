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
     labs(title = "Relação Receita-Multa", x = "Qtde de Multas", y = "Receita Arrecadada")

cormat <- cor(multassp[,c(3:5)]) # matriz correlação entre variáveis do banco
install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(cormat) # transforma matriz em banco de dados
ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
     geom_tile() # gráfico de relação entre as variáveis

tema_massa <- function(base_size = 12, base_family = "") { # configuração de tema
       theme_minimal(base_size = base_size, base_family = base_family) %+replace%
         theme(axis.ticks = element_line(size = 1, colour = "grey70" ),
               axis.text.x = element_text(colour= "black",size=8,hjust=.5,vjust=.5,face="plain"),
               axis.text.y = element_text(size=8,angle=0,hjust=1,vjust=0,face="plain"),
               axis.title.x = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
               axis.title.y = element_text(colour="black",size=11,angle=90,hjust=.5,vjust=0.5,face="plain"),
               title = element_text(colour="black",size=9,angle=0,hjust=.5,vjust=.5,face="plain"),
               panel.grid.major = element_line(colour = grey(0.85)),
               panel.grid.minor = element_line(colour = grey(1)),
               legend.key.size = unit(9, "mm"),
               legend.text = element_text(size = 9),
               legend.title = element_text(size = 9),
               axis.line = element_line(size = 1, colour = "grey70"))
     }

  ggplot(data = melted_cormat, aes(x=Var1, y=Var2) )+
  geom_tile(aes( fill=value))+
  labs(x = "", y = "")+
  guides(fill=guide_legend(title="Correlação de Pearson"))+ # título da legenda
  tema_massa()%+replace% # alterar o tema para a execução de um gráfico específico
  theme(legend.key.size = unit(7, "mm"), axis.text.x = element_text(size=8, angle = 50, hjust=1,vjust=.9,face="plain"),
  axis.text.y = element_text(size=8,angle=0,hjust=1,vjust=0,face="plain"))
  ggsave("cormat.png", width = 6, height = 5, units = "in")

  # agregar valores por ano
dataAno <- aggregate(multassp$receita, by = list(ano = multassp$ano), sum)

# grafico
ggplot(data = dataAno, aes(x = ano, y = x, group = 1)) +
  geom_line(color = "#003152", size = 1.3) +
  geom_label_repel(data = dataAno, aes(x = ano, y = x, label = dataAno$x), size= 3.2)+
  labs(y= "Receita Bruta Repassada ao DETRAN")+
  tema_massa()
