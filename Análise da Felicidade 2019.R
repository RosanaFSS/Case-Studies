# ___________________________________________________________________________ #                                                                             #
#                                                                             #
#                         Felicidade Mundial 2019                             #
#                                                                             #
# | Autora |    Rosana Ferreira Soares dos Santos                             #
#               https:///www.linkedin.com/in/rosanafssantos                   #
#                                                                             #
# | Dataset |   World Happiness Report 2019, ref. Kaggle                      # 
#               É uma pesquisa sobre estado global de felicidade mundial.     #
#               Foram pesquisados 6 fatores                                   #
#               suporte social, liberdade para fazer escolhas,generosidade,   #
#               percepção de corrupção, expectativa de vida saudável e GDP    #                                         #
#                                                                             #
# | Análise |   GDP e Expectativa de Vida Saudável são os fatores             #
#               com coeficiente de correlação forte = 0.87 (ref. Pearson)     #
#               GDP vem do inglês "Gross Domestic Product"                    #
#               GDP = soma da produção econômica do país em um período        #
#               Em português equivale ao PIB, Produto Interno Bruto           #
# ___________________________________________________________________________ #  


library(tidyverse)
library(ggplot2)
library(corrplot)

# Carregando o dataset "World Happiness Report 2019" do tipo data.frame
# Data frame é uma lista de vetores
Happiness <- read.csv(file.choose(), header = T, sep = ',')

# Todas as variaveis do dataset representam a classificação do país no ranking de 0 a 156.
# Quanto mais baixo o valor, mais alta a posição no ranking, melhor o score de felicidade.

# Praticando funções que aprendi nos cursos R-Ladies 
class    (Happiness)    # class()     - tipo, neste caso fata.frame
str      (Happiness)    # str()       - estrutura do data.frame
summary  (Happiness)    # summary()   - resumo do data.frame
colnames (Happiness)    # colnames()  - nomes das colunas ou variaveis
names    (Happiness)    # names()     - nomes das colunas ou variaveis
rownames (Happiness)    # rownames()  - nomes das linhas
dim      (Happiness)    # dim()       - quantidade de linhas e colunas
ncol     (Happiness)    # ncol()      - quantidade de colunas
nrow     (Happiness)    # nrow()      - quantidade de linhas
head     (Happiness)    # head()      - 06 primeiras linhas
tail     (Happiness)    # tail()      - 06 últimas linhas

# Renomeando variáveis para facilitar compreensão
names(Happiness) <- c("País","Satisfação", "Desvio","Positivo","Negativo","Social",
                      "Liberdade","Corrupção", "Generosidade", "GDP", "Expectativa")

# Satisfação    = Cantril Ladder é uma medida de satisfação de vida = Índice de Felicidade
# Desvio        = Desvio padrão do ladder
# Positiva      = Mensuração de emoção positiva
# Negativa      = Mensuração de emoção negativa
# Social        = Até que ponto o suporte SOCIAL contribuiu para o cálculo do score de Felicidade
# Liberdade     = Até que ponto a LIBERDADE contribuiu para o cálculo do score de Felicidade
# Corrupção     = Até que ponto a CORRUPÇÃO contribuiu para o cálculo do score de Felicidade
# Generosidade  = Até que ponto a GENEROSIDADE contribuiu para o cálculo do score de Felicidade
# GDP           = Até que ponto o índice GDP (Gross Domestuc Product) de economia per capta contribuiu para o cálculo do score de Felicidade
# Expectativa   = Até que ponto a EXPECTATIVA de vida saudável contribuiu para o cálculo do score de Felicidade

Happiness[1]                                                        # Uma outra maneira de Visualizar a primeira coluna = países 
summary  (Happiness$Satisfação)                                     # Resumo de Índice de Satisfação

Happiness[2:11] <- lapply(Happiness[2:11], as.numeric)              # convertendo do formato integer para numeric
str  (Happiness)                                                    # str()- estrutura do data.frame

# Criando uma matriz para analisar padrões, visualizar correlações entre as variaveis
pairs(Happiness[5:11])
###### Expectativa de vida saudável x GPD apresenta forte correlação


# Calculando os coeficientes de correlação
correlações <- round(cor(Happiness[2:11], use="complete.obs"), 2)    # Análise de correlação entre adas variáveis
correlações[5:10,5:10]   
# Aprensentando matriz de correlação normalizada                                              
min <- min(correlações)
correlações_symnum <- symnum(correlações, cutpoints = c(min, 0.3, 0.6, 0.85, 1),
      symbols = c("_",".",":","$"), abbr.colnames = F)
correlações_symnum[5:10,5:10]

forte <- correlações[9,10]  
forte

# Correlogram
corrplot(correlações[5:10,5:10], type = "upper", method = "circle", col = topo.colors(100), diag=T)

###### Expectativa de vida saudável x GPD apresenta forte correlação
###### Detalhamento gráfico
plot(Happiness$GDP, Happiness$Expectativa, type = "p",
     pch = 1, lwd=1, col = "blue",  main = "GDP x Expectativa",
     xlab = "GDP", ylab = "Expectativa",
     cex.main=0.7, cex.lab=1, cex.axis=0.5,
     font.main=2, col.main="white",
     xlim=c(0,156), ylim=c(0,156), axes=T)
abline(lm(Happiness$GDP ~ Happiness$Expectativa), col=1, lwd=2)

# Outra forma de plotar
ggplot(Happiness, aes(x=Expectativa, y=GDP)) + 
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = T) 
  theme_classic()

# Próximos passos: aprender novas funções e simplificar o script



