# ___________________________________________________________________________ #                                                                             #
#                                                                             #
#                         Felicidade Mundial 2019                             #
#                                                                             #
# | Autora |    Rosana Ferreira Soares dos Santos                             #
#               https:///www.linkedin.com/in/rosanafssantos                   #
#                                                                             #
# | Dataset |   World Happiness Report 2019, ref. Kaggle                      # 
#               � uma pesquisa sobre estado global de felicidade mundial.     #
#               Foram pesquisados 6 fatores                                   #
#               suporte social, liberdade para fazer escolhas,generosidade,   #
#               percep��o de corrup��o, expectativa de vida saud�vel e GDP    #                                         #
#                                                                             #
# | An�lise |   GDP e Expectativa de Vida Saud�vel s�o os fatores             #
#               com coeficiente de correla��o forte = 0.87 (ref. Pearson)     #
#               GDP vem do ingl�s "Gross Domestic Product"                    #
#               GDP = soma da produ��o econ�mica do pa�s em um per�odo        #
#               Em portugu�s equivale ao PIB, Produto Interno Bruto           #
# ___________________________________________________________________________ #  


library(tidyverse)
library(ggplot2)
library(corrplot)

# Carregando o dataset "World Happiness Report 2019" do tipo data.frame
# Data frame � uma lista de vetores
Happiness <- read.csv(file.choose(), header = T, sep = ',')

# Todas as variaveis do dataset representam a classifica��o do pa�s no ranking de 0 a 156.
# Quanto mais baixo o valor, mais alta a posi��o no ranking, melhor o score de felicidade.

# Praticando fun��es que aprendi nos cursos R-Ladies 
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
tail     (Happiness)    # tail()      - 06 �ltimas linhas

# Renomeando vari�veis para facilitar compreens�o
names(Happiness) <- c("Pa�s","Satisfa��o", "Desvio","Positivo","Negativo","Social",
                      "Liberdade","Corrup��o", "Generosidade", "GDP", "Expectativa")

# Satisfa��o    = Cantril Ladder � uma medida de satisfa��o de vida = �ndice de Felicidade
# Desvio        = Desvio padr�o do ladder
# Positiva      = Mensura��o de emo��o positiva
# Negativa      = Mensura��o de emo��o negativa
# Social        = At� que ponto o suporte SOCIAL contribuiu para o c�lculo do score de Felicidade
# Liberdade     = At� que ponto a LIBERDADE contribuiu para o c�lculo do score de Felicidade
# Corrup��o     = At� que ponto a CORRUP��O contribuiu para o c�lculo do score de Felicidade
# Generosidade  = At� que ponto a GENEROSIDADE contribuiu para o c�lculo do score de Felicidade
# GDP           = At� que ponto o �ndice GDP (Gross Domestuc Product) de economia per capta contribuiu para o c�lculo do score de Felicidade
# Expectativa   = At� que ponto a EXPECTATIVA de vida saud�vel contribuiu para o c�lculo do score de Felicidade

Happiness[1]                                                        # Uma outra maneira de Visualizar a primeira coluna = pa�ses 
summary  (Happiness$Satisfa��o)                                     # Resumo de �ndice de Satisfa��o

Happiness[2:11] <- lapply(Happiness[2:11], as.numeric)              # convertendo do formato integer para numeric
str  (Happiness)                                                    # str()- estrutura do data.frame

# Criando uma matriz para analisar padr�es, visualizar correla��es entre as variaveis
pairs(Happiness[5:11])
###### Expectativa de vida saud�vel x GPD apresenta forte correla��o


# Calculando os coeficientes de correla��o
correla��es <- round(cor(Happiness[2:11], use="complete.obs"), 2)    # An�lise de correla��o entre adas vari�veis
correla��es[5:10,5:10]   
# Aprensentando matriz de correla��o normalizada                                              
min <- min(correla��es)
correla��es_symnum <- symnum(correla��es, cutpoints = c(min, 0.3, 0.6, 0.85, 1),
      symbols = c("_",".",":","$"), abbr.colnames = F)
correla��es_symnum[5:10,5:10]

forte <- correla��es[9,10]  
forte

# Correlogram
corrplot(correla��es[5:10,5:10], type = "upper", method = "circle", col = topo.colors(100), diag=T)

###### Expectativa de vida saud�vel x GPD apresenta forte correla��o
###### Detalhamento gr�fico
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

# Pr�ximos passos: aprender novas fun��es e simplificar o script



