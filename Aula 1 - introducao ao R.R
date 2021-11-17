##################################################################
#                                                               #
#       MINICURSO: INTRODUCAO AO USO DE R - UDESC/2021          #
#                                                               #
#################################################################

#--------  PARTE 1 : COMANDOS B?SICOS E OBJETOS NO R -------------#####

## 1. Criando um projeto no R Studio
#--- 1.1 escolher o diret?rio (pasta no seu computador)
#--- 1.2. criar um projeto


## 2. Comandos b?sicos no R 
# calculadora
2+2
2-1
2*4
3^2

# objetos
a <- 2
b <- "Rstudio"
nome = "Rafael"
nome

vetor1 <- 1:10
vetor2 <- c(2.4,4.6,5,6,8)
vetor3 <- rep(1,10)
vetor4 <- seq(from = 1, to = 10, by = 2)
nomes <- c("rafaela", "bruno")

class(vetor1)
class(vetor2)
class(nomes)

length(vetor4)

vetor5 <- 1:5
vetor5[2]

#selecionando elementos
vetor2[1]

matriz1 <- matrix(data = 1:10, nrow = 5, ncol = 2)
matriz2 <- matrix(data = c("bruno", "rafaela", "andressa", "lucas"),
                  nrow = 4, ncol = 3)

nova_matriz <- cbind(vetor6[1:5], vetor4)
nova_matriz
vetor6 <- seq(from = 10, to = 50, by = 4)
nrow(nova_matriz)
ncol(nova_matriz)
dim(nova_matriz)
#criando uma matriz partir de vetores
vetor1 <- 1:10
vetor2 <- 11:20

cbind(vetor1, vetor2)
rbind(vetor1, vetor2)

# e com vetores de classes diferentes?
cbind(vetor1, nomes)

#dataframes
df1 <- as.data.frame(matriz1)
df1
df2 <- data.frame(vetor1, nomes)
df2

ncol(df2)
nrow(df2)
colnames(df2)
rownames(df2)

df2$vetor1
df1[3,2]

colnames(df1) 
colnames(df1) <- c("variavel_a", "variavel_b")
colnames(df1)[2] <- "laguna"
## como colocar nomes nas linhas da matriz ou dataframe?

#listas
lista1 <- list(matriz1, df2)
lista1[[1]]
lista1[[2]]
names(lista1) <- c("matriz_1", "planilha_nomes")
lista1$planilha_nomes

## selecionando colunas ou linhas
matriz2[1,]
matriz2[,3]
matriz2[1:3,]
#todas menos uma
matriz1[-2,]

## Como selecionar o elemento na terceira linha e segunda coluna?

## operadores boleanos
vetor2 == 2
vetor2 == 13
vetor2 != 5
matriz1 == 2
matriz2 == "bruno"
matriz1[,1]==2
matriz1 > 3
matriz1 < 4 & matriz1 > 2
matriz1 < 4 | matriz1 > 2

which(vetor2 == 11) #posicao 1
which(matriz1[,1]==2) #linha 2 na coluna indicada

# quais elementos do vetor1 são maiores que 3 e quais posições
which(vetor1 > 3)
vetor1[which(vetor1 > 3)]

vetor6[1] = 1
which(vetor6 > 3)
vetor6[which(vetor6 > 3)]

## como saber quantos elementos satisfazem uma condicao?
as.numeric(vetor2>13)
sum(vetor2>13)

length(which(vetor2>12))

## como somar as colunas de uma matriz?

# 3. Importando planilhas 
#vendo quais objetos estao no meu diretorio
dir()

# planilha em txt
salarios <- read.table("salarios_basebal.txt", h=T) # pode usar h(ou header) e T(ou TRUE)
class(salarios)
dim(salarios)
head(salarios)
colnames(salarios)

#como selecionar a coluna "salario"?
salarios[,"salario"] # ou salarios$salario
#como selecionar a 5a linha?
salarios[1:5,]
#como selecionar da primeira ? d?cima linha?
salarios[1:10,]

salarios$salario
class(salarios$salario)
salarios$salario <- as.numeric(salarios$salario)
class(salarios$salario)

#como saber quais e quantos jogadores ganham mais que 500 mil dolares?
posicoes <- which(salarios$salario > 500000)

salarios[posicoes, "id_jogador"]
salarios$id_jogador[posicoes]

# qual jogador ganha exatos 518290 dolares?
which(salarios$salario==518290)
salarios[98,]

#trocando o valor do salario por outro
salarios[98,"salario"] <- 518290.50

#planilha em csv
pinguins <- read.csv("dados_pinguins.csv")
head(pinguins)

#como saber o numero de linhas e o numero de colunas de penguins?

#como saber o nome das colunas de penguins?

#transformando classe das variaveis
class(pinguins$sexo)

pinguins$sexo <- as.factor(pinguins$sexo)
class(pinguins$sexo)
levels(pinguins$sexo)

## dados faltantes
is.na(pinguins$comprimento_bico)
which(is.na(pinguins$comprimento_bico))

#como remover o NA?
pinguins_sem_na <- pinguins[-which(is.na(pinguins$comprimento_bico)), ]

#transformar variaveis
colnames(pinguins_sem_na)
pinguins.log <- log(pinguins_sem_na[,c(3,4,5,6)])

## HELP - fun??es e argumentos
?mean
?which
?levels
??mean

#outras funcoes importantes: sort, order, length, select, aggregate (ver mais no tidyverse)
#apply, lapply, sapply


## ------- PARTE 3: GR?FICOS COM O R BASE PLOT --------------#######

## importando dados
pinguins <- read.csv("dados usar/dados_pinguins.csv")

# Qual tipo de grafico vamos fazer? Vamos observar primeiro as nossas variaveis
#site legal: https://www.data-to-viz.com/
str(pinguins)

#histograma do comprimento da nadadeira
hist(pinguins$comprimento_nadadeira)
hist(pinguins$sexo)

tabela <- table(pinguins$sexo)
tabela <- as.data.frame(tabela)
barplot(height = tabela$Freq, names = tabela$Var1, col = c("black", "red"))

#comprimento da nadadeira e o comprimento do bico
plot(x = pinguins$comprimento_nadadeira, y = pinguins$comprimento_bico)

par(las = 1, mar=c(4,4,1.5,0.5), mgp = c(2.5,0.8,0), cex.axis = 0.9, cex.lab = 1, cex.main = 1)
plot(x = pinguins$comprimento_nadadeira, y = pinguins$comprimento_bico, 
    xlab = "Comprimento da nadadeira", ylab = "Comprimento do bico", 
    pch = 21, col = "black", bg = "violetred3", xlim = c(175,235), 
    cex = 1.2, main = "Pinguins")


## Agora voc?! Tente plotar o comprimento do bico versus a profundidade do bico

#https://www.r-graph-gallery.com/6-graph-parameters-reminder.html

## ser? que o comprimento da nadadeira muda entre os sexos?
boxplot(pinguins$comprimento_nadadeira ~ pinguins$sexo) #d? erro se a vari?vel categ?rica n?o for um fator

boxplot(pinguins$comprimento_nadadeira ~ pinguins$sexo, xlab = "Sexo", 
        ylab = "Comprimento da nadadeira", col = "chocolate") 

boxplot(pinguins$comprimento_nadadeira ~ pinguins$especie)

## e se eu quiser plotar os dois graficos em uma mesma figura, separadamente?
par(mar=c(3,3,0,0), mfrow = c(1,2), mgp = c(2,0.6,0), 
    las=1, cex.axis = 0.6, oma = c(1, 1, 0.5, 0.5))
boxplot(pinguins$comprimento_nadadeira ~ pinguins$especie, 
        xlab = "Esp?cie", ylab = "Comprimento da nadadeira", col = "seagreen3",
        pch = 16)
boxplot(pinguins$comprimento_nadadeira ~ pinguins$sexo, xlab = "Sexo", ylab = "", col = "chocolate") 
#colocar uma legenda, o N em cima
text(x = 0.8, y = 229.1, labels = "N = 30", cex = 0.8)

## e se quiser colocar as duas informacoes na mesma figura?
par(mar=c(4,4,0.5,0.5), mgp = c(2.5,0.8,0), las=1, cex.axis = 0.8, mfrow=c(1,1))
boxplot(pinguins$comprimento_nadadeira ~ pinguins$especie*pinguins$sexo,
        col = c("slateblue1" , "tomato"), xaxt="n", xlab = "Esp?cie", ylab = "Comprimento da nadadeira")
axis(side = 1, at = c(1.5,3.5,5.5), labels = c("Adelia", "Barbicha", "Guentoo"))

legend(x=0.5, y=230, legend = c("Femea", "Macho"), 
       col=c("slateblue1" , "tomato"),
       pch = 15, bty = "n", cex = 0.9)

abline(v=2.5,lty=2, col="grey30")
abline(v=4.5,lty=2, col="grey30")


#funcoes importantes para graficos
?par
?lines
?points
?axis
?text
?legend

