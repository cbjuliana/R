#Estat�stica

#Definindo o diret�rio de trabalho
setwd("C:/Projetos/Git/pos/estatistica/r/analise_estatistica_salario_populacao_eua")

#Carregando a Base de Dados
dados <- read.csv("Dados.csv", header = T, sep = ";", dec = ",")
attach(dados)

#Quest�o 1 (1,50) - IN�CIO --------------------------------------------------------------------------------------------------
#Calcule as medidas de posi��o (M�dia, Mediana, M�ximo, M�nimo, 1� Quartil e 3� quartil) 
#para as vari�veis "salario", "instrucao", "experi�ncia" e "idade". 

#An�lise do Sal�rio
mean(dados$salario) #M�dia
median(dados$salario) #Mediana
max(dados$salario) #M�ximo
min(dados$salario) #M�nimo
quantile(dados$salario, probs = 0.25) #1� Quartil
quantile(dados$salario, probs = 0.75) #3� Quartil

#An�lise da Instru��o
mean(dados$instrucao) #M�dia
median(dados$instrucao) #Mediana
max(dados$instrucao) #M�ximo
min(dados$instrucao) #M�nimo
quantile(dados$instrucao, probs = 0.25) #1� Quartil
quantile(dados$instrucao, probs = 0.75) #3� Quartil

#An�lise da Experi�ncia
mean(dados$experiencia) #M�dia
median(dados$experiencia) #Mediana
max(dados$experiencia) #M�ximo
min(dados$experiencia) #M�nimo
quantile(dados$experiencia, probs = 0.25) #1� Quartil
quantile(dados$experiencia, probs = 0.75) #3� Quartil

#An�lise da Idade
round(mean(dados$idade)) #M�dia
median(dados$idade) #Mediana
max(dados$idade) #M�ximo
min(dados$idade) #M�nimo
quantile(dados$idade, probs = 0.25) #1� Quartil
quantile(dados$idade, probs = 0.75) #3� Quartil

#Quest�o 2 (1,50) - IN�CIO -----------------------------------------------------------------------------------------------
#Calcule as medidas de dispers�o (Amplitude, Desvio-Padr�o, Vari�ncia, Coeficiente de Varia��o, Assimetria e Curtose)
#para as vari�veis "salario", "instrucao", "experi�ncia" e "idade". 

# Habilitar a fun��o fBasics para Assimetria e Curtose
library(fBasics)

# An�lise de Sal�rio
max(dados$salario) - min(dados$salario) #Amplitude
sd(dados$salario) #Desvio Padr�o
var(dados$salario) #Vari�ncia
sd(dados$salario)/mean(dados$salario) #Coeficiente de Varia��o
skewness(dados$salario) #Assimetria - padr�o � moment
kurtosis(dados$salario) #Curtose

# An�lise da Instru��o
max(dados$instrucao) - min(dados$instrucao) #Amplitude
sd(dados$instrucao) #Desvio Padr�o
var(dados$instrucao) #Vari�ncia
sd(dados$instrucao)/mean(dados$instrucao) #Coeficiente de Varia��o
skewness(dados$instrucao) #Assimetria - padr�o � moment
kurtosis(dados$instrucao) #Curtose

# An�lise da Experi�ncia
max(dados$experiencia) - min(dados$experiencia) #Amplitude
sd(dados$experiencia) #Desvio Padr�o
var(dados$experiencia) #Vari�ncia
sd(dados$experiencia)/mean(dados$experiencia) #Coeficiente de Varia��o
skewness(dados$experiencia) #Assimetria - padr�o � moment
kurtosis(dados$experiencia) #Curtose

# An�lise da Idade
max(dados$idade) - min(dados$idade) #Amplitude
sd(dados$idade) #Desvio Padr�o
var(dados$idade) #Vari�ncia
sd(dados$idade)/mean(dados$idade) #Coeficiente de Varia��o
skewness(dados$idade) #Assimetria - padr�o � moment
kurtosis(dados$idade) #Curtose


#Quest�o 3 (1,50) - IN�CIO -----------------------------------------------------------------------------------------------
#Considere uma an�lise que possa ser realizada sobre a vari�vel salario. 
#Fa�a os procedimentos destacados a seguir: 

# a. Calcule a m�dia e a mediana do "salario" para mulheres e homens separadamente.
mean(dados$salario[dados$sexo == 1]) #Mulheres
mean(dados$salario[dados$sexo == 0]) #Homens

median(dados$salario[dados$sexo == 1]) #Mulheres
median(dados$salario[dados$sexo == 0]) #Homens

# Qual � a tend�ncia apresentada para m�dia e para mediana?

# b. Calcule a m�dia do "salario" para brancos e n�o brancos.
mean(dados$salario[dados$cor == 1]) #N�o Branca
mean(dados$salario[dados$cor == 0]) #Branca

median(dados$salario[dados$cor == 1]) #N�o Branca
median(dados$salario[dados$cor == 0]) #Branca


#Quest�o 4 (1,00) - IN�CIO -----------------------------------------------------------------------------------------------
#Fa�a um gr�fico Box-Plot para as vari�veis salario, instrucao, experiencia e idade 
boxplot(dados$salario, main="Sal�rio") # acima de 30 mil reais
boxplot(dados$instrucao, main="Instru��o") 
boxplot(dados$experiencia, main="Experi�ncia")
boxplot(dados$idade, main="Idade")
#e identifique se existem outliers.

#Quantas observa��es deveriam ser exclu�das em cada vari�vel por serem prov�veis outliers? 

q1 <- quantile(dados$salario, probs = 0.25) #1� Quartil
q3 <- quantile(dados$salario, probs = 0.75) #3� Quartil
outliers <- q3 + 1.5 * (q3 - q1)
length(dados$salario[dados$salario >= outliers]) #n�mero de observa��es que devem ser exclu�das por serem poss�veis outliers

q1 <- quantile(dados$instrucao, probs = 0.25) #1� Quartil
q3 <- quantile(dados$instrucao, probs = 0.75) #3� Quartil
outliers <- q1 - 1.5 * (q3 - q1)
length(dados$instrucao[dados$instrucao <= outliers])  #n�mero de observa��es que devem ser exclu�das por serem poss�veis outliers

q1 <- quantile(dados$experiencia, probs = 0.25) #1� Quartil
q3 <- quantile(dados$experiencia, probs = 0.75) #3� Quartil
outliers <- q3 + 1.5 * (q3 - q1)
length(dados$experiencia[dados$experiencia >= outliers])  #n�mero de observa��es que devem ser exclu�das por serem poss�veis outliers


#Quest�o 5 (2,00) - Considerando os gr�ficos de dispers�o, construa-os conforme pedido a seguir:
#a. Fa�a um gr�fico que relacione o "salario" com o tempo de "instrucao". 

plot(dados$salario, dados$instrucao, xlab="Sal�rios", ylab="Tempo de Instru��o", main = "Sal�rio versus Tempo de Instru��o")
#adicionalmente 
tend <- lm(dados$instrucao ~ dados$salario)
abline(tend, col="red")
abline(h=mean(dados$instrucao), col="green")
abline(v=mean(dados$salario), col="green")

plot(dados$instrucao, dados$salario, xlab="Instru��o", ylab="Sal�rio", main = "Tempo de Instru��o versus Sal�rio")
#adicionalmente 
tend <- lm(dados$salario ~ dados$instrucao)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$instrucao), col="green")

#Analise uma eventual tend�ncia dos dados. 

#b. Fa�a um gr�fico que relacione o "salario" com o tempo de "experi�ncia". 
plot(dados$experiencia, dados$salario, xlab="Experi�ncia", ylab="Sal�rio", main = "Tempo de Experi�ncia versus Sal�rio")
#adicionalmente 
tend <- lm(dados$salario ~ dados$experiencia)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$experiencia), col="green")

#Analise uma eventual tend�ncia dos dados. 

#c. Fa�a um gr�fico que relacione o "salario" com a "idade". 
plot(dados$idade, dados$salario, xlab="Idade", ylab="Sal�rio", main = "Idade versus Sal�rio")
#adicionalmente 
tend <- lm(dados$salario ~ dados$idade)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$idade), col="green")

#Analise uma eventual tend�ncia dos dados. 
#d. Fa�a um gr�fico que relacione a "experiencia" com o tempo de "instrucao". 
plot(dados$experiencia, dados$instrucao, xlab="Experi�ncia", ylab="Tempo de Instru��o", main = "Experi�ncia versus Tempo de Instru��o")
#adicionalmente 
tend <- lm(dados$instrucao ~ dados$experiencia)
abline(tend, col="red")
abline(h=mean(dados$instrucao), col="green")
abline(v=mean(dados$experiencia), col="green")

#Analise uma eventual tend�ncia dos dados. 

#Quest�o 6 (1,00) - IN�CIO -----------------------------------------------------------------------------------------------
#Considerando as vari�veis estritamente quantitativas. 
#Construa um Histograma e identifique a vari�vel com melhor ajuste percebido para a distribui��o normal de probabilidade. 

library(ggplot2)
ggplot(dados, aes(x = salario)) + 
geom_histogram(bins = 30)

ggplot(dados, aes(x = idade)) + 
  geom_histogram(bins = 50)

ggplot(dados, aes(x = instrucao)) + 
  geom_histogram(bins = 10)

ggplot(dados, aes(x = experiencia)) + 
  geom_histogram(bins = 20)

#Quest�o 7 (1,50) - IN�CIO -----------------------------------------------------------------------------------------------
#Considere que a vari�vel "salario" segue uma distribui��o normal de probabilidade. 
#A m�dia e o desvio-padr�o j� foram calculados. Assim determine o que se pede: 

# a. Qual a probabilidade estimada de uma pessoa ganhar mais do que o 3� quartil? 

Media <- mean(dados$salario)
DesvPad <- sd(dados$salario)
Q3 <- quantile(dados$salario, probs=0.75)
1-pnorm(Q3, mean = Media, sd = DesvPad)

# b. Qual a probabilidade estimada de uma pessoa ganhar menos do que o 1� quartil? 

Media <- mean(dados$salario)
DesvPad <- sd(dados$salario)
Q1 <- quantile(dados$salario, probs=0.25)
pnorm(Q1, mean = Media, sd = DesvPad)


        