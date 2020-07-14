#Definindo o diretório de trabalho
setwd("C:/Projetos/Git/pos/estatistica/r/analise_salario_populacao_eua")

#Carregando a Base de Dados
dados <- read.csv("Dados.csv", header = T, sep = ";", dec = ",")
attach(dados)

#Questão 1 (1,50) - INÍCIO --------------------------------------------------------------------------------------------------
#Calcule as medidas de posição (Média, Mediana, Máximo, Mínimo, 1º Quartil e 3º quartil) 
#para as variáveis "salario", "instrucao", "experiência" e "idade". 

#Análise do Salário
mean(dados$salario) #Média
median(dados$salario) #Mediana
max(dados$salario) #Máximo
min(dados$salario) #Mínimo
quantile(dados$salario, probs = 0.25) #1º Quartil
quantile(dados$salario, probs = 0.75) #3º Quartil

#Análise da Instrução
mean(dados$instrucao) #Média
median(dados$instrucao) #Mediana
max(dados$instrucao) #Máximo
min(dados$instrucao) #Mínimo
quantile(dados$instrucao, probs = 0.25) #1º Quartil
quantile(dados$instrucao, probs = 0.75) #3º Quartil

#Análise da Experiência
mean(dados$experiencia) #Média
median(dados$experiencia) #Mediana
max(dados$experiencia) #Máximo
min(dados$experiencia) #Mínimo
quantile(dados$experiencia, probs = 0.25) #1º Quartil
quantile(dados$experiencia, probs = 0.75) #3º Quartil

#Análise da Idade
round(mean(dados$idade)) #Média
median(dados$idade) #Mediana
max(dados$idade) #Máximo
min(dados$idade) #Mínimo
quantile(dados$idade, probs = 0.25) #1º Quartil
quantile(dados$idade, probs = 0.75) #3º Quartil

#Questão 2 (1,50) - INÍCIO -----------------------------------------------------------------------------------------------
#Calcule as medidas de dispersão (Amplitude, Desvio-Padrão, Variância, Coeficiente de Variação, Assimetria e Curtose)
#para as variáveis "salario", "instrucao", "experiência" e "idade". 

# Habilitar a função fBasics para Assimetria e Curtose
library(fBasics)

# Análise de Salário
max(dados$salario) - min(dados$salario) #Amplitude
sd(dados$salario) #Desvio Padrão
var(dados$salario) #Variância
sd(dados$salario)/mean(dados$salario) #Coeficiente de Variação
skewness(dados$salario) #Assimetria - padrão é moment
kurtosis(dados$salario) #Curtose

# Análise da Instrução
max(dados$instrucao) - min(dados$instrucao) #Amplitude
sd(dados$instrucao) #Desvio Padrão
var(dados$instrucao) #Variância
sd(dados$instrucao)/mean(dados$instrucao) #Coeficiente de Variação
skewness(dados$instrucao) #Assimetria - padrão é moment
kurtosis(dados$instrucao) #Curtose

# Análise da Experiência
max(dados$experiencia) - min(dados$experiencia) #Amplitude
sd(dados$experiencia) #Desvio Padrão
var(dados$experiencia) #Variância
sd(dados$experiencia)/mean(dados$experiencia) #Coeficiente de Variação
skewness(dados$experiencia) #Assimetria - padrão é moment
kurtosis(dados$experiencia) #Curtose

# Análise da Idade
max(dados$idade) - min(dados$idade) #Amplitude
sd(dados$idade) #Desvio Padrão
var(dados$idade) #Variância
sd(dados$idade)/mean(dados$idade) #Coeficiente de Variação
skewness(dados$idade) #Assimetria - padrão é moment
kurtosis(dados$idade) #Curtose


#Questão 3 (1,50) - INÍCIO -----------------------------------------------------------------------------------------------
#Considere uma análise que possa ser realizada sobre a variável salario. 
#Faça os procedimentos destacados a seguir: 

# a. Calcule a média e a mediana do "salario" para mulheres e homens separadamente.
mean(dados$salario[dados$sexo == 1]) #Mulheres
mean(dados$salario[dados$sexo == 0]) #Homens

median(dados$salario[dados$sexo == 1]) #Mulheres
median(dados$salario[dados$sexo == 0]) #Homens

# Qual é a tendência apresentada para média e para mediana?

# b. Calcule a média do "salario" para brancos e não brancos.
mean(dados$salario[dados$cor == 1]) #Não Branca
mean(dados$salario[dados$cor == 0]) #Branca

median(dados$salario[dados$cor == 1]) #Não Branca
median(dados$salario[dados$cor == 0]) #Branca


#Questão 4 (1,00) - INÍCIO -----------------------------------------------------------------------------------------------
#Faça um gráfico Box-Plot para as variáveis salario, instrucao, experiencia e idade 
boxplot(dados$salario, main="Salário") # acima de 30 mil reais
boxplot(dados$instrucao, main="Instrução") 
boxplot(dados$experiencia, main="Experiência")
boxplot(dados$idade, main="Idade")
#e identifique se existem outliers.

#Quantas observações deveriam ser excluídas em cada variável por serem prováveis outliers? 

q1 <- quantile(dados$salario, probs = 0.25) #1º Quartil
q3 <- quantile(dados$salario, probs = 0.75) #3º Quartil
outliers <- q3 + 1.5 * (q3 - q1)
length(dados$salario[dados$salario >= outliers]) #número de observações que devem ser excluídas por serem possíveis outliers

q1 <- quantile(dados$instrucao, probs = 0.25) #1º Quartil
q3 <- quantile(dados$instrucao, probs = 0.75) #3º Quartil
outliers <- q1 - 1.5 * (q3 - q1)
length(dados$instrucao[dados$instrucao <= outliers])  #número de observações que devem ser excluídas por serem possíveis outliers

q1 <- quantile(dados$experiencia, probs = 0.25) #1º Quartil
q3 <- quantile(dados$experiencia, probs = 0.75) #3º Quartil
outliers <- q3 + 1.5 * (q3 - q1)
length(dados$experiencia[dados$experiencia >= outliers])  #número de observações que devem ser excluídas por serem possíveis outliers


#Questão 5 (2,00) - Considerando os gráficos de dispersão, construa-os conforme pedido a seguir:
#a. Faça um gráfico que relacione o "salario" com o tempo de "instrucao". 

plot(dados$salario, dados$instrucao, xlab="Salários", ylab="Tempo de Instrução", main = "Salário versus Tempo de Instrução")
#adicionalmente 
tend <- lm(dados$instrucao ~ dados$salario)
abline(tend, col="red")
abline(h=mean(dados$instrucao), col="green")
abline(v=mean(dados$salario), col="green")

plot(dados$instrucao, dados$salario, xlab="Instrução", ylab="Salário", main = "Tempo de Instrução versus Salário")
#adicionalmente 
tend <- lm(dados$salario ~ dados$instrucao)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$instrucao), col="green")

#Analise uma eventual tendência dos dados. 

#b. Faça um gráfico que relacione o "salario" com o tempo de "experiência". 
plot(dados$experiencia, dados$salario, xlab="Experiência", ylab="Salário", main = "Tempo de Experiência versus Salário")
#adicionalmente 
tend <- lm(dados$salario ~ dados$experiencia)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$experiencia), col="green")

#Analise uma eventual tendência dos dados. 

#c. Faça um gráfico que relacione o "salario" com a "idade". 
plot(dados$idade, dados$salario, xlab="Idade", ylab="Salário", main = "Idade versus Salário")
#adicionalmente 
tend <- lm(dados$salario ~ dados$idade)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$idade), col="green")

#Analise uma eventual tendência dos dados. 
#d. Faça um gráfico que relacione a "experiencia" com o tempo de "instrucao". 
plot(dados$experiencia, dados$instrucao, xlab="Experiência", ylab="Tempo de Instrução", main = "Experiência versus Tempo de Instrução")
#adicionalmente 
tend <- lm(dados$instrucao ~ dados$experiencia)
abline(tend, col="red")
abline(h=mean(dados$instrucao), col="green")
abline(v=mean(dados$experiencia), col="green")

#Analise uma eventual tendência dos dados. 

#Questão 6 (1,00) - INÍCIO -----------------------------------------------------------------------------------------------
#Considerando as variáveis estritamente quantitativas. 
#Construa um Histograma e identifique a variável com melhor ajuste percebido para a distribuição normal de probabilidade. 

library(ggplot2)
ggplot(dados, aes(x = salario)) + 
geom_histogram(bins = 30)

ggplot(dados, aes(x = idade)) + 
  geom_histogram(bins = 50)

ggplot(dados, aes(x = instrucao)) + 
  geom_histogram(bins = 10)

ggplot(dados, aes(x = experiencia)) + 
  geom_histogram(bins = 20)

#Questão 7 (1,50) - INÍCIO -----------------------------------------------------------------------------------------------
#Considere que a variável "salario" segue uma distribuição normal de probabilidade. 
#A média e o desvio-padrão já foram calculados. Assim determine o que se pede: 

# a. Qual a probabilidade estimada de uma pessoa ganhar mais do que o 3º quartil? 

Media <- mean(dados$salario)
DesvPad <- sd(dados$salario)
Q3 <- quantile(dados$salario, probs=0.75)
1-pnorm(Q3, mean = Media, sd = DesvPad)

# b. Qual a probabilidade estimada de uma pessoa ganhar menos do que o 1º quartil? 

Media <- mean(dados$salario)
DesvPad <- sd(dados$salario)
Q1 <- quantile(dados$salario, probs=0.25)
pnorm(Q1, mean = Media, sd = DesvPad)


        