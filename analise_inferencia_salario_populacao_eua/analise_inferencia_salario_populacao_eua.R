# -----------------------------------------------
# Inferência Estatística
# -----------------------------------------------

# Mudando o diretório
setwd("C:/Projetos/Git/pos/r/analise_inferencia_salario_populacao_eua")

#Pacotes
library(fBasics) # Estatística Básicas
library(car) # Teste de Levene
#library(normtest) #Testes de Normalidade

# Banco de Dados
dados <- read.csv("Dados.csv", header = T, sep = ";", dec = ",")

#Questão 1

#a. O Salário médio dos homens se difere do salário médio das mulheres?

#Estatística Descritiva
summary(dados$salario)
summary(dados$salario[dados$sexo==0]) #Salário Homens
summary(dados$salario[dados$sexo==1]) #Salário Mulheres
#Pela estatística descritiva já observamos que os homens ganham em média mais que as mulheres

basicStats(dados$salario)
basicStats(dados$salario[dados$sexo==0]) #Salário Homens
basicStats(dados$salario[dados$sexo==1]) #Salário Mulheres

#Podemos perceber problema de assimetria em relação ao salário dos homens
# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 1.426094


#Podemos perceber problema de assimetria e curtose em relação ao salário das mulheres
# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 2.571936
# Problema de Curtose existe quando Curt(x) < -7 ou Curt(x) > +7
# Neste caso, tem-se curtose = 11.303823 

#Teste de Normalidade de Shapiro
#Pressupostos
#   Normalidade
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
library(normtest)
shapiro.test(dados$salario[dados$sexo==0]) #Salário Homens
shapiro.test(dados$salario[dados$sexo==1]) #Salário Mulheres
#     Conclusão: Para ambos os grupos, o p-value = 0.0000 < 0.05 = alpha, então rejeitamos H0 e aceitamos H1
#                Logo as distribuição não são normais

#Teste de Homocedasticidade - Levene
#Pressupostos
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$sexo)
#      Conclusão: As variâncias dos grupos são diferentes, pois
#                 Pr(>F) = 0.0000 < 0.05 = alpha. Logo, rejeitamos H0 e aceitamos H1.

#Teste Paramétrico t - 2 amostras Independentes
#Pressupostos
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$salario~dados$sexo)
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, rejeitamos H0 e aceitamos H1.
#   Logo, há diferença significativa no salário de homens e mulheres
#   Os homens, em média, possuem um salário maior do que as mulheres.

#Teste Não Paramétrico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$salario ~ dados$sexo)
#Pressupostos
#H0 - Salário dos homens é igual ao salário das mulheres
#H1 - Salário dos homens é diferente do salário das mulheres 

#Conclusão: Como p-value = 0.0000 < 0.05 = alpha, logo rejeitamos H0 e aceitamos H1. 


#b. O Salário médio das pessoas não brancas se difere das pessoas brancas?

#Estatística Descritiva
summary(dados$cor)
summary(dados$salario[dados$cor==0]) #Salário Brancos
summary(dados$salario[dados$cor==1]) #Salário Não Brancos
#Pela estatística descritiva já observamos que os brancos ganham em média mais que não brancos

basicStats(dados$salario)
basicStats(dados$salario[dados$cor==0]) #Salário Brancos
basicStats(dados$salario[dados$cor==1]) #Salário Não Brancos

# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 1.809920

# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 1.588035 


#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
library(normtest)
shapiro.test(dados$salario[dados$cor==0]) #Salário Brancos
shapiro.test(dados$salario[dados$cor==1]) #Salário Não Brancos
#     Conclusão: Para ambos os grupos, as distribuição não são normais


#Teste de Homocedasticidade - Levene, para testar se as variâncias dos grupos são iguais ou diferentes
#Pressupostos
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$cor)
#     Conclusão: As variâncias dos grupos são diferentes


#Teste Paramétrico - Teste t - 2 amostras Independentes, para testar se as médias dos grupos são iguais ou diferentes.
#Pressupostos
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$salario~dados$cor)
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa no salário de brancos e não brancos
#   Os brancos, em média, possuem um salário maior do que os não brancos.


#Teste Não Paramétrico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$salario ~ dados$cor)


#c) O Salário médio das pessoas casadas se difere das pessoas solteiras?

#Estatística Descritiva
summary(dados$est_civil)
summary(dados$salario[dados$est_civil==0]) #Soletiro
summary(dados$salario[dados$est_civil==1]) #Casado
#Pela estatística descritiva já observamos que os casados ganham em média mais que os solteiros

basicStats(dados$est_civil)
basicStats(dados$salario[dados$est_civil==0]) #Salário Brancos
basicStats(dados$salario[dados$est_civil==1]) #Salário Não Brancos

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
library(normtest)
shapiro.test(dados$salario[dados$est_civil==0]) #Solteiros
shapiro.test(dados$salario[dados$est_civil==1]) #Casados
#     Conclusão: Para ambos os grupos, as distribuição não são normais

#Teste de Homocedasticidade - Levene, para testar se as variâncias dos grupos são iguais ou diferentes
#Pressupostos
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$est_civil)
#     Conclusão: As variâncias dos grupos são diferentes

#Teste Paramétrico - Teste t - 2 amostras Independentes, para testar se as médias dos grupos são iguais ou diferentes.
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$salario~dados$est_civil)
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa no salário de solteiros e casados
#   Os casados, em média, possuem um salário maior do que os solteiros

#Teste Não Paramétrico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes.
wilcox.test(dados$salario ~ dados$est_civil)

#d) Considerando o sexo das pessoas e seu estado civil conjuntamente, é possível afirmar que
#algumas dos grupos formados possui média salarial diferente dos demais?

#Criando nova coluna indicando o sexo e o estado civil conjuntamente
#Onde:
# 0 = Homens solteiros
# 1 = Homens casados
# 2 = Mulheres solteiras
# 3 = Mulheres casadas

dados["sexo_estCivil"] <- (dados$sexo*2) + dados$est_civil

#Estatísticas Descritivas
summary(dados$salario)
summary(dados$salario[dados$sexo_estCivil==0]) #Homens solteiros
summary(dados$salario[dados$sexo_estCivil==1]) #Homens casados
summary(dados$salario[dados$sexo_estCivil==2]) #Mulheres solteiras
summary(dados$salario[dados$sexo_estCivil==3]) #Mulheres casadas

basicStats(dados$salario)
basicStats(dados$salario[dados$sexo_estCivil==0]) #Homens solteiros
basicStats(dados$salario[dados$sexo_estCivil==1]) #Homens casados
basicStats(dados$salario[dados$sexo_estCivil==2]) #Mulheres solteiras
basicStats(dados$salario[dados$sexo_estCivil==3]) #Mulheres casadas

View(dados)

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(dados$salario[dados$sexo_estCivil==0]) #Homens solteiros 
shapiro.test(dados$salario[dados$sexo_estCivil==1]) #Homens casados   
shapiro.test(dados$salario[dados$sexo_estCivil==2]) #Mulheres solteiras        
shapiro.test(dados$salario[dados$sexo_estCivil==3]) #Mulheres casadas
#     Conclusão: Para todos os grupos, as distribuições não são normais


#Teste de Homocedasticidade - Levene, para testar se as variâncias dos grupos são iguais ou diferentes
#Pressupostos
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(dados$salario, dados$sexo_estCivil)
#     Conclusão: Pr(>F) (p-value) = 0.0000 < 0.05, rejeitamos H0 e aceitamos H1
#     As variâncias dos grupos são diferentes

# Testes Não-Paramétricos de Kruskal - alternativo ao Teste ANOVA
#Pressupostos
#   H0 - Todas as média são iguais
#   H1 - Há pelo menos dois grupos com médias diferentes.
kruskal.test(dados$salario, dados$sexo_estCivil)

#Teste ANOVA
#Pressupostos
#   H0 - Todas as média são iguais
#   H1 - Há pelo menos dois grupos com médias diferentes.
anova <- aov(dados$salario ~ dados$sexo_estCivil)
summary(anova)
#   Conclusão: Como o p-value (PR(>F))= 0,0000 < 0,05, Rejeitamos H0 e Aceitamos H1.
#   Logo, há, pelo menos, um grupo que se diferencia no salário dos demais grupos.


#e) O tempo de experiência médio é diferente para homens e mulheres?

#Estatística Descritiva
summary(dados$experiencia)
summary(dados$experiencia[dados$sexo==0]) #Homens
summary(dados$experiencia[dados$sexo==1]) #Mulheres
#Pela estatística descritiva já observamos que os homens tem em média mais experiência que as mulheres

basicStats(dados$experiencia)
basicStats(dados$experiencia[dados$sexo==0]) #Homens
basicStats(dados$experiencia[dados$sexo==1]) #Mulheres

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
library(normtest)
shapiro.test(dados$experiencia[dados$sexo==0]) #Homens
shapiro.test(dados$experiencia[dados$sexo==1]) #Mulheres
#     Conclusão: Para ambos os grupos, as distribuição não são normais

#Teste de Homocedasticidade - Levene, para testar se as variâncias dos grupos são iguais ou diferentes
#Pressupostos
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(dados$experiencia, dados$sexo)
#     Conclusão: As variâncias dos grupos são iguais


#Teste Paramétrico - Teste t - 2 amostras Independentes, para testar se as médias dos grupos são iguais ou diferentes.
#Pressupostos
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$experiencia~dados$sexo, var.equal=T)
#   Conclusão: Como p-value - 0.4164 > 0,05 = alpha, Aceitamos H0 e Rejeitamos H1.
#   Logo, não há diferença significativa no tempo de experiência de homens e mulheres

#Teste Não Paramétrico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$experiencia ~ dados$sexo)


#f) A idade média dos casados é diferente da idade média dos solteiros?

#Estatística Descritiva
summary(dados$idade)
summary(dados$idade[dados$est_civil==0]) #Solteiros
summary(dados$idade[dados$est_civil==1]) #Casados
#Pela estatística descritiva já observamos que os casados tem em média mais idade que os solteiros

basicStats(dados$idade)
basicStats(dados$idade[dados$est_civil==0]) #Solteiros
basicStats(dados$idade[dados$est_civil==1]) #Casados

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
library(normtest)
shapiro.test(dados$idade[dados$est_civil==0]) #Solteiros
shapiro.test(dados$idade[dados$est_civil==1]) #Casados
#     Conclusão: Para os Solteiros a distribuição de dados não é normal
#                Para os casados a distribuição dos dados é normal

#Teste de Homocedasticidade - Levene, para testar se as variâncias dos grupos são iguais ou diferentes
#Pressupostos
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(dados$idade, dados$est_civil)
#     Conclusão: As variâncias dos grupos são diferentes


#Teste Paramétrico - Teste t - 2 amostras Independentes, para testar se as médias dos grupos são iguais ou diferentes.
#Pressupostos
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(dados$idade~dados$est_civil, var.equal=T)
#   Conclusão: Como p-value - 0.0000 < 0,05 = alpha, Aceitamos H1 e Rejeitamos H0.
#   Logo, há diferença significativa na idade dos solteiros e dos casados

#Teste Não Paramétrico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$idade ~ dados$est_civil)


#Questão 2


#g) Há relação entre Salário e Experiência?

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(dados$experiencia) #Distribuição não é normal
shapiro.test(dados$salario) #Distribuição não é normal
# Conclusão: as distribuições não são normais

#Correção de Pearson - Para amostras com distribuições normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$experiencia, dados$salario, method = "pearson")
# Conclusão: o coeficiente de correlação = 0.1731733 = Correlação FRACA

#Correção de Spearman - Para amostras com distribuições não normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$experiencia, dados$salario, method = "spearman")

#Conclusão: o coeficiente de correlação = 0.2414725 = Correlação FRACA

# conclusão: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e experiência.


#h) Há relação entre Salário e tempo de Instrução?

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(dados$instrucao) #Distribuição não é normal
shapiro.test(dados$salario) #Distribuição não é normal

#Correção de Pearson - Para amostras com distribuições normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$instrucao, dados$salario, method = "pearson")
# o coeficiente de correlação = 0.456518 = Correlação MODERADA

#Correção de Spearman - Para amostras com distribuições não normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$instrucao, dados$salario, method = "spearman")
# o coeficiente de correlação = 0.4574217 = Correlação MODERADA

# conclusão: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e tempo de instrução

#i) Há relação entre Salário e Idade dos indivíduos investigados?

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(dados$idade) #Distribuição não é normal
shapiro.test(dados$salario) #Distribuição não é normal

#Correção de Pearson - Para amostras com distribuições normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$idade, dados$salario, method = "pearson")
# o coeficiente de correlação = 0.2874694 = Correlação FRACA

#Correção de Spearman - Para amostras com distribuições não normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$idade, dados$salario, method = "spearman")
# o coeficiente de correlação = 0.3409942 = Correlação MODERADA

# conclusão: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e idade

#j) Há relação entre a Experiência e a Idade dos Indivíduos?

#Teste de Normalidade - Shapiro, para testar se as distribuições são normais
#Pressupostos
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(dados$experiencia) #Distribuição não é normal
shapiro.test(dados$idade) #Distribuição não é normal

#Correção de Pearson - Para amostras com distribuições normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$idade, dados$experiencia, method = "pearson")
# o coeficiente de correlação = 0.970575  = Correlação FORTE

#Correção de Spearman - Para amostras com distribuições não normais
#Pressupostos
#   H0 - Não há uma associação.
#   H1 - Há uma associação.
cor.test(dados$idade, dados$experiencia, method = "spearman")
# o coeficiente de correlação = 0.971742  = Correlação FORTE

# conclusão: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre experiência e idade


#Questão 3

# Modelo Regressão Linear Múltipla

#Análise das Correlações
#Pressupostos
#   H0 - (Não há uma associação)
#   H1 - (Há uma associação)
cor.test(dados$salario, dados$sexo, method = "pearson")
#Correlação -0.2233018 correlação negativa p=value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e sexo

cor.test(dados$salario, dados$cor, method = "pearson")
#Correlação -0.1278338 correlação negativa p=value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e cor

cor.test(dados$salario, dados$est_civil, method = "pearson")
#Correlação 0.1022467 correlação positiva p=value = 0.000236 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e estado civil


cor.test(dados$sexo, dados$cor, method = "pearson") 
cor.test(dados$sexo, dados$est_civil, method = "pearson")
cor.test(dados$cor, dados$est_civil, method = "pearson")


cor.test(dados$salario, dados$instrucao, method = "pearson")
#Correlação 0.456518 correlação positiva p=value = 0.0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e instrução

cor.test(dados$salario, dados$experiencia, method = "pearson")
#Correlação 0.1731733 correlação positiva p=value = 0.0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e experiência

cor.test(dados$salario, dados$idade, method = "pearson")
#Correlação 0.2874694 correlação positiva p=value = 0.0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há uma relação entre salário e idade

modelo <- lm(dados$salario ~ dados$sexo+dados$cor+dados$est_civil+dados$instrucao+dados$experiencia+dados$idade)
summary(modelo)

# Análise do Modelo
# R² : Coeficiente de Determinação
#   R² = 0.3233 (32,33% da Variação do Salário é explicado pelas variáveis sexo, cor, estado cívil, instrução, experiência e idade)
#   Teste ANOVA - Significância do modelo como um todo.
#   Conclusão: Como o p-value = 2.2e-16 (0.0000) < 0.05 = alpha, Rejeitamos H0 (modelo não significativo) e aceitamos H1 (modelo significativo)
#     Logo, existe uma coerência nas variáveis explicativas frente a variável dependente
#   Análise de Coeficientes: 
#     Conclusão: b0 - é significativamente diferente de 0 (p-value Pr(>|t|)=2.51e-12) < 0.05)
#               
#                b1 - (p-value Pr(>|t|)< 2e-16) < 0.05) é significativo à alpha = 0.05, Logo existe efeito do sexo sobre o salário
#               
#                b2 - (p-value Pr(>|t|)=0.00216) < 0.05) é significativo à alpha = 0,05, Logo existe efeito da cor sobre o salário
#                b3 - (p-value Pr(>|t|)=0.03052) < 0.05) é significativo à alpha = 0,05, Logo existe efeito do estado cívil sobre o salário
#
#                b4 - (p-value Pr(>|t|)< 2e-16) é significativo à alpha = 0,05, Logo existe efeito da instrução sobre o salário
#                b5 - (p-value Pr(>|t|)< 2e-16) é significativo à alpha = 0,05, Logo existe efeito do experiência sobre o salário
#


plot(dados$idade, dados$salario, xlab="Idade", ylab="Salário", main = "Idade versus Salário")
#adicionalmente 
tend <- lm(dados$salario ~ dados$idade)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$idade), col="green")

plot(dados$instrucao, dados$salario, xlab="Instrução", ylab="Salário", main = "Tempo de Instrução versus Salário")
#adicionalmente 
tend <- lm(dados$salario ~ dados$instrucao)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$instrucao), col="green")

plot(dados$experiencia, dados$salario, xlab="Experiência", ylab="Salário", main = "Tempo de Experiência versus Salário")
#adicionalmente 
tend <- lm(dados$salario ~ dados$experiencia)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$experiencia), col="green")



