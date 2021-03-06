# -----------------------------------------------
# Infer�ncia Estat�stica
# -----------------------------------------------

# Mudando o diret�rio
setwd("C:/Projetos/Git/pos/r/analise_inferencia_salario_populacao_eua")

#Pacotes
library(fBasics) # Estat�stica B�sicas
library(car) # Teste de Levene
#library(normtest) #Testes de Normalidade

# Banco de Dados
dados <- read.csv("Dados.csv", header = T, sep = ";", dec = ",")

#Quest�o 1

#a. O Sal�rio m�dio dos homens se difere do sal�rio m�dio das mulheres?

#Estat�stica Descritiva
summary(dados$salario)
summary(dados$salario[dados$sexo==0]) #Sal�rio Homens
summary(dados$salario[dados$sexo==1]) #Sal�rio Mulheres
#Pela estat�stica descritiva j� observamos que os homens ganham em m�dia mais que as mulheres

basicStats(dados$salario)
basicStats(dados$salario[dados$sexo==0]) #Sal�rio Homens
basicStats(dados$salario[dados$sexo==1]) #Sal�rio Mulheres

#Podemos perceber problema de assimetria em rela��o ao sal�rio dos homens
# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 1.426094


#Podemos perceber problema de assimetria e curtose em rela��o ao sal�rio das mulheres
# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 2.571936
# Problema de Curtose existe quando Curt(x) < -7 ou Curt(x) > +7
# Neste caso, tem-se curtose = 11.303823 

#Teste de Normalidade de Shapiro
#Pressupostos
#   Normalidade
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
library(normtest)
shapiro.test(dados$salario[dados$sexo==0]) #Sal�rio Homens
shapiro.test(dados$salario[dados$sexo==1]) #Sal�rio Mulheres
#     Conclus�o: Para ambos os grupos, o p-value = 0.0000 < 0.05 = alpha, ent�o rejeitamos H0 e aceitamos H1
#                Logo as distribui��o n�o s�o normais

#Teste de Homocedasticidade - Levene
#Pressupostos
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(dados$salario, dados$sexo)
#      Conclus�o: As vari�ncias dos grupos s�o diferentes, pois
#                 Pr(>F) = 0.0000 < 0.05 = alpha. Logo, rejeitamos H0 e aceitamos H1.

#Teste Param�trico t - 2 amostras Independentes
#Pressupostos
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(dados$salario~dados$sexo)
#   Conclus�o: Como p-value - 0,0000 < 0,05 = alpha, rejeitamos H0 e aceitamos H1.
#   Logo, h� diferen�a significativa no sal�rio de homens e mulheres
#   Os homens, em m�dia, possuem um sal�rio maior do que as mulheres.

#Teste N�o Param�trico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$salario ~ dados$sexo)
#Pressupostos
#H0 - Sal�rio dos homens � igual ao sal�rio das mulheres
#H1 - Sal�rio dos homens � diferente do sal�rio das mulheres 

#Conclus�o: Como p-value = 0.0000 < 0.05 = alpha, logo rejeitamos H0 e aceitamos H1. 


#b. O Sal�rio m�dio das pessoas n�o brancas se difere das pessoas brancas?

#Estat�stica Descritiva
summary(dados$cor)
summary(dados$salario[dados$cor==0]) #Sal�rio Brancos
summary(dados$salario[dados$cor==1]) #Sal�rio N�o Brancos
#Pela estat�stica descritiva j� observamos que os brancos ganham em m�dia mais que n�o brancos

basicStats(dados$salario)
basicStats(dados$salario[dados$cor==0]) #Sal�rio Brancos
basicStats(dados$salario[dados$cor==1]) #Sal�rio N�o Brancos

# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 1.809920

# Problema de Assimetria existe quando Ass(x) < -1 ou Ass(x) > +1
# Neste caso, tem-se assimetria = 1.588035 


#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
library(normtest)
shapiro.test(dados$salario[dados$cor==0]) #Sal�rio Brancos
shapiro.test(dados$salario[dados$cor==1]) #Sal�rio N�o Brancos
#     Conclus�o: Para ambos os grupos, as distribui��o n�o s�o normais


#Teste de Homocedasticidade - Levene, para testar se as vari�ncias dos grupos s�o iguais ou diferentes
#Pressupostos
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(dados$salario, dados$cor)
#     Conclus�o: As vari�ncias dos grupos s�o diferentes


#Teste Param�trico - Teste t - 2 amostras Independentes, para testar se as m�dias dos grupos s�o iguais ou diferentes.
#Pressupostos
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(dados$salario~dados$cor)
#   Conclus�o: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, h� diferen�a significativa no sal�rio de brancos e n�o brancos
#   Os brancos, em m�dia, possuem um sal�rio maior do que os n�o brancos.


#Teste N�o Param�trico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$salario ~ dados$cor)


#c) O Sal�rio m�dio das pessoas casadas se difere das pessoas solteiras?

#Estat�stica Descritiva
summary(dados$est_civil)
summary(dados$salario[dados$est_civil==0]) #Soletiro
summary(dados$salario[dados$est_civil==1]) #Casado
#Pela estat�stica descritiva j� observamos que os casados ganham em m�dia mais que os solteiros

basicStats(dados$est_civil)
basicStats(dados$salario[dados$est_civil==0]) #Sal�rio Brancos
basicStats(dados$salario[dados$est_civil==1]) #Sal�rio N�o Brancos

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
library(normtest)
shapiro.test(dados$salario[dados$est_civil==0]) #Solteiros
shapiro.test(dados$salario[dados$est_civil==1]) #Casados
#     Conclus�o: Para ambos os grupos, as distribui��o n�o s�o normais

#Teste de Homocedasticidade - Levene, para testar se as vari�ncias dos grupos s�o iguais ou diferentes
#Pressupostos
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(dados$salario, dados$est_civil)
#     Conclus�o: As vari�ncias dos grupos s�o diferentes

#Teste Param�trico - Teste t - 2 amostras Independentes, para testar se as m�dias dos grupos s�o iguais ou diferentes.
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(dados$salario~dados$est_civil)
#   Conclus�o: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, h� diferen�a significativa no sal�rio de solteiros e casados
#   Os casados, em m�dia, possuem um sal�rio maior do que os solteiros

#Teste N�o Param�trico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes.
wilcox.test(dados$salario ~ dados$est_civil)

#d) Considerando o sexo das pessoas e seu estado civil conjuntamente, � poss�vel afirmar que
#algumas dos grupos formados possui m�dia salarial diferente dos demais?

#Criando nova coluna indicando o sexo e o estado civil conjuntamente
#Onde:
# 0 = Homens solteiros
# 1 = Homens casados
# 2 = Mulheres solteiras
# 3 = Mulheres casadas

dados["sexo_estCivil"] <- (dados$sexo*2) + dados$est_civil

#Estat�sticas Descritivas
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

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(dados$salario[dados$sexo_estCivil==0]) #Homens solteiros 
shapiro.test(dados$salario[dados$sexo_estCivil==1]) #Homens casados   
shapiro.test(dados$salario[dados$sexo_estCivil==2]) #Mulheres solteiras        
shapiro.test(dados$salario[dados$sexo_estCivil==3]) #Mulheres casadas
#     Conclus�o: Para todos os grupos, as distribui��es n�o s�o normais


#Teste de Homocedasticidade - Levene, para testar se as vari�ncias dos grupos s�o iguais ou diferentes
#Pressupostos
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(dados$salario, dados$sexo_estCivil)
#     Conclus�o: Pr(>F) (p-value) = 0.0000 < 0.05, rejeitamos H0 e aceitamos H1
#     As vari�ncias dos grupos s�o diferentes

# Testes N�o-Param�tricos de Kruskal - alternativo ao Teste ANOVA
#Pressupostos
#   H0 - Todas as m�dia s�o iguais
#   H1 - H� pelo menos dois grupos com m�dias diferentes.
kruskal.test(dados$salario, dados$sexo_estCivil)

#Teste ANOVA
#Pressupostos
#   H0 - Todas as m�dia s�o iguais
#   H1 - H� pelo menos dois grupos com m�dias diferentes.
anova <- aov(dados$salario ~ dados$sexo_estCivil)
summary(anova)
#   Conclus�o: Como o p-value (PR(>F))= 0,0000 < 0,05, Rejeitamos H0 e Aceitamos H1.
#   Logo, h�, pelo menos, um grupo que se diferencia no sal�rio dos demais grupos.


#e) O tempo de experi�ncia m�dio � diferente para homens e mulheres?

#Estat�stica Descritiva
summary(dados$experiencia)
summary(dados$experiencia[dados$sexo==0]) #Homens
summary(dados$experiencia[dados$sexo==1]) #Mulheres
#Pela estat�stica descritiva j� observamos que os homens tem em m�dia mais experi�ncia que as mulheres

basicStats(dados$experiencia)
basicStats(dados$experiencia[dados$sexo==0]) #Homens
basicStats(dados$experiencia[dados$sexo==1]) #Mulheres

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
library(normtest)
shapiro.test(dados$experiencia[dados$sexo==0]) #Homens
shapiro.test(dados$experiencia[dados$sexo==1]) #Mulheres
#     Conclus�o: Para ambos os grupos, as distribui��o n�o s�o normais

#Teste de Homocedasticidade - Levene, para testar se as vari�ncias dos grupos s�o iguais ou diferentes
#Pressupostos
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(dados$experiencia, dados$sexo)
#     Conclus�o: As vari�ncias dos grupos s�o iguais


#Teste Param�trico - Teste t - 2 amostras Independentes, para testar se as m�dias dos grupos s�o iguais ou diferentes.
#Pressupostos
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(dados$experiencia~dados$sexo, var.equal=T)
#   Conclus�o: Como p-value - 0.4164 > 0,05 = alpha, Aceitamos H0 e Rejeitamos H1.
#   Logo, n�o h� diferen�a significativa no tempo de experi�ncia de homens e mulheres

#Teste N�o Param�trico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$experiencia ~ dados$sexo)


#f) A idade m�dia dos casados � diferente da idade m�dia dos solteiros?

#Estat�stica Descritiva
summary(dados$idade)
summary(dados$idade[dados$est_civil==0]) #Solteiros
summary(dados$idade[dados$est_civil==1]) #Casados
#Pela estat�stica descritiva j� observamos que os casados tem em m�dia mais idade que os solteiros

basicStats(dados$idade)
basicStats(dados$idade[dados$est_civil==0]) #Solteiros
basicStats(dados$idade[dados$est_civil==1]) #Casados

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
library(normtest)
shapiro.test(dados$idade[dados$est_civil==0]) #Solteiros
shapiro.test(dados$idade[dados$est_civil==1]) #Casados
#     Conclus�o: Para os Solteiros a distribui��o de dados n�o � normal
#                Para os casados a distribui��o dos dados � normal

#Teste de Homocedasticidade - Levene, para testar se as vari�ncias dos grupos s�o iguais ou diferentes
#Pressupostos
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(dados$idade, dados$est_civil)
#     Conclus�o: As vari�ncias dos grupos s�o diferentes


#Teste Param�trico - Teste t - 2 amostras Independentes, para testar se as m�dias dos grupos s�o iguais ou diferentes.
#Pressupostos
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(dados$idade~dados$est_civil, var.equal=T)
#   Conclus�o: Como p-value - 0.0000 < 0,05 = alpha, Aceitamos H1 e Rejeitamos H0.
#   Logo, h� diferen�a significativa na idade dos solteiros e dos casados

#Teste N�o Param�trico de Mann-Whitney - Alternativa ao Teste t de 2 amostras independentes. 
wilcox.test(dados$idade ~ dados$est_civil)


#Quest�o 2


#g) H� rela��o entre Sal�rio e Experi�ncia?

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(dados$experiencia) #Distribui��o n�o � normal
shapiro.test(dados$salario) #Distribui��o n�o � normal
# Conclus�o: as distribui��es n�o s�o normais

#Corre��o de Pearson - Para amostras com distribui��es normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$experiencia, dados$salario, method = "pearson")
# Conclus�o: o coeficiente de correla��o = 0.1731733 = Correla��o FRACA

#Corre��o de Spearman - Para amostras com distribui��es n�o normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$experiencia, dados$salario, method = "spearman")

#Conclus�o: o coeficiente de correla��o = 0.2414725 = Correla��o FRACA

# conclus�o: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e experi�ncia.


#h) H� rela��o entre Sal�rio e tempo de Instru��o?

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(dados$instrucao) #Distribui��o n�o � normal
shapiro.test(dados$salario) #Distribui��o n�o � normal

#Corre��o de Pearson - Para amostras com distribui��es normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$instrucao, dados$salario, method = "pearson")
# o coeficiente de correla��o = 0.456518 = Correla��o MODERADA

#Corre��o de Spearman - Para amostras com distribui��es n�o normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$instrucao, dados$salario, method = "spearman")
# o coeficiente de correla��o = 0.4574217 = Correla��o MODERADA

# conclus�o: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e tempo de instru��o

#i) H� rela��o entre Sal�rio e Idade dos indiv�duos investigados?

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(dados$idade) #Distribui��o n�o � normal
shapiro.test(dados$salario) #Distribui��o n�o � normal

#Corre��o de Pearson - Para amostras com distribui��es normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$idade, dados$salario, method = "pearson")
# o coeficiente de correla��o = 0.2874694 = Correla��o FRACA

#Corre��o de Spearman - Para amostras com distribui��es n�o normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$idade, dados$salario, method = "spearman")
# o coeficiente de correla��o = 0.3409942 = Correla��o MODERADA

# conclus�o: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e idade

#j) H� rela��o entre a Experi�ncia e a Idade dos Indiv�duos?

#Teste de Normalidade - Shapiro, para testar se as distribui��es s�o normais
#Pressupostos
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(dados$experiencia) #Distribui��o n�o � normal
shapiro.test(dados$idade) #Distribui��o n�o � normal

#Corre��o de Pearson - Para amostras com distribui��es normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$idade, dados$experiencia, method = "pearson")
# o coeficiente de correla��o = 0.970575  = Correla��o FORTE

#Corre��o de Spearman - Para amostras com distribui��es n�o normais
#Pressupostos
#   H0 - N�o h� uma associa��o.
#   H1 - H� uma associa��o.
cor.test(dados$idade, dados$experiencia, method = "spearman")
# o coeficiente de correla��o = 0.971742  = Correla��o FORTE

# conclus�o: Em ambos os testes Como p-value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre experi�ncia e idade


#Quest�o 3

# Modelo Regress�o Linear M�ltipla

#An�lise das Correla��es
#Pressupostos
#   H0 - (N�o h� uma associa��o)
#   H1 - (H� uma associa��o)
cor.test(dados$salario, dados$sexo, method = "pearson")
#Correla��o -0.2233018 correla��o negativa p=value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e sexo

cor.test(dados$salario, dados$cor, method = "pearson")
#Correla��o -0.1278338 correla��o negativa p=value = -0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e cor

cor.test(dados$salario, dados$est_civil, method = "pearson")
#Correla��o 0.1022467 correla��o positiva p=value = 0.000236 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e estado civil


cor.test(dados$sexo, dados$cor, method = "pearson") 
cor.test(dados$sexo, dados$est_civil, method = "pearson")
cor.test(dados$cor, dados$est_civil, method = "pearson")


cor.test(dados$salario, dados$instrucao, method = "pearson")
#Correla��o 0.456518 correla��o positiva p=value = 0.0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e instru��o

cor.test(dados$salario, dados$experiencia, method = "pearson")
#Correla��o 0.1731733 correla��o positiva p=value = 0.0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e experi�ncia

cor.test(dados$salario, dados$idade, method = "pearson")
#Correla��o 0.2874694 correla��o positiva p=value = 0.0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� uma rela��o entre sal�rio e idade

modelo <- lm(dados$salario ~ dados$sexo+dados$cor+dados$est_civil+dados$instrucao+dados$experiencia+dados$idade)
summary(modelo)

# An�lise do Modelo
# R� : Coeficiente de Determina��o
#   R� = 0.3233 (32,33% da Varia��o do Sal�rio � explicado pelas vari�veis sexo, cor, estado c�vil, instru��o, experi�ncia e idade)
#   Teste ANOVA - Signific�ncia do modelo como um todo.
#   Conclus�o: Como o p-value = 2.2e-16 (0.0000) < 0.05 = alpha, Rejeitamos H0 (modelo n�o significativo) e aceitamos H1 (modelo significativo)
#     Logo, existe uma coer�ncia nas vari�veis explicativas frente a vari�vel dependente
#   An�lise de Coeficientes: 
#     Conclus�o: b0 - � significativamente diferente de 0 (p-value Pr(>|t|)=2.51e-12) < 0.05)
#               
#                b1 - (p-value Pr(>|t|)< 2e-16) < 0.05) � significativo � alpha = 0.05, Logo existe efeito do sexo sobre o sal�rio
#               
#                b2 - (p-value Pr(>|t|)=0.00216) < 0.05) � significativo � alpha = 0,05, Logo existe efeito da cor sobre o sal�rio
#                b3 - (p-value Pr(>|t|)=0.03052) < 0.05) � significativo � alpha = 0,05, Logo existe efeito do estado c�vil sobre o sal�rio
#
#                b4 - (p-value Pr(>|t|)< 2e-16) � significativo � alpha = 0,05, Logo existe efeito da instru��o sobre o sal�rio
#                b5 - (p-value Pr(>|t|)< 2e-16) � significativo � alpha = 0,05, Logo existe efeito do experi�ncia sobre o sal�rio
#


plot(dados$idade, dados$salario, xlab="Idade", ylab="Sal�rio", main = "Idade versus Sal�rio")
#adicionalmente 
tend <- lm(dados$salario ~ dados$idade)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$idade), col="green")

plot(dados$instrucao, dados$salario, xlab="Instru��o", ylab="Sal�rio", main = "Tempo de Instru��o versus Sal�rio")
#adicionalmente 
tend <- lm(dados$salario ~ dados$instrucao)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$instrucao), col="green")

plot(dados$experiencia, dados$salario, xlab="Experi�ncia", ylab="Sal�rio", main = "Tempo de Experi�ncia versus Sal�rio")
#adicionalmente 
tend <- lm(dados$salario ~ dados$experiencia)
abline(tend, col="red")
abline(h=mean(dados$salario), col="green")
abline(v=mean(dados$experiencia), col="green")



