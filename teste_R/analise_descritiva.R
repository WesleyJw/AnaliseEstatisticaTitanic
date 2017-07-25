#JWLS 18/07/17

#Pacotes
library(ggplot2)
library(hydroGOF)
library(caTools)
library(randomForest)
library(mice)
library(ggthemes) 
library(scales) 
library(dplyr) 
library(mice) 
library(Amelia)



#Importar banco de dados

dados <- read.csv('/home/wesley/MEGAsync/TesteEstatistica/titanic.csv', h = T)
View(dados)
head(dados)

#Verificando falhas no banco de dados
missmap(dados)


#Preparando os dados

masc <- dados[dados$Sex == 'male',]
fem <- dados[dados$Sex == 'female',]

#Tratando os valores perdidos

#Porto - substituindo NA pela moda
dados1 <- dados 
moda <- table(dados$Embarked)[3]

dados1$Embarked[which(is.na(dados1$Embarked))] <-"S"


#Idade - substituindo NA por...

#mediana
dados2 <- dados1
dados2$Age[which(is.na(dados2$Age))] <- median(dados2$Age,na.rm = TRUE)

#por classes e sexo
classe1 <- dados1[dados$Pclass == 1,] 
classe2 <- dados1[dados$Pclass == 2,] 
classe3 <- dados1[dados$Pclass == 3,] 


classe1_m <- classe1[classe1$Sex == 'male',]
classe1_m$Age[which(is.na(classe1_m$Age))] <- median(classe1_m$Age,na.rm = TRUE)
classe1_f <- classe1[classe1$Sex == 'female',]
classe1_f$Age[which(is.na(classe1_f$Age))] <- median(classe1_f$Age,na.rm = TRUE)

classe2_m <- classe2[classe2$Sex == 'male',]
classe2_m$Age[which(is.na(classe2_m$Age))] <- median(classe2_m$Age,na.rm = TRUE)
classe2_f <- classe2[classe2$Sex == 'female',]
classe2_f$Age[which(is.na(classe2_f$Age))] <- median(classe2_f$Age,na.rm = TRUE)

classe3_m <- classe3[classe3$Sex == 'male',]
classe3_m$Age[which(is.na(classe3_m$Age))] <- median(classe3_m$Age,na.rm = TRUE)
classe3_f <- classe3[classe3$Sex == 'female',]
classe3_f$Age[which(is.na(classe3_f$Age))] <- median(classe3_f$Age,na.rm = TRUE)

#Novo banco de dados

dados3 <- rbind(classe1_f,classe1_m, classe2_f, classe2_m, classe3_f, classe3_m)

#Número de sobreviventes por sexo

sexo <- c('Masculino', 'Masculino', 'Feminino', 'Feminino')
l_sobrev <- rep(c('Vítimas','Sobreviventes'),2) 
n_sobrev <- c(as.vector(table(masc$Survived)), as.vector(table(fem$Survived)))

ns_sexo <- data.frame(Sexo = sexo, Sobrevivencia = l_sobrev, N_sobrev = n_sobrev)

#Gráfico

sobrevbysexo <- ggplot(ns_sexo, aes(x = Sobrevivencia, y = N_sobrev, fill = Sexo)) +
    geom_bar(position = 'dodge',stat="identity") +
    ylab('Número de pessoas') +
    xlab('Relação de sobreviventes e vítimas') +
    scale_fill_manual(values=c("#669933", "#FFCC66"))


#Número de sobreviventes por classe

classes <- c(rep('1° Classe',2), rep('2° Classe',2), rep('3° Classe',2))
l_sobrev1 <- rep(c('Vítimas','Sobreviventes'),3) 
n_sobrev1 <- c(as.vector(table(classe1$Survived)), as.vector(table(classe2$Survived)),as.vector(table(classe3$Survived)))

ns_classe <- data.frame(Classes = classes, Sobrevivencia = l_sobrev1, N_sobrev = n_sobrev1)

#Gráfico

sobrevbyclasse <- ggplot(ns_classe, aes(x = Classes , y = N_sobrev, fill =Sobrevivencia)) +
    geom_bar(position = 'dodge',stat="identity") +
    ylab('Número de pessoas') +
    xlab('Relação de sobreviventes e vítimas') +
    scale_fill_manual(values=c("#669933", "#FFCC66"))


#Substituindo NA na idade com uma funcao de probabilidade com pacote mice
dados4 <- dados1

# Transformar variaveis em fatores
var_fatores <- c('PassengerId','Pclass','Sex','Embarked')

dados4[var_fatores] <- lapply(dados4[var_fatores], function(x) as.factor(x))

# Fixando uma semente
set.seed(128)
head(dados4)
#Estimar a idade e substituir onde exite NA's
#PassengerId e Survived nao sao uteis para explicar a idade por isso foram excluidos
est_mice <- mice(dados4[, !names(dados4) %in% c('PassengerId','Survived')], method='rf') 
saida_mice <- complete(est_mice)

#Substituindo no banco de dados

dados4$Age <- saida_mice$Age


#Histograma para comparar suvirved por Idade
#Preencher a saida no banco de dados onde existe NA


par(mfrow=c(1,3))
hist(dados$Age, freq=F, main="Idade desconsiderando os NA's", 
     col='#669933', ylim=c(0,0.05), ylab = "Densidade", xlab = "Idade")
hist(dados3$Age, freq=F, main="Idade com NA's preenchidos pela mediana por classe e por sexo", 
     col='#FFCC66', ylim=c(0,0.05), ylab = "Densidade", xlab = "Idade")
hist(dados4$Age, freq=F, main="Idade com NA's preenchidos com modelo", 
     col='#CD853F', ylim=c(0,0.05), ylab = "Densidade", xlab = "Idade")

#Analise Descritiva

#Testes estatísticos

#Existe diferença significativa entre as proporções de sobreviventes entre 
#homens e mulheres?
chisq.test(as.factor(dados3$Survived), as.factor(dados3$Sex))
prop.table(table(dados3$Sex,dados3$Survived))

#Existe diferença significativa entre as proporções de sobreviventes entre 
#classes diferentes?
chisq.test(as.factor(dados3$Survived), as.factor(dados3$Pclass))
prop.table(table(dados3$Pclass, dados3$Survived))

#Existe diferença significativa entre as proporções de sobreviventes entre 
#faixas etárias diferentes? Quão mais velho você precisa ser para que você não 
#saísse vivo do desastre?

#Tabelas de classes


###Em relacao aos sobreviventes

#Número de classes
c <- 10
#Amplitude da classe
amp <- ceiling((max(dados4$Age)-min(dados4$Age))/c)
#intevalos
intervalo <- seq(trunc(min(dados4$Age)), (max(dados4$Age)+8), amp)
nome_classes <- c('0-7','8-15','16-23','24-31','32-39','40-47','48-55','56-63','64-71','72-79',
                  '80-81')

##Dados de sobreviventes
sobrev <- dados4[dados4$Survived == 1,]


propbyidade <- prop.table(table(cut(sobrev$Age,breaks=intervalo,right=FALSE,labels=nome_classes)))
propbyidade <- as.vector(propbyidade)
freqbyidade <- table(cut(sobrev$Age,breaks=intervalo,right=FALSE,labels=nome_classes))
freqbyidade <- as.vector(freqbyidade)
percentagem <- propbyidade*100

#Tabela de frequencia de sobreviventes
tabela_idade <- data.frame(Classes = nome_classes, Frquencia = freqbyidade, Proporcao = propbyidade, Porcentagem = percentagem)

#Teste qui-quadrado para verificar se existe diferencas entre as proporcoes
chisq.test(tabela_idade$Proporcao,as.factor(tabela_idade$Classes))


##Modelo para predizer novos valores


###Modelo com RandomForest
#Dividindo o banco de dados em treino e teste
split <- sample.split(dados4$Survived, SplitRatio = 0.8)
train_set <- subset(dados4, split == TRUE)
test_set <- subset(dados4, split == F)


modelo_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                              Fare + Embarked, data = train_set)

##Grafico dos residuos do modelo
plot(modelo_rf, ylim=c(0,0.36))
legend('topright', colnames(modelo_rf$err.rate), col=1:3, fill=1:3)
head(train_set)


#Quais variáveis explicam melhor os dados? Explique quais testes e 
#modelos foram utilizados em sua resposta.

importancia <- importance(modelo_rf)
variavel_imp <- data.frame(variaveis = row.names(importancia), 
                           importancia = round(importancia[,'MeanDecreaseGini'],2))

ranque <- variavel_imp %>%
    mutate(Ranque = paste0('°',dense_rank(desc(importancia))))


#Visualizacao grafica das vairiaveis mais importantes
ggplot(ranque, aes(x = reorder(variaveis, importancia), 
                   y = importancia, fill = importancia)) +
    geom_bar(stat='identity') + 
    geom_text(aes(x = variaveis, y = 0.5, label = Ranque),
              hjust=0, vjust=0.55, size = 4, colour = 'red') +
    labs(x = 'Variaveis', y = 'Importância') +
    coord_flip() + 
    theme_few()



#Estimar novos valores

y_pred <- as.numeric(as.vector(predict(modelo_rf, test_set)))
y_real <- test_set$Survived


###Modelo Logistico

modelo_log <- glm(Survived ~ Pclass + Sex + Age + Fare, 
                  family = binomial("logit"), data = train_set)

# Avaliar o modelo
summary(modelo_log)



novo <- dados4[1,]


novo$Pclass <- as.factor(1)
novo$Sex <- as.factor('male')
novo$Age <- 19
novo$Fare <- 0

novo$Survived <- predict(modelo_log, estimando, type = "response")
novo


