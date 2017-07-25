#JWLS 19/07

#Pacotes
library(ggplot2)



#Dados
dados <- read.csv('/home/wesley/MEGAsync/teste_estatistico/titanic.csv', h = T)
head(dados)

        
####Gráfico de barras


#Preparando os dados

masc <- dados[dados$Sex == 'male',]
fem <- dados[dados$Sex == 'female',]
classe1 <- dados[dados$Pclass == 1,] 
classe2 <- dados[dados$Pclass == 2,] 
classe3 <- dados[dados$Pclass == 3,] 

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


#Histograma para comparar suvirved por Idade
#Preencher a saida no banco de dados onde existe NA


par(mfrow=c(1,3))
hist(dados$Age, freq=F, main="Idade desconsiderando os NA's", 
     col='#669933', ylim=c(0,0.05), ylab = "Densidade", xlab = "Idade")
hist(dados3$Age, freq=F, main="Idade com NA's preenchidos pela mediana por classe e por sexo", 
     col='#FFCC66', ylim=c(0,0.05), ylab = "Densidade", xlab = "Idade")
hist(dados4$Age, freq=F, main="Idade com NA's preenchidos com modelo", 
     col='#CD853F', ylim=c(0,0.05), ylab = "Densidade", xlab = "Idade")



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
