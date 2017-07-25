library(plotly)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') 

USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[, c('Categorie', 'X1960')]

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('$', X1960, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
    layout(title = 'United States Personal Expenditures by Categories in 1960',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="pie/styled")
chart_link






####Grafico de barras

library(gcookbook)
install.packages('gcookbook')

data_country <- data.frame(country = c("China", "Germany", "UK", "US"), 
                           conversion_rate = c(0.001331558,0.062428188, 0.052612025, 0.037800687))
ggplot(data_country, aes(x=country,y = conversion_rate)) +geom_bar(stat = "identity")


ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
    geom_bar(position="dodge", stat="identity",  colour="black") +
    scale_fill_brewer(palette="Pastel1")




#Análise gráfica

#Sobreviventes por sexo

sobreviventes <- as.factor(dados$Sex)
totais_sobreviventes <- as.vector(table(sobreviventes))
Sexo <- c('Feminino','Masculino')

data <- data.frame(Sexo = Sexo, Sobreviventes = totais_sobreviventes)

g_ss <- plot_ly(data, labels = ~Sexo, values = ~Sobreviventes, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste('', Sobreviventes, 'pessoas'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
    layout(title = 'Sexo dos passageiros do Titanic',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



head(dados)
sum(is.na(dados$Age))

dadost <- dados
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked')

dadost[factor_vars] <- lapply(dadost[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(dadost[, !names(dadost) %in% c('PassengerId','Survived')], method='rf') 
mice_output <- complete(mice_mod)

par(mfrow=c(1,2))
hist(dadost$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
