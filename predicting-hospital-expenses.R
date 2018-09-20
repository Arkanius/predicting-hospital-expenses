setwd("~/Documents/dsa/RFundamentos/miniprojeto3")
getwd()


## Etapa 1 - Coletando os Dados
data <- read.csv("./despesas.csv", stringsAsFactors = TRUE)
any(is.na(data))
data$sexo = as.factor(data$sexo)
head(data)
str(data)

correlationData <- data

# Pegando as colunas numericas para observar as correlações entra as variáveis
numericColumns = sapply(correlationData, is.numeric)
numericColumns

correlationMap <- cor(data[, numericColumns])
correlationMap

library(corrplot)

corrplot(correlationMap, method = 'color')

plot(data$idade, data$gastos)
?plot

?pairs
pairs(data[c('idade', 'bmi', 'filhos', 'gastos')])

# Etapa 2 - Gerando subsets
library(caTools)

set.seed(101) 
sample <- sample.split(data$idade, SplitRatio = 0.70)

trainingData = subset(data, sample == TRUE)
testData = subset(data, sample == FALSE)


# Etapa 3 - Criando o modelo
model <- lm(gastos ~ ., data = trainingData)
class(model)
summary(model)

?predict
prevision <- predict(model, testData)
class(prevision)
head(prevision)

res <-residuals(model)
class(res)


# MSE 
#mse <- mean(())
