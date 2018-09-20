setwd("~/Documents/dsa/RFundamentos/miniprojeto3")
getwd()

install.packages("ggplot2")
install.packages("ggthemes")
install.packages("dplyr")

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
res <- as.data.frame(res)
head(res)

library(ggplot2)
library(ggthemes)
library(dplyr)

ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

?cbind
result <- cbind(prevision, testData$gastos) 
colnames(result) <- c('predicted','real')
class(result)
result <- as.data.frame(result)
head(result)
min(result)
class(result)


# MSE 
mse <- mean((result$real - result$predicted)^2)
mse

# RMSE 
rmse <- mse ^ 0.5

# R Squared
sse = sum((result$predicted - result$real)^2)
sst = sum((mean(data$gastos) - result$real)^2)
rSquared = 1 - (sse/sst)

rSquared