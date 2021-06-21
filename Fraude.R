#Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile

#Diretório do Projeto
setwd("C:/FCD/01-BigData-R-Azure/Projetofinal")
getwd()

#Problema de classificação (resposta será "sim" ou "não")

#Instalando e carregando os pacotes necessários
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(ROSE)
library(randomForest)
library(C50)
library(caret)

#Carregando o dataset "train_sample" para análise exploratória dos dados.

df<- read.csv("train_sample.csv", header=TRUE)

colNames <- c("data", "hora")
View(df)
head(df)

#Verificando o tipo dos dados
str(df)

#Fazendo as conversões necessárias nas varáveis
#As variáveis "is attributed", "app", "os", "device" e "channel
#devem ser do tipo fator.

df$is_attributed<- as.factor(df$is_attributed)
df$ip<- as.factor(df$ip)
df$app<- as.factor(df$app)
df$os<- as.factor(df$os)
df$channel<- as.factor(df$channel)
df$device<- as.factor(df$device)
str(df)

#Ajustando as colunas click_time e attributed_time para POSIXct

df$click_time<- as.POSIXct(df$click_time)

df<- separate(df, click_time, c("data", "hora"), sep=" ", remove=FALSE)
df<- separate(df, hora, c("hora", "minuto", "segundo"), sep=":", remove=FALSE)
df$data<- as.factor(df$data)
df$hora<- as.factor(df$hora)

#Verificando se existem valores missing

is.null(df)
is.na(df)

#Explorando as variáveis categóricas.
#Existe uma porcentagem muito pequena de valores "sim" (representados pelo número 1).
#Apenas 227 de 100.000 cliques foram convertidos em downloads do app.

summary(df$is_attributed)

#A grande maioria dos devices é do tipo 1.
ggplot(df, aes(x = (device))) + geom_bar()

#A grande maioria dos sistemas operacionais é dos tipos 19 e 13
summary(df$os)

#Explorando em qual horário mais ocorreram os cliques convertidos em downloads.
#Criando um dataset novo (df_download) para as manipulações.
df_download<- filter(df, is_attributed == 1)
df_download$data<- as.factor(df_download$data)
df_download$hora<- as.factor(df_download$hora)
View(df_download)
str(df_download)

# Explorando dias e horários em que os cliques falsos e verdadeiros mais ocorrem.
#Com essa visualização, pode-se concluir que o maior número de cliques falsos ocorre
#às 4h.

#O dia da semana em que mais ocorreram cliques falsos é na quarta-feira e em menor número
#na segunda-feira.

lapply(colNames, function(x){
  if(is.factor(df[,x])) {
    ggplot(df, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ is_attributed) + 
      ggtitle(paste("Total de cliques por",x))}})

#Fazendo o mesmo procedimento para o dataset df_download, onde somente temos os cliques verdadeiros.
#Observa-se que às 14h é o horário onde mais temos cliques verdadeiros.
#O comportamento em relação à dias da semana é o mesmo dos cliques falsos.

lapply(colNames, function(x){
  if(is.factor(df_download[,x])) {
    ggplot(df_download, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ is_attributed)
      ggtitle(paste("Total de cliques por",x))}})

#Criando um terceiro dataset e removendo as colunas que não serão usadas no modelo.
df_modelo<- separate(df, click_time, c("data", "hora"), sep=" ", remove=FALSE)
View(df_modelo)
df_modelo$click_time<- NULL
df_modelo$attributed_time<- NULL
df_modelo$minuto<- NULL
df_modelo$segundo<- NULL
df_modelo$ip<-NULL
df_modelo$hora<-NULL

df_modelo$data<- as.factor(df_modelo$data)


#Dividindo os dados em treino e teste, sendo 70% para treino e 30% para teste
#Nesse primeiro momento, usarei todas as variáveis para o modelo com exceção de "attributed time" e "ip"

divisor <- sample(1:nrow(df_modelo), 0.7 * nrow(df_modelo))
dados_treino <- df_modelo[divisor,]
dados_teste <- df_modelo[-divisor]
View(dados_teste)
View(dados_treino)

#Verificar proporção da variável target

prop.table(table(dados_treino$is_attributed))
prop.table(table(dados_teste$is_attributed))

#Necessário fazer o balanceamento dos dados

?ROSE

#Balanceamento nos dados de treino
rose_treino <- ROSE(is_attributed ~ ., data = dados_treino)$data
prop.table(table(rose_treino$is_attributed))

#O resultado foi quase 50% de dados para cada categoria.

#Balaneamento nos dados de teste
rose_teste <- ROSE(is_attributed ~ ., data = dados_teste)$data
prop.table(table(rose_teste$is_attributed))

#O resultado foi quase 50% de dados para cada categoria.

#Criando a primeira versão do modelo com o algoritmo C50 (árvore de decisão)

modelo1 <- C5.0(is_attributed ~ ., data = rose_treino)
modelo1

#Previsões com os dados de teste
previsoes1 <- predict(modelo1, rose_teste)
previsoes1

#Confusion Matrix para analisar os resultados do modelo
#A classe zero é a positiva, que indica que o clique é uma fraude.

caret::confusionMatrix(rose_teste$is_attributed, previsoes1, positive = '0')

#O modelo gerou 93,8% de acurácia conforme a Confusion Matrix.

