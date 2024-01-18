# Codigo Portifolio Eficiente

library(tidyverse)
library(magrittr)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyquant)
library(quantmod)
library(readxl)
library(reshape2)
library(rbcb)

rm(list=ls())

#### Ativos escolhidos
# benchmark será a Ibovespa e o ativo livre de risco a Selic
####

benchmark = 'BOVA11.sa'

acoes = c('VALE3.SA', 'RADL3.SA', 'JBSS3.SA', 'UNIP6.SA')

#####SELIC - Ativo livre de risco
selic <- read.table("/Users/Juan_/OneDrive/Documentos/Eco1/fin1/taxa_selic_apurada.txt"
                    ,sep=";"
                    ,dec=","
                    ,header = T)
selic$Data<- as.Date(format(as.Date(selic$Data, format="%d/%m/%Y"), "%Y-%m-%d"))

colnames(selic)[colnames(selic) == "Taxa....a.a.."] <- "SELIC_ano" 
selic <- selic %>%
  mutate(SELIC_dia = (1+(SELIC_ano/100)^1/252)-1)


#Função que pega apenas a coluna dos dados de fechamento diário
yahoo <- function(a){
  if (class(a) != "character"){
    a <- deparse(substitute(a))
  }
  res <- getSymbols(a, src = "yahoo", from = as.Date("2018-06-22"),
                                      to = as.Date("2023-05-26")
                                        , auto.assign = FALSE)
  res[,4]
}


bova <- data.frame(yahoo(benchmark),
                   Date = index(yahoo(benchmark)))
bova <- bova %>%
  mutate(R_diario_BOVA11 = (BOVA11.SA.Close / lag(BOVA11.SA.Close)) - 1)
row.names(bova) <- NULL

ativos = data.frame(VALE3 = yahoo(acoes[1]), 
                    RADL3 = yahoo(acoes[2]), 
                    JBSS3 = yahoo(acoes[3]),
                    UNIP6 = yahoo(acoes[4]),
                    Date = index(yahoo(acoes[1])))

#Variação percentual diária
ativos <- ativos %>%
  mutate(R_diario_VALE3 = (VALE3.SA.Close / lag(VALE3.SA.Close)) - 1,
         R_diario_RADL3 = (RADL3.SA.Close / lag(RADL3.SA.Close)) - 1,
         R_diario_JBSS3 = (JBSS3.SA.Close / lag(JBSS3.SA.Close)) - 1,
         R_diario_UNIP6 = (UNIP6.SA.Close / lag(UNIP6.SA.Close)) - 1)
Ret_dia_atvs <- data.frame(ativos$R_diario_VALE3, ativos$R_diario_RADL3, ativos$R_diario_JBSS3, ativos$R_diario_UNIP6)
Ret_dia_atvs <- na.omit(Ret_dia_atvs)

###### ESTATÍSTICAS DESCRITIVAS
##### Retornos esperados
#retorno diario do ativo livre de risco
risk.free = mean(selic$SELIC_dia)
#retorno medio das acoes
Ret_Med_atvs = colMeans(Ret_dia_atvs)
ret_esperado_ativos <- (1+Ret_Med_atvs)^252-1 #anualizada
#benchmark
ret_esperado_BOVA11 = mean(bova$R_diario_BOVA11, na.rm = TRUE)

###### variancia
#acoes
MatrizCov = cov(Ret_dia_atvs)
#correlacao
matriz_correlacao <- cor(Ret_dia_atvs) 

###### desvio-padrao
#acoes
dp_acoes = sqrt(diag(MatrizCov))
#benchmark
dp_bm = sd(bova$R_diario_BOVA11, na.rm = TRUE)

#matriz inversa de variancia e covariancia
sigma <- MatrizCov
sigma.inv <- solve(MatrizCov)

#vetor retorno esperado
mu <- matrix(ret_esperado_ativos)
mu.t <- t(mu)

# vetores contendo numeros 1
ones <- matrix(c(1, 1, 1, 1), nrow = 4, ncol = 1)  # vetor coluna
ones.t <- t(ones)                               # vetor linha

###vetor de pesos ótimos, isto eh, maior sharpe
#pesos otimos de cada ativo
wotm = sigma.inv %*% (mu - risk.free) / sum(sigma.inv %*% (mu - risk.free))
#retorno esperado da carteira otima
rotm = sum(wotm*mu)
#variancia da carteira otima
varotm = t(wotm) %*% sigma %*% wotm
#desvio-padrao da carteira otima
sotm = sqrt(varotm)
#indice de sharpe da carteira otima
shrp_otm = (rotm - risk.free) / sotm 

############ Gerando carteiras aleatorias e graficando
lista_retornos =      matrix(
  vector(), 0, 1, dimnames = list(c(), c("Retorno")))
lista_volatilidade =  matrix(
  vector(), 0, 1, dimnames = list(c(), c("Desvio-Padrão")))

lista_sharpe_ratio = matrix(
  vector(), 0, 1, dimnames = list(c(), c("Sharpe")))

lista_pesos =     matrix(
  vector(), 0, 4, dimnames = list(c(), acoes))

num_carteiras = 5000
for (i in 1: num_carteiras){
  peso = array(runif(4 , -1, 1))
  peso = (1/sum(abs(peso))) * peso
  lista_pesos <- rbind(lista_pesos, peso)
  lista_retornos <- rbind(lista_retornos, peso %*% mu)
  lista_volatilidade <- rbind(lista_volatilidade ,sqrt(t(peso) %*% sigma %*%peso))
  lista_sharpe_ratio <- rbind(lista_sharpe_ratio ,(lista_retornos[i] - risk.free) 
                              / lista_volatilidade[i])
}

Carteiras <- cbind(lista_retornos, lista_volatilidade, lista_sharpe_ratio, lista_pesos)
df_carteiras <- data.frame(Carteiras)

# gráfico da fronteira de oportunidades
df_carteiras$cor <- ifelse(df_carteiras$Desvio.Padrão == min(df_carteiras$Desvio.Padrão), "blueviolet",
                       ifelse(df_carteiras$Sharpe == max(df_carteiras$Sharpe), "red", "pink"))
ggplot(df_carteiras, aes(x = Desvio.Padrão, y = Retorno)) +
  geom_point(aes(x = dp_bm, y = ret_esperado_BOVA11))+
  geom_point(aes(color = cor), shape = 16) +
  scale_color_manual(values = c("blueviolet", "red", "pink"),
                     breaks = c("blueviolet", "red", "pink"),
                     labels = c("Menor Variância", "Melhor Sharpe", ".")) +
  labs(x = "Volatilidade", y = "Retorno",
       title = "Fronteira eficiente") +
  geom_abline(intercept = risk.free, slope = shrp_otm)



#Gráfico dos retornos diários dos ativos
#BPAC11.SA
ggplot(ativos, aes(x = Date, y = UNIP6.SA.Close)) +
  geom_line() +
  labs(x = "Data", y = "Retorno diário",
       title = "Retornos diários do ativo UNIP6 - Fonte: Yahoo Finance")


###############################################################################
#######################betas################################################

Ativos_arriscados <- inner_join(bova, ativos, by="Date")
retornos_atvsArriscados <- na.omit(Ativos_arriscados[, 
                          c( "R_diario_VALE3",
                             "R_diario_RADL3",
                             "R_diario_JBSS3",
                             "R_diario_UNIP6",
                             "R_diario_BOVA11")])
cov_cMercado <- cov(retornos_atvsArriscados)
cov_com_mercado <- cov_cMercado[,4]
varMercado = cov_cMercado[5,5]
beta_acao <- cov_com_mercado / varMercado
beta_acao1 <- beta_acao[c("R_diario_VALE3",
                          "R_diario_RADL3",
                          "R_diario_JBSS3",
                          "R_diario_UNIP6")]

menor_volatilidade = min(df_carteiras$Desvio.Padrão)
carteira_min_variancia = df_carteiras[df_carteiras$Desvio.Padrão == menor_volatilidade,]

CartMinVar <- t(na.omit(carteira_min_variancia[c("VALE3.SA",
                                                 "RADL3.SA",
                                                 "JBSS3.SA",
                                                 "UNIP6.SA")]))
betaCarteiraMV <- as.numeric(beta_acao1 %*% CartMinVar)

retornoEsperadoMV = risk.free + betaCarteiraMV * (varMercado - risk.free)

betaCarteiraMS = as.numeric(beta_acao1 %*% wotm)

retornoEsperadoMS = risk.free + betaCarteiraMS * (varMercado - risk.free)

carteirasConjunto = data.frame(c(betaCarteiraMV, betaCarteiraMS )
                               ,c(retornoEsperadoMV, retornoEsperadoMS))
carteirasConjunto <- carteirasConjunto %>%
  rename( "beta" =
            "c.betaCarteiraMV..betaCarteiraMS.")
carteirasConjunto <- carteirasConjunto %>%
  rename( "retorno" =
            "c.retornoEsperadoMV..retornoEsperadoMS.")

retorno_acao = list(1,1,1,1)
for (i in  1 :(length(beta_acao))){
  retorno_acao[i] =risk.free + beta_acao[i] * (varMercado - risk.free)
}

betas = data.frame(cbind(as.numeric(beta_acao), as.numeric(retorno_acao)), stringsAsFactors = F)
betas <- betas %>%
  rename("beta" = "X1")
betas <- betas %>%
  rename("retorno" = "X2")

betaseCartConjunta = rbind(betas, carteirasConjunto)

ggplot(betas, aes(x = retorno, y = beta))+
  geom_point() +
  labs(x = "beta", y = "Retorno esperado",
       title = "Retorno x Beta")



#Gráfico dos retornos diários dos ativos
#UNIP6.SA
ggplot(ativos, aes(x = Date, y = R_diario_UNIP6)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = R_diario_UNIP6), color = "deeppink")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo UNIP6 - Fonte: Yahoo Finance")

ggplot(ativos, aes(x = Date, y = UNIP6.SA.Close)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = UNIP6.SA.Close), color = "deeppink")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo UNIP6 - Fonte: Yahoo Finance")

#RADL3.SA  
ggplot(ativos, aes(x = Date, y = R_diario_RADL3)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = R_diario_RADL3), color = "green")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo RADL3 - Fonte: Yahoo Finance")

ggplot(ativos, aes(x = Date, y = RADL3.SA.Close)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = RADL3.SA.Close), color = "green")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo RADL3 - Fonte: Yahoo Finance")

#VALE3.SA
ggplot(ativos, aes(x = Date, y = R_diario_VALE3)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = R_diario_VALE3), color = "blue")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo VALE3 - Fonte: Yahoo Finance")

ggplot(ativos, aes(x = Date, y = VALE3.SA.Close)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = VALE3.SA.Close), color = "blue")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo VALE3 - Fonte: Yahoo Finance")

#JBSS3.SA
ggplot(ativos, aes(x = Date, y = R_diario_JBSS3)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = R_diario_JBSS3), color = "purple")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo JBSS3 - Fonte: Yahoo Finance")

ggplot(ativos, aes(x = Date, y = JBSS3.SA.Close)) +
  geom_line() +
  geom_line(data = ativos, aes(x = Date, y = JBSS3.SA.Close), color = "purple")
labs(x = "Data", y = "Retorno diário",
     title = "Retornos diários do ativo JBSS3 - Fonte: Yahoo Finance")

############################################ Risco

mu.anual = mu * 252

# riscos idiossincratico e sistemático
# risco sistematico
varMercado.anual <- varMercado * 252

betaq = matrix(nrow = 1, ncol= 4, dimnames = list(c(), acoes))
for (i in 1:4){
  betaq[i] = betas$beta[i]^2
}
betaquadrado = data.frame(betaq) 
risco.sistemico =  varMercado.anual * betaquadrado 

#########################################################
omega.otimo <- function(rp) {
  denominador <- (mu.t%*%sigma.inv%*%mu)%*%(ones.t%*%sigma.inv%*%ones)-(ones.t%*%sigma.inv%*%mu)^2
  numerador.lambda.1 <-(ones.t%*%sigma.inv%*%ones)*rp-(ones.t%*%sigma.inv%*%mu)
  numerador.lambda.2 <- (mu.t%*%sigma.inv%*%mu)-(ones.t%*%sigma.inv%*%mu)*rp
  lambda_1 <- as.numeric(numerador.lambda.1/denominador)
  lambda_2 <- as.numeric(numerador.lambda.2/denominador)
  omega_otimo <- (lambda_1*(sigma.inv%*%mu))+(lambda_2*(sigma.inv%*%ones))
  omega_otimo
}

# fun??o criada para gerar a vari?ncia m?nima, dado um retorno esperado igual a rep
# a fun??o vari?ncia ? uma fun??o da fun??o omega.otimo(rp)

var.carteira <-function(rp) {
  t(omega.otimo(rp))%*%sigma%*%omega.otimo(rp)
}

# par?metros para gerar o eixo vetical do gr?fico do conjunto de oportunidades

return.min <- 0
return.max <- 0.001
steps <- 0.00001
return <- seq(from=return.min, to=return.max, by=steps)

# Loop para gerar a sequ?ncia de desvios-padr?o m?nimos (via Markowitz, fun??o var.carteira(rp)) para cada retorno esperado

sd.min <- c()

for (rp in return) {
  x <- var.carteira(rp)^(1/2)
  sd.min <- c(sd.min,x)
}

#risk.free <- 0.8*min(mu)
risk.free1 <- 0.035


sharpe <- c()
for (rp in return) {
  sharpe.int <- (rp-risk.free1)/(var.carteira(rp)^(1/2))
  sharpe <- c(sharpe,sharpe.int)
}
max(sharpe)

# gera o gr?fico desejado: conjunto de oportunidades

dados <- as.data.frame(cbind(return,sd.min))
conj.oport <- ggplot(dados, aes(sd.min,return)) + geom_point() +
  labs(title = 'Markowitz-Tobin', x = 'Desvio-Padrão', y = 'Retorno Esperado')
conj.oport

tobin.markowitz <-  conj.oport + geom_abline(data=dados,aes(slope=max(sharpe),intercept=risk.free1))
tobin.markowitz
