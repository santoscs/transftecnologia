
### tidy data

rm(list= ls())
load("~/Documents/R/transftecnologia/data/raw_data/cedentes.RData")
load("~/Documents/R/transftecnologia/data/raw_data/cessionaria.RData")
load("~/Documents/R/transftecnologia/data/raw_data/ctt_cedentes.RData")
load("~/Documents/R/transftecnologia/data/raw_data/ctt_cessionarias.RData")
load("~/Documents/R/transftecnologia/data/raw_data/patentes.RData")
load("~/Documents/R/transftecnologia/data/raw_data/df_empresas.RData")



library(tidyverse)
library(lubridate)
library(skimr)

#df <- df %>% dplyr::filter(main.sector!="Financeiro")


# skim(df)
# df %>%
#   filter(is.na(date.constitution)) %>%
#   select(cnpj) %>%
#   as.character()

df[df$cnpj=="92781335000102", "date.constitution"] <- c("1996-08-11")
df[df$cnpj=="85778074000106", "date.constitution"] <- c("1996-01-31")
df[df$cnpj=="359742000108", "date.constitution"] <- c("1994-12-20")
df[df$cnpj=="8424178000171", "date.constitution"] <- c("1969-12-15")

df$cnpj <- gsub(" ", "0", as.character(format(df$cnpj, digits = 14, width = 14)))

df <- as_tibble(df)




### Dados com informacoes gerais


## cnpj ctt


sele <- lapply(cedentes, function(x) is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ x[,1] })
teste <- lapply(aux, function(x){ length(x) })
teste <- unlist(teste)

nomes <- names(aux)

nomes <- gsub(" ", "0", as.character(format(as.numeric(nomes), digits = 14, width = 14)))
tmp <- NULL
for(i in 1:length(nomes)){
  tmp <- c(tmp, rep(nomes[i], teste[[i]]))
}

aux <- gsub(" ", "", unlist(aux))
aux2 <- cbind(tmp, aux)
colnames(aux2) <- c("cnpj", "requerimento")
ctt.cnpj.cedente  <- as_tibble(aux2)


sele <- lapply(cessionaria, function(x) is.matrix(x))
aux <- lapply(cessionaria[sele==TRUE], function(x){ x[,1] })
teste <- lapply(aux, function(x){ length(x) })
teste <- unlist(teste)

nomes <- names(aux)

nomes <- gsub(" ", "0", as.character(format(as.numeric(nomes), digits = 14, width = 14)))
tmp <- NULL
for(i in 1:length(nomes)){
  tmp <- c(tmp, rep(nomes[i], teste[[i]]))
}

aux <- gsub(" ", "", unlist(aux))
aux2 <- cbind(tmp, aux)
colnames(aux2) <- c("cnpj", "requerimento")
ctt.cnpj.cessionaria  <- as_tibble(aux2)



## geral

geral <-df

sele <- lapply(cedentes, function(x) is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
cnpj <- gsub(" ", "0", as.character(format(names(cedentes[sele==TRUE]), digits = 14, width = 14)))
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
sele <- lapply(aux, function(x) dim(x)[1]>0)

nomes <- cnpj[sele==TRUE]

aux2 <- cbind(nomes, 1)
colnames(aux2) <- c("cnpj", "e_cedentes")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(e_cedentes = as.numeric(e_cedentes))

geral <- left_join(geral, aux2)

geral$e_cedentes[is.na(geral$e_cedentes)] <- 0


sele <- lapply(cessionaria, function(x) is.matrix(x))
aux <- lapply(cessionaria[sele==TRUE], function(x){ year(dmy(x[,2]))})
cnpj <- gsub(" ", "0", as.character(format(names(cessionaria[sele==TRUE]), digits = 14, width = 14)))
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
sele <- lapply(aux, function(x) dim(x)[1]>0)

nomes <- cnpj[sele==TRUE]

aux2 <- cbind(nomes, 1)
colnames(aux2) <- c("cnpj", "e_cessionaria")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(e_cessionaria = as.numeric(e_cessionaria))

geral <- left_join(geral, aux2)

geral$e_cessionaria[is.na(geral$e_cessionaria)] <- 0


sele <- lapply(patentes, function(x) is.matrix(x))
aux <- lapply(patentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
cnpj <- gsub(" ", "0", as.character(format(names(patentes[sele==TRUE]), digits = 14, width = 14)))
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
sele <- lapply(aux, function(x) dim(x)[1]>0)

nomes <- cnpj[sele==TRUE]

aux2 <- cbind(nomes, 1)
colnames(aux2) <- c("cnpj", "e_patentes")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(e_patentes = as.numeric(e_patentes))

geral <- left_join(geral, aux2)

geral$e_patentes[is.na(geral$e_patentes)] <- 0



### dados em painel

aux <- expand(df, ano = 2010:2017, cnpj)
df <- left_join(aux, df)

sele <- lapply(patentes, function(x) is.matrix(x))
aux <- lapply(patentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
cnpj <- gsub(" ", "0", as.character(format(names(patentes), digits = 14, width = 14)))
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "patentes", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(patentes = as.numeric(patentes))

df <- left_join(df, aux2)



# cedentes
sele <- lapply(cedentes, function(x)is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
cnpj <- gsub(" ", "0", as.character(format(names(cedentes), digits = 14, width = 14)))
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "cedentes", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(cedentes = as.numeric(cedentes))

df <- left_join(df, aux2)

## cessionaria
sele <- lapply(cessionaria, function(x)is.matrix(x))
aux <- lapply(cessionaria[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
cnpj <- gsub(" ", "0", as.character(format(names(cessionaria), digits = 14, width = 14)))
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "cessionaria", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(cessionaria = as.numeric(cessionaria))

df <- left_join(df, aux2)

df$cedentes[is.na(df$cedentes)] <- 0
df$cessionaria[is.na(df$cessionaria)] <- 0
df$patentes[is.na(df$patentes)] <- 0

df.painel <- df %>%
  select(cnpj, ano, name.company, main.sector, patentes, cessionaria, cedentes)


df.painel <- clean_names(df.painel)
df.geral <- clean_names(geral)


save(df.painel, df.geral, ctt.cnpj.cedente, ctt.cnpj.cessionaria, file = "data/tidy_data/cnpj_bases.RData")

# 


