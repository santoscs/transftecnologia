

rm(list= ls())
load("~/Documents/R/transftecnologia/data/raw_data/ctt_cedentes.RData")
load("~/Documents/R/transftecnologia/data/raw_data/ctt_cessionarias.RData")
load("data/tidy_data/cnpj_bases.RData")
library(tidyverse)
library(janitor)
library(lubridate)

# extrai textos da publicacoes e peticoes

textab <- function(teste){
  id <- grep("- -", teste)
  teste <- teste[-id]
  tab1 <- teste[1:6]
  results <- matrix(teste[-c(1:6)], ncol = 9, byrow = T)
  results <- results[,-c(6,7)]
  results <- rbind(c(tab1[1:5], "Descricao Despacho", tab1[6]), results)
  return(results)
}


textab2 <- function(teste){
  id <- grep("Informações do Banco", teste)
  teste <- teste[-id]
  id <- grep("Leia-me", teste)
  teste <- teste[1:(id-3)]
  tab1 <- teste[1:7]
  results <- matrix(teste[-c(1:7)], ncol = 14, byrow = T)
  results <- results[,-c(1:3,6, 7,  9, 10, 13, 14)]
  results <- rbind(c("Protocolo", "Data", "Serviço", "Desc Servico", "Cliente"), results)
  return(results)
}



for(i in 1:length(ctt.cedentes)){
  ctt.cedentes[[i]]$publicacoes <- textab(ctt.cedentes[[i]]$publicacoes)
  ctt.cedentes[[i]]$peticao <- textab2(ctt.cedentes[[i]]$peticao)
}

for(i in 1:length(ctt.cessionarias)){
  ctt.cessionarias[[i]]$publicacoes <- textab(ctt.cessionarias[[i]]$publicacoes)
 # ctt.cessionarias[[i]]$peticao <- textab2(ctt.cessionarias[[i]]$peticao)
  
}


#### Cedentes
###################


cedentes1 <- t(ctt.cedentes[[1]]$transf)

for(i in 2:length(ctt.cedentes)){
  cedentes1 <- rbind(cedentes1, t(ctt.cedentes[[i]]$transf)[2,])
}
  

df.cedentes <- cedentes1[-1,]
colnames(df.cedentes) <- cedentes1[1,]
df.cedentes <- as_tibble(df.cedentes)
df.cedentes <- janitor::clean_names(df.cedentes)
df.cedentes %>%
  mutate(setor_da_cessionaria= str_replace(setor_da_cessionaria, "\\([:alpha:]", "")) %>%
  mutate(setor_da_cessionaria= str_replace(setor_da_cessionaria, "[:alpha:]\\)", "")) %>%
  separate(setor_da_cessionaria, into = c("setor_name", "setor_num"), sep = "\\(") %>%
  mutate(setor_num= str_replace(setor_num, "\\)", "")) %>%
  separate(setor_num, into=c("setor_main", "setor_sub"), sep= "\\.", fill = "right") -> df.cedentes

aux <- ctt.cedentes[[1]]$publicacoes
rownames(aux) <- rep(ctt.cedentes[[1]]$transf[1,2], dim(aux)[1])
cedentes1 <- aux
for(i in 2:length(ctt.cedentes)){
  aux <- ctt.cedentes[[i]]$publicacoes
  rownames(aux) <- rep(ctt.cedentes[[i]]$transf[1,2], dim(aux)[1])
  cedentes1 <- rbind(cedentes1, aux)
  # aux2 <- rbind(aux2, ctt.cedentes[[i]]$transf[1,2])
}
id <- grep("RPI", cedentes1[,1])
nomes <- cedentes1[1,]
cedentes1 <- cedentes1[-id,]
df.publica <- cbind(rownames(cedentes1),cedentes1)
colnames(df.publica) <- c("requerimento", nomes)

df.publica <- as_tibble(df.publica)
df.publica <- clean_names(df.publica)

# exclui contratos indeferidos
indeferidos <- df.publica %>%
  filter(despacho==130) 
df.cedentes <- df.cedentes %>%
  anti_join(indeferidos) 

# exclui contratos repetidos
df.cedentes <- distinct(df.cedentes)

## relaciona com informacoes da B3
df.cedentes  <- df.cedentes %>%
  mutate(cnpj = ctt.cnpj.cedente$cnpj[match(requerimento, ctt.cnpj.cedente$requerimento)])  %>%
  mutate(name_company = df.geral$name_company[match(cnpj, df.geral$cnpj)]) %>%
  mutate(ano = year(dmy(entrada))) %>%
  mutate(setor_b3 = df.geral$main_sector[match(cnpj, df.geral$cnpj)])

## exclui empresas sem setores na b3 e contratos fora do periodo
df.cedentes <- df.cedentes %>%
  filter(!is.na(setor_b3)) %>%
  filter(ano<2018 & ano>2009)


## cria variaveis do tipo de contrato
df.cedentes <- df.cedentes %>%
  mutate(transferencia = if_else(
    agrepl("SERVIÇO DE ASSISTÊNCIA TÉCNICA E CIENTÍFICA", categoria_contratual) | 
      agrepl("FORNECIMENTO DE TECNOLOGIA", categoria_contratual) |
      agrepl("FRANQUIA", categoria_contratual), 1, 0)) %>%
  mutate(licensa = if_else(
    agrepl("USO DE MARCA", categoria_contratual) | 
      agrepl("EXPLORAÇÃO DE PATENTE", categoria_contratual) |
      agrepl("EXPLORAÇÃO DE DESENHO INDUSTRIAL", categoria_contratual), 1, 0)) %>%
  mutate(cessao = if_else(
    agrepl("CESSÃO DE MARCA", categoria_contratual), 1, 0))





#### Cessionarias
###################

cessionarias1 <- t(ctt.cessionarias[[1]]$transf)

for(i in 2:length(ctt.cessionarias)){
  cessionarias1 <- rbind(cessionarias1, t(ctt.cessionarias[[i]]$transf)[2,])
}


df.cessionarias <- cessionarias1[-1,]
colnames(df.cessionarias) <- cessionarias1[1,]
df.cessionarias <- as_tibble(df.cessionarias)
df.cessionarias <- janitor::clean_names(df.cessionarias)
df.cessionarias %>%
  mutate(setor_da_cessionaria= str_replace(setor_da_cessionaria, "\\([:alpha:]", "")) %>%
  mutate(setor_da_cessionaria= str_replace(setor_da_cessionaria, "[:alpha:]\\)", "")) %>%
  separate(setor_da_cessionaria, into = c("setor_name", "setor_num"), sep = "\\(") %>%
  mutate(setor_num= str_replace(setor_num, "\\)", "")) %>%
  separate(setor_num, into=c("setor_main", "setor_sub"), sep= "\\.", fill = "right") -> df.cessionarias

### organiza informacoes das publicacoes
aux <- ctt.cessionarias[[1]]$publicacoes
rownames(aux) <- rep(ctt.cessionarias[[1]]$transf[1,2], dim(aux)[1])
cessionarias1 <- aux
for(i in 2:length(ctt.cessionarias)){
  aux <- ctt.cessionarias[[i]]$publicacoes
  rownames(aux) <- rep(ctt.cessionarias[[i]]$transf[1,2], dim(aux)[1])
  cessionarias1 <- rbind(cessionarias1, aux)
  # aux2 <- rbind(aux2, ctt.cessionarias[[i]]$transf[1,2])
}
id <- grep("RPI", cessionarias1[,1])
nomes <- cessionarias1[1,]
cessionarias1 <- cessionarias1[-id,]
df.publica <- cbind(rownames(cessionarias1),cessionarias1)
colnames(df.publica) <- c("requerimento", nomes)
df.publica <- as_tibble(df.publica)
df.publica <- clean_names(df.publica)

### Exclui contratos indeferidos
indeferidos <- df.publica %>%
  filter(despacho==130) 
df.cessionarias <- df.cessionarias %>%
  anti_join(indeferidos) 

## Exclui contratos repetidos
df.cessionarias <- distinct(df.cessionarias)

## relaciona com informacoes da B3
df.cessionarias  <- df.cessionarias %>%
  mutate(cnpj = ctt.cnpj.cessionaria$cnpj[match(requerimento, ctt.cnpj.cessionaria$requerimento)])  %>%
  mutate(name_company = df.geral$name_company[match(cnpj, df.geral$cnpj)]) %>%
  mutate(ano = year(dmy(entrada))) %>%
  mutate(setor_b3 = df.geral$main_sector[match(cnpj, df.geral$cnpj)])

## exclui empresas sem setores na b3
df.cessionarias <- df.cessionarias %>%
  filter(!is.na(setor_b3))

## exclui contratos fora do periodo
df.cessionarias <- df.cessionarias %>%
  filter(ano<2018 & ano>2009)

## cria variaveis do tipo de contrato
df.cessionarias <- df.cessionarias %>%
  mutate(transferencia = if_else(
    agrepl("SERVIÇO DE ASSISTÊNCIA TÉCNICA E CIENTÍFICA", categoria_contratual) | 
      agrepl("FORNECIMENTO DE TECNOLOGIA", categoria_contratual) |
      agrepl("FRANQUIA", categoria_contratual), 1, 0)) %>%
  mutate(licensa = if_else(
    agrepl("USO DE MARCA", categoria_contratual) | 
      agrepl("EXPLORAÇÃO DE PATENTE", categoria_contratual) |
      agrepl("EXPLORAÇÃO DE DESENHO INDUSTRIAL", categoria_contratual), 1, 0)) %>%
  mutate(cessao = if_else(
    agrepl("CESSÃO DE MARCA", categoria_contratual), 1, 0))







 
 teste <- df.publica %>%
   filter(despacho==350) %>%
   mutate(pais_cedente = substr(complemento_do_despacho, regexpr("País da cedente:", complemento_do_despacho) + 16,
                                regexpr( "Cessionária", complemento_do_despacho)-1)) %>%
   filter(str_count(pais_cedente)>2 & str_count(pais_cedente)<30 ) %>%
   select(requerimento, pais_cedente) 
 
 teste <- distinct(teste)

 
 sele <- str_detect(df.cessionarias$pais_da_cedente, "/")
 df.cessionarias[sele,] <- df.cessionarias[sele,] %>%
   mutate(pais_da_cedente = teste$pais_cedente[match(requerimento, teste$requerimento)])
 
 
 
 df.cessionarias <- df.cessionarias %>%
   mutate(pais_da_cedente = str_trim(pais_da_cedente))
 
 
 df.cessionarias$pais_da_cedente[agrep("COREIA",df.cessionarias$pais_da_cedente)] <- "COREIA"
 df.cessionarias$pais_da_cedente[agrep("HOLANDA",df.cessionarias$pais_da_cedente)] <- "HOLANDA"
 df.cessionarias$pais_da_cedente[agrep("RUSSIA",df.cessionarias$pais_da_cedente)] <- "RUSSIA"
 df.cessionarias$pais_da_cedente[agrep("REINO UNIDO",df.cessionarias$pais_da_cedente)] <- "REINO UNIDO"
 df.cessionarias$pais_da_cedente[agrep("HUNGRIA",df.cessionarias$pais_da_cedente)] <- "HUNGRIA"
 df.cessionarias$pais_da_cedente[agrep("POLONIA",df.cessionarias$pais_da_cedente)] <- "POLONIA"
 df.cessionarias$pais_da_cedente[agrep("CHINA",df.cessionarias$pais_da_cedente)] <- "CHINA"
 df.cessionarias$pais_da_cedente[agrep("ESTADOS UNIDOS",df.cessionarias$pais_da_cedente)] <- "ESTADOS UNIDOS"
 df.cessionarias$pais_da_cedente[agrep("BRASIL;BRASIL;BRASIL;BRASIL",df.cessionarias$pais_da_cedente)] <- "BRASIL"
 
 
save(df.cessionarias, df.cedentes, df.geral, file = "data/tidy_data/tranfteconologia.RData")

