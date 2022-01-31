require(data.table)
library(reldist)
library(foreign)
require(magrittr)
require(sfsmisc) # para as integrais
require(reshape2)
## 2013 a 2017





## Dados Prova Brasil NSE


load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/dados_tudo.Rdata")



# Carrega dados imputados

#Matematica
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/2007-2011/imputacao_nivel_49_resultado_MAT9.Rdata")

dados_tudo<-dados_tudo[COD_MUNICIPIO<6000000]
amostras_simuladas35[[1]]

dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1],Matematica=amostras_simuladas35[[1]])

tamanho_amostras<-sapply(amostras_simuladas35,length)

dados_simulados<-data.table(COD_MUNICIPIO=rep(codigos_municipios,tamanho_amostras),
Matematica=unlist(amostras_simuladas35))


dados_simulados[COD_MUNICIPIO==1100908]
length(amostras_simuladas35[[1]])

dados_mat_9ano<-rbind(dados_tudo[(Ano %in% c(2007,2009,2011)) & ID_SERIE==9,c('COD_MUNICIPIO','Matematica')],
                      dados_simulados)


#LP
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/2007-2011/imputacao_nivel_49_resultado_LP9.Rdata")

tamanho_amostras<-sapply(amostras_simuladas35,length)

dados_simulados<-data.table(COD_MUNICIPIO=rep(codigos_municipios,tamanho_amostras),
                            Leitura=unlist(amostras_simuladas35))


dados_simulados[COD_MUNICIPIO==1100908]
length(amostras_simuladas35[[1]])

dados_lp_9ano<-rbind(dados_tudo[(Ano %in% c(2007,2009,2011)) & ID_SERIE==9,c('COD_MUNICIPIO','Leitura')],
                      dados_simulados)



## Funcao que calcula a KL
calculaKL<- function(Leitura,referencia){ 
  if( sum(!is.na(Leitura))>0 & sum(!is.na(referencia))>0){  # Retira aquele que nao tem informacao
    distribuicao_relativa<-reldist(y= referencia, yo=na.omit(Leitura),  graph=FALSE)
    cdfgr <- cumsum(distribuicao_relativa$y)/sum(distribuicao_relativa$y)
    area=integrate.xy(distribuicao_relativa$x,cdfgr)-.5
    return("Kl"=sign(area)*distribuicao_relativa$entropy)}
}

## Usando a referencia do Victor

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/Referencias_victor.RData")

amostras_referencia<-data.table(
  Leitura5=Referencia.Leitura.Quinto)
amostras_referencia$Leitura9<-Referencia.Leitura.Nono
amostras_referencia$Matematica5<-Referencia.Matematica.Quinto
amostras_referencia$Matematica9<-Referencia.Matematica.Nono


## Nono
kl_nono_ano_mat<-dados_mat_9ano[,.(KL.Matematica=
                                          calculaKL(Matematica,amostras_referencia$Matematica9),
                                        nmatematica=length(na.omit(Matematica))
                                     ),
                          by=COD_MUNICIPIO]

kl_nono_ano_lp<-dados_lp_9ano[,.(KL.Leitura=
                                       calculaKL(Leitura,amostras_referencia$Leitura9),
                                     nmatematica=length(na.omit(Leitura))
),
by=COD_MUNICIPIO]





# Salva resultado

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Modelo Imputacao\\Dados Imputados\\Resultados KL')


save(kl_nono_ano_lp,kl_nono_ano_mat,file='KL_Nono_ano_2007_2011.Rdata')


