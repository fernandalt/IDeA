require(data.table)
library(reldist)
library(foreign)
require(magrittr)
require(sfsmisc) # para as integrais
require(reshape2)
## 2011 a 2015





## Dados Prova Brasil NSE


load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/dados_tudo.Rdata")


#####################################
# Carrega dados imputados Raca
##################################

#Matematica
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/2009-2013/imputacao_raca_30_resultado_MAT5.Rdata")



dados_tudo
amostras_simuladas[[1]]

colnames(codigos_municipios)[which(codigos_municipios[1,]==TRUE)]

dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],Matematica=amostras_simuladas[[1]]$Matematica,
                      Raça=amostras_simuladas[[1]]$Raça)

data.table(COD_MUNICIPIO=codigos_municipios[5,ID_MUNICIPIO],Matematica=amostras_simuladas[[5]]$Matematica,
           Raça=amostras_simuladas[[5]]$Raça)


dados_simulados<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],Matematica=amostras_simuladas[[1]]$Matematica,
                            Raça=amostras_simuladas[[1]]$Raça)


for(i in 2:length(amostras_simuladas)){
  
  dados_simulados<-rbind(dados_simulados,
                         data.table(COD_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],Matematica=amostras_simuladas[[i]]$Matematica,
                                    Raça=amostras_simuladas[[i]]$Raça))
  
}

dados_mat_5ano<-rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==5,c('COD_MUNICIPIO','Matematica','Raça')],
                      dados_simulados)


#LP
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/2009-2013/imputacao_raca_30_resultado_LP5.Rdata")


dados_tudo
amostras_simuladas[[1]]


dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],Leitura=amostras_simuladas[[1]]$Leitura,
                      Raça=amostras_simuladas[[1]]$Raça[1])


dados_simulados<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],Leitura=amostras_simuladas[[1]]$Leitura,
                            Raça=amostras_simuladas[[1]]$Raça)


for(i in 2:length(amostras_simuladas)){
  
  dados_simulados<-rbind(dados_simulados,
                         data.table(COD_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],Leitura=amostras_simuladas[[i]]$Leitura,
                                    Raça=amostras_simuladas[[i]]$Raça))
  
}

dados_lp_5ano<-rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==5,c('COD_MUNICIPIO','Leitura','Raça')],
                      dados_simulados)


## Funcao que calcula a KL
calculaKL<- function(Leitura,referencia){ 
  if( sum(!is.na(Leitura))>0 & sum(!is.na(referencia))>0){  # Retira aquele que nao tem informacao
    distribuicao_relativa<-reldist(y= referencia, yo=na.omit(Leitura),  graph=FALSE)
    cdfgr <- cumsum(distribuicao_relativa$y)/sum(distribuicao_relativa$y)
    area=integrate.xy(distribuicao_relativa$x,cdfgr)-.5
    return("Kl"=sign(area)*distribuicao_relativa$entropy)}
}

## QUINTO



dados_mat_5ano[,.(Desigualdade.Matematica=
                    calculaKL(Matematica[Raça=='Preto'],
                              Matematica[Raça=='Branco']),
                  nmatematica_preto=length(na.omit(Matematica[Raça=='Preto'])),
                  nmatematica_branco=length(na.omit(Matematica[Raça=='Branco']))
                  
       )]

desigualdade_raca_quinto_ano_mat<-dados_mat_5ano[,.(Desigualdade.Matematica=
                                       calculaKL(Matematica[Raça=='Preto'],
                                                 Matematica[Raça=='Branco']),
                                     nmatematica_preto=length(na.omit(Matematica[Raça=='Preto'])),
                                     nmatematica_branco=length(na.omit(Matematica[Raça=='Branco']))
                                     
),
                          by=COD_MUNICIPIO]



calculaKL(dados_mat_5ano[COD_MUNICIPIO== 1100346 & Raça=='Preto',Matematica],
           dados_mat_5ano[COD_MUNICIPIO== 1100346 & Raça=='Branco',Matematica])



desigualdade_raca_quinto_ano_lp<-dados_lp_5ano[,.(Desigualdade.Leitura=
                                                     calculaKL(Leitura[Raça=='Preto'],
                                                               Leitura[Raça=='Branco']),
                                                   nmatematica_preto=length(na.omit(Leitura[Raça=='Preto'])),
                                                   nmatematica_branco=length(na.omit(Leitura[Raça=='Branco']))
                                                   
),
by=COD_MUNICIPIO]






# Salva resultado

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Modelo Imputacao\\Dados Imputados\\Resultados KL')


desigualdade_raca_quinto_ano_mat[COD_MUNICIPIO<6000000]

save(desigualdade_raca_quinto_ano_mat,
     desigualdade_raca_quinto_ano_lp,file='Desigualdade_raca_Quinto_ano_2009_2013.Rdata')


require(ggplot2)
ggplot(desigualdade_raca_quinto_ano_mat,aes(x=Desigualdade.Matematica))+geom_histogram()

ggplot(desigualdade_raca_quinto_ano_lp,aes(x=Desigualdade.Leitura))+geom_histogram()




#####################################
# Carrega dados imputados NSE
##################################

#Matematica
rm(amostras_simuladas)
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/2009-2013/imputacao_nse_30_resultado_MAT5.Rdata")


dados_tudo
amostras_simuladas[[1]]

colnames(codigos_municipios)[which(codigos_municipios[1,]==TRUE)]

dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],Matematica=amostras_simuladas[[1]]$Matematica,
                      classificacao_NSE=amostras_simuladas[[1]]$NSE)

data.table(COD_MUNICIPIO=codigos_municipios[5,ID_MUNICIPIO],Matematica=amostras_simuladas[[5]]$Matematica,
           classificacao_NSE=amostras_simuladas[[5]]$NSE)


dados_simulados<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],
                            Matematica=amostras_simuladas[[1]]$Matematica,
                            classificacao_NSE=amostras_simuladas[[1]]$NSE)


for(i in 2:length(amostras_simuladas)){
  
  dados_simulados<-rbind(dados_simulados,
                         data.table(COD_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                    Matematica=amostras_simuladas[[i]]$Matematica,
                                    classificacao_NSE=amostras_simuladas[[i]]$NSE))
  
}

dados_mat_5ano<-rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==5,c('COD_MUNICIPIO','Matematica','classificacao_NSE')],
                      dados_simulados)


#LP
rm(amostras_simuladas)
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/2009-2013/imputacao_nse_30_resultado_LP5.Rdata")

dados_tudo
amostras_simuladas[[1]]

colnames(codigos_municipios)[which(codigos_municipios[1,]==TRUE)]

dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],
                      Leitura=amostras_simuladas[[1]]$Leitura,
                      classificacao_NSE=amostras_simuladas[[1]]$NSE)



dados_simulados<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],
                            Leitura=amostras_simuladas[[1]]$Leitura,
                            classificacao_NSE=amostras_simuladas[[1]]$NSE)

for(i in 2:length(amostras_simuladas)){
  
  dados_simulados<-rbind(dados_simulados,
                         data.table(COD_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                    Leitura=amostras_simuladas[[i]]$Leitura,
                                    classificacao_NSE=amostras_simuladas[[i]]$NSE))
  
}

dados_lp_5ano<-rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==5,c('COD_MUNICIPIO','Leitura','classificacao_NSE')],
                      dados_simulados)

## Funcao que calcula a KL
calculaKL<- function(Leitura,referencia){ 
  if( sum(!is.na(Leitura))>0 & sum(!is.na(referencia))>0){  # Retira aquele que nao tem informacao
    distribuicao_relativa<-reldist(y= referencia, yo=na.omit(Leitura),  graph=FALSE)
    cdfgr <- cumsum(distribuicao_relativa$y)/sum(distribuicao_relativa$y)
    area=integrate.xy(distribuicao_relativa$x,cdfgr)-.5
    return("Kl"=sign(area)*distribuicao_relativa$entropy)}
}

## QUINTO



dados_mat_5ano[,.(Desigualdade.Matematica=
                    calculaKL(Matematica[classificacao_NSE==1],
                              Matematica[classificacao_NSE==5]),
                  nmatematica_preto=length(na.omit(Matematica[classificacao_NSE==1])),
                  nmatematica_branco=length(na.omit(Matematica[classificacao_NSE==5]))
                  
)]

desigualdade_NSE_quinto_ano_mat<-dados_mat_5ano[,.(Desigualdade.Matematica=
                                                     calculaKL(Matematica[classificacao_NSE==1],
                                                               Matematica[classificacao_NSE==5]),
                                                   nmatematica_nse1=length(na.omit(Matematica[classificacao_NSE==1])),
                                                   nmatematica_nse5=length(na.omit(Matematica[classificacao_NSE==5]))
                                                   
),
by=COD_MUNICIPIO]



calculaKL(dados_mat_5ano[COD_MUNICIPIO==1100205 & classificacao_NSE==1,Matematica],
          dados_mat_5ano[COD_MUNICIPIO==1100205 & classificacao_NSE==5,Matematica])



desigualdade_NSE_quinto_ano_lp<-dados_lp_5ano[,.(Desigualdade.Leitura=
                                                     calculaKL(Leitura[classificacao_NSE==1],
                                                               Leitura[classificacao_NSE==5]),
                                                   nmatematica_nse1=length(na.omit(Leitura[classificacao_NSE==1])),
                                                   nmatematica_nse5=length(na.omit(Leitura[classificacao_NSE==5]))
                                                   
),
by=COD_MUNICIPIO]






# Salva resultado

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Modelo Imputacao\\Dados Imputados\\Resultados KL')


desigualdade_NSE_quinto_ano_mat[COD_MUNICIPIO<6000000]

save(desigualdade_NSE_quinto_ano_mat,
     desigualdade_NSE_quinto_ano_lp,file='Desigualdade_NSE_Quinto_ano_2009_2013.Rdata')


require(ggplot2)
ggplot(desigualdade_NSE_quinto_ano_mat,aes(x=Desigualdade.Matematica))+geom_histogram()

ggplot(desigualdade_NSE_quinto_ano_lp,aes(x=Desigualdade.Leitura))+geom_histogram()



#####################################
# Carrega dados imputados Sexo
##################################

#Matematica
rm(amostras_simuladas)
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/imputacao_sexo/imputacao_sexo_MAT5_09a13.Rdata")


dados_tudo
amostras_simuladas[[1]]

colnames(codigos_municipios)[which(codigos_municipios[1,]==TRUE)]

dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],Matematica=amostras_simuladas[[1]]$Matematica,
                      classificacao_NSE=amostras_simuladas[[1]]$NSE)

data.table(COD_MUNICIPIO=codigos_municipios[5,ID_MUNICIPIO],Matematica=amostras_simuladas[[5]]$Matematica,
           Sexo=amostras_simuladas[[5]]$Sexo)


dados_simulados<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],
                            Matematica=amostras_simuladas[[1]]$Matematica,
                            Sexo=amostras_simuladas[[1]]$Sexo)


for(i in 2:length(amostras_simuladas)){
  
  dados_simulados<-rbind(dados_simulados,
                         data.table(COD_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                    Matematica=amostras_simuladas[[i]]$Matematica,
                                    Sexo=amostras_simuladas[[i]]$Sexo))
  
}

dados_mat_5ano<-rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==5,c('COD_MUNICIPIO','Matematica','Sexo')],
                      dados_simulados)


#LP
rm(amostras_simuladas)
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Dados/imputacao_sexo/imputacao_sexo_LP5_09a13.Rdata")


dados_tudo
amostras_simuladas[[1]]

colnames(codigos_municipios)[which(codigos_municipios[1,]==TRUE)]

dados_aux<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],
                      Leitura=amostras_simuladas[[1]]$Leitura,
                      Sexo=amostras_simuladas[[1]]$Sexo)



dados_simulados<-data.table(COD_MUNICIPIO=codigos_municipios[1,ID_MUNICIPIO],
                            Leitura=amostras_simuladas[[1]]$Leitura,
                            Sexo=amostras_simuladas[[1]]$Sexo)

for(i in 2:length(amostras_simuladas)){
  
  dados_simulados<-rbind(dados_simulados,
                         data.table(COD_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                    Leitura=amostras_simuladas[[i]]$Leitura,
                                    Sexo=amostras_simuladas[[i]]$Sexo))
  
}

dados_lp_5ano<-rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==5,c('COD_MUNICIPIO','Leitura','Sexo')],
                     dados_simulados)

## Funcao que calcula a KL
calculaKL<- function(Leitura,referencia){ 
  if( sum(!is.na(Leitura))>0 & sum(!is.na(referencia))>0){  # Retira aquele que nao tem informacao
    distribuicao_relativa<-reldist(y= referencia, yo=na.omit(Leitura),  graph=FALSE)
    cdfgr <- cumsum(distribuicao_relativa$y)/sum(distribuicao_relativa$y)
    area=integrate.xy(distribuicao_relativa$x,cdfgr)-.5
    return("Kl"=sign(area)*distribuicao_relativa$entropy)}
}

## QUINTO



dados_mat_5ano[,.(Desigualdade.Matematica=
                    calculaKL(Matematica[Sexo=='Feminino'],
                              Matematica[Sexo=='Masculino']),
                  nmatematica_feminino=length(na.omit(Matematica[Sexo=='Feminino'])),
                  nmatematica_masculino=length(na.omit(Matematica[Sexo=='Masculino']))
                  
)]

desigualdade_sexo_quinto_ano_mat<-dados_mat_5ano[,.(Desigualdade.Matematica=
                                                      calculaKL(Matematica[Sexo=='Feminino'],
                                                                Matematica[Sexo=='Masculino']),
                                                    nmatematica_feminino=length(na.omit(Matematica[Sexo=='Feminino'])),
                                                    nmatematica_masculino=length(na.omit(Matematica[Sexo=='Masculino']))),
                                                 by=COD_MUNICIPIO]


desigualdade_sexo_quinto_ano_lp<-dados_lp_5ano[,.(Desigualdade.Leitura=
                                                    calculaKL(Leitura[Sexo=='Feminino'],
                                                              Leitura[Sexo=='Masculino']),
                                                  nleitura_feminino=length(na.omit(Leitura[Sexo=='Feminino'])),
                                                  nleitura_masculino=length(na.omit(Leitura[Sexo=='Masculino']))
                                                  
),
by=COD_MUNICIPIO]






# Salva resultado

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Modelo Imputacao\\Dados Imputados\\Resultados KL')
desigualdade_sexo_quinto_ano_mat[COD_MUNICIPIO<6000000]

save(desigualdade_sexo_quinto_ano_mat,
     desigualdade_sexo_quinto_ano_lp,file='Desigualdade_sexo_Quinto_ano_2009_2013.Rdata')


require(ggplot2)
ggplot(desigualdade_sexo_quinto_ano_mat,aes(x=Desigualdade.Matematica))+geom_histogram()

ggplot(desigualdade_sexo_quinto_ano_lp,aes(x=Desigualdade.Leitura))+geom_histogram()


