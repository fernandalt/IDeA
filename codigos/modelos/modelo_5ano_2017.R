rm(list=ls(all=T))

## Pacotes
require(data.table)
library(nlme)

## Base de dados
wd = getwd()
path = paste0(dirname(dirname(wd)), "/dados/raw")
load(paste0(path,"/DadosProvaBrasil_2007_2017.Rdata"))

dados_tudo<-dados_tudo[COD_MUNICIPIO<6000000]

colnames(dados_tudo)[4]<-'ID_MUNICIPIO'

NSE_Medio_Escola<-dados_tudo[,.(NSE_Escola=mean(NSE,na.rm=T)),by='PK_COD_ENTIDADE']

dados_tudo<-merge(dados_tudo,NSE_Medio_Escola,all.x=T)

dados_tudo[is.na(Raça),'Raça']<-'Ausente'

## Modelos
memory.limit(size=1000000)

set.seed(1000)
modelo.Completo.5ano.lp.13.17= lme(Leitura ~ as.factor(Ano) + Sexo + Raça + classificacao_NSE+NSE_Escola, random = ~  classificacao_NSE+Raça+Sexo|ID_MUNICIPIO, 
                             na.action = na.omit, data = dados_tudo[ID_SERIE == 5 & Leitura > 0 & Ano %in% c(2013,2015,2017),], control = lmeControl(opt = "optim"))

path = paste0(dirname(dirname(wd)), "/dados/modelos")
save(modelo.Completo.5ano.lp.13.17,
     file=paste0(path,'modelo_5ano_lp_2017.Rdata'))


set.seed(1000)
modelo.Completo.5ano.mat.13.17= lme(Matematica ~ as.factor(Ano) + Sexo + Raça + classificacao_NSE+NSE_Escola, random = ~  classificacao_NSE+Raça+Sexo|ID_MUNICIPIO, 
                                    na.action = na.omit, data = dados_tudo[ID_SERIE == 5 & Matematica > 0 & Ano %in% c(2013,2015,2017),], control = lmeControl(opt = "optim"))

save(modelo.Completo.5ano.mat.13.17,
     file=paste0(path,"modelo_5ano_mat_2017.Rdata"))