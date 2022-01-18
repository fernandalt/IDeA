require(data.table)



## Raca
# Carrega os dados

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_raca_escola_co_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_raca_escola_nordeste_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_raca_escola_norte_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_raca_escola_sudeste_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_raca_escola_sul_9ano.Rdata")


prop_raca_escola<-rbind(prop_raca_escola_co,
      prop_raca_escola_nordeste,
      prop_raca_escola_norte,
      prop_raca_escola_sudeste,
      prop_raca_escola_sul)



## Sexo
# Carrega os dados

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_sexo_escola_co_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_sexo_escola_nordeste_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_sexo_escola_norte_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_sexo_escola_sudeste_9ano.Rdata")
load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Censo Escolar/Prop_sexo_escola_sul_9ano.Rdata")


prop_sexo_escola<-rbind(prop_sexo_escola_co,
                        prop_sexo_escola_nordeste,
                        prop_sexo_escola_norte,
                        prop_sexo_escola_sudeste,
                        prop_sexo_escola_sul)

prop_sexo_escola[CO_ENTIDADE==61200600 ]
## Dados Prova Brasil NSE


load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/dados2017.Rdata")

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Calculo_KL_municipio_porano/quintis_nse.Rdata")


dados_2017$classificacao_NSE<-cut(dados_2017$NSE, breaks=quintis_nse,right=T, 
                                  include.lowest = T)

levels(dados_2017$classificacao_NSE)<-c(1,2,3,4,5)



dados_2017[ID_SERIE==5,.(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                         proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                         media_NSE=mean(NSE,na.rm = T)
                           )]


prop_nse_escola<-dados_2017[ID_SERIE==5,.(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
),by=ID_ESCOLA]


# Proporcao de alunos por escola em cada municipio


setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Modelo Imputacao\\Imputacao')
save(prop_nse_escola,prop_raca_escola,prop_sexo_escola,file='Dados_perfis_Escolas.Rdata')


