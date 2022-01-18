rm(list=ls(all=T))

## Pacotes
require(data.table)

## Base de dados
wd = getwd()
path = paste0(dirname(dirname(wd)), "/dados/raw")
load(paste0(path,"/DadosProvaBrasil_2007_2017.Rdata"))

colnames(dados_tudo)[4]<-'ID_MUNICIPIO'
dados_tudo<-dados_tudo[ID_MUNICIPIO<6000000]

# proporcao NSE municipios
# janela 07 a 11
prop_nse_municipio_07.11_5ano = dados_tudo[Ano %in% c(2007,2009,2011) & ID_SERIE==5,
                                        .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
                                          ),by=ID_MUNICIPIO]

prop_nse_municipio_07.11_9ano = dados_tudo[Ano %in% c(2007,2009,2011) & ID_SERIE==9,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=ID_MUNICIPIO]

# janela 09 a 13
prop_nse_municipio_09.13_5ano = dados_tudo[Ano %in% c(2009,2011,2013) & ID_SERIE==5,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=ID_MUNICIPIO]

prop_nse_municipio_09.13_9ano = dados_tudo[Ano %in% c(2009,2011,2013) & ID_SERIE==9,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=ID_MUNICIPIO]

# janela 11 a 15
prop_nse_municipio_11.15_5ano = dados_tudo[Ano %in% c(2011,2013,2015) & ID_SERIE==5,
                                        .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
                                        ),by=ID_MUNICIPIO]

prop_nse_municipio_11.15_9ano = dados_tudo[Ano %in% c(2011,2013,2015) & ID_SERIE==9,
                                        .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
                                        ),by=ID_MUNICIPIO]

# janela 13 a 17
prop_nse_municipio_13.17_5ano = dados_tudo[Ano %in% c(2013,2015,2017) & ID_SERIE==5,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=ID_MUNICIPIO]


prop_nse_municipio_13.17_9ano = dados_tudo[Ano %in% c(2013,2015,2017) & ID_SERIE==9,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=ID_MUNICIPIO] 


path = paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios")

save(prop_nse_municipio_07.11_5ano, prop_nse_municipio_09.13_5ano, prop_nse_municipio_11.15_5ano, prop_nse_municipio_13.17_5ano,
     file=paste0(path,'/perfis_municipios_5ano_nse.Rdata'))

save(prop_nse_municipio_07.11_9ano, prop_nse_municipio_09.13_9ano, prop_nse_municipio_11.15_9ano, prop_nse_municipio_13.17_9ano,
     file=paste0(path,'/perfis_municipios_9ano_nse.Rdata'))



# proporcao NSE escolas
# janela 07 a 11
prop_nse_escola_07.11_5ano = dados_tudo[Ano %in% c(2007,2009,2011) & ID_SERIE==5,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=PK_COD_ENTIDADE]

prop_nse_escola_07.11_9ano = dados_tudo[Ano %in% c(2007,2009,2011) & ID_SERIE==9,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=PK_COD_ENTIDADE]

# janela 09 a 13
prop_nse_escola_09.13_5ano = dados_tudo[Ano %in% c(2009,2011,2013) & ID_SERIE==5,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=PK_COD_ENTIDADE]

prop_nse_escola_09.13_9ano = dados_tudo[Ano %in% c(2009,2011,2013) & ID_SERIE==9,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=PK_COD_ENTIDADE]

# janela 11 a 15
prop_nse_escola_11.15_5ano = dados_tudo[Ano %in% c(2011,2013,2015) & ID_SERIE==5,
                                        .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
                                        ),by=PK_COD_ENTIDADE]

prop_nse_escola_11.15_9ano = dados_tudo[Ano %in% c(2011,2013,2015) & ID_SERIE==9,
                                        .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
                                        ),by=PK_COD_ENTIDADE]

# janela 13 a 17
prop_nse_escola_13.17_5ano = dados_tudo[Ano %in% c(2013,2015,2017) & ID_SERIE==5,
                                           .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                             media_NSE=mean(NSE,na.rm = T)
                                           ),by=PK_COD_ENTIDADE] 

prop_nse_escola_13.17_9ano = dados_tudo[Ano %in% c(2013,2015,2017) & ID_SERIE==9,
                                        .(proporcao_NSE1=sum(classificacao_NSE==1,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          proporcao_NSE5=sum(classificacao_NSE==5,na.rm=T)/sum(!is.na(classificacao_NSE)),
                                          media_NSE=mean(NSE,na.rm = T)
                                        ),by=PK_COD_ENTIDADE] 

save(prop_nse_escola_07.11_5ano, prop_nse_escola_09.13_5ano, prop_nse_escola_11.15_5ano, prop_nse_escola_13.17_5ano,
     file=paste0(path,'/perfis_escolas_5ano_nse.Rdata'))

save(prop_nse_escola_07.11_9ano, prop_nse_escola_09.13_9ano, prop_nse_escola_11.15_9ano, prop_nse_escola_13.17_9ano,
     file=paste0(path,'/perfis_escolas_9ano_nse.Rdata'))
