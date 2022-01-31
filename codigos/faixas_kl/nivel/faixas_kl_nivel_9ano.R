rm(list=ls(all=T))

## Pacotes
require(data.table)
require(readxl)
require(readr)

## Base de dados
wd = getwd()
path = dirname(dirname(dirname(wd)))
load(paste0(path,"/dados/raw/DadosProvaBrasil_2007_2017.Rdata"))

colnames(dados_tudo)[4]<-'ID_MUNICIPIO'
dados_tudo<-dados_tudo[ID_MUNICIPIO<6000000]

# Ideb 
#https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb/resultados
dados_ideb = fread(paste0(path,'/dados/raw/Dados_Ideb_anos_finais.csv'),
                   na.strings = '-',dec=',')

dados_ideb = dados_ideb[Rede=='Pública']

# resultados kl
load(paste0(path,'/dados/kl/nivel/kl_nivel_9ano_2017.Rdata'))

# valor de kl dos municipios por faixa do Ideb
# LP
kl_nono_ano_lp[ID_MUNICIPIO %in% dados_ideb[IDEB_2017<=3.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Baixo'
kl_nono_ano_lp[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>3.5 & IDEB_2017<=4.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Medio_Baixo'
kl_nono_ano_lp[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>4.5 & IDEB_2017<=5.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Medio'
kl_nono_ano_lp[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>5.5 & IDEB_2017<=6.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Medio_Alto'
kl_nono_ano_lp[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>6.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Alto'

kl_nono_ano_lp$Faixa_IDEB = factor(kl_nono_ano_lp$Faixa_IDEB)
kl_nono_ano_lp$Faixa_IDEB = factor(kl_nono_ano_lp$Faixa_IDEB,
                                   levels=c("Baixo","Medio_Baixo","Medio","Medio_Alto","Alto"))

quartis_lp = kl_nono_ano_lp[!is.na(Faixa_IDEB),
                            .(Quartil_1=quantile(KL.Leitura,na.rm=T)[2],
                              Quartil_3=quantile(KL.Leitura,na.rm=T)[4]),
                            by=Faixa_IDEB][order(Quartil_1)]

# MAT
kl_nono_ano_mat[ID_MUNICIPIO %in% dados_ideb[IDEB_2017<=3.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Baixo'
kl_nono_ano_mat[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>3.5 & IDEB_2017<=4.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Medio_Baixo'
kl_nono_ano_mat[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>4.5 & IDEB_2017<=5.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Medio'
kl_nono_ano_mat[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>5.5 & IDEB_2017<=6.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Medio_Alto'
kl_nono_ano_mat[ID_MUNICIPIO %in% dados_ideb[IDEB_2017>6.5,ID_MUNICIPIO],'Faixa_IDEB' ] = 'Alto'

kl_nono_ano_mat$Faixa_IDEB = factor(kl_nono_ano_mat$Faixa_IDEB)
kl_nono_ano_mat$Faixa_IDEB = factor(kl_nono_ano_mat$Faixa_IDEB,
                                    levels=c("Baixo","Medio_Baixo","Medio","Medio_Alto","Alto"))

quartis_mat = kl_nono_ano_mat[!is.na(Faixa_IDEB),
                              .(Quartil_1=quantile(KL.Matematica,na.rm=T)[2],
                                Quartil_3=quantile(KL.Matematica,na.rm=T)[4]),
                              by=Faixa_IDEB][order(Quartil_1)]

# combina resultados
kl_nono_ano_lp$Disciplina = 'Leitura'
kl_nono_ano_mat$Disciplina = 'Matematica'

kl_nono = rbind(kl_nono_ano_lp,kl_nono_ano_mat,use.names=FALSE)
colnames(kl_nono)[2] = 'KL.Nivel'

kl_nono$Faixa_IDEB = factor(kl_nono$Faixa_IDEB,
                            levels=c("Baixo","Medio_Baixo","Medio","Medio_Alto","Alto"))

quartis = kl_nono[!is.na(Faixa_IDEB),
                  .(Quartil_1=quantile(KL.Nivel,na.rm=T)[2],
                    Quartil_3=quantile(KL.Nivel,na.rm=T)[4]),
                  by=Faixa_IDEB][order(Quartil_1)]

# Salva resultado
quartis_lp$Ano = '2017'
quartis_mat$Ano = '2017'
quartis$Ano = '2017'

save(quartis,quartis_lp,quartis_mat,
     file = paste0(path, '/dados/faixas_kl/nivel/ponto_corte_faixa_9ano.Rdata'))