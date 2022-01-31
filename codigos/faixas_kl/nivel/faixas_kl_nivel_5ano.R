require(data.table)
library(reldist)
library(foreign)
require(magrittr)
require(sfsmisc) # para as integrais
require(reshape2)
require(ggplot2)

#Faixa IDEB
# < 3.5 Baixo
# 3,5 <x < 4,5 Medio baixo
# 4,5 < x < 5,5 Medio
# 5,5 < x < 6,5 Medio Alto
# > 6,5 Alto

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Atualizacao Faixas\\Proposta Chico')


dados_ideb<-fread('Dados_Ideb_anos_iniciais.csv',na.strings = '-',dec=',')

dados_ideb<-dados_ideb[Rede=='Pública']


## Dados Prova Brasil 


load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/dados_tudo.Rdata")


dados_ideb[IDEB_2017<=3.5,COD_MUNICIPIO]


dados_ideb_baixo<-dados_tudo[COD_MUNICIPIO %in% dados_ideb[IDEB_2017<=3.5,COD_MUNICIPIO] ]


dados_ideb_medio<-dados_tudo[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>3.5 & IDEB_2017<=4.5,COD_MUNICIPIO] ]



# Carrega dados KL

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/Dados Imputados/Resultados KL/KL_Quinto_ano_2013_2017.Rdata")


# LP


kl_quinto_ano_lp[COD_MUNICIPIO %in% dados_ideb[IDEB_2017<=3.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Baixo'
kl_quinto_ano_lp[Faixa_IDEB=='Baixo']

kl_quinto_ano_lp[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>3.5 & IDEB_2017<=4.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Medio_Baixo'
kl_quinto_ano_lp[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>4.5 & IDEB_2017<=5.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Medio'
kl_quinto_ano_lp[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>5.5 & IDEB_2017<=6.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Medio_Alto'
kl_quinto_ano_lp[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>6.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Alto'

kl_quinto_ano_lp$Faixa_IDEB<-factor(kl_quinto_ano_lp$Faixa_IDEB)

levels(kl_quinto_ano_lp$Faixa_IDEB)

prop.table(table(kl_quinto_ano_lp$Faixa_IDEB))

ggplot(kl_quinto_ano_lp[!is.na(Faixa_IDEB)],aes(x=KL.Leitura,col=Faixa_IDEB))+geom_density(size=1.5)



quantile(kl_quinto_ano_lp$KL.Leitura,na.rm = T)[4]


quartis_lp<-kl_quinto_ano_lp[!is.na(Faixa_IDEB),.(Quartil_1=quantile(KL.Leitura,na.rm=T)[2],
                                      Quartil_3=quantile(KL.Leitura,na.rm=T)[4]),by=Faixa_IDEB][order(Quartil_1)]




ggplot(kl_quinto_ano_lp[!is.na(Faixa_IDEB)],aes(y=KL.Leitura,col=Faixa_IDEB))+geom_boxplot()






# MAT
kl_quinto_ano_mat$Faixa_IDEB<-NULL

kl_quinto_ano_mat[COD_MUNICIPIO %in% dados_ideb[IDEB_2017<=3.5,COD_MUNICIPIO]]
kl_quinto_ano_mat[COD_MUNICIPIO %in% dados_ideb[IDEB_2017<=3.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Baixo'
kl_quinto_ano_mat[Faixa_IDEB=='Baixo']

kl_quinto_ano_mat[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>3.5 & IDEB_2017<=4.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Medio_Baixo'
kl_quinto_ano_mat[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>4.5 & IDEB_2017<=5.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Medio'
kl_quinto_ano_mat[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>5.5 & IDEB_2017<=6.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Medio_Alto'
kl_quinto_ano_mat[COD_MUNICIPIO %in% dados_ideb[IDEB_2017>6.5,COD_MUNICIPIO],'Faixa_IDEB' ]<-'Alto'

kl_quinto_ano_mat$Faixa_IDEB<-factor(kl_quinto_ano_mat$Faixa_IDEB)

levels(kl_quinto_ano_mat$Faixa_IDEB)

kl_quinto_ano_mat$Faixa_IDEB<-factor(kl_quinto_ano_mat$Faixa_IDEB,
                                     levels=c( "Baixo","Medio_Baixo","Medio","Medio_Alto","Alto"))

prop.table(table(kl_quinto_ano_mat$Faixa_IDEB))

ggplot(kl_quinto_ano_mat[!is.na(Faixa_IDEB)],aes(x=KL.Matematica,col=Faixa_IDEB))+geom_density(size=1.5)



quantile(kl_quinto_ano_mat$KL.Matematica,na.rm = T)[4]
kl_quinto_ano_mat[!is.na(Faixa_IDEB),quantile(KL.Matematica,na.rm=T)[4],by=Faixa_IDEB][order(V1)]

quartis_mat<-kl_quinto_ano_mat[!is.na(Faixa_IDEB),.(Quartil_1=quantile(KL.Matematica,na.rm=T)[2],
                                                  Quartil_3=quantile(KL.Matematica,na.rm=T)[4]),by=Faixa_IDEB][order(Quartil_1)]


ggplot(kl_quinto_ano_mat[!is.na(Faixa_IDEB)],aes(y=KL.Matematica,col=Faixa_IDEB))+geom_boxplot()


## Junta LP e MAT

kl_quinto_ano_lp$Disciplina<-'Leitura'


kl_quinto_ano_mat$Disciplina<-'Matematica'

kl_quinto<-rbind(kl_quinto_ano_lp,kl_quinto_ano_mat,use.names=FALSE)
colnames(kl_quinto)[2]<-'KL.Nivel'


levels(kl_quinto$Faixa_IDEB)

kl_quinto$Faixa_IDEB<-factor(kl_quinto$Faixa_IDEB,
                                     levels=c( "Baixo","Medio_Baixo","Medio","Medio_Alto","Alto"))

ggplot(kl_quinto[!is.na(Faixa_IDEB)],aes(x=KL.Nivel,col=Faixa_IDEB))+geom_density(size=1.5)

ggplot(kl_quinto[!is.na(Faixa_IDEB)],aes(y=KL.Nivel,col=Faixa_IDEB))+geom_boxplot()



quartis<-kl_quinto[!is.na(Faixa_IDEB),.(Quartil_1=quantile(KL.Nivel,na.rm=T)[2],
                                                    Quartil_3=quantile(KL.Nivel,na.rm=T)[4]),by=Faixa_IDEB][order(Quartil_1)]


# Salva resultados

quartis_lp$Ano<-'2017'
quartis_mat$Ano<-'2017'

(quartis_lp$Quartil_3+quartis_mat$Quartil_3)/2

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Atualizacao Faixas\\Proposta Chico\\Resultados')


#save(quartis_lp,quartis_mat,file='Quartis_2017.Rdata')

# Colocar as proporcoes em cada grupo de nivel de aprendizado

#- Verificar como ficaria usando 2017 a 2013
# verificar se muda quando mudo a janela


#Niveis

# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)

# Carrega dados completos

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Modelo Imputacao/dados_tudo.Rdata")

dados_tudo[ID_SERIE==5,'Nivel_Matematica']<-cut(dados_tudo[ID_SERIE==5,Matematica],breaks=limites_quinto_ano_Matematica,include.lowest = T,
                                                right=F)


levels(dados_tudo$Nivel_Matematica)<-c(1,2,3)



dados_tudo[ID_SERIE==5,'Nivel_Leitura']<-cut(dados_tudo[ID_SERIE==5,Leitura],breaks=limites_quinto_ano_Leitura,include.lowest = T,
                                             right=F)

levels(dados_tudo$Nivel_Leitura)<-c(1,2,3)



# Proporcao no AB e Adequado

proporcao_niveis_leitura<-dados_tudo[ID_SERIE==5 & (Ano %in% c(2013,2015,2017)),
                                     .(prop_abaixo_basico=prop.table(table(Nivel_Leitura))[1],
                                       prop_basico=prop.table(table(Nivel_Leitura))[2],
                                       prop_adequado_avancado=prop.table(table(Nivel_Leitura))[3]),
                                     by=COD_MUNICIPIO]


proporcao_niveis_matematica<-dados_tudo[ID_SERIE==5 & (Ano %in% c(2013,2015,2017)),
                                        .(prop_abaixo_basico=prop.table(table(Nivel_Matematica))[1],
                                          prop_basico=prop.table(table(Nivel_Matematica))[2],
                                          prop_adequado_avancado=prop.table(table(Nivel_Matematica))[3]),
                                        by=COD_MUNICIPIO]



# Grupo 1 LP
# KL < -1.68

quartis_lp
kl_quinto_ano_lp<-kl_quinto_ano_lp[COD_MUNICIPIO<6000000]

kl_quinto_ano_lp[KL.Leitura< -1.68,COD_MUNICIPIO]

library(tidyr)

quartis_lp$Quartil_3[1]
prop_long<-proporcao_niveis_leitura[COD_MUNICIPIO %in% kl_quinto_ano_lp[KL.Leitura< quartis_lp$Quartil_3[1],COD_MUNICIPIO]]


prop_long<-gather(prop_long, nivel_aprendizado, proporcao,prop_abaixo_basico: prop_adequado_avancado)


ggplot(prop_long, aes(y=proporcao,col=nivel_aprendizado))+geom_boxplot()

quartis_lp

## Classificacao KL

quartis

## LP
proporcao_niveis_leitura[COD_MUNICIPIO %in% kl_quinto_ano_lp[KL.Leitura< quartis$Quartil_3[1],COD_MUNICIPIO],'Grupo_KL']<-'Baixa'
proporcao_niveis_leitura[COD_MUNICIPIO %in% kl_quinto_ano_lp[KL.Leitura> quartis$Quartil_3[1] & KL.Leitura< quartis$Quartil_3[2],COD_MUNICIPIO],'Grupo_KL']<-'Media_Baixa'
proporcao_niveis_leitura[COD_MUNICIPIO %in% kl_quinto_ano_lp[KL.Leitura> quartis$Quartil_3[2] & KL.Leitura< quartis$Quartil_3[3],COD_MUNICIPIO],'Grupo_KL']<-'Media'
proporcao_niveis_leitura[COD_MUNICIPIO %in% kl_quinto_ano_lp[KL.Leitura> quartis$Quartil_3[3] & KL.Leitura< quartis$Quartil_3[4],COD_MUNICIPIO],'Grupo_KL']<-'Media_Alta'
proporcao_niveis_leitura[COD_MUNICIPIO %in% kl_quinto_ano_lp[KL.Leitura> quartis$Quartil_3[4] ,COD_MUNICIPIO],'Grupo_KL']<-'Alta'


proporcao_niveis_leitura_long<-gather(proporcao_niveis_leitura[!is.na(Grupo_KL)],nivel_aprendizado, proporcao,prop_abaixo_basico: prop_adequado_avancado)
proporcao_niveis_leitura_long$Grupo_KL<-factor(proporcao_niveis_leitura_long$Grupo_KL)
levels(proporcao_niveis_leitura_long$Grupo_KL)

proporcao_niveis_leitura_long$Grupo_KL<-factor(proporcao_niveis_leitura_long$Grupo_KL,
                                               levels=c('Baixa','Media_Baixa','Media','Media_Alta','Alta'))

levels(proporcao_niveis_leitura_long$Grupo_KL)


proporcao_niveis_leitura_long$nivel_aprendizado<-factor(proporcao_niveis_leitura_long$nivel_aprendizado)
levels(proporcao_niveis_leitura_long$nivel_aprendizado)

proporcao_niveis_leitura_long$nivel_aprendizado<-factor(proporcao_niveis_leitura_long$nivel_aprendizado,
                                                        levels=c("prop_abaixo_basico" ,
                                                                 "prop_basico","prop_adequado_avancado"))


levels(proporcao_niveis_leitura_long$nivel_aprendizado)
ggplot(proporcao_niveis_leitura_long, aes(y=proporcao,col=nivel_aprendizado))+geom_boxplot()+facet_grid(.~Grupo_KL)


## Mat
proporcao_niveis_matematica[COD_MUNICIPIO %in% kl_quinto_ano_mat[KL.Matematica< quartis$Quartil_3[1],COD_MUNICIPIO],'Grupo_KL_novo']<-'Baixa'
proporcao_niveis_matematica[COD_MUNICIPIO %in% kl_quinto_ano_mat[KL.Matematica> quartis$Quartil_3[1] & KL.Matematica< quartis$Quartil_3[2],COD_MUNICIPIO],'Grupo_KL_novo']<-'Media_Baixa'
proporcao_niveis_matematica[COD_MUNICIPIO %in% kl_quinto_ano_mat[KL.Matematica> quartis$Quartil_3[2] & KL.Matematica< quartis$Quartil_3[3],COD_MUNICIPIO],'Grupo_KL_novo']<-'Media'
proporcao_niveis_matematica[COD_MUNICIPIO %in% kl_quinto_ano_mat[KL.Matematica> quartis$Quartil_3[3] & KL.Matematica< quartis$Quartil_3[4],COD_MUNICIPIO],'Grupo_KL_novo']<-'Media_Alta'
proporcao_niveis_matematica[COD_MUNICIPIO %in% kl_quinto_ano_mat[KL.Matematica> quartis$Quartil_3[4] ,COD_MUNICIPIO],'Grupo_KL_novo']<-'Alta'


proporcao_niveis_matematica_long<-gather(proporcao_niveis_matematica[!is.na(Grupo_KL_novo)],nivel_aprendizado, proporcao,prop_abaixo_basico: prop_adequado_avancado)
proporcao_niveis_matematica_long$Grupo_KL_novo<-factor(proporcao_niveis_matematica_long$Grupo_KL_novo)
levels(proporcao_niveis_matematica_long$Grupo_KL_novo)

proporcao_niveis_matematica_long$Grupo_KL_novo<-factor(proporcao_niveis_matematica_long$Grupo_KL_novo,
                                                       levels=c('Baixa','Media_Baixa','Media','Media_Alta','Alta'))

levels(proporcao_niveis_matematica_long$Grupo_KL_novo)


proporcao_niveis_matematica_long$nivel_aprendizado<-factor(proporcao_niveis_matematica_long$nivel_aprendizado)
levels(proporcao_niveis_matematica_long$nivel_aprendizado)

proporcao_niveis_matematica_long$nivel_aprendizado<-factor(proporcao_niveis_matematica_long$nivel_aprendizado,
                                                           levels=c("prop_abaixo_basico" ,
                                                                    "prop_basico","prop_adequado_avancado"))


levels(proporcao_niveis_matematica_long$nivel_aprendizado)
ggplot(proporcao_niveis_matematica_long, aes(y=proporcao,col=nivel_aprendizado))+geom_boxplot()+facet_grid(.~Grupo_KL_novo)


# Salva pontos de corte

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Processa Base Final\\Pontos de corte faixas')


save(quartis,file='Pontos_corte_nivel_quinto_mat_lp.Rdata')
