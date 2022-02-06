require(sfsmisc) # para as integrais
require(reshape2)
require(ggplot2)
require(tidyr)
require(data.table)

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Atualizacao Faixas/Faixas Desigualdade/Limites igualdade/Dados_completos.Rdata")


# Decisao 3 grupos
# Desigualdade baixa, Desigualdade, Desigualdade alta
# Pegar a media entre os quartis como ponto de corte

##################################
# Quinto ano
##################################


#################################################################
# Cluster via KL
###############################################################



##############################################################
## Mat - Sexo
######################################################

# Classifica casos atipicos e igualdade

resumo_municipios_quinto_ano$Desigualdade.Matematica_sexo_novo<- -abs(resumo_municipios_quinto_ano$Desigualdade.Matematica_sexo)



resumo_municipios_quinto_ano[Desigualdade.Matematica_sexo_novo > corte_nse_mat_quinto,
                             'Classificacao_desigualdade_sexo_mat']<-'Igualdade'

resumo_municipios_quinto_ano[Classificacao_desigualdade_sexo_mat=="Igualdade",range(Desigualdade.Matematica_sexo_novo)]
resumo_municipios_quinto_ano[is.na(Classificacao_desigualdade_sexo_mat)]
resumo_municipios_quinto_ano[is.na(Classificacao_desigualdade_sexo_mat),range(Desigualdade.Matematica_sexo_novo)]



range(resumo_municipios_quinto_ano$Desigualdade.Matematica_sexo_novo)
# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                                 c('Igualdade')),
                                             Desigualdade.Matematica_sexo_novo]) # standardize variables

############
## 3 grupos
###########

clusters <- kmeans(mydata , 3)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                 c('Igualdade')),
                             'Classificacao_desigualdade_sexo_mat']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_mat][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_mat][order(V1),Classificacao_desigualdade_sexo_mat]



resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Matematica_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Matematica_sexo_novo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo_novo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo_novo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Matematica_sexo_novo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


# Razao de chance


# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_mat_5ano_sexo[,'Nivel_Matematica']<-cut(dados_mat_5ano_sexo[,Matematica],breaks=limites_quinto_ano_Matematica,include.lowest = T,
                                              right=F)



levels(dados_mat_5ano_sexo$Nivel_Matematica)<-c(1,2,3)



log_razao_chance_sexo_niveis_matematica<-dados_mat_5ano_sexo[,.(prop_abaixo_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[1])-
                                                                 log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[1]),
                                                               prop_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[2])-
                                                                 log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[2]),
                                                               prop_adequado_avancado_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[3])-
                                                                 log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[3])),
                                                             by=COD_MUNICIPIO]


resumo_municipios_quinto_ano<-merge(resumo_municipios_quinto_ano,
                                    log_razao_chance_sexo_niveis_matematica,all.x=T)



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()








## Pega as faixas
## Peguei os valores positivos

medianas_clusters<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                                    c('Igualdade')),median(Desigualdade.Matematica_sexo_novo),
                                                by=Classificacao_desigualdade_sexo_mat]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_sexo_mat]

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                 c('Igualdade')),quantile(Desigualdade.Matematica_sexo_novo)]

quartis_cluster<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Matematica_sexo_novo),
                                              by=Classificacao_desigualdade_sexo_mat]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_mat_sexo_quinto<-c(ponto_corte_1,ponto_corte_2)




############
## 2 grupos
###########

set.seed(3)
clusters <- kmeans(mydata , 2)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                 c('Igualdade')),
                             'Classificacao_desigualdade_sexo_mat']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_mat][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                                  by=Classificacao_desigualdade_sexo_mat][order(V1),Classificacao_desigualdade_sexo_mat]



resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_mat)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Matematica_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Matematica_sexo_novo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo_novo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo_novo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Matematica_sexo_novo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


# Razao de chance


# Limites prova brasil



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




##############################################################
## LP - Sexo
######################################################

# Classifica casos atipicos e igualdade

resumo_municipios_quinto_ano$Desigualdade.Leitura_sexo_novo<- -abs(resumo_municipios_quinto_ano$Desigualdade.Leitura_sexo)



resumo_municipios_quinto_ano[Desigualdade.Leitura_sexo_novo > corte_nse_lp_quinto,
                             'Classificacao_desigualdade_sexo_lp']<-'Igualdade'

resumo_municipios_quinto_ano[Classificacao_desigualdade_sexo_lp=="Igualdade",range(Desigualdade.Leitura_sexo_novo)]
resumo_municipios_quinto_ano[is.na(Classificacao_desigualdade_sexo_lp)]
resumo_municipios_quinto_ano[is.na(Classificacao_desigualdade_sexo_lp),range(Desigualdade.Leitura_sexo_novo)]



range(resumo_municipios_quinto_ano$Desigualdade.Leitura_sexo_novo)
# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                                 c('Igualdade')),
                                             Desigualdade.Leitura_sexo_novo]) # standardize variables

############
## 3 grupos
###########

clusters <- kmeans(mydata , 3)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                 c('Igualdade')),
                             'Classificacao_desigualdade_sexo_lp']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_lp][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                                  by=Classificacao_desigualdade_sexo_lp][order(V1),Classificacao_desigualdade_sexo_lp]



resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Leitura_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Leitura_sexo_novo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo_novo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo_novo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_portugues_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_portugues_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Leitura_sexo_novo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


# Razao de chance


# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_lp_5ano_sexo[,'Nivel_Leitura']<-cut(dados_lp_5ano_sexo[,Leitura],breaks=limites_quinto_ano_Leitura,include.lowest = T,
                                              right=F)



levels(dados_lp_5ano_sexo$Nivel_Leitura)<-c(1,2,3)


log_razao_chance_sexo_niveis_matematica<-dados_lp_5ano_sexo[,.(prop_abaixo_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[1])-
                                                                  log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[1]),
                                                                prop_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[2])-
                                                                  log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[2]),
                                                                prop_adequado_avancado_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[3])-
                                                                  log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[3])),
                                                             by=COD_MUNICIPIO]


resumo_municipios_quinto_ano<-merge(resumo_municipios_quinto_ano,
                                    log_razao_chance_sexo_niveis_matematica,all.x=T)



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()





## Pega as faixas
## Peguei os valores positivos

medianas_clusters<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                                    c('Igualdade')),median(Desigualdade.Leitura_sexo_novo),
                                                by=Classificacao_desigualdade_sexo_lp]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_sexo_lp]

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                 c('Igualdade')),quantile(Desigualdade.Leitura_sexo_novo)]

quartis_cluster<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Leitura_sexo_novo),
                                              by=Classificacao_desigualdade_sexo_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_lp_sexo_quinto<-c(ponto_corte_1,ponto_corte_2)




############
## 2 grupos
###########

set.seed(3)
clusters <- kmeans(mydata , 2)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                 c('Igualdade')),
                             'Classificacao_desigualdade_sexo_lp']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_lp][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                                  by=Classificacao_desigualdade_sexo_lp][order(V1),Classificacao_desigualdade_sexo_lp]



resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_sexo_lp)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Leitura_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Leitura_sexo_novo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo_novo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo_novo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_portugues_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_portugues_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Leitura_sexo_novo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_portugues_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


# Razao de chance


# Limites prova brasil



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



###########################################################################################


##################
## Nono ano
###################



#################################################################
# Cluster via KL
###############################################################



##############################################################
## Mat - Sexo
######################################################

# Classifica casos atipicos e igualdade

resumo_municipios_nono_ano$Desigualdade.Matematica_sexo_novo<- -abs(resumo_municipios_nono_ano$Desigualdade.Matematica_sexo)



resumo_municipios_nono_ano[Desigualdade.Matematica_sexo_novo > corte_nse_mat_nono,
                             'Classificacao_desigualdade_sexo_mat']<-'Igualdade'

resumo_municipios_nono_ano[Classificacao_desigualdade_sexo_mat=="Igualdade",range(Desigualdade.Matematica_sexo_novo)]
resumo_municipios_nono_ano[is.na(Classificacao_desigualdade_sexo_mat)]
resumo_municipios_nono_ano[is.na(Classificacao_desigualdade_sexo_mat),range(Desigualdade.Matematica_sexo_novo)]



range(resumo_municipios_nono_ano$Desigualdade.Matematica_sexo_novo)
# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                                 c('Igualdade')),
                                             Desigualdade.Matematica_sexo_novo]) # standardize variables

############
## 3 grupos
###########

clusters <- kmeans(mydata , 3)

resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                 c('Igualdade')),
                             'Classificacao_desigualdade_sexo_mat']<-as.factor(clusters$cluster)


resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_mat<-factor(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_mat)
levels(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_mat)

resumo_municipios_nono_ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_mat][order(V1),]

aux<-resumo_municipios_nono_ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                                  by=Classificacao_desigualdade_sexo_mat][order(V1),Classificacao_desigualdade_sexo_mat]



resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_mat<-
  factor(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_mat,
         labels=aux)

levels(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_mat)


ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Matematica_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Matematica_sexo_novo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo_novo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_sexo_novo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_nono_ano,aes(y=Diferenca_matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_matematica_sexo,x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_nono_ano,
       aes(x=Desigualdade.Matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Matematica_sexo_novo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


ggplot(resumo_municipios_nono_ano,
       aes(x=Diferenca_matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_sexo,col=Classificacao_desigualdade_sexo_mat))+geom_density()


# Razao de chance


# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_mat_9ano_sexo[,'Nivel_Matematica']<-cut(dados_mat_9ano_sexo[,Matematica],breaks=limites_nono_ano_Matematica,include.lowest = T,
                                              right=F)



levels(dados_mat_9ano_sexo$Nivel_Matematica)<-c(1,2,3)



log_razao_chance_sexo_niveis_matematica<-dados_mat_9ano_sexo[,.(prop_abaixo_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[1])-
                                                                  log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[1]),
                                                                prop_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[2])-
                                                                  log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[2]),
                                                                prop_adequado_avancado_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[3])-
                                                                  log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[3])),
                                                             by=COD_MUNICIPIO]


resumo_municipios_nono_ano<-merge(resumo_municipios_nono_ano,
                                    log_razao_chance_sexo_niveis_matematica,all.x=T)



ggplot(resumo_municipios_nono_ano,aes(y=prop_abaixo_basico_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_basico_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_adequado_avancado_mat_sexo,
                                        x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_sexo,
                                                                              x=Classificacao_desigualdade_sexo_mat))+geom_boxplot()








## Pega as faixas
## Peguei os valores positivos

medianas_clusters<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                                    c('Igualdade')),median(Desigualdade.Matematica_sexo_novo),
                                                by=Classificacao_desigualdade_sexo_mat]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_sexo_mat]

resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                 c('Igualdade')),quantile(Desigualdade.Matematica_sexo_novo)]

quartis_cluster<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_mat %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Matematica_sexo_novo),
                                              by=Classificacao_desigualdade_sexo_mat]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_mat_sexo_nono<-c(ponto_corte_1,ponto_corte_2)






##############################################################
## LP - Sexo
######################################################

# Classifica casos atipicos e igualdade

resumo_municipios_nono_ano$Desigualdade.Leitura_sexo_novo<- -abs(resumo_municipios_nono_ano$Desigualdade.Leitura_sexo)



resumo_municipios_nono_ano[Desigualdade.Leitura_sexo_novo > corte_nse_lp_nono,
                             'Classificacao_desigualdade_sexo_lp']<-'Igualdade'

resumo_municipios_nono_ano[Classificacao_desigualdade_sexo_lp=="Igualdade",range(Desigualdade.Leitura_sexo_novo)]
resumo_municipios_nono_ano[is.na(Classificacao_desigualdade_sexo_lp)]
resumo_municipios_nono_ano[is.na(Classificacao_desigualdade_sexo_lp),range(Desigualdade.Leitura_sexo_novo)]



range(resumo_municipios_nono_ano$Desigualdade.Leitura_sexo_novo)
# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                                 c('Igualdade')),
                                             Desigualdade.Leitura_sexo_novo]) # standardize variables

############
## 3 grupos
###########

clusters <- kmeans(mydata , 3)

resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                 c('Igualdade')),
                             'Classificacao_desigualdade_sexo_lp']<-as.factor(clusters$cluster)


resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_lp<-factor(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_lp)
levels(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_lp)

resumo_municipios_nono_ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                             by=Classificacao_desigualdade_sexo_lp][order(V1),]

aux<-resumo_municipios_nono_ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                                  by=Classificacao_desigualdade_sexo_lp][order(V1),Classificacao_desigualdade_sexo_lp]



resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_lp<-
  factor(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_lp,
         labels=aux)

levels(resumo_municipios_nono_ano$Classificacao_desigualdade_sexo_lp)


ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Leitura_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Leitura_sexo_novo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo_novo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_sexo_novo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_nono_ano,aes(y=Diferenca_portugues_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_portugues_sexo,x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_nono_ano,
       aes(x=Desigualdade.Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Leitura_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade'))],
       aes(x=Desigualdade.Leitura_sexo_novo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


ggplot(resumo_municipios_nono_ano,
       aes(x=Diferenca_portugues_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_sexo,col=Classificacao_desigualdade_sexo_lp))+geom_density()


# Razao de chance


# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_lp_9ano_sexo[,'Nivel_Leitura']<-cut(dados_lp_9ano_sexo[,Leitura],breaks=limites_nono_ano_Leitura,include.lowest = T,
                                          right=F)



levels(dados_lp_9ano_sexo$Nivel_Leitura)<-c(1,2,3)


log_razao_chance_sexo_niveis_matematica<-dados_lp_9ano_sexo[,.(prop_abaixo_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[1])-
                                                                 log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[1]),
                                                               prop_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[2])-
                                                                 log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[2]),
                                                               prop_adequado_avancado_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[3])-
                                                                 log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[3])),
                                                            by=COD_MUNICIPIO]


resumo_municipios_nono_ano<-merge(resumo_municipios_nono_ano,
                                    log_razao_chance_sexo_niveis_matematica,all.x=T)



ggplot(resumo_municipios_nono_ano,aes(y=prop_abaixo_basico_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_basico_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_adequado_avancado_lp_sexo,
                                        x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_sexo,
                                                                              x=Classificacao_desigualdade_sexo_lp))+geom_boxplot()





## Pega as faixas
## Peguei os valores positivos

medianas_clusters<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                                    c('Igualdade')),median(Desigualdade.Leitura_sexo_novo),
                                                by=Classificacao_desigualdade_sexo_lp]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_sexo_lp]

resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                 c('Igualdade')),quantile(Desigualdade.Leitura_sexo_novo)]

quartis_cluster<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_sexo_lp %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Leitura_sexo_novo),
                                              by=Classificacao_desigualdade_sexo_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_lp_sexo_nono<-c(ponto_corte_1,ponto_corte_2)


setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Atualizacao Faixas\\Faixas Desigualdade\\Grupos desigualdade')

save.image('Resultados_cluters_sexo.Rdata')

