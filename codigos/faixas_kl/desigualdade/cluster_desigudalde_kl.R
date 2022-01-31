require(sfsmisc) # para as integrais
require(reshape2)
require(ggplot2)
require(tidyr)
require(data.table)

load("C:/Users/Erica/Dropbox/UFOP/Grupo NAVE/Trabalho Chico Soare/Atualizacao_2017/Atualizacao Faixas/Faixas Desigualdade/Limites igualdade/Dados_completos.Rdata")

##################################
# Quinto ano
##################################


#################################################################
# Cluster via KL
###############################################################



##############################################################
## Mat - NSE
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_quinto_ano[abs(Desigualdade.Matematica_NSE)< -corte_nse_mat_quinto,
                               'Classificacao_desigualdade_NSE_mat']<-'Igualdade'

resumo_municipios_quinto_ano[Desigualdade.Matematica_NSE> -corte_nse_mat_quinto,
                             'Classificacao_desigualdade_NSE_mat']<-'Casos atípicos'


resumo_municipios_quinto_ano[Classificacao_desigualdade_NSE_mat=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                               c('Igualdade','Casos atípicos')),'Desigualdade.Matematica_NSE']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_NSE_mat']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_mat<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_mat)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_mat)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_NSE)),
                             by=Classificacao_desigualdade_NSE_mat][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_NSE)),
                             by=Classificacao_desigualdade_NSE_mat][order(V1),][,Classificacao_desigualdade_NSE_mat]



resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_mat<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_mat,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_mat)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Matematica_NSE,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_NSE,x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()

ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_matematica_NSE,x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_matematica_NSE,x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()




## Razao de chance

#Niveis

# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_mat_5ano_nse[,'Nivel_Matematica']<-cut(dados_mat_5ano_nse[,Matematica],breaks=limites_quinto_ano_Matematica,include.lowest = T,
                                             right=F)



levels(dados_mat_5ano_nse$Nivel_Matematica)<-c(1,2,3)



log_razao_chance_NSE_niveis_matematica<-dados_mat_5ano_nse[,
                                                           .(prop_abaixo_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[1])-
                                                               log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[1]),
                                                             prop_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[2])-
                                                               log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[2]),
                                                             prop_adequado_avancado_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[3])-
                                                               log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[3])),
                                                           by=COD_MUNICIPIO]


resumo_municipios_quinto_ano<-merge(resumo_municipios_quinto_ano,log_razao_chance_NSE_niveis_matematica,all.x=T)



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()




## Pega as faixas

medianas_clusters<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                 c('Igualdade','Casos atípicos')),median(Desigualdade.Matematica_NSE),
                             by=Classificacao_desigualdade_NSE_mat]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_NSE_mat ]

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                 c('Igualdade','Casos atípicos')),quantile(Desigualdade.Matematica_NSE)]
                             
quartis_cluster<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                 c('Igualdade','Casos atípicos')),
                             quantile(Desigualdade.Matematica_NSE),
                             by=Classificacao_desigualdade_NSE_mat]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)

quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[1] &
                  Quartil=='75%']

quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] &
                  Quartil=='25%']


ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[1] &
                       Quartil=='75%',V1],
    quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] &
                       Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_mat_nse_quinto<-c(ponto_corte_1,ponto_corte_2)


##############################################################
## LP - NSE
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_quinto_ano[abs(Desigualdade.Leitura_NSE)< -corte_nse_lp_quinto,
                             'Classificacao_desigualdade_NSE_lp']<-'Igualdade'

resumo_municipios_quinto_ano[Desigualdade.Leitura_NSE> -corte_nse_lp_quinto,
                             'Classificacao_desigualdade_NSE_lp']<-'Casos atípicos'


resumo_municipios_quinto_ano[Classificacao_desigualdade_NSE_lp=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Leitura_NSE']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_NSE_lp']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_lp<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_lp)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_lp)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_NSE)),
                             by=Classificacao_desigualdade_NSE_lp][order(V1)]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_NSE)),
                                  by=Classificacao_desigualdade_NSE_lp][order(V1),Classificacao_desigualdade_NSE_lp]



resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_lp<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_lp,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_NSE_lp)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Leitura_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()

ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_portugues_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(y=Diferenca_portugues_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Leitura_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Leitura_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_portugues_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()




## Razao de chance

#Niveis

# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_lp_5ano_nse[,'Nivel_Leitura']<-cut(dados_lp_5ano_nse[,Leitura],breaks=limites_quinto_ano_Leitura,include.lowest = T,
                                             right=F)



levels(dados_lp_5ano_nse$Nivel_Leitura)<-c(1,2,3)



log_razao_chance_NSE_niveis_leitura<-dados_lp_5ano_nse[,
                                                           .(prop_abaixo_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[1])-
                                                               log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[1]),
                                                             prop_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[2])-
                                                               log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[2]),
                                                             prop_adequado_avancado_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[3])-
                                                               log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[3])),
                                                           by=COD_MUNICIPIO]


resumo_municipios_quinto_ano<-merge(resumo_municipios_quinto_ano,log_razao_chance_NSE_niveis_leitura,all.x=T)



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()




## Pega as faixas

medianas_clusters<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                                    c('Igualdade','Casos atípicos')),
                                                median(Desigualdade.Leitura_NSE),
                                                by=Classificacao_desigualdade_NSE_lp]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_NSE_lp ]


quartis_cluster<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Leitura_NSE),
                                              by=Classificacao_desigualdade_NSE_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)

ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_lp_nse_quinto<-c(ponto_corte_1,ponto_corte_2)


##############################################################
## Mat - Raça
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_quinto_ano[abs(Desigualdade.Matematica_raca)< -corte_nse_mat_quinto,
                             'Classificacao_desigualdade_raca_mat']<-'Igualdade'

resumo_municipios_quinto_ano[Desigualdade.Matematica_raca> -corte_nse_mat_quinto,
                             'Classificacao_desigualdade_raca_mat']<-'Casos atípicos'


resumo_municipios_quinto_ano[Classificacao_desigualdade_raca_mat=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Matematica_raca']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_raca_mat']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_mat<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_mat)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_mat)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_raca)),
                             by=Classificacao_desigualdade_raca_mat][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Matematica_raca)),
                                  by=Classificacao_desigualdade_raca_mat][order(V1),][,Classificacao_desigualdade_raca_mat]



resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_mat<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_mat,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_mat)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Matematica_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_raca,x=Classificacao_desigualdade_raca_mat))+geom_boxplot()

ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_matematica_raca,x=Classificacao_desigualdade_raca_mat))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_matematica_raca,x=Classificacao_desigualdade_raca_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()


# Razao de chance

dados_mat_5ano_raca[,'Nivel_Matematica']<-cut(dados_mat_5ano_raca[,Matematica],breaks=limites_quinto_ano_Matematica,include.lowest = T,
                                             right=F)



levels(dados_mat_5ano_raca$Nivel_Matematica)<-c(1,2,3)



log_razao_chance_raca_niveis_matematica<-dados_mat_5ano_raca[,
                                                           .(prop_abaixo_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[1])-
                                                               log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[1]),
                                                             prop_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[2])-
                                                               log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[2]),
                                                             prop_adequado_avancado_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[3])-
                                                               log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[3])),
                                                           by=COD_MUNICIPIO]


resumo_municipios_quinto_ano<-merge(resumo_municipios_quinto_ano,
                                    log_razao_chance_raca_niveis_matematica,all.x=T)



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_mat_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_mat_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_mat_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_raca,
                                                                              x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_raca,
                                                                              x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_raca,
                                                                              x=Classificacao_desigualdade_raca_mat))+geom_boxplot()






## Pega as faixas

medianas_clusters<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                                    c('Igualdade','Casos atípicos')),
                                                median(Desigualdade.Matematica_raca),
                                                by=Classificacao_desigualdade_raca_mat]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_raca_mat ]


quartis_cluster<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_mat %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Matematica_raca),
                                              by=Classificacao_desigualdade_raca_mat]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)



ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_mat_raca_quinto<-c(ponto_corte_1,ponto_corte_2)



##############################################################
## LP - Raça
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_quinto_ano[abs(Desigualdade.Leitura_raca)< -corte_nse_lp_quinto,
                             'Classificacao_desigualdade_raca_lp']<-'Igualdade'

resumo_municipios_quinto_ano[Desigualdade.Leitura_raca> -corte_nse_lp_quinto,
                             'Classificacao_desigualdade_raca_lp']<-'Casos atípicos'


resumo_municipios_quinto_ano[Classificacao_desigualdade_raca_lp=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Leitura_raca']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_raca_lp']<-as.factor(clusters$cluster)


resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_lp<-factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_lp)
levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_lp)

resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_raca)),
                             by=Classificacao_desigualdade_raca_lp][order(V1),]

aux<-resumo_municipios_quinto_ano[,.(median(Desigualdade.Leitura_raca)),
                                  by=Classificacao_desigualdade_raca_lp][order(V1),][,Classificacao_desigualdade_raca_lp]



resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_lp<-
  factor(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_lp,
         labels=aux)

levels(resumo_municipios_quinto_ano$Classificacao_desigualdade_raca_lp)


ggplot(resumo_municipios_quinto_ano,aes(y=Desigualdade.Leitura_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_raca,x=Classificacao_desigualdade_raca_lp))+geom_boxplot()

ggplot(resumo_municipios_quinto_ano,aes(y=Diferenca_leitura_raca,x=Classificacao_desigualdade_raca_lp))+geom_boxplot()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_portugues_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_quinto_ano,
       aes(x=Desigualdade.Leitura_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Leitura_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()


ggplot(resumo_municipios_quinto_ano,
       aes(x=Diferenca_portugues_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()



ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()


# Razao de chance

dados_lp_5ano_raca[,'Nivel_Leitura']<-cut(dados_lp_5ano_raca[,Leitura],breaks=limites_quinto_ano_Leitura,include.lowest = T,
                                              right=F)



levels(dados_lp_5ano_raca$Nivel_Leitura)<-c(1,2,3)



log_razao_chance_raca_niveis_leitura<-dados_lp_5ano_raca[,
                                                             .(prop_abaixo_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[1])-
                                                                 log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[1]),
                                                               prop_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[2])-
                                                                 log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[2]),
                                                               prop_adequado_avancado_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[3])-
                                                                 log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[3])),
                                                             by=COD_MUNICIPIO]


resumo_municipios_quinto_ano<-merge(resumo_municipios_quinto_ano,
                                    log_razao_chance_raca_niveis_leitura,all.x=T)



ggplot(resumo_municipios_quinto_ano,aes(y=prop_abaixo_basico_lp_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_basico_lp_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano,aes(y=prop_adequado_avancado_lp_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()




ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()






## Pega as faixas

medianas_clusters<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                                    c('Igualdade','Casos atípicos')),
                                                median(Desigualdade.Leitura_raca),
                                                by=Classificacao_desigualdade_raca_lp]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_raca_lp ]


quartis_cluster<-resumo_municipios_quinto_ano[!(Classificacao_desigualdade_raca_lp %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Leitura_raca),
                                              by=Classificacao_desigualdade_raca_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)

ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_lp_raca_quinto<-c(ponto_corte_1,ponto_corte_2)


##################################
# Nono ano
##################################


#################################################################
# Cluster via KL
###############################################################



##############################################################
## Mat - NSE
######################################################

# Classifica casos atipicos e igualdade
resumo_municipios_nono_ano[abs(Desigualdade.Matematica_NSE)< -corte_nse_lp_nono,]
                           

resumo_municipios_nono_ano[abs(Desigualdade.Matematica_NSE)< -corte_nse_lp_nono,
                             'Classificacao_desigualdade_NSE_mat']<-'Igualdade'

resumo_municipios_nono_ano[Desigualdade.Matematica_NSE> -corte_nse_mat_nono,
                             'Classificacao_desigualdade_NSE_mat']<-'Casos atípicos'


resumo_municipios_nono_ano[Classificacao_desigualdade_NSE_mat=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Matematica_NSE']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_NSE_mat']<-as.factor(clusters$cluster)


resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_mat<-factor(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_mat)
levels(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_mat)

resumo_municipios_nono_ano[,.(median(Desigualdade.Matematica_NSE)),
                             by=Classificacao_desigualdade_NSE_mat][order(V1),]

aux<-resumo_municipios_nono_ano[,.(median(Desigualdade.Matematica_NSE)),
                                  by=Classificacao_desigualdade_NSE_mat][order(V1),][,Classificacao_desigualdade_NSE_mat]



resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_mat<-
  factor(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_mat,
         labels=aux)

levels(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_mat)


ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Matematica_NSE,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_NSE,x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()

ggplot(resumo_municipios_nono_ano,aes(y=Diferenca_matematica_NSE,x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(y=Diferenca_matematica_NSE,x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_nono_ano,
       aes(x=Desigualdade.Matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()


ggplot(resumo_municipios_nono_ano,
       aes(x=Diferenca_matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_NSE,col=Classificacao_desigualdade_NSE_mat))+geom_density()




## Razao de chance

#Niveis

# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_mat_9ano_nse[,'Nivel_Matematica']<-cut(dados_mat_9ano_nse[,Matematica],breaks=limites_nono_ano_Matematica,include.lowest = T,
                                             right=F)



levels(dados_mat_9ano_nse$Nivel_Matematica)<-c(1,2,3)



log_razao_chance_NSE_niveis_matematica<-dados_mat_9ano_nse[,
                                                           .(prop_abaixo_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[1])-
                                                               log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[1]),
                                                             prop_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[2])-
                                                               log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[2]),
                                                             prop_adequado_avancado_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[3])-
                                                               log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[3])),
                                                           by=COD_MUNICIPIO]


resumo_municipios_nono_ano<-merge(resumo_municipios_nono_ano,log_razao_chance_NSE_niveis_matematica,all.x=T)



ggplot(resumo_municipios_nono_ano,aes(y=prop_abaixo_basico_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_basico_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_adequado_avancado_mat_nse,
                                        x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_nse,
                                                                              x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_nse,
                                                                              x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_nse,
                                                                              x=Classificacao_desigualdade_NSE_mat))+geom_boxplot()




## Pega as faixas

medianas_clusters<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                                    c('Igualdade','Casos atípicos')),median(Desigualdade.Matematica_NSE),
                                                by=Classificacao_desigualdade_NSE_mat]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_NSE_mat ]

quartis_cluster<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_mat %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Matematica_NSE),
                                              by=Classificacao_desigualdade_NSE_mat]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_mat_nse_nono<-c(ponto_corte_1,ponto_corte_2)




##############################################################
## LP - NSE
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_nono_ano[abs(Desigualdade.Leitura_NSE)< -corte_nse_lp_nono,
                             'Classificacao_desigualdade_NSE_lp']<-'Igualdade'

resumo_municipios_nono_ano[Desigualdade.Leitura_NSE> -corte_nse_lp_nono,
                             'Classificacao_desigualdade_NSE_lp']<-'Casos atípicos'


resumo_municipios_nono_ano[Classificacao_desigualdade_NSE_lp=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Leitura_NSE']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_NSE_lp']<-as.factor(clusters$cluster)


resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_lp<-factor(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_lp)
levels(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_lp)

resumo_municipios_nono_ano[,.(median(Desigualdade.Leitura_NSE)),
                             by=Classificacao_desigualdade_NSE_lp][order(V1)]

aux<-resumo_municipios_nono_ano[,.(median(Desigualdade.Leitura_NSE)),
                                  by=Classificacao_desigualdade_NSE_lp][order(V1),Classificacao_desigualdade_NSE_lp]



resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_lp<-
  factor(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_lp,
         labels=aux)

levels(resumo_municipios_nono_ano$Classificacao_desigualdade_NSE_lp)


ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Leitura_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()

ggplot(resumo_municipios_nono_ano,aes(y=Diferenca_portugues_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(y=Diferenca_portugues_NSE,x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_nono_ano,
       aes(x=Desigualdade.Leitura_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Leitura_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()


ggplot(resumo_municipios_nono_ano,
       aes(x=Diferenca_portugues_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_NSE,col=Classificacao_desigualdade_NSE_lp))+geom_density()




## Razao de chance

#Niveis

# Limites prova brasil

limites_quinto_ano_Leitura<-c(0,150,200,10^6)

limites_quinto_ano_Matematica<-c(0,175,225,10^6)

limites_nono_ano_Leitura<-c(0,200,275,10^6)

limites_nono_ano_Matematica<-c(0,225,300,10^6)


dados_lp_9ano_nse[,'Nivel_Leitura']<-cut(dados_lp_9ano_nse[,Leitura],breaks=limites_nono_ano_Leitura,include.lowest = T,
                                         right=F)



levels(dados_lp_9ano_nse$Nivel_Leitura)<-c(1,2,3)



log_razao_chance_NSE_niveis_leitura<-dados_lp_9ano_nse[,
                                                       .(prop_abaixo_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[1])-
                                                           log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[1]),
                                                         prop_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[2])-
                                                           log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[2]),
                                                         prop_adequado_avancado_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[3])-
                                                           log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[3])),
                                                       by=COD_MUNICIPIO]


resumo_municipios_nono_ano<-merge(resumo_municipios_nono_ano,log_razao_chance_NSE_niveis_leitura,all.x=T)



ggplot(resumo_municipios_nono_ano,aes(y=prop_abaixo_basico_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_basico_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_adequado_avancado_lp_nse,
                                        x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                      c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_nse,
                                      x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                      c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_nse,
                                      x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                      c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_nse,
                                      x=Classificacao_desigualdade_NSE_lp))+geom_boxplot()






## Pega as faixas

medianas_clusters<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                                  c('Igualdade','Casos atípicos')),
                                              median(Desigualdade.Leitura_NSE),
                                              by=Classificacao_desigualdade_NSE_lp]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_NSE_lp ]


quartis_cluster<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_NSE_lp %in%
                                                c('Igualdade','Casos atípicos')),
                                            quantile(Desigualdade.Leitura_NSE),
                                            by=Classificacao_desigualdade_NSE_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)

ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_lp_nse_nono<-c(ponto_corte_1,ponto_corte_2)



##############################################################
## Mat - Raça
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_nono_ano[abs(Desigualdade.Matematica_raca)< -corte_nse_mat_nono,
                             'Classificacao_desigualdade_raca_mat']<-'Igualdade'

resumo_municipios_nono_ano[Desigualdade.Matematica_raca> -corte_nse_mat_nono,
                             'Classificacao_desigualdade_raca_mat']<-'Casos atípicos'


resumo_municipios_nono_ano[Classificacao_desigualdade_raca_mat=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Matematica_raca']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_raca_mat']<-as.factor(clusters$cluster)


resumo_municipios_nono_ano$Classificacao_desigualdade_raca_mat<-factor(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_mat)
levels(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_mat)

resumo_municipios_nono_ano[,.(median(Desigualdade.Matematica_raca)),
                             by=Classificacao_desigualdade_raca_mat][order(V1),]

aux<-resumo_municipios_nono_ano[,.(median(Desigualdade.Matematica_raca)),
                                  by=Classificacao_desigualdade_raca_mat][order(V1),][,Classificacao_desigualdade_raca_mat]



resumo_municipios_nono_ano$Classificacao_desigualdade_raca_mat<-
  factor(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_mat,
         labels=aux)

levels(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_mat)


ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Matematica_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Matematica_raca,x=Classificacao_desigualdade_raca_mat))+geom_boxplot()

ggplot(resumo_municipios_nono_ano,aes(y=Diferenca_matematica_raca,x=Classificacao_desigualdade_raca_mat))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_matematica_raca,x=Classificacao_desigualdade_raca_mat))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_nono_ano,
       aes(x=Desigualdade.Matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()


ggplot(resumo_municipios_nono_ano,
       aes(x=Diferenca_matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_matematica_raca,col=Classificacao_desigualdade_raca_mat))+geom_density()


# Razao de chance

dados_mat_9ano_raca[,'Nivel_Matematica']<-cut(dados_mat_9ano_raca[,Matematica],breaks=limites_nono_ano_Matematica,include.lowest = T,
                                              right=F)



levels(dados_mat_9ano_raca$Nivel_Matematica)<-c(1,2,3)



log_razao_chance_raca_niveis_matematica<-dados_mat_9ano_raca[,
                                                             .(prop_abaixo_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[1])-
                                                                 log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[1]),
                                                               prop_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[2])-
                                                                 log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[2]),
                                                               prop_adequado_avancado_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[3])-
                                                                 log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[3])),
                                                             by=COD_MUNICIPIO]


resumo_municipios_nono_ano<-merge(resumo_municipios_nono_ano,
                                    log_razao_chance_raca_niveis_matematica,all.x=T)



ggplot(resumo_municipios_nono_ano,aes(y=prop_abaixo_basico_mat_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_basico_mat_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_adequado_avancado_mat_raca,
                                        x=Classificacao_desigualdade_raca_mat))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_mat_raca,
                                                                              x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_mat_raca,
                                                                              x=Classificacao_desigualdade_raca_mat))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_mat_raca,
                                                                              x=Classificacao_desigualdade_raca_mat))+geom_boxplot()


## Pega as faixas

medianas_clusters<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                                    c('Igualdade','Casos atípicos')),
                                                median(Desigualdade.Matematica_raca),
                                                by=Classificacao_desigualdade_raca_mat]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_raca_mat ]


quartis_cluster<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_mat %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Matematica_raca),
                                              by=Classificacao_desigualdade_raca_mat]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)



ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_mat_raca_nono<-c(ponto_corte_1,ponto_corte_2)




##############################################################
## LP - Raça
######################################################

# Classifica casos atipicos e igualdade


resumo_municipios_nono_ano[abs(Desigualdade.Leitura_raca)< -corte_nse_lp_nono,
                             'Classificacao_desigualdade_raca_lp']<-'Igualdade'

resumo_municipios_nono_ano[Desigualdade.Leitura_raca> -corte_nse_lp_nono,
                             'Classificacao_desigualdade_raca_lp']<-'Casos atípicos'


resumo_municipios_nono_ano[Classificacao_desigualdade_raca_lp=='Casos atípicos']

# K-means pela KL
set.seed(20)

#mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                                 c('Igualdade','Casos atípicos')),'Desigualdade.Leitura_raca']) # standardize variables

clusters <- kmeans(mydata , 3)

resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                 c('Igualdade','Casos atípicos')),
                             'Classificacao_desigualdade_raca_lp']<-as.factor(clusters$cluster)


resumo_municipios_nono_ano$Classificacao_desigualdade_raca_lp<-factor(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_lp)
levels(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_lp)

resumo_municipios_nono_ano[,.(median(Desigualdade.Leitura_raca)),
                             by=Classificacao_desigualdade_raca_lp][order(V1),]

aux<-resumo_municipios_nono_ano[,.(median(Desigualdade.Leitura_raca)),
                                  by=Classificacao_desigualdade_raca_lp][order(V1),][,Classificacao_desigualdade_raca_lp]



resumo_municipios_nono_ano$Classificacao_desigualdade_raca_lp<-
  factor(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_lp,
         labels=aux)

levels(resumo_municipios_nono_ano$Classificacao_desigualdade_raca_lp)


ggplot(resumo_municipios_nono_ano,aes(y=Desigualdade.Leitura_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Desigualdade.Leitura_raca,x=Classificacao_desigualdade_raca_lp))+geom_boxplot()

ggplot(resumo_municipios_nono_ano,aes(y=Diferenca_portugues_raca,x=Classificacao_desigualdade_raca_lp))+geom_boxplot()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=Diferenca_portugues_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()



# Densidades


ggplot(resumo_municipios_nono_ano,
       aes(x=Desigualdade.Leitura_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()


ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Desigualdade.Leitura_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()


ggplot(resumo_municipios_nono_ano,
       aes(x=Diferenca_portugues_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()



ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],
       aes(x=Diferenca_portugues_raca,col=Classificacao_desigualdade_raca_lp))+geom_density()


# Razao de chance

dados_lp_9ano_raca[,'Nivel_Leitura']<-cut(dados_lp_9ano_raca[,Leitura],breaks=limites_nono_ano_Leitura,include.lowest = T,
                                          right=F)



levels(dados_lp_9ano_raca$Nivel_Leitura)<-c(1,2,3)



log_razao_chance_raca_niveis_leitura<-dados_lp_9ano_raca[,
                                                         .(prop_abaixo_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[1])-
                                                             log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[1]),
                                                           prop_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[2])-
                                                             log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[2]),
                                                           prop_adequado_avancado_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[3])-
                                                             log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[3])),
                                                         by=COD_MUNICIPIO]


resumo_municipios_nono_ano<-merge(resumo_municipios_nono_ano,
                                    log_razao_chance_raca_niveis_leitura,all.x=T)



ggplot(resumo_municipios_nono_ano,aes(y=prop_abaixo_basico_lp_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_basico_lp_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano,aes(y=prop_adequado_avancado_lp_raca,
                                        x=Classificacao_desigualdade_raca_lp))+geom_boxplot()




ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_abaixo_basico_lp_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_basico_lp_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()
ggplot(resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                        c('Igualdade','Casos atípicos'))],aes(y=prop_adequado_avancado_lp_raca,
                                                                              x=Classificacao_desigualdade_raca_lp))+geom_boxplot()




## Pega as faixas

medianas_clusters<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                                    c('Igualdade','Casos atípicos')),
                                                median(Desigualdade.Leitura_raca),
                                                by=Classificacao_desigualdade_raca_lp]

clusters_ordenados<-medianas_clusters[order(V1),Classificacao_desigualdade_raca_lp ]


quartis_cluster<-resumo_municipios_nono_ano[!(Classificacao_desigualdade_raca_lp %in%
                                                  c('Igualdade','Casos atípicos')),
                                              quantile(Desigualdade.Leitura_raca),
                                              by=Classificacao_desigualdade_raca_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)

ponto_corte_1<-mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[1] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] &
                                      Quartil=='25%',V1])


ponto_corte_2<-mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] &
                                      Quartil=='75%',V1],
                    quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[3] &
                                      Quartil=='25%',V1])

cortes_lp_raca_nono<-c(ponto_corte_1,ponto_corte_2)
                                                          
                                                             
setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Atualizacao Faixas\\Faixas Desigualdade\\Grupos desigualdade')

save.image('Resultados_cluters.Rdata')
