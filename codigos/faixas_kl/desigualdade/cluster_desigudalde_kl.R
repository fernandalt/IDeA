rm(list=ls(all=T))

## Pacotes
require(data.table)

## Base de dados
wd = getwd()
path = dirname(dirname(dirname(wd)))
load(paste0(path,"/dados/raw/DadosProvaBrasil_2007_2017.Rdata"))

colnames(dados_tudo)[4] = 'ID_MUNICIPIO'
dados_tudo = dados_tudo[ID_MUNICIPIO<6000000]

# Pontos de corte 
load(paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_equidade_5ano.Rdata"))
load(paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_equidade_9ano.Rdata"))

# Dados para razao de chance
load(paste0(path,"/dados/faixas_kl/desigualdade/resumo_5ano.Rdata"))
load(paste0(path,"/dados/faixas_kl/desigualdade/resumo_9ano.Rdata"))

# Limites prova brasil
limites_quinto_ano_Leitura = c(0,150,200,10^6)
limites_quinto_ano_Matematica = c(0,175,225,10^6)
limites_nono_ano_Leitura = c(0,200,275,10^6)
limites_nono_ano_Matematica = c(0,225,300,10^6)

# 5 ano
# Cluster NSE via KL
# MAT
# Classifica casos atipicos e equidade
resumo_5ano[abs(Desigualdade.Matematica_NSE)< (-corte_nse_mat_quinto),
            'Classificacao_desigualdade_NSE_mat'] = 'Equidade'

resumo_5ano[Desigualdade.Matematica_NSE> (-corte_nse_mat_quinto),
            'Classificacao_desigualdade_NSE_mat'] = 'Situações atípicas'

set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_5ano[!(Classificacao_desigualdade_NSE_mat %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Matematica_NSE']) 

clusters = kmeans(df_aux_kl, 3)

resumo_5ano[!(Classificacao_desigualdade_NSE_mat %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_NSE_mat'] = as.factor(clusters$cluster)

resumo_5ano$Classificacao_desigualdade_NSE_mat = factor(resumo_5ano$Classificacao_desigualdade_NSE_mat)

aux = resumo_5ano[,.(median(Desigualdade.Matematica_NSE)),
                  by=Classificacao_desigualdade_NSE_mat][order(V1),][,Classificacao_desigualdade_NSE_mat]

resumo_5ano$Classificacao_desigualdade_NSE_mat = factor(resumo_5ano$Classificacao_desigualdade_NSE_mat,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_5ano[!(Classificacao_desigualdade_NSE_mat %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Matematica_NSE),
                                by=Classificacao_desigualdade_NSE_mat]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_NSE_mat ]

quartis_cluster = resumo_5ano[!(Classificacao_desigualdade_NSE_mat %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Matematica_NSE),
                              by=Classificacao_desigualdade_NSE_mat]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_mat_nse_quinto = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_nse_mat5[,'Nivel_Matematica'] = cut(dados_nse_mat5[,Matematica],breaks=limites_quinto_ano_Matematica,
                                          include.lowest = T, right=F)

levels(dados_nse_mat5$Nivel_Matematica) = c(1,2,3)

log_razao_chance_NSE_niveis_matematica = dados_nse_mat5[, .(prop_abaixo_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[1])-
                                                              log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[1]),
                                                            prop_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[2])-
                                                              log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[2]),
                                                            prop_adequado_avancado_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[3])-
                                                              log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[3])),
                                                        by=ID_MUNICIPIO]

resumo_5ano = merge(resumo_5ano,log_razao_chance_NSE_niveis_matematica,all.x=T)

# LP
# Classifica equidade e situacoes atipicas
resumo_5ano[abs(Desigualdade.Leitura_NSE)< (-corte_nse_lp_quinto),
            'Classificacao_desigualdade_NSE_lp'] = 'Equidade'

resumo_5ano[Desigualdade.Leitura_NSE> (-corte_nse_lp_quinto),
            'Classificacao_desigualdade_NSE_lp'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_5ano[!(Classificacao_desigualdade_NSE_lp %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Leitura_NSE']) 

clusters = kmeans(df_aux_kl, 3)

resumo_5ano[!(Classificacao_desigualdade_NSE_lp %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_NSE_lp'] = as.factor(clusters$cluster)

resumo_5ano$Classificacao_desigualdade_NSE_lp = factor(resumo_5ano$Classificacao_desigualdade_NSE_lp)

aux = resumo_5ano[,.(median(Desigualdade.Leitura_NSE)),
                  by=Classificacao_desigualdade_NSE_lp][order(V1),Classificacao_desigualdade_NSE_lp]

resumo_5ano$Classificacao_desigualdade_NSE_lp = factor(resumo_5ano$Classificacao_desigualdade_NSE_lp,
                                                       labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_5ano[!(Classificacao_desigualdade_NSE_lp %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Leitura_NSE),
                                by=Classificacao_desigualdade_NSE_lp]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_NSE_lp ]

quartis_cluster = resumo_5ano[!(Classificacao_desigualdade_NSE_lp %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Leitura_NSE),
                              by=Classificacao_desigualdade_NSE_lp]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)

ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_lp_nse_quinto = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_nse_lp5[,'Nivel_Leitura'] = cut(dados_nse_lp5[,Leitura],breaks=limites_quinto_ano_Leitura,
                                          include.lowest = T,right=F)

levels(dados_nse_lp5$Nivel_Leitura) = c(1,2,3)

log_razao_chance_NSE_niveis_leitura = dados_nse_lp5[, .(prop_abaixo_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[1])-
                                                              log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[1]),
                                                            prop_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[2])-
                                                              log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[2]),
                                                            prop_adequado_avancado_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[3])-
                                                              log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[3])),
                                                        by=ID_MUNICIPIO]


resumo_5ano = merge(resumo_5ano,log_razao_chance_NSE_niveis_leitura,all.x=T)


# Raca
# MAT
# Classifica equidade e situacoes atipicas
resumo_5ano[abs(Desigualdade.Matematica_raca)< (-corte_nse_mat_quinto),
            'Classificacao_desigualdade_raca_mat'] = 'Equidade'

resumo_5ano[Desigualdade.Matematica_raca> (-corte_nse_mat_quinto),
            'Classificacao_desigualdade_raca_mat'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_5ano[!(Classificacao_desigualdade_raca_mat %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Matematica_raca'])

clusters = kmeans(df_aux_kl, 3)

resumo_5ano[!(Classificacao_desigualdade_raca_mat %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_raca_mat'] = as.factor(clusters$cluster)

resumo_5ano$Classificacao_desigualdade_raca_mat = factor(resumo_5ano$Classificacao_desigualdade_raca_mat)

aux = resumo_5ano[,.(median(Desigualdade.Matematica_raca)),
                  by=Classificacao_desigualdade_raca_mat][order(V1),][,Classificacao_desigualdade_raca_mat]

resumo_5ano$Classificacao_desigualdade_raca_mat = factor(resumo_5ano$Classificacao_desigualdade_raca_mat,
                                                         labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_5ano[!(Classificacao_desigualdade_raca_mat %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Matematica_raca),
                                by=Classificacao_desigualdade_raca_mat]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_raca_mat ]

quartis_cluster = resumo_5ano[!(Classificacao_desigualdade_raca_mat %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Matematica_raca),
                              by=Classificacao_desigualdade_raca_mat]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_mat_raca_quinto = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_raca_mat5[,'Nivel_Matematica'] = cut(dados_raca_mat5[,Matematica],breaks=limites_quinto_ano_Matematica,
                                           include.lowest = T, right=F)

levels(dados_raca_mat5$Nivel_Matematica) = c(1,2,3)

log_razao_chance_raca_niveis_matematica = dados_raca_mat5[, .(prop_abaixo_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[1])-
                                                                log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[1]),
                                                              prop_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[2])-
                                                                log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[2]),
                                                              prop_adequado_avancado_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[3])-
                                                                log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[3])),
                                                          by=ID_MUNICIPIO]

resumo_5ano = merge(resumo_5ano,log_razao_chance_raca_niveis_matematica,all.x=T)

# LP
# Classifica equidade e situacoes atipicas
resumo_5ano[abs(Desigualdade.Leitura_raca)< (-corte_nse_lp_quinto),
            'Classificacao_desigualdade_raca_lp'] = 'Equidade'

resumo_5ano[Desigualdade.Leitura_raca> (-corte_nse_lp_quinto),
            'Classificacao_desigualdade_raca_lp'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_5ano[!(Classificacao_desigualdade_raca_lp %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Leitura_raca'])

clusters = kmeans(df_aux_kl, 3)

resumo_5ano[!(Classificacao_desigualdade_raca_lp %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_raca_lp'] = as.factor(clusters$cluster)

resumo_5ano$Classificacao_desigualdade_raca_lp = factor(resumo_5ano$Classificacao_desigualdade_raca_lp)

aux = resumo_5ano[,.(median(Desigualdade.Leitura_raca)),
                  by=Classificacao_desigualdade_raca_lp][order(V1),][,Classificacao_desigualdade_raca_lp]

resumo_5ano$Classificacao_desigualdade_raca_lp = factor(resumo_5ano$Classificacao_desigualdade_raca_lp,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_5ano[!(Classificacao_desigualdade_raca_lp %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Leitura_raca),
                                by=Classificacao_desigualdade_raca_lp]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_raca_lp ]

quartis_cluster = resumo_5ano[!(Classificacao_desigualdade_raca_lp %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Leitura_raca),
                              by=Classificacao_desigualdade_raca_lp]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_lp_raca_quinto = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_raca_lp5[,'Nivel_Leitura'] = cut(dados_raca_lp5[,Leitura],breaks=limites_quinto_ano_Leitura,
                                       include.lowest = T, right=F)

levels(dados_raca_lp5$Nivel_Leitura) = c(1,2,3)

log_razao_chance_raca_niveis_leitura = dados_raca_lp5[, .(prop_abaixo_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[1])-
                                                            log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[1]),
                                                          prop_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[2])-
                                                            log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[2]),
                                                          prop_adequado_avancado_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[3])-
                                                            log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[3])),
                                                      by=ID_MUNICIPIO]

resumo_5ano = merge(resumo_5ano, log_razao_chance_raca_niveis_leitura,all.x=T)

# Salva pontos de corte desigualdades
save(cortes_lp_nse_quinto, cortes_mat_nse_quinto, cortes_lp_raca_quinto,cortes_mat_raca_quinto,
     file=paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_desigualdade_5ano.Rdata"))


# 9 ano
# Cluster NSE via KL
# MAT
# Classifica equidade e situacoes atipicas
resumo_9ano[abs(Desigualdade.Matematica_NSE)< (-corte_nse_mat_nono),
            'Classificacao_desigualdade_NSE_mat'] = 'Equidade'

resumo_9ano[Desigualdade.Matematica_NSE> (-corte_nse_mat_nono),
            'Classificacao_desigualdade_NSE_mat'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_9ano[!(Classificacao_desigualdade_NSE_mat %in%
                                 c('Equidade','Situações atípicas')),
                             'Desigualdade.Matematica_NSE'])

clusters = kmeans(df_aux_kl , 3)

resumo_9ano[!(Classificacao_desigualdade_NSE_mat %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_NSE_mat'] = as.factor(clusters$cluster)

resumo_9ano$Classificacao_desigualdade_NSE_mat = factor(resumo_9ano$Classificacao_desigualdade_NSE_mat)

aux = resumo_9ano[,.(median(Desigualdade.Matematica_NSE)),
                  by=Classificacao_desigualdade_NSE_mat][order(V1),][,Classificacao_desigualdade_NSE_mat]

resumo_9ano$Classificacao_desigualdade_NSE_mat = factor(resumo_9ano$Classificacao_desigualdade_NSE_mat,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_9ano[!(Classificacao_desigualdade_NSE_mat %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Matematica_NSE),
                                by=Classificacao_desigualdade_NSE_mat]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_NSE_mat ]

quartis_cluster = resumo_9ano[!(Classificacao_desigualdade_NSE_mat %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Matematica_NSE),
                              by=Classificacao_desigualdade_NSE_mat]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_mat==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_mat_nse_nono = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
#Niveis aprendizagem
dados_nse_mat9[,'Nivel_Matematica'] = cut(dados_nse_mat9[,Matematica],breaks=limites_nono_ano_Matematica,
                                          include.lowest = T, right=F)

levels(dados_nse_mat9$Nivel_Matematica) = c(1,2,3)

log_razao_chance_NSE_niveis_matematica = dados_nse_mat9[,.(prop_abaixo_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[1])-
                                                             log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[1]),
                                                           prop_basico_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[2])-
                                                             log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[2]),
                                                           prop_adequado_avancado_mat_nse=log(prop.table(table(Nivel_Matematica[classificacao_NSE==5]))[3])-
                                                             log(prop.table(table(Nivel_Matematica[classificacao_NSE==1]))[3])),
                                                        by=ID_MUNICIPIO]

resumo_9ano = merge(resumo_9ano,log_razao_chance_NSE_niveis_matematica,all.x=T)

# LP
# Classifica equidade e situacoes atipicas
resumo_9ano[abs(Desigualdade.Leitura_NSE)< (-corte_nse_lp_nono),
            'Classificacao_desigualdade_NSE_lp'] = 'Equidade'

resumo_9ano[Desigualdade.Leitura_NSE> (-corte_nse_lp_nono),
            'Classificacao_desigualdade_NSE_lp'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_9ano[!(Classificacao_desigualdade_NSE_lp %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Leitura_NSE'])

clusters = kmeans(df_aux_kl, 3)

resumo_9ano[!(Classificacao_desigualdade_NSE_lp %in% 
                c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_NSE_lp'] = as.factor(clusters$cluster)

resumo_9ano$Classificacao_desigualdade_NSE_lp = factor(resumo_9ano$Classificacao_desigualdade_NSE_lp)

aux = resumo_9ano[,.(median(Desigualdade.Matematica_NSE)),
                  by=Classificacao_desigualdade_NSE_lp][order(V1),][,Classificacao_desigualdade_NSE_lp]

resumo_9ano$Classificacao_desigualdade_NSE_lp = factor(resumo_9ano$Classificacao_desigualdade_NSE_lp,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_9ano[!(Classificacao_desigualdade_NSE_lp %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Leitura_NSE),
                                by=Classificacao_desigualdade_NSE_lp]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_NSE_lp ]


quartis_cluster = resumo_9ano[!(Classificacao_desigualdade_NSE_lp %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Leitura_NSE),
                              by=Classificacao_desigualdade_NSE_lp]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)

ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_NSE_lp==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_lp_nse_nono = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
#Niveis aprendizagem
dados_nse_lp9[,'Nivel_Leitura'] = cut(dados_nse_lp9[,Leitura],breaks=limites_nono_ano_Leitura,
                                      include.lowest = T, right=F)

levels(dados_nse_lp9$Nivel_Leitura) = c(1,2,3)

log_razao_chance_NSE_niveis_leitura = dados_nse_lp9[, .(prop_abaixo_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[1])-
                                                          log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[1]),
                                                        prop_basico_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[2])-
                                                          log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[2]),
                                                        prop_adequado_avancado_lp_nse=log(prop.table(table(Nivel_Leitura[classificacao_NSE==5]))[3])-
                                                          log(prop.table(table(Nivel_Leitura[classificacao_NSE==1]))[3])),
                                                    by=ID_MUNICIPIO]

resumo_9ano = merge(resumo_9ano,log_razao_chance_NSE_niveis_leitura,all.x=T)


# Raca
# MAT
# Classifica equidade e situacoes atipicas
resumo_9ano[abs(Desigualdade.Matematica_raca)< (-corte_nse_mat_nono),
            'Classificacao_desigualdade_raca_mat'] = 'Equidade'

resumo_9ano[Desigualdade.Matematica_raca> (-corte_nse_mat_nono),
            'Classificacao_desigualdade_raca_mat'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_9ano[!(Classificacao_desigualdade_raca_mat %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Matematica_raca'])

clusters = kmeans(df_aux_kl, 3)

resumo_9ano[!(Classificacao_desigualdade_raca_mat %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_raca_mat'] = as.factor(clusters$cluster)

resumo_9ano$Classificacao_desigualdade_raca_mat = factor(resumo_9ano$Classificacao_desigualdade_raca_mat)

aux = resumo_9ano[,.(median(Desigualdade.Matematica_raca)),
                  by=Classificacao_desigualdade_raca_mat][order(V1),][,Classificacao_desigualdade_raca_mat]

resumo_9ano$Classificacao_desigualdade_raca_mat = factor(resumo_9ano$Classificacao_desigualdade_raca_mat,
                                                         labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_9ano[!(Classificacao_desigualdade_raca_mat %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Matematica_raca),
                                by=Classificacao_desigualdade_raca_mat]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_raca_mat ]

quartis_cluster = resumo_9ano[!(Classificacao_desigualdade_raca_mat %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Matematica_raca),
                              by=Classificacao_desigualdade_raca_mat]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_mat==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_mat_raca_nono = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_raca_mat9[,'Nivel_Matematica'] = cut(dados_raca_mat9[,Matematica],breaks=limites_nono_ano_Matematica,
                                           include.lowest = T, right=F)

levels(dados_raca_mat9$Nivel_Matematica) = c(1,2,3)

log_razao_chance_raca_niveis_matematica = dados_raca_mat9[, .(prop_abaixo_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[1])-
                                                                log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[1]),
                                                              prop_basico_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[2])-
                                                                log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[2]),
                                                              prop_adequado_avancado_mat_raca=log(prop.table(table(Nivel_Matematica[Raça=="Branco"]))[3])-
                                                                log(prop.table(table(Nivel_Matematica[Raça=="Preto"]))[3])),
                                                          by=ID_MUNICIPIO]

resumo_9ano = merge(resumo_9ano, log_razao_chance_raca_niveis_matematica,all.x=T)

# LP 
# Classifica equidade e situacoes atipicas
resumo_9ano[abs(Desigualdade.Leitura_raca)< (-corte_nse_lp_nono),
            'Classificacao_desigualdade_raca_lp'] = 'Equidade'

resumo_9ano[Desigualdade.Leitura_raca> (-corte_nse_lp_nono),
            'Classificacao_desigualdade_raca_lp'] = 'Situações atípicas'


set.seed(20)
# padroniza valores de kl nao classificados como situacoes atipicas ou equidade
df_aux_kl = scale(resumo_9ano[!(Classificacao_desigualdade_raca_lp %in%
                                  c('Equidade','Situações atípicas')),
                              'Desigualdade.Leitura_raca'])

clusters = kmeans(df_aux_kl, 3)

resumo_9ano[!(Classificacao_desigualdade_raca_lp %in% c('Equidade','Situações atípicas')),
            'Classificacao_desigualdade_raca_lp'] = as.factor(clusters$cluster)

resumo_9ano$Classificacao_desigualdade_raca_lp = factor(resumo_9ano$Classificacao_desigualdade_raca_lp)

aux = resumo_9ano[,.(median(Desigualdade.Leitura_raca)),
                  by=Classificacao_desigualdade_raca_lp][order(V1),][,Classificacao_desigualdade_raca_lp]

resumo_9ano$Classificacao_desigualdade_raca_lp = factor(resumo_9ano$Classificacao_desigualdade_raca_lp,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_9ano[!(Classificacao_desigualdade_raca_lp %in% c('Equidade','Situações atípicas')),
                                median(Desigualdade.Leitura_raca),
                                by=Classificacao_desigualdade_raca_lp]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_raca_lp ]

quartis_cluster = resumo_9ano[!(Classificacao_desigualdade_raca_lp %in% c('Equidade','Situações atípicas')),
                              quantile(Desigualdade.Leitura_raca),
                              by=Classificacao_desigualdade_raca_lp]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_raca_lp==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_lp_raca_nono = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_raca_lp9[,'Nivel_Leitura'] = cut(dados_raca_lp9[,Leitura],breaks=limites_nono_ano_Leitura,
                                           include.lowest = T, right=F)

levels(dados_raca_lp9$Nivel_Leitura) = c(1,2,3)

log_razao_chance_raca_niveis_leitura = dados_raca_lp9[, .(prop_abaixo_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[1])-
                                                            log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[1]),
                                                          prop_basico_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[2])-
                                                            log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[2]),
                                                          prop_adequado_avancado_lp_raca=log(prop.table(table(Nivel_Leitura[Raça=="Branco"]))[3])-
                                                            log(prop.table(table(Nivel_Leitura[Raça=="Preto"]))[3])),
                                                      by=ID_MUNICIPIO]

resumo_9ano = merge(resumo_9ano, log_razao_chance_raca_niveis_leitura,all.x=T)

# Salva pontos de corte desigualdades
save(cortes_lp_nse_nono, cortes_mat_nse_nono, cortes_lp_raca_nono,cortes_mat_raca_nono,
     file=paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_desigualdade_9ano.Rdata"))