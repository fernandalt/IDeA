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
# Cluster Sexo via KL
# MAT
# Classifica equidade
resumo_5ano$Desigualdade.Matematica_sexo_novo = -abs(resumo_5ano$Desigualdade.Matematica_sexo)

resumo_5ano[Desigualdade.Matematica_sexo_novo > corte_nse_mat_quinto,
            'Classificacao_desigualdade_sexo_mat'] = 'Equidade'


set.seed(20)
# padroniza valores de kl nao classificados como equidade
df_aux_kl = scale(resumo_5ano[!(Classificacao_desigualdade_sexo_mat %in%
                                  c('Equidade')),
                              'Desigualdade.Matematica_sexo_novo']) 

clusters = kmeans(df_aux_kl, 3)

resumo_5ano[!(Classificacao_desigualdade_sexo_mat %in% c('Equidade')),
            'Classificacao_desigualdade_sexo_mat'] = as.factor(clusters$cluster)

resumo_5ano$Classificacao_desigualdade_sexo_mat = factor(resumo_5ano$Classificacao_desigualdade_sexo_mat)

aux = resumo_5ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                  by=Classificacao_desigualdade_sexo_mat][order(V1),Classificacao_desigualdade_sexo_mat]

resumo_5ano$Classificacao_desigualdade_sexo_mat = factor(resumo_5ano$Classificacao_desigualdade_sexo_mat,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_5ano[!(Classificacao_desigualdade_sexo_mat %in% c('Equidade')),
                                median(Desigualdade.Matematica_sexo_novo),
                                by=Classificacao_desigualdade_sexo_mat]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_sexo_mat ]

quartis_cluster = resumo_5ano[!(Classificacao_desigualdade_sexo_mat %in% c('Equidade')),
                              quantile(Desigualdade.Matematica_sexo_novo),
                              by=Classificacao_desigualdade_sexo_mat]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_mat_sexo_quinto = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_sexo_mat5[,'Nivel_Matematica'] = cut(dados_sexo_mat5[,Matematica],breaks=limites_quinto_ano_Matematica,
                                          include.lowest = T, right=F)

levels(dados_sexo_mat5$Nivel_Matematica) = c(1,2,3)

log_razao_chance_sexo_niveis_matematica = dados_sexo_mat5[,.(prop_abaixo_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[1])-
                                                               log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[1]),
                                                             prop_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[2])-
                                                               log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[2]),
                                                             prop_adequado_avancado_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[3])-
                                                               log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[3])),
                                                          by=ID_MUNICIPIO]

resumo_5ano = merge(resumo_5ano,log_razao_chance_sexo_niveis_matematica, all.x=T)

# LP
# Classifica equidade
resumo_5ano$Desigualdade.Leitura_sexo_novo = -abs(resumo_5ano$Desigualdade.Leitura_sexo)

resumo_5ano[Desigualdade.Leitura_sexo_novo > corte_nse_lp_quinto,
            'Classificacao_desigualdade_sexo_lp'] = 'Equidade'

set.seed(20)
# padroniza valores de kl nao classificados como equidade
df_aux_kl = scale(resumo_5ano[!(Classificacao_desigualdade_sexo_lp %in%
                                  c('Equidade')),
                              'Desigualdade.Leitura_sexo_novo']) 

clusters = kmeans(df_aux_kl, 3)

resumo_5ano[!(Classificacao_desigualdade_sexo_lp %in% c('Equidade')),
            'Classificacao_desigualdade_sexo_lp'] = as.factor(clusters$cluster)

resumo_5ano$Classificacao_desigualdade_sexo_lp = factor(resumo_5ano$Classificacao_desigualdade_sexo_lp)

aux = resumo_5ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                  by=Classificacao_desigualdade_sexo_lp][order(V1),][,Classificacao_desigualdade_sexo_lp]

resumo_5ano$Classificacao_desigualdade_sexo_lp = factor(resumo_5ano$Classificacao_desigualdade_sexo_lp,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_5ano[!(Classificacao_desigualdade_sexo_lp %in% c('Equidade')),
                                median(Desigualdade.Leitura_sexo_novo),
                                by=Classificacao_desigualdade_sexo_lp]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_sexo_lp]

quartis_cluster = resumo_5ano[!(Classificacao_desigualdade_sexo_lp %in% c('Equidade')),
                              quantile(Desigualdade.Leitura_sexo_novo),
                              by=Classificacao_desigualdade_sexo_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_lp_sexo_quinto = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_sexo_lp5[,'Nivel_Leitura'] = cut(dados_sexo_lp5[,Leitura],breaks=limites_quinto_ano_Leitura,
                                        include.lowest = T, right=F)

levels(dados_lp_5ano_sexo$Nivel_Leitura) = c(1,2,3)

log_razao_chance_sexo_niveis_matematica = dados_sexo_lp5[,.(prop_abaixo_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[1])-
                                                              log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[1]),
                                                            prop_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[2])-
                                                              log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[2]),
                                                            prop_adequado_avancado_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[3])-
                                                              log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[3])),
                                                         by=ID_MUNICIPIO]


resumo_municipios_quinto_ano = merge(resumo_5ano,log_razao_chance_sexo_niveis_matematica,all.x=T)

# Salva pontos de corte desigualdades
save(cortes_lp_sexo_quinto, cortes_mat_sexo_quinto,
     file=paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_desigualdade_sexo_5ano.Rdata"))

# 9 ano
# Cluster Sexo via KL
# MAT
# Classifica equidade
resumo_9ano$Desigualdade.Matematica_sexo_novo = -abs(resumo_9ano$Desigualdade.Matematica_sexo)

resumo_9ano[Desigualdade.Matematica_sexo_novo > corte_nse_mat_nono,
            'Classificacao_desigualdade_sexo_mat'] = 'Equidade'


set.seed(20)
# padroniza valores de kl nao classificados como equidade
df_aux_kl = scale(resumo_9ano[!(Classificacao_desigualdade_sexo_mat %in%
                                  c('Equidade')),
                              'Desigualdade.Matematica_sexo_novo']) 

clusters = kmeans(df_aux_kl, 3)

resumo_9ano[!(Classificacao_desigualdade_sexo_mat %in% c('Equidade')),
            'Classificacao_desigualdade_sexo_mat'] = as.factor(clusters$cluster)

resumo_9ano$Classificacao_desigualdade_sexo_mat = factor(resumo_9ano$Classificacao_desigualdade_sexo_mat)

aux = resumo_9ano[,.(median(Desigualdade.Matematica_sexo_novo)),
                  by=Classificacao_desigualdade_sexo_mat][order(V1),Classificacao_desigualdade_sexo_mat]

resumo_9ano$Classificacao_desigualdade_sexo_mat = factor(resumo_9ano$Classificacao_desigualdade_sexo_mat,
                                                         labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_9ano[!(Classificacao_desigualdade_sexo_mat %in% c('Equidade')),
                                median(Desigualdade.Matematica_sexo_novo),
                                by=Classificacao_desigualdade_sexo_mat]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_sexo_mat ]

quartis_cluster = resumo_9ano[!(Classificacao_desigualdade_sexo_mat %in% c('Equidade')),
                              quantile(Desigualdade.Matematica_sexo_novo),
                              by=Classificacao_desigualdade_sexo_mat]

quartis_cluster$Quartil = rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_mat==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_mat_sexo_nono = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_sexo_mat9[,'Nivel_Matematica'] = cut(dados_sexo_mat9[,Matematica],breaks=limites_nono_ano_Matematica,
                                           include.lowest = T, right=F)

levels(dados_sexo_mat9$Nivel_Matematica) = c(1,2,3)

log_razao_chance_sexo_niveis_matematica = dados_sexo_mat9[,.(prop_abaixo_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[1])-
                                                               log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[1]),
                                                             prop_basico_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[2])-
                                                               log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[2]),
                                                             prop_adequado_avancado_mat_sexo=log(prop.table(table(Nivel_Matematica[Sexo=='Masculino']))[3])-
                                                               log(prop.table(table(Nivel_Matematica[Sexo=='Feminino']))[3])),
                                                          by=ID_MUNICIPIO]

resumo_9ano = merge(resumo_9ano,log_razao_chance_sexo_niveis_matematica, all.x=T)

# LP
# Classifica equidade
resumo_9ano$Desigualdade.Leitura_sexo_novo = -abs(resumo_9ano$Desigualdade.Leitura_sexo)

resumo_9ano[Desigualdade.Leitura_sexo_novo > corte_nse_lp_nono,
            'Classificacao_desigualdade_sexo_lp'] = 'Equidade'

set.seed(20)
# padroniza valores de kl nao classificados como equidade
df_aux_kl = scale(resumo_9ano[!(Classificacao_desigualdade_sexo_lp %in%
                                  c('Equidade')),
                              'Desigualdade.Leitura_sexo_novo']) 

clusters = kmeans(df_aux_kl, 3)

resumo_9ano[!(Classificacao_desigualdade_sexo_lp %in% c('Equidade')),
            'Classificacao_desigualdade_sexo_lp'] = as.factor(clusters$cluster)

resumo_9ano$Classificacao_desigualdade_sexo_lp = factor(resumo_9ano$Classificacao_desigualdade_sexo_lp)

aux = resumo_9ano[,.(median(Desigualdade.Leitura_sexo_novo)),
                  by=Classificacao_desigualdade_sexo_lp][order(V1),][,Classificacao_desigualdade_sexo_lp]

resumo_9ano$Classificacao_desigualdade_sexo_lp = factor(resumo_9ano$Classificacao_desigualdade_sexo_lp,
                                                        labels=aux)

# faixas de desigualdade
medianas_clusters = resumo_9ano[!(Classificacao_desigualdade_sexo_lp %in% c('Equidade')),
                                median(Desigualdade.Leitura_sexo_novo),
                                by=Classificacao_desigualdade_sexo_lp]

clusters_ordenados = medianas_clusters[order(V1),Classificacao_desigualdade_sexo_lp]

quartis_cluster = resumo_9ano[!(Classificacao_desigualdade_sexo_lp %in% c('Equidade')),
                              quantile(Desigualdade.Leitura_sexo_novo),
                              by=Classificacao_desigualdade_sexo_lp]

quartis_cluster$Quartil<-rep(c('0%','25%','50%','75%','100%'),3)


ponto_corte_1 = mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[1] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] & Quartil=='25%',V1])

ponto_corte_2 = mean(quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[2] & Quartil=='75%',V1],
                     quartis_cluster[Classificacao_desigualdade_sexo_lp==clusters_ordenados[3] & Quartil=='25%',V1])

cortes_lp_sexo_nono = c(ponto_corte_1,ponto_corte_2)

# Razao de chance
# Niveis aprendizagem
dados_sexo_lp9[,'Nivel_Leitura'] = cut(dados_sexo_lp9[,Leitura],breaks=limites_nono_ano_Leitura,
                                       include.lowest = T, right=F)

levels(dados_sexo_lp9$Nivel_Leitura) = c(1,2,3)

log_razao_chance_sexo_niveis_matematica = dados_sexo_lp9[,.(prop_abaixo_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[1])-
                                                              log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[1]),
                                                            prop_basico_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[2])-
                                                              log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[2]),
                                                            prop_adequado_avancado_lp_sexo=log(prop.table(table(Nivel_Leitura[Sexo=='Masculino']))[3])-
                                                              log(prop.table(table(Nivel_Leitura[Sexo=='Feminino']))[3])),
                                                         by=ID_MUNICIPIO]


resumo_municipios_nono_ano = merge(resumo_9ano,log_razao_chance_sexo_niveis_matematica,all.x=T)

# Salva pontos de corte desigualdades
save(cortes_lp_sexo_nono, cortes_mat_sexo_nono,
     file=paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_desigualdade_sexo_9ano.Rdata"))