rm(list=ls(all=T))

## Pacotes
require(data.table)

## Base de dados do censo por regiao
wd = getwd()
path = paste0(dirname(dirname(wd)), "/dados/raw")

# Centro-oeste
dados_matricula_co = fread('matricula_co.csv')

# raca
dados_matricula_co$TP_COR_RACA = factor(dados_matricula_co$TP_COR_RACA)
levels(dados_matricula_co$TP_COR_RACA) = c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# sexo
dados_matricula_co$TP_SEXO = factor(dados_matricula_co$TP_SEXO)
levels(dados_matricula_co$TP_SEXO) = c('Masculino','Feminino')

# etapa de ensino
dados_matricula_co$Segmento_Ensino = factor(dados_matricula_co$TP_ETAPA_ENSINO)
levels(dados_matricula_co$Segmento_Ensino) = c('Educação Infantil','Educação Infantil',
                                               rep('Fundamental I',5),
                                               rep('Fundamental II',3),
                                               rep('Ensino Médio',4),
                                               rep('Ensino técnico',5),
                                               rep('Ensino Médio',4),
                                               rep('Ensino Técnico',2),
                                               rep('Fundamental II',1),
                                               rep('FIC',2),rep('EJA',6))
                                              
# proporcoes                                          
prop_raca_escola_co = dados_matricula_co[Segmento_Ensino=='Fundamental I',
                                         .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                           prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))),
                                         by=CO_ENTIDADE]

prop_sexo_escola_co = dados_matricula_co[Segmento_Ensino=='Fundamental I',
                                        .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                          prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))),
                                        by=CO_ENTIDADE]


# Nordeste
dados_matricula_nordeste = fread('matricula_nordeste.csv')

# raca
dados_matricula_nordeste$TP_COR_RACA = factor(dados_matricula_nordeste$TP_COR_RACA)
levels(dados_matricula_nordeste$TP_COR_RACA) = c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# sexo
dados_matricula_nordeste$TP_SEXO = factor(dados_matricula_nordeste$TP_SEXO)
levels(dados_matricula_nordeste$TP_SEXO) = c('Masculino','Feminino')

# etapa de ensino
dados_matricula_nordeste$Segmento_Ensino = factor(dados_matricula_nordeste$TP_ETAPA_ENSINO)
levels(dados_matricula_nordeste$Segmento_Ensino) = c('Educação Infantil','Educação Infantil',
                                                     rep('Fundamental I',5),
                                                     rep('Fundamental II',3),
                                                     rep('Ensino Médio',4),
                                                     rep('Ensino técnico',5),
                                                     rep('Ensino Médio',4),
                                                     rep('Ensino Técnico',2),
                                                     rep('Fundamental II',1),
                                                     rep('FIC',2),rep('EJA',6))

# proporcoes                                          
prop_raca_escola_nordeste = dados_matricula_nordeste[Segmento_Ensino=='Fundamental I',
                                         .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                           prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))),
                                         by=CO_ENTIDADE]

prop_sexo_escola_nordeste = dados_matricula_nordeste[Segmento_Ensino=='Fundamental I',
                                        .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                          prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))),
                                        by=CO_ENTIDADE]


# Sudeste
memory.limit(size=300000)
gc(reset = T)
dados_matricula_sudeste = fread('matricula_sudeste.csv')

# raca
dados_matricula_sudeste$TP_COR_RACA = factor(dados_matricula_sudeste$TP_COR_RACA)
levels(dados_matricula_sudeste$TP_COR_RACA) = c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# sexo
dados_matricula_sudeste$TP_SEXO = factor(dados_matricula_sudeste$TP_SEXO)
levels(dados_matricula_sudeste$TP_SEXO) = c('Masculino','Feminino')

# etapa de ensino
dados_matricula_sudeste$Segmento_Ensino = factor(dados_matricula_sudeste$TP_ETAPA_ENSINO)
levels(dados_matricula_sudeste$Segmento_Ensino) = c('Educação Infantil','Educação Infantil',
                                                    rep('Fundamental I',5),
                                                    rep('Fundamental II',3),
                                                    rep('Ensino Médio',4),
                                                    rep('Ensino técnico',5),
                                                    rep('Ensino Médio',4),
                                                    rep('Ensino Técnico',2),
                                                    rep('Fundamental II',1),
                                                    rep('FIC',2),rep('EJA',6))

# proporcoes
prop_raca_escola_sudeste = dados_matricula_sudeste[Segmento_Ensino=='Fundamental I',
                                                   .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                                     prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))),
                                                   by=CO_ENTIDADE]

prop_sexo_escola_sudeste = dados_matricula_sudeste[Segmento_Ensino=='Fundamental I',
                                                   .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                                     prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))),
                                                   by=CO_ENTIDADE]


# Norte
dados_matricula_norte = fread('matricula_norte.csv')

# raca
dados_matricula_norte$TP_COR_RACA = factor(dados_matricula_norte$TP_COR_RACA)
levels(dados_matricula_norte$TP_COR_RACA) = c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# sexo
dados_matricula_norte$TP_SEXO = factor(dados_matricula_norte$TP_SEXO)
levels(dados_matricula_norte$TP_SEXO) = c('Masculino','Feminino')

# etapa de ensino
dados_matricula_norte$Segmento_Ensino = factor(dados_matricula_norte$TP_ETAPA_ENSINO)
levels(dados_matricula_norte$Segmento_Ensino) = c(rep('Educação Infantil',2),
                                                  rep('Fundamental I',5),
                                                  rep('Fundamental II',4),
                                                  rep('Ensino Médio',4),
                                                  rep('Ensino Técnico',5),
                                                  rep('Ensino Médio',4),
                                                  rep('Ensino Técnico',2),
                                                  rep('FIC',2),rep('EJA',3),
                                                  rep('FIC',2))

# proporcoes
prop_raca_escola_norte = dados_matricula_norte[Segmento_Ensino=='Fundamental I',
                                               .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                                 prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))),
                                               by=CO_ENTIDADE]

prop_sexo_escola_norte = dados_matricula_norte[Segmento_Ensino=='Fundamental I',
                                               .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                                 prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))),
                                               by=CO_ENTIDADE]


# Sul
dados_matricula_sul = fread('matricula_sul.csv')

# raca
dados_matricula_sul$TP_COR_RACA = factor(dados_matricula_sul$TP_COR_RACA)
levels(dados_matricula_sul$TP_COR_RACA) = c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# sexo
dados_matricula_sul$TP_SEXO = factor(dados_matricula_sul$TP_SEXO)
levels(dados_matricula_sul$TP_SEXO) = c('Masculino','Feminino')

# etapa de ensino
dados_matricula_sul$Segmento_Ensino = factor(dados_matricula_sul$TP_ETAPA_ENSINO)
levels(dados_matricula_sul$Segmento_Ensino) = c('Educação Infantil','Educação Infantil',
                                                rep('Fundamental I',5),
                                                rep('Fundamental II',3),
                                                rep('Ensino Médio',4),
                                                rep('Ensino técnico',5),
                                                rep('Ensino Médio',4),
                                                rep('Ensino Técnico',2),
                                                rep('Fundamental II',1),
                                                rep('FIC',2),rep('EJA',6))

# proporcoes
prop_raca_escola_sul = dados_matricula_sul[Segmento_Ensino=='Fundamental I',
                                           .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                             prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))),
                                           by=CO_ENTIDADE]

prop_sexo_escola_sul = dados_matricula_sul[Segmento_Ensino=='Fundamental I',
                                           .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                             prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))),
                                           by=CO_ENTIDADE]


# Salva resultados por regiao
save(prop_sexo_escola_co,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_sexo_escola_co.Rdata"))

save(prop_raca_escola_co,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_raca_escola_co.Rdata"))

save(prop_sexo_escola_nordeste,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_sexo_escola_nordeste.Rdata"))

save(prop_raca_escola_nordeste,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_raca_escola_nordeste.Rdata"))

save(prop_sexo_escola_sudeste,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_sexo_escola_sudeste.Rdata"))

save(prop_raca_escola_sudeste,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_raca_escola_sudeste.Rdata"))

save(prop_sexo_escola_norte,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_sexo_escola_norte.Rdata"))

save(prop_raca_escola_norte,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_raca_escola_norte.Rdata"))

save(prop_sexo_escola_sul,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_sexo_escola_sul.Rdata"))

save(prop_raca_escola_sul,
     file=paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios/Prop_raca_escola_sul.Rdata"))