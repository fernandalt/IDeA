require(data.table)

setwd('C:\\Users\\Erica\\Dropbox\\UFOP\\Grupo NAVE\\Trabalho Chico Soare\\Atualizacao_2017\\Modelo Imputacao\\Censo Escolar')


# Centro-oeste

dados_matricula_co<-fread('matricula_co.csv')


table(dados_matricula_co$TP_COR_RACA)

dados_matricula_co$TP_COR_RACA<-factor(dados_matricula_co$TP_COR_RACA)
#"1 - Branca
#2 - Preta
#3 - Parda
#4 - Amarela
#5 - Indígena
#9 - Não declarada"


levels(dados_matricula_co$TP_COR_RACA)<-c(NA,'Branca','Preta','Parda','Amarela','Indígena')


# Sexo
dados_matricula_co$TP_SEXO<-factor(dados_matricula_co$TP_SEXO)
levels(dados_matricula_co$TP_SEXO)<-c('Masculino','Feminino')


dados_matricula_co$Segmento_Ensino<-factor(dados_matricula_co$TP_ETAPA_ENSINO)
levels(dados_matricula_co$Segmento_Ensino)


levels(dados_matricula_co$Segmento_Ensino)<-c('Educação Infantil','Educação Infantil',
                                              rep('Fundamental I',5),
                                              rep('Fundamental II',3),
                                              rep('Ensino Médio',4),
                                              rep('Ensino técnico',5),
                                              rep('Ensino Médio',4),
                                              rep('Ensino Técnico',2),
                                              rep('Fundamental II',1),rep('FIC',2),rep('EJA',6))
                                              
                                              


#1 - Educação Infantil - Creche
#2 - Educação Infantil - Pré-escola
#4 - Ensino Fundamental de 8 anos - 1ª Série
#5 - Ensino Fundamental de 8 anos - 2ª Série
#6 - Ensino Fundamental de 8 anos - 3ª Série
#7 - Ensino Fundamental de 8 anos - 4ª Série
#8 - Ensino Fundamental de 8 anos - 5ª Série
#9 - Ensino Fundamental de 8 anos - 6ª Série
#10 - Ensino Fundamental de 8 anos - 7ª Série
#11 - Ensino Fundamental de 8 anos - 8ª Série
#14 - Ensino Fundamental de 9 anos - 1º Ano
#15 - Ensino Fundamental de 9 anos - 2º Ano
#16 - Ensino Fundamental de 9 anos - 3º Ano
#17 - Ensino Fundamental de 9 anos - 4º Ano
#18 - Ensino Fundamental de 9 anos - 5º Ano
#19 - Ensino Fundamental de 9 anos - 6º Ano
#20 - Ensino Fundamental de 9 anos - 7º Ano
#21 - Ensino Fundamental de 9 anos - 8º Ano
#41 - Ensino Fundamental de 9 anos - 9º Ano
#25 - Ensino Médio - 1º ano/1ª Série
#26 - Ensino Médio - 2º ano/2ª Série
#27 - Ensino Médio - 3ºano/3ª Série
#28 - Ensino Médio - 4º ano/4ª Série
#29 - Ensino Médio - Não Seriada
#30 - Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série
#31 - Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série
#32 - Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série
#33 - Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série
#34 - Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada
#35 - Ensino Médio - Modalidade Normal/Magistério 1ª Série
#36 - Ensino Médio - Modalidade Normal/Magistério 2ª Série
#37 - Ensino Médio - Modalidade Normal/Magistério 3ª Série
#38 - Ensino Médio - Modalidade Normal/Magistério 4ª Série
#39 - Curso Técnico - Concomitante
#40 - Curso Técnico - Subsequente
#65 - EJA - Ensino Fundamental - Projovem Urbano
#67 - Curso FIC integrado na modalidade EJA  - Nível Médio
#68 - Curso FIC Concomitante 
#69 - EJA - Ensino Fundamental - Anos Iniciais
#70 - EJA - Ensino Fundamental - Anos Finais
#71 - EJA - Ensino Médio
#72 - EJA - Ensino Fundamental  - Anos iniciais e Anos finais5
#73 - Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental) 
#74 - Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)
#      - Não aplicável para turmas de atendimento educacional especializado (AEE) e atividade complementar"


dados_matricula_co[Segmento_Ensino=='Fundamental I',
                   .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                     prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))
                     
                     )]


prop_raca_escola_co<-dados_matricula_co[Segmento_Ensino=='Fundamental I',
                   .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                     prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))
                     
                   ),by=CO_ENTIDADE]


prop_sexo_escola_co<-dados_matricula_co[Segmento_Ensino=='Fundamental I',
                                        .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                          prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))
                                          
                                        ),by=CO_ENTIDADE]


save(prop_sexo_escola_co,file='Prop_sexo_escola_co.Rdata')
save(prop_raca_escola_co,file='Prop_raca_escola_co.Rdata')


########################
# Nordeste
##############################

dados_matricula_nordeste<-fread('matricula_nordeste.csv')


# Raca
dados_matricula_nordeste$TP_COR_RACA<-factor(dados_matricula_nordeste$TP_COR_RACA)

levels(dados_matricula_nordeste$TP_COR_RACA)<-c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# Sexo
dados_matricula_nordeste$TP_SEXO<-factor(dados_matricula_nordeste$TP_SEXO)
levels(dados_matricula_nordeste$TP_SEXO)<-c('Masculino','Feminino')

# Segmento
dados_matricula_nordeste$Segmento_Ensino<-factor(dados_matricula_nordeste$TP_ETAPA_ENSINO)
levels(dados_matricula_nordeste$Segmento_Ensino)<-c('Educação Infantil','Educação Infantil',
                                              rep('Fundamental I',5),
                                              rep('Fundamental II',3),
                                              rep('Ensino Médio',4),
                                              rep('Ensino técnico',5),
                                              rep('Ensino Médio',4),
                                              rep('Ensino Técnico',2),
                                              rep('Fundamental II',1),rep('FIC',2),rep('EJA',6))

# Proporcao Raça escola

prop_raca_escola_nordeste<-dados_matricula_nordeste[Segmento_Ensino=='Fundamental I',
                                        .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                          prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))
                                          
                                        ),by=CO_ENTIDADE]

# Proporcao sexo escola
prop_sexo_escola_nordeste<-dados_matricula_nordeste[Segmento_Ensino=='Fundamental I',
                                        .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                          prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))
                                          
                                        ),by=CO_ENTIDADE]



save(prop_sexo_escola_nordeste,file='Prop_sexo_escola_nordeste.Rdata')
save(prop_raca_escola_nordeste,file='Prop_raca_escola_nordeste.Rdata')


########################
# Sudeste
##############################
memory.limit(size=300000)
gc(reset = T)
dados_matricula_sudeste<-fread('matricula_sudeste.csv')


# Raca
dados_matricula_sudeste$TP_COR_RACA<-factor(dados_matricula_sudeste$TP_COR_RACA)

levels(dados_matricula_sudeste$TP_COR_RACA)<-c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# Sexo
dados_matricula_sudeste$TP_SEXO<-factor(dados_matricula_sudeste$TP_SEXO)
levels(dados_matricula_sudeste$TP_SEXO)<-c('Masculino','Feminino')

# Segmento
dados_matricula_sudeste$Segmento_Ensino<-factor(dados_matricula_sudeste$TP_ETAPA_ENSINO)
levels(dados_matricula_sudeste$Segmento_Ensino)<-c('Educação Infantil','Educação Infantil',
                                                    rep('Fundamental I',5),
                                                    rep('Fundamental II',3),
                                                    rep('Ensino Médio',4),
                                                    rep('Ensino técnico',5),
                                                    rep('Ensino Médio',4),
                                                    rep('Ensino Técnico',2),
                                                    rep('Fundamental II',1),rep('FIC',2),rep('EJA',6))

# Proporcao Raça escola

prop_raca_escola_sudeste<-dados_matricula_sudeste[Segmento_Ensino=='Fundamental I',
                                                    .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                                      prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))
                                                      
                                                    ),by=CO_ENTIDADE]

# Proporcao sexo escola
prop_sexo_escola_sudeste<-dados_matricula_sudeste[Segmento_Ensino=='Fundamental I',
                                                    .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                                      prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))
                                                      
                                                    ),by=CO_ENTIDADE]

save(prop_sexo_escola_sudeste,file='Prop_sexo_escola_sudeste.Rdata')
save(prop_raca_escola_sudeste,file='Prop_raca_escola_sudeste.Rdata')


########################
# Norte
##############################

dados_matricula_norte<-fread('matricula_norte.csv')


# Raca
dados_matricula_norte$TP_COR_RACA<-factor(dados_matricula_norte$TP_COR_RACA)

levels(dados_matricula_norte$TP_COR_RACA)<-c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# Sexo
dados_matricula_norte$TP_SEXO<-factor(dados_matricula_norte$TP_SEXO)
levels(dados_matricula_norte$TP_SEXO)<-c('Masculino','Feminino')

# Segmento
dados_matricula_norte$Segmento_Ensino<-factor(dados_matricula_norte$TP_ETAPA_ENSINO)
levels(dados_matricula_norte$Segmento_Ensino)<-c(rep('Educação Infantil',2),
                                                 rep('Fundamental I',5),
                                                 rep('Fundamental II',4),
                                                 rep('Ensino Médio',4),
                                                 rep('Ensino Técnico',5),
                                                 rep('Ensino Médio',4),
                                                 rep('Ensino Técnico',2),
                                                 rep('FIC',2),rep('EJA',3),rep('FIC',2))

# Proporcao Raça escola

prop_raca_escola_norte<-dados_matricula_norte[Segmento_Ensino=='Fundamental I',
                                                  .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                                    prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))
                                                    
                                                  ),by=CO_ENTIDADE]

# Proporcao sexo escola
prop_sexo_escola_norte<-dados_matricula_norte[Segmento_Ensino=='Fundamental I',
                                                  .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                                    prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))
                                                    
                                                  ),by=CO_ENTIDADE]





save(prop_sexo_escola_norte,file='Prop_sexo_escola_norte.Rdata')
save(prop_raca_escola_norte,file='Prop_raca_escola_norte.Rdata')


########################
# Sul
##############################

dados_matricula_sul<-fread('matricula_sul.csv')


# Raca
dados_matricula_sul$TP_COR_RACA<-factor(dados_matricula_sul$TP_COR_RACA)

levels(dados_matricula_sul$TP_COR_RACA)<-c(NA,'Branca','Preta','Parda','Amarela','Indígena')

# Sexo
dados_matricula_sul$TP_SEXO<-factor(dados_matricula_sul$TP_SEXO)
levels(dados_matricula_sul$TP_SEXO)<-c('Masculino','Feminino')

# Segmento
dados_matricula_sul$Segmento_Ensino<-factor(dados_matricula_sul$TP_ETAPA_ENSINO)
levels(dados_matricula_sul$Segmento_Ensino)<-c('Educação Infantil','Educação Infantil',
                                                 rep('Fundamental I',5),
                                                 rep('Fundamental II',3),
                                                 rep('Ensino Médio',4),
                                                 rep('Ensino técnico',5),
                                                 rep('Ensino Médio',4),
                                                 rep('Ensino Técnico',2),
                                                 rep('Fundamental II',1),rep('FIC',2),rep('EJA',6))
dados_matricula_sul[CO_ENTIDADE==43065589 & Segmento_Ensino=='Fundamental I']
dados_matricula_sul[CO_ENTIDADE==61200600 & Segmento_Ensino=='Fundamental I']

# Proporcao Raça escola

prop_raca_escola_sul<-dados_matricula_sul[Segmento_Ensino=='Fundamental I',
                                              .(prop_branco=sum(TP_COR_RACA=='Branca',na.rm=T)/sum(!is.na(TP_COR_RACA)),
                                                prop_preto=sum(TP_COR_RACA=='Preta',na.rm=T)/sum(!is.na(TP_COR_RACA))
                                                
                                              ),by=CO_ENTIDADE]

# Proporcao sexo escola
prop_sexo_escola_sul<-dados_matricula_sul[Segmento_Ensino=='Fundamental I',
                                              .(prop_masculino=sum(TP_SEXO=='Masculino',na.rm=T)/sum(!is.na(TP_SEXO)),
                                                prop_feminino=sum(TP_SEXO=='Feminino',na.rm=T)/sum(!is.na(TP_SEXO))
                                                
                                              ),by=CO_ENTIDADE]

save(prop_sexo_escola_sul,file='Prop_sexo_escola_sul.Rdata')
save(prop_raca_escola_sul,file='Prop_raca_escola_sul.Rdata')
