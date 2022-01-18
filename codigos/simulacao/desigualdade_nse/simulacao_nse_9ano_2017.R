rm(list=ls(all=T))

## Pacotes
require(data.table)
library(lme4)

## Base de dados
wd = getwd()
path = paste0(dirname(dirname(dirname(wd))), "/dados/raw")
load(paste0(path,"/DadosProvaBrasil_2007_2017.Rdata"))

dados_tudo=dados_tudo[COD_MUNICIPIO<6000000]

colnames(dados_tudo)[4]='ID_MUNICIPIO'

NSE_Medio_Escola=dados_tudo[,.(NSE_Escola=mean(NSE,na.rm=T)),by='PK_COD_ENTIDADE']

dados_tudo=merge(dados_tudo,NSE_Medio_Escola,all.x=T)

dados_tudo[is.na(Raça),'Raça']='Ausente'

# Carrega proporcao alunos por escola
path = paste0(dirname(dirname(dirname(wd))), "/dados/perfis_escolas_municipios")

load(paste0(path,'/perfis_escolas_9ano_raca_sexo.Rdata'))
load(paste0(path,'/perfis_escolas_9ano_nse.Rdata'))

prop_nse_escola = prop_nse_escola_13.17_9ano
rm(prop_nse_escola_07.11_9ano, prop_nse_escola_09.13_9ano, prop_nse_escola_11.15_9ano, prop_nse_escola_13.17_9ano)

prop_raca_escola = prop_raca_escola[!(is.na(prop_branco) | is.na(prop_preto))]
prop_sexo_escola = prop_sexo_escola[!(is.na(prop_masculino) | is.na(prop_feminino))]
prop_nse_escola = prop_nse_escola[!(is.na(proporcao_NSE1) | is.na(proporcao_NSE5))]

# Carrega proporcao alunos por municipio
load(paste0(path,'/perfis_municipios_9ano_raca_sexo.Rdata'))
load(paste0(path,'/perfis_municipios_9ano_nse.Rdata'))

prop_nse_municipio = prop_nse_municipio_13.17_9ano
rm(prop_nse_municipio_07.11_9ano, prop_nse_municipio_09.13_9ano, prop_nse_municipio_11.15_9ano, prop_nse_municipio_13.17_9ano)

names(prop_sexo_municipio)[1] = "ID_MUNICIPIO"
names(prop_raca_municipio)[1] = "ID_MUNICIPIO"
names(prop_nse_municipio)[1] = "ID_MUNICIPIO"

# Identifica municipios com numero de alunos insuficiente por grupo
tamanho_amostra = dados_tudo[!is.na(Leitura) & !is.na(Matematica) & classificacao_NSE %in% c(1,5) & Ano %in% c(2013,2015,2017)
                              ,.N,.(ID_MUNICIPIO, ID_SERIE, classificacao_NSE)]

tamanho_amostra = data.table::dcast(tamanho_amostra,
                  ID_MUNICIPIO + ID_SERIE ~ classificacao_NSE, value.var=c("N"))

names(tamanho_amostra)[3:4] = c("NSE1", "NSE5")

tamanho_amostra$NSE1 = ifelse(is.na(tamanho_amostra$NSE1), 0, tamanho_amostra$NSE1)
tamanho_amostra$NSE5 = ifelse(is.na(tamanho_amostra$NSE5), 0, tamanho_amostra$NSE5)

codigos_municipios = tamanho_amostra[ID_SERIE == 9 & (NSE1 <30 | NSE5 < 30),
                                     .(ID_MUNICIPIO,NSE1=NSE1<30,NSE5=NSE5<30)]

# Define funcao de simulacao
retorna_valor_esperados = function(codigo_municipio,codigo_escola,cod_escolas,nse){
  
  # simulacoes por escola
  n_escola = cod_escolas[PK_COD_ENTIDADE==codigo_escola,n_imputacao] 
  
  # Verifica se ha proporcoes de alunos por municipio
  if(nrow(prop_raca_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  if(nrow(prop_sexo_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  epsilon = 0.000001
  
  # Fixa NSE
  classificacao_nse = ifelse(nse == 1, 0, 1) #NSE 1 = 0, NSE 5 =1
  
  perfis_nse = rep(classificacao_nse, n_escola) 
  
  parcela_NSE = perfis_nse*unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                 which(colnames(efeito_aleatorio_final_Completo)== "NSE5_Novo"),with=F])
  # sorteia raca
  
  if(nrow(prop_raca_escola[CO_ENTIDADE==codigo_escola,])==0)
    prob_raca = (prop_raca_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)]+epsilon)/sum(prop_raca_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)]+epsilon)
  else
    prob_raca = (prop_raca_escola[CO_ENTIDADE==codigo_escola,c(2,3)]+epsilon)/sum(prop_raca_escola[CO_ENTIDADE==codigo_escola,c(2,3)]+epsilon)
  
  perfis_raca = rbinom(n_escola,size=1,prob_raca[,prop_branco] ) #1 é branco
  
  parcela_raca = (1-perfis_raca)*unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                       which(colnames(efeito_aleatorio_final_Completo)== "RaçaPreto"),with=F])
  # sorteia sexo
  if(nrow(prop_sexo_escola[CO_ENTIDADE==codigo_escola,])==0)
    prob_sexo = prop_sexo_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)]/sum(prop_sexo_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])
  else
    prob_sexo = prop_sexo_escola[CO_ENTIDADE==codigo_escola,c(2,3)]/sum(prop_sexo_escola[CO_ENTIDADE==codigo_escola,c(2,3)])
  
  perfis_sexo = rbinom(n_escola,size=1,prob_sexo[,prop_masculino] ) #1 é sexo masculino
  
  parcela_sexo = (1-perfis_sexo)*unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                       which(colnames(efeito_aleatorio_final_Completo)== "SexoFeminino"),with=F])
  # sorteia ano
  perfil_ano = sample(1:3,size=n_escola,alunos_ano[,prop],replace = T)
  
  parcela_ano = c('2013' = 0,unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                   which(colnames(efeito_aleatorio_final_Completo) %in% 
                                                                           c("as.factor.Ano.2015","as.factor.Ano.2017")),with=F]))[perfil_ano]
  
  # NSE Escola 
  if(nrow(dados_tudo[PK_COD_ENTIDADE==codigo_escola]) == 0){
    parcela_escola = unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,'NSE_Escola']*mean(dados_tudo[ID_MUNICIPIO==codigo_municipio,NSE_Escola]))
  } else{
    parcela_escola = unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,'NSE_Escola']*dados_tudo[PK_COD_ENTIDADE==codigo_escola,NSE_Escola][1])
  }
  # Valor esperado por tipo de aluno    
  return(unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,'X.Intercept.'])+
           parcela_ano+parcela_NSE+parcela_raca+parcela_sexo+parcela_escola)
}

# Dados modelo LP
path = paste0(dirname(dirname(dirname(wd))), "/dados")
load(paste0(path,'/modelos/modelo_9ano_lp_2017.Rdata'))

sd_intercepto = VarCorr(modelo.Completo.9ano.lp.13.17)[[24]]
sd_intercepto = as.numeric(sd_intercepto)

efeito_aleatorio_final_Completo = read.csv2(paste0(path,'/correcao_nse/coeficientes_9Ano_lp_2017.csv'))
efeito_aleatorio_final_Completo = data.table(efeito_aleatorio_final_Completo)

# Executa simulacao LP
amostras_simuladas = list()

for(i in 1:nrow(codigos_municipios)){
  print(i)

  cod_escolas = dados_tudo[ID_MUNICIPIO==codigos_municipios[i,ID_MUNICIPIO] &  ID_SERIE == 9 & !is.na(Matematica) & 
                            !is.na(Leitura) & classificacao_NSE %in% c(1,5) & Ano %in% c(2013,2015,2017),
                          .N,.(PK_COD_ENTIDADE,classificacao_NSE)]
  
  alunos_ano = dados_tudo[ID_MUNICIPIO==codigos_municipios[i, ID_MUNICIPIO] &  ID_SERIE == 9
                         & !is.na(Leitura) & Ano %in% c(2013,2015,2017),.N,Ano]
  
  alunos_ano$prop = alunos_ano$N/sum(alunos_ano$N)
  
  if(nrow(alunos_ano)<3){
    ind = which(is.na(match(c(2013,2015,2017),alunos_ano$Ano)))
    alunos_ano = rbind(alunos_ano,data.table(Ano=c(2013,2015,2017)[ind],N=0,prop=0))
  }

  df_amostras = data.frame()
  
  # NSE 1
  if(codigos_municipios[i,NSE1]){
    
    cod_escolas_aux = cod_escolas[classificacao_NSE == 1]
    
    # se não tiver alunos de NSE 1
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              classificacao_NSE = 1,
                                              N = 0,
                                              n_imputacao = 30))
    }else{
      cod_escolas_aux$prop = cod_escolas_aux$N/sum(cod_escolas_aux$N)
      cod_escolas_aux$n_imputacao = round((30-sum(cod_escolas_aux$N))*cod_escolas_aux$prop+0.5)
    }
    
    medias = c(unlist(sapply(cod_escolas_aux$PK_COD_ENTIDADE, function(x)
      retorna_valor_esperados (codigo_municipio = codigos_municipios[i,ID_MUNICIPIO],
                               codigo_escola = x,
                               cod_escolas = cod_escolas_aux,
                               nse=1))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   NSE = 1, Leitura = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  # NSE 5
  if(codigos_municipios[i,NSE5]){
    
    cod_escolas_aux = cod_escolas[classificacao_NSE == 5]
    
    # se não tiver alunos de NSE 5
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              classificacao_NSE = 5,
                                              N = 0,
                                              n_imputacao = 30))
    }else{
      cod_escolas_aux$prop = cod_escolas_aux$N/sum(cod_escolas_aux$N)
      cod_escolas_aux$n_imputacao = round((30-sum(cod_escolas_aux$N))*cod_escolas_aux$prop+0.5)
    }
    
    medias = c(unlist(sapply(cod_escolas_aux$PK_COD_ENTIDADE, function(x)
      retorna_valor_esperados(codigo_municipio = codigos_municipios[i,ID_MUNICIPIO],
                              codigo_escola = x,
                              cod_escolas = cod_escolas_aux,
                              nse=5))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   NSE = 5, Leitura = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  
  row.names(df_amostras) = 1:nrow(df_amostras)
  amostras_simuladas[[i]] = df_amostras
}

save(amostras_simuladas, codigos_municipios, tamanho_amostra, 
     file = paste0(path, '/simulacao/desigualdade_nse/simulacao_nse_9ano_lp_2017.Rdata'))

# Dados modelo MAT
load(paste0(path,'/modelos/modelo_9ano_mat_2017.Rdata'))

sd_intercepto = VarCorr(modelo.Completo.9ano.mat.13.17)[[24]]
sd_intercepto = as.numeric(sd_intercepto)

efeito_aleatorio_final_Completo = read.csv2(paste0(path,'/correcao_nse/coeficientes_9Ano_mat_2017.csv'))
efeito_aleatorio_final_Completo = data.table(efeito_aleatorio_final_Completo)

# Executa simulacao MAT
amostras_simuladas = list()

for(i in 1:nrow(codigos_municipios)){
  print(i)

  cod_escolas = dados_tudo[ID_MUNICIPIO==codigos_municipios[i,ID_MUNICIPIO] &  ID_SERIE == 9 & !is.na(Leitura) &
                             !is.na(Matematica) & classificacao_NSE %in% c(1,5) & Ano %in% c(2013,2015,2017),
                           .N,.(PK_COD_ENTIDADE,classificacao_NSE)]
  
  alunos_ano = dados_tudo[ID_MUNICIPIO==codigos_municipios[i, ID_MUNICIPIO] &  ID_SERIE == 9
                         & !is.na(Matematica) & Ano %in% c(2013,2015,2017),.N,Ano]
  
  alunos_ano$prop = alunos_ano$N/sum(alunos_ano$N)
  
  if(nrow(alunos_ano)<3){
    ind = which(is.na(match(c(2013,2015,2017),alunos_ano$Ano)))
    alunos_ano = rbind(alunos_ano,data.table(Ano=c(2013,2015,2017)[ind],N=0,prop=0))
  }
  
  df_amostras = data.frame()
  
  # NSE 1
  if(codigos_municipios[i,NSE1]){
    
    cod_escolas_aux = cod_escolas[classificacao_NSE == 1]
    
    # se não tiver alunos de NSE 1
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              classificacao_NSE = 1,
                                              N = 0,
                                              n_imputacao = 30))
    }else{
      cod_escolas_aux$prop = cod_escolas_aux$N/sum(cod_escolas_aux$N)
      cod_escolas_aux$n_imputacao = round((30-sum(cod_escolas_aux$N))*cod_escolas_aux$prop+0.5)
    }
    
    medias = c(unlist(sapply(cod_escolas_aux$PK_COD_ENTIDADE, function(x)
      retorna_valor_esperados (codigo_municipio = codigos_municipios[i,ID_MUNICIPIO],
                               codigo_escola = x,
                               cod_escolas = cod_escolas_aux,
                               nse=1))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   NSE = 1, Matematica = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  # NSE 5
  if(codigos_municipios[i,NSE5]){
    
    cod_escolas_aux = cod_escolas[classificacao_NSE == 5]
    
    # se não tiver alunos de NSE 5
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              classificacao_NSE = 5,
                                              N = 0,
                                              n_imputacao = 30))
    }else{
      cod_escolas_aux$prop = cod_escolas_aux$N/sum(cod_escolas_aux$N)
      cod_escolas_aux$n_imputacao = round((30-sum(cod_escolas_aux$N))*cod_escolas_aux$prop+0.5)
    }
    
    medias = c(unlist(sapply(cod_escolas_aux$PK_COD_ENTIDADE, function(x)
      retorna_valor_esperados(codigo_municipio = codigos_municipios[i,ID_MUNICIPIO],
                              codigo_escola = x,
                              cod_escolas = cod_escolas_aux,
                              nse=5))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   NSE = 5, Matematica = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  
  row.names(df_amostras) = 1:nrow(df_amostras)
  amostras_simuladas[[i]] = df_amostras
}

save(amostras_simuladas, codigos_municipios, tamanho_amostra, 
     file = paste0(path, '/simulacao/desigualdade_nse/simulacao_nse_9ano_mat_2017.Rdata'))