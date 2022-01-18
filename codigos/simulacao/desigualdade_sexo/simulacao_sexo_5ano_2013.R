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

load(paste0(path,'/perfis_escolas_5ano_raca_sexo.Rdata'))
load(paste0(path,'/perfis_escolas_5ano_nse.Rdata'))

prop_nse_escola = prop_nse_escola_09.13_5ano
rm(prop_nse_escola_07.11_5ano, prop_nse_escola_09.13_5ano, prop_nse_escola_11.15_5ano, prop_nse_escola_13.17_5ano)

prop_raca_escola = prop_raca_escola[!(is.na(prop_branco) | is.na(prop_preto))]
prop_nse_escola = prop_nse_escola[!(is.na(proporcao_NSE1) | is.na(proporcao_NSE5))]
prop_sexo_escola = prop_sexo_escola[!(is.na(prop_masculino) | is.na(prop_feminino))]

# Carrega proporcao alunos por municipio
load(paste0(path,'/perfis_municipios_5ano_raca_sexo.Rdata'))
load(paste0(path,'/perfis_municipios_5ano_nse.Rdata'))

prop_nse_municipio = prop_nse_municipio_09.13_5ano
rm(prop_nse_municipio_07.11_5ano, prop_nse_municipio_09.13_5ano, prop_nse_municipio_11.15_5ano, prop_nse_municipio_13.17_5ano)

names(prop_sexo_municipio)[1] = "ID_MUNICIPIO"
names(prop_raca_municipio)[1] = "ID_MUNICIPIO"
names(prop_nse_municipio)[1] = "ID_MUNICIPIO"

# Identifica municipios com numero de alunos insuficiente por grupo 
tamanho_amostra = dados_tudo[!is.na(Leitura) & !is.na(Matematica) & Ano %in% c(2009,2011,2013) & 
                               !is.na(Sexo), .N,.(ID_MUNICIPIO, ID_SERIE, Sexo)]

tamanho_amostra = data.table::dcast(tamanho_amostra,
                  ID_MUNICIPIO + ID_SERIE ~ Sexo, value.var=c("N"))

tamanho_amostra$Masculino = ifelse(is.na(tamanho_amostra$Masculino), 0, tamanho_amostra$Masculino)
tamanho_amostra$Feminino = ifelse(is.na(tamanho_amostra$Feminino), 0, tamanho_amostra$Feminino)

codigos_municipios = tamanho_amostra[ID_SERIE == 5 & (Masculino <30 | Feminino < 30),
                                     .(ID_MUNICIPIO, Masculino=Masculino<30, Feminino=Feminino<30)]

# Define funcao de simulacao
retorna_valor_esperados = function(codigo_municipio,codigo_escola,cod_escolas,sexo){
  
  # simulacoes por escola
  n_escola = cod_escolas[PK_COD_ENTIDADE==codigo_escola,n_imputacao] 
  
  # Verifica se ha proporcoes de alunos por municipio
  if(nrow(prop_raca_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  if(nrow(prop_nse_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  epsilon = 0.000001
  
  # sorteia nse
  if(nrow(prop_nse_escola[PK_COD_ENTIDADE==codigo_escola,])==0)
    prob_nse = (prop_nse_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)]+epsilon)/sum(prop_nse_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)]+epsilon)
  else
    prob_nse = (prop_nse_escola[PK_COD_ENTIDADE==codigo_escola,c(2,3)]+epsilon)/sum(prop_nse_escola[PK_COD_ENTIDADE==codigo_escola,c(2,3)]+epsilon)
  
  perfis_nse = rbinom(n_escola,size=1,prob_nse[,proporcao_NSE5]) #1 é NSE 5
  
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
  # Fixa sexo 
  classificacao_sexo = ifelse(sexo == 'Feminino', 0, 1) #sexo Masculino = 1, Sexo Feminino = 0
  
  perfis_sexo = rep(classificacao_sexo, n_escola) 
  
  parcela_sexo = (1-perfis_sexo)*unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                 which(colnames(efeito_aleatorio_final_Completo)== "SexoFeminino"),with=F])
  # sorteia ano
  perfil_ano = sample(1:3,size=n_escola,alunos_ano[,prop],replace = T)
  
  parcela_ano = c('2009' = 0,unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                   which(colnames(efeito_aleatorio_final_Completo) %in% 
                                                                           c("as.factor.Ano.2011","as.factor.Ano.2013")),with=F]))[perfil_ano]

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
load(paste0(path,'/modelos/modelo_5ano_lp_2013.Rdata'))

sd_intercepto = VarCorr(modelo.Completo.5ano.lp.09.13)[[24]]
sd_intercepto = as.numeric(sd_intercepto)

efeito_aleatorio_final_Completo = read.csv2(paste0(path,'/correcao_nse/coeficientes_5Ano_lp_2013.csv'))
efeito_aleatorio_final_Completo = data.table(efeito_aleatorio_final_Completo)

# Executa simulacao LP
amostras_simuladas = list()

for(i in 1:nrow(codigos_municipios)){
  print(i)
  
  cod_escolas = dados_tudo[ID_MUNICIPIO==codigos_municipios[i,ID_MUNICIPIO] &  ID_SERIE==5 & 
                            !is.na(Leitura) & !is.na(Matematica) & !is.na(Sexo) & 
                             Ano %in% c(2009,2011,2013),
                          .N,.(PK_COD_ENTIDADE,Sexo)]
  
  alunos_ano = dados_tudo[ID_MUNICIPIO==codigos_municipios[i, ID_MUNICIPIO] &  ID_SERIE==5
                         & !is.na(Leitura) & !is.na(Matematica) & Ano %in% c(2009,2011,2013),.N,Ano]
  
  alunos_ano$prop = alunos_ano$N/sum(alunos_ano$N)
  
  if(nrow(alunos_ano)<3){
    ind = which(is.na(match(c(2009,2011,2013),alunos_ano$Ano)))
    alunos_ano = rbind(alunos_ano,data.table(Ano=c(2009,2011,2013)[ind],N=0,prop=0))
  }
  alunos_ano = alunos_ano %>% arrange(Ano)
  df_amostras = data.frame()
  
  # Sexo masculino
  if(codigos_municipios[i,Masculino]){
    
    cod_escolas_aux = cod_escolas[Sexo == 'Masculino']
    
    # se não tiver alunos do sexo masculino
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              Sexo = 'Masculino',
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
                               sexo='Masculino'))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   Sexo = 'Masculino', Leitura = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  # Sexo feminino
  if(codigos_municipios[i,Feminino]){
    
    cod_escolas_aux = cod_escolas[Sexo == 'Feminino']
    
    # se não tiver alunos do sexo feminino
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              Sexo = 'Feminino',
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
                              sexo='Feminino'))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   Sexo = 'Feminino', Leitura = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  
  row.names(df_amostras) = 1:nrow(df_amostras)
  amostras_simuladas[[i]] = df_amostras
}

save(amostras_simuladas, codigos_municipios, tamanho_amostra, 
     file = paste0(path, '/simulacao/desigualdade_sexo/simulacao_sexo_5ano_lp_2013.Rdata'))

# Dados modelo MAT
load(paste0(path,'/modelos/modelo_5ano_mat_2013.Rdata'))

sd_intercepto = VarCorr(modelo.Completo.5ano.mat.09.13)[[24]]
sd_intercepto = as.numeric(sd_intercepto)

efeito_aleatorio_final_Completo = read.csv2(paste0(path,'/correcao_nse/coeficientes_5Ano_mat_2013.csv'))
efeito_aleatorio_final_Completo = data.table(efeito_aleatorio_final_Completo)

# Executa simulacao MAT
amostras_simuladas = list()

for(i in 1:nrow(codigos_municipios)){
  print(i)

  cod_escolas = dados_tudo[ID_MUNICIPIO==codigos_municipios[i,ID_MUNICIPIO] &  ID_SERIE==5 & 
                             !is.na(Leitura) & !is.na(Matematica) & !is.na(Sexo) & 
                             Ano %in% c(2009,2011,2013),
                           .N,.(PK_COD_ENTIDADE,Sexo)]
  
  alunos_ano = dados_tudo[ID_MUNICIPIO==codigos_municipios[i, ID_MUNICIPIO] &  ID_SERIE==5
                         & !is.na(Leitura) & !is.na(Matematica) & Ano %in% c(2009,2011,2013),.N,Ano]
  
  alunos_ano$prop = alunos_ano$N/sum(alunos_ano$N)
  
  if(nrow(alunos_ano)<3){
    ind = which(is.na(match(c(2009,2011,2013),alunos_ano$Ano)))
    alunos_ano = rbind(alunos_ano,data.table(Ano=c(2009,2011,2013)[ind],N=0,prop=0))
  }
  alunos_ano = alunos_ano %>% arrange(Ano)
  df_amostras = data.frame()
  
  # Sexo masculino
  if(codigos_municipios[i,Masculino]){
    
    cod_escolas_aux = cod_escolas[Sexo == 'Masculino']
    
    # se não tiver alunos do sexo masculino
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              Sexo = 'Masculino',
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
                               sexo='Masculino'))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   Sexo = 'Masculino', Matematica = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  # Sexo feminino
  if(codigos_municipios[i,Feminino]){
    
    cod_escolas_aux = cod_escolas[Sexo == 'Feminino']
    
    # se não tiver alunos do sexo feminino
    if(nrow(cod_escolas_aux) == 0){
      cod_escolas_aux = data.table(data.frame(PK_COD_ENTIDADE = 0,
                                              Sexo = 'Feminino',
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
                              sexo='Feminino'))))
    
    amostras = sapply(medias, function(x) rnorm(1,x,sd_intercepto))
    
    df_amostras = rbind(df_amostras,
                        data.frame(ID_MUNICIPIO = codigos_municipios[i, ID_MUNICIPIO],
                                   Sexo = 'Feminino', Matematica = amostras, Medias = medias))
    
    rm(medias, amostras)
  }
  
  row.names(df_amostras) = 1:nrow(df_amostras)
  amostras_simuladas[[i]] = df_amostras
}

save(amostras_simuladas, codigos_municipios, tamanho_amostra, 
     file = paste0(path, '/simulacao/desigualdade_sexo/simulacao_sexo_5ano_mat_2013.Rdata'))