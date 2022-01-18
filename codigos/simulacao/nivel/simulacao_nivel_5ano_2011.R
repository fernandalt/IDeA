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

prop_nse_escola = prop_nse_escola_07.11_5ano
rm(prop_nse_escola_07.11_5ano, prop_nse_escola_09.13_5ano, prop_nse_escola_11.15_5ano, prop_nse_escola_13.17_5ano)

prop_raca_escola = prop_raca_escola[!(is.na(prop_branco) | is.na(prop_preto))]
prop_nse_escola = prop_nse_escola[!(is.na(proporcao_NSE1) | is.na(proporcao_NSE5))]
prop_sexo_escola = prop_sexo_escola[!(is.na(prop_masculino) | is.na(prop_feminino))]

# Carrega proporcao alunos por municipio
load(paste0(path,'/perfis_municipios_5ano_raca_sexo.Rdata'))
load(paste0(path,'/perfis_municipios_5ano_nse.Rdata'))

prop_nse_municipio = prop_nse_municipio_07.11_5ano
rm(prop_nse_municipio_07.11_5ano, prop_nse_municipio_09.13_5ano, prop_nse_municipio_11.15_5ano, prop_nse_municipio_13.17_5ano)

names(prop_sexo_municipio)[1] = "ID_MUNICIPIO"
names(prop_raca_municipio)[1] = "ID_MUNICIPIO"
names(prop_nse_municipio)[1] = "ID_MUNICIPIO"

# Identifica municipios com numero de alunos insuficiente
tamanho_amostra = dados_tudo[!is.na(Leitura) & !is.na(Matematica) & Ano %in% c(2007,2009,2011)
                             ,.N, .(ID_MUNICIPIO, ID_SERIE)]

codigos_municipios = tamanho_amostra[N<100 & ID_SERIE==5,ID_MUNICIPIO]

n_sim = quantile(tamanho_amostra[N<100 & ID_SERIE==5,N],0.25) - min(tamanho_amostra[N<100 & ID_SERIE==5,N])
n_sim = ceiling(n_sim)

# Define funcao de simulacao
retorna_valor_esperados = function(codigo_municipio,codigo_escola){
  
  # simulacoes por escola
  n_escola = cod_escolas[PK_COD_ENTIDADE==codigo_escola,n_imputacao] 
  
  # Verifica se ha proporcoes de alunos por municipio
  if(nrow(prop_nse_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  if(nrow(prop_raca_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  if(nrow(prop_sexo_municipio[ID_MUNICIPIO==codigo_municipio,c(2,3)])==0)
    return(NA)
  
  epsilon = 0.000001
  
  # sorteia NSE
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
  
  parcela_ano = c('2007' = 0,unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,
                                                                   which(colnames(efeito_aleatorio_final_Completo) %in% 
                                                                           c("as.factor.Ano.2009","as.factor.Ano.2011")),with=F]))[perfil_ano]
  
  # NSE Escola 
  parcela_escola = unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,'NSE_Escola' ]*dados_tudo[PK_COD_ENTIDADE==codigo_escola,NSE_Escola][1])
  
  # Valor esperado por tipo de aluno    
  return(unlist(efeito_aleatorio_final_Completo[COD_MUNICIPIO==codigo_municipio,'X.Intercept.'])+
           parcela_ano+parcela_NSE+parcela_raca+parcela_sexo+parcela_escola)
}

# Dados modelo LP
path = paste0(dirname(dirname(dirname(wd))), "/dados")
load(paste0(path,'/modelos/modelo_5ano_lp_2011.Rdata'))

sd_intercepto = VarCorr(modelo.Completo.5ano.lp.07.11)[[24]]
sd_intercepto = as.numeric(sd_intercepto)

efeito_aleatorio_final_Completo = read.csv2(paste0(path,'/correcao_nse/coeficientes_5Ano_lp_2011.csv'))
efeito_aleatorio_final_Completo = data.table(efeito_aleatorio_final_Completo)

# Executa simulacao LP
amostras_simuladas35 = list()
medias_perfis35 = list()

for(i in 1:length(codigos_municipios)){
  print(i)
  
  cod_escolas = dados_tudo[ID_MUNICIPIO==codigos_municipios[i] &  ID_SERIE==5 & !is.na(Leitura) & !is.na(Matematica)
                          & Ano %in% c(2007,2009,2011),.N,PK_COD_ENTIDADE]
  
  cod_escolas$prop = cod_escolas$N/sum(cod_escolas$N)
  
  total = min(n_sim, 100-sum(cod_escolas$N))
  
  cod_escolas$n_imputacao = round(total*cod_escolas$prop+0.5)
  
  alunos_ano = dados_tudo[ID_MUNICIPIO==codigos_municipios[i] &  ID_SERIE==5
                          & !is.na(Leitura) & Ano %in% c(2007,2009,2011),.N,Ano]
  
  alunos_ano$prop = alunos_ano$N/sum(alunos_ano$N)
  
  if(nrow(alunos_ano)<3){
    ind = which(is.na(match(c(2007,2009,2011),alunos_ano$Ano)))
    alunos_ano = rbind(alunos_ano,data.table(Ano=c(2007,2009,2011)[ind],N=0,prop=0))
  }
  
  medias_perfis35[[i]] = unlist(sapply(cod_escolas$PK_COD_ENTIDADE, function(x) retorna_valor_esperados (codigos_municipios[i] ,x)))
  amostras_simuladas35[[i]] = sapply(medias_perfis35[[i]], function(x) rnorm(1,x,sd_intercepto))
}

save(amostras_simuladas35, medias_perfis35,codigos_municipios, 
     file = paste0(path, '/simulacao/nivel/simulacao_nivel_5ano_lp_2011.Rdata'))

# Dados modelo MAT
load(paste0(path,'/modelos/modelo_5ano_mat_2011.Rdata'))

sd_intercepto = VarCorr(modelo.Completo.5ano.mat.07.11)[[24]]
sd_intercepto = as.numeric(sd_intercepto)

efeito_aleatorio_final_Completo = read.csv2(paste0(path,'/correcao_nse/coeficientes_5Ano_mat_2011.csv'))
efeito_aleatorio_final_Completo = data.table(efeito_aleatorio_final_Completo)

# Executa simulacao MAT
amostras_simuladas35=list()
medias_perfis35=list()

for(i in 1:length(codigos_municipios)){
  print(i)
  
  cod_escolas = dados_tudo[ID_MUNICIPIO==codigos_municipios[i] &  ID_SERIE==5 & !is.na(Leitura) & !is.na(Matematica) 
                          & Ano %in% c(2007,2009,2011),.N,PK_COD_ENTIDADE]
  
  cod_escolas$prop = cod_escolas$N/sum(cod_escolas$N)
  
  total = min(n_sim, 100-sum(cod_escolas$N))
  
  cod_escolas$n_imputacao = round(total*cod_escolas$prop+0.5)
  
  alunos_ano = dados_tudo[ID_MUNICIPIO==codigos_municipios[i] &  ID_SERIE==5
                         & !is.na(Matematica) & Ano %in% c(2007,2009,2011),.N,Ano]
  
  alunos_ano$prop = alunos_ano$N/sum(alunos_ano$N)
  
  if(nrow(alunos_ano)<3){
    ind = which(is.na(match(c(2007,2009,2011),alunos_ano$Ano)))
    alunos_ano = rbind(alunos_ano,data.table(Ano=c(2007,2009,2011)[ind],N=0,prop=0))
  }
  
  medias_perfis35[[i]] = unlist(sapply(cod_escolas$PK_COD_ENTIDADE, function(x) retorna_valor_esperados (codigos_municipios[i] ,x)))
  amostras_simuladas35[[i]] = sapply(medias_perfis35[[i]], function(x) rnorm(1,x,sd_intercepto))
}

save(amostras_simuladas35, medias_perfis35,codigos_municipios, 
     file = paste0(path, '/simulacao/nivel/simulacao_nivel_5ano_mat_2011.Rdata'))