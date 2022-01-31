rm(list=ls(all=T))

## Pacotes
require(data.table)
library(reldist)
require(sfsmisc)

## Base de dados
wd = getwd()
path = dirname(dirname(dirname(wd)))
load(paste0(path,"/dados/raw/DadosProvaBrasil_2007_2017.Rdata"))

colnames(dados_tudo)[4]<-'ID_MUNICIPIO'
dados_tudo<-dados_tudo[ID_MUNICIPIO<6000000]

# NSE
# Dados simulados
# MAT
load(paste0(path, "/dados/simulacao/desigualdade_nse/simulacao_nse_9ano_mat_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Matematica=amostras_simuladas[[i]]$Matematica,
                                     classificacao_NSE=amostras_simuladas[[i]]$NSE))
  
}

dados_mat9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                              c('ID_MUNICIPIO','Matematica','classificacao_NSE')],
                   dados_simulados)

#LP
load(paste0(path, "/dados/simulacao/desigualdade_nse/simulacao_nse_9ano_lp_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Leitura=amostras_simuladas[[i]]$Leitura,
                                     classificacao_NSE=amostras_simuladas[[i]]$NSE))
  
}

dados_lp9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                             c('ID_MUNICIPIO','Leitura','classificacao_NSE')],
                  dados_simulados)


# Define funcao que calcula a KL
calculaKL = function(proficiencia, referencia){ 
  
  if( sum(!is.na(proficiencia))>0 & sum(!is.na(referencia))>0){ 
    
    distribuicao_relativa = reldist(y= referencia, yo=na.omit(proficiencia), graph=FALSE)
    cdfgr = cumsum(distribuicao_relativa$y)/sum(distribuicao_relativa$y)
    area = integrate.xy(distribuicao_relativa$x,cdfgr)-.5
    
    return("Kl"=sign(area)*distribuicao_relativa$entropy)
  }
}

# Calcula KL
desigualdade_NSE_nono_ano_mat = dados_mat9[,.(Desigualdade.Matematica=calculaKL(Matematica[classificacao_NSE==1],
                                                                                Matematica[classificacao_NSE==5]),
                                              nmatematica_nse1=length(na.omit(Matematica[classificacao_NSE==1])),
                                              nmatematica_nse5=length(na.omit(Matematica[classificacao_NSE==5]))),
                                           by=ID_MUNICIPIO]

desigualdade_NSE_nono_ano_lp = dados_lp9[,.(Desigualdade.Leitura=calculaKL(Leitura[classificacao_NSE==1],
                                                                           Leitura[classificacao_NSE==5]),
                                            nmatematica_nse1=length(na.omit(Leitura[classificacao_NSE==1])),
                                            nmatematica_nse5=length(na.omit(Leitura[classificacao_NSE==5]))),
                                         by=ID_MUNICIPIO]

desigualdade_NSE_nono_ano_mat = desigualdade_NSE_nono_ano_mat[ID_MUNICIPIO<6000000]
desigualdade_NSE_nono_ano_lp = desigualdade_NSE_nono_ano_lp[ID_MUNICIPIO<6000000]

# Salva resultado
save(desigualdade_NSE_nono_ano_lp,desigualdade_NSE_nono_ano_mat,
     file = paste0(path, '/dados/kl/desigualdade_nse/kl_nse_9ano_2017.Rdata'))