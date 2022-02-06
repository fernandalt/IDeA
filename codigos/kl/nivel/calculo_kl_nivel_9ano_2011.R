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

# Dados simulados
# MAT
load(paste0(path, "/dados/simulacao/nivel/simulacao_nivel_9ano_mat_2011.Rdata"))

tamanho_amostras = sapply(amostras_simuladas35,length)
dados_simulados = data.table(ID_MUNICIPIO = rep(codigos_municipios,tamanho_amostras),
                             Matematica = unlist(amostras_simuladas35))

dados_mat9 = rbind(dados_tudo[Ano %in% c(2007,2009,2011) & ID_SERIE==9,
                              c('ID_MUNICIPIO','Matematica')],
                   dados_simulados)

#LP
load(paste0(path, "/dados/simulacao/nivel/simulacao_nivel_9ano_lp_2011.Rdata"))

tamanho_amostras = sapply(amostras_simuladas35,length)
dados_simulados = data.table(ID_MUNICIPIO = rep(codigos_municipios,tamanho_amostras),
                             Leitura = unlist(amostras_simuladas35))

dados_lp9 = rbind(dados_tudo[Ano %in% c(2007,2009,2011) & ID_SERIE==9,
                             c('ID_MUNICIPIO','Leitura')],
                  dados_simulados)


# Define funcao que calcula a KL
calculaKL = function(proficiencia, referencia){
  
  if( sum(!is.na(proficiencia))>0 & sum(!is.na(referencia))>0){  
    
    distribuicao_relativa = reldist(y=referencia, yo=na.omit(proficiencia), graph=FALSE)
    cdfgr = cumsum(distribuicao_relativa$y)/sum(distribuicao_relativa$y)
    area = integrate.xy(distribuicao_relativa$x,cdfgr)-.5
    
    return("Kl"=sign(area)*distribuicao_relativa$entropy)
  }
}

# Distribuicao de referencia
load(paste0(path, "/dados/kl/nivel/dist_referencia.Rdata"))

amostras_referencia = data.table(
  Leitura5 = Referencia.Leitura.Quinto,
  Leitura9 = Referencia.Leitura.Nono,
  Matematica5 = Referencia.Matematica.Quinto,
  Matematica9 = Referencia.Matematica.Nono)

# Calcula KL
kl_nono_ano_mat = dados_mat9[,.(KL.Matematica = calculaKL(Matematica,amostras_referencia$Matematica9),
                                nmatematica=length(na.omit(Matematica))),
                             by=ID_MUNICIPIO]

kl_nono_ano_lp = dados_lp9[,.(KL.Leitura = calculaKL(Leitura,amostras_referencia$Leitura9),
                              nleitura=length(na.omit(Leitura))),
                           by=ID_MUNICIPIO]

# Salva resultado
save(kl_nono_ano_lp,kl_nono_ano_mat,
     file = paste0(path, '/dados/kl/nivel/kl_nivel_9ano_2011.Rdata'))