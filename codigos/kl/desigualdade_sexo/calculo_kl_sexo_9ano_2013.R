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

# Sexo
# Dados simulados
# MAT
load(paste0(path, "/dados/simulacao/desigualdade_sexo/simulacao_sexo_9ano_mat_2013.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Matematica=amostras_simuladas[[i]]$Matematica,
                                     Sexo=amostras_simuladas[[i]]$Sexo))
  
}

dados_mat9 = rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==9,
                              c('ID_MUNICIPIO','Matematica','Sexo')],
                   dados_simulados)

#LP
load(paste0(path, "/dados/simulacao/desigualdade_sexo/simulacao_sexo_9ano_lp_2013.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Leitura=amostras_simuladas[[i]]$Leitura,
                                     Sexo=amostras_simuladas[[i]]$Sexo))
  
}

dados_lp9 = rbind(dados_tudo[(Ano %in% c(2009,2011,2013)) & ID_SERIE==9,
                             c('ID_MUNICIPIO','Leitura','Sexo')],
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
desigualdade_sexo_nono_ano_mat = dados_mat9[,.(Desigualdade.Matematica=calculaKL(Matematica[Sexo=='Feminino'],
                                                                                 Matematica[Sexo=='Masculino']),
                                               nmatematica_feminino=length(na.omit(Matematica[Sexo=='Feminino'])),
                                               nmatematica_masculino=length(na.omit(Matematica[Sexo=='Masculino']))),
                                            by=ID_MUNICIPIO]

desigualdade_sexo_nono_ano_lp = dados_lp9[,.(Desigualdade.Leitura=calculaKL(Leitura[Sexo=='Feminino'],
                                                                            Leitura[Sexo=='Masculino']),
                                             nleitura_feminino=length(na.omit(Leitura[Sexo=='Feminino'])),
                                             nleitura_masculino=length(na.omit(Leitura[Sexo=='Masculino']))),
                                          by=ID_MUNICIPIO]

desigualdade_sexo_nono_ano_mat = desigualdade_sexo_nono_ano_mat[ID_MUNICIPIO<6000000]
desigualdade_sexo_nono_ano_lp = desigualdade_sexo_nono_ano_lp[ID_MUNICIPIO<6000000]

# Salva resultado
save(desigualdade_sexo_nono_ano_lp,desigualdade_sexo_nono_ano_mat,
     file = paste0(path, '/dados/kl/desigualdade_sexo/kl_sexo_9ano_2013.Rdata'))