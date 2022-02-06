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

# Raca
# Dados simulados
# MAT
load(paste0(path, "/dados/simulacao/desigualdade_raca/simulacao_raca_5ano_mat_2015.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                    Matematica=amostras_simuladas[[i]]$Matematica,
                                    Raça=amostras_simuladas[[i]]$Raça))
  
}

dados_mat5 = rbind(dados_tudo[(Ano %in% c(2011,2013,2015)) & ID_SERIE==5,
                              c('ID_MUNICIPIO','Matematica','Raça')],
                   dados_simulados)

#LP
load(paste0(path, "/dados/simulacao/desigualdade_raca/simulacao_raca_5ano_lp_2015.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Leitura=amostras_simuladas[[i]]$Leitura,
                                     Raça=amostras_simuladas[[i]]$Raça))
  
}

dados_lp5 = rbind(dados_tudo[(Ano %in% c(2011,2013,2015)) & ID_SERIE==5,
                              c('ID_MUNICIPIO','Leitura','Raça')],
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
desigualdade_raca_quinto_ano_mat = dados_mat5[,.(Desigualdade.Matematica=calculaKL(Matematica[Raça=='Preto'],
                                                                                 Matematica[Raça=='Branco']),
                                               nmatematica_preto=length(na.omit(Matematica[Raça=='Preto'])),
                                               nmatematica_branco=length(na.omit(Matematica[Raça=='Branco']))),
                                            by=ID_MUNICIPIO]

desigualdade_raca_quinto_ano_lp = dados_lp5[,.(Desigualdade.Leitura=calculaKL(Leitura[Raça=='Preto'],
                                                                            Leitura[Raça=='Branco']),
                                             nleitura_preto=length(na.omit(Leitura[Raça=='Preto'])),
                                             nleitura_branco=length(na.omit(Leitura[Raça=='Branco']))),
                                          by=ID_MUNICIPIO]

desigualdade_raca_quinto_ano_mat = desigualdade_raca_quinto_ano_mat[ID_MUNICIPIO<6000000]
desigualdade_raca_quinto_ano_lp = desigualdade_raca_quinto_ano_lp[ID_MUNICIPIO<6000000]

# Salva resultado
save(desigualdade_raca_quinto_ano_lp,desigualdade_raca_quinto_ano_mat,
     file = paste0(path, '/dados/kl/desigualdade_raca/kl_raca_5ano_2015.Rdata'))