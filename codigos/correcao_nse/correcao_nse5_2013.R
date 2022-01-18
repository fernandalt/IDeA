rm(list=ls(all=T))

## Pacotes
require(data.table)
library("edfun")
require(dplyr)

## Base de dados
wd = getwd()
path = paste0(dirname(dirname(wd)), "/dados")

#5 ANO
# carrega coeficientes do modelo para LP
load(paste0(path, '/modelos/modelo_5ano_lp_2013.Rdata'))

efeito_aleatorio_final_Completo = data.table(coefficients(modelo.Completo.5ano.lp.09.13))
efeito_aleatorio_final_Completo$COD_MUNICIPIO = row.names(coefficients(modelo.Completo.5ano.lp.09.13))

efeito_positivo = efeito_aleatorio_final_Completo$classificacao_NSE5[efeito_aleatorio_final_Completo$classificacao_NSE5 > 0]

# distribuicao empirica dos efeitos positivos
x_dist = edfun(efeito_positivo)
f = x_dist$qfun

step = 1/(nrow(efeito_aleatorio_final_Completo)-1)
x1 = seq(0, 1, by= step)

# associa os novos efeitos de NSE 5 aos municípios na ordem dos efeitos originais
aux1 = efeito_aleatorio_final_Completo %>% select(COD_MUNICIPIO, classificacao_NSE5) %>% 
  arrange(classificacao_NSE5) %>% mutate(quantil = x1, NSE5_Novo = f(x1))

efeito_aleatorio_final_Completo = full_join(efeito_aleatorio_final_Completo,
                                            aux1[,c('COD_MUNICIPIO','NSE5_Novo')],
                                            by = 'COD_MUNICIPIO')

fwrite(efeito_aleatorio_final_Completo[,c(15,1:13,16,14)],
       file=paste0(path, '/correcao_nse/coeficientes_5Ano_lp_2013.csv'),
       dec=',',sep=';',row.names = F)


# carrega coeficientes do modelo para MAT
load(paste0(path, '/modelos/modelo_5ano_mat_2013.Rdata'))

efeito_aleatorio_final_Completo = data.table(coefficients(modelo.Completo.5ano.mat.09.13))
efeito_aleatorio_final_Completo$COD_MUNICIPIO = row.names(coefficients(modelo.Completo.5ano.mat.09.13))

efeito_positivo = efeito_aleatorio_final_Completo$classificacao_NSE5[efeito_aleatorio_final_Completo$classificacao_NSE5 > 0]

# distribuicao empirica dos efeitos positivos
x_dist = edfun(efeito_positivo)
f = x_dist$qfun

step = 1/(nrow(efeito_aleatorio_final_Completo)-1)
x1 = seq(0, 1, by= step)

# associa os novos efeitos de NSE 5 aos municípios na ordem dos efeitos originais
aux1 = efeito_aleatorio_final_Completo %>% select(COD_MUNICIPIO, classificacao_NSE5) %>% 
  arrange(classificacao_NSE5) %>% mutate(quantil = x1, NSE5_Novo = f(x1))

efeito_aleatorio_final_Completo = full_join(efeito_aleatorio_final_Completo,
                                            aux1[,c('COD_MUNICIPIO','NSE5_Novo')],
                                            by = 'COD_MUNICIPIO')

fwrite(efeito_aleatorio_final_Completo[,c(15,1:13,16,14)],
       file=paste0(path, '/correcao_nse/coeficientes_5Ano_mat_2013.csv'),
       dec=',',sep=';',row.names = F)


#9 ANO
# carrega coeficientes do modelo para LP
load(paste0(path, '/modelos/modelo_9ano_lp_2013.Rdata'))

efeito_aleatorio_final_Completo = data.table(coefficients(modelo.Completo.9ano.lp.09.13))
efeito_aleatorio_final_Completo$COD_MUNICIPIO = row.names(coefficients(modelo.Completo.9ano.lp.09.13))

efeito_positivo = efeito_aleatorio_final_Completo$classificacao_NSE5[efeito_aleatorio_final_Completo$classificacao_NSE5 > 0]

# distribuicao empirica dos efeitos positivos
x_dist = edfun(efeito_positivo)
f = x_dist$qfun

step = 1/(nrow(efeito_aleatorio_final_Completo)-1)
x1 = seq(0, 1, by= step)

# associa os novos efeitos de NSE 5 aos municípios na ordem dos efeitos originais
aux1 = efeito_aleatorio_final_Completo %>% select(COD_MUNICIPIO, classificacao_NSE5) %>% 
  arrange(classificacao_NSE5) %>% mutate(quantil = x1, NSE5_Novo = f(x1))

efeito_aleatorio_final_Completo = full_join(efeito_aleatorio_final_Completo,
                                            aux1[,c('COD_MUNICIPIO','NSE5_Novo')],
                                            by = 'COD_MUNICIPIO')

fwrite(efeito_aleatorio_final_Completo[,c(15,1:13,16,14)],
       file=paste0(path, '/correcao_nse/coeficientes_9Ano_lp_2013.csv'),
       dec=',',sep=';',row.names = F)


# carrega coeficientes do modelo para MAT
load(paste0(path, '/modelos/modelo_9ano_mat_2013.Rdata'))

efeito_aleatorio_final_Completo = data.table(coefficients(modelo.Completo.9ano.mat.09.13))
efeito_aleatorio_final_Completo$COD_MUNICIPIO<-row.names(coefficients(modelo.Completo.9ano.mat.09.13))

efeito_positivo = efeito_aleatorio_final_Completo$classificacao_NSE5[efeito_aleatorio_final_Completo$classificacao_NSE5 > 0]

# distribuicao empirica dos efeitos positivos
x_dist = edfun(efeito_positivo)
f = x_dist$qfun

step = 1/(nrow(efeito_aleatorio_final_Completo)-1)
x1 = seq(0, 1, by= step)

# associa os novos efeitos de NSE 5 aos municípios na ordem dos efeitos originais
aux1 = efeito_aleatorio_final_Completo %>% select(COD_MUNICIPIO, classificacao_NSE5) %>% 
  arrange(classificacao_NSE5) %>% mutate(quantil = x1, NSE5_Novo = f(x1))

efeito_aleatorio_final_Completo = full_join(efeito_aleatorio_final_Completo,
                                            aux1[,c('COD_MUNICIPIO','NSE5_Novo')],
                                            by = 'COD_MUNICIPIO')

fwrite(efeito_aleatorio_final_Completo[,c(15,1:13,16,14)],
       file=paste0(path, '/correcao_nse/coeficientes_9Ano_mat_2013.csv'),
       dec=',',sep=';',row.names = F)