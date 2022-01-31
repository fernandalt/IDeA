rm(list=ls(all=T))

## Pacotes
require(data.table)

## Base de dados
wd = getwd()
path = dirname(dirname(dirname(wd)))
load(paste0(path,"/dados/raw/DadosProvaBrasil_2007_2017.Rdata"))

colnames(dados_tudo)[4] = 'ID_MUNICIPIO'
dados_tudo = dados_tudo[ID_MUNICIPIO<6000000]


# Dados simulados raca
# MAT
load(paste0(path, "/dados/simulacao/desigualdade_raca/simulacao_raca_9ano_mat_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Matematica=amostras_simuladas[[i]]$Matematica,
                                     Raça=amostras_simuladas[[i]]$Raça))
  
}

dados_raca_mat9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                              c('ID_MUNICIPIO','Matematica','Raça')],
                   dados_simulados)

#LP
load(paste0(path, "/dados/simulacao/desigualdade_raca/simulacao_raca_9ano_lp_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Leitura=amostras_simuladas[[i]]$Leitura,
                                     Raça=amostras_simuladas[[i]]$Raça))
  
}

dados_raca_lp9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                             c('ID_MUNICIPIO','Leitura','Raça')],
                  dados_simulados)

# Dados simulados NSE
# MAT
load(paste0(path, "/dados/simulacao/desigualdade_nse/simulacao_nse_9ano_mat_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Matematica=amostras_simuladas[[i]]$Matematica,
                                     classificacao_NSE=amostras_simuladas[[i]]$NSE))
  
}

dados_nse_mat9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
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

dados_nse_lp9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                             c('ID_MUNICIPIO','Leitura','classificacao_NSE')],
                  dados_simulados)

# Dados simulados sexo
# MAT
load(paste0(path, "/dados/simulacao/desigualdade_sexo/simulacao_sexo_9ano_mat_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Matematica=amostras_simuladas[[i]]$Matematica,
                                     Sexo=amostras_simuladas[[i]]$Sexo))
  
}

dados_sexo_mat9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                              c('ID_MUNICIPIO','Matematica','Sexo')],
                   dados_simulados)

#LP
load(paste0(path, "/dados/simulacao/desigualdade_sexo/simulacao_sexo_9ano_lp_2017.Rdata"))

dados_simulados = data.table()
for(i in 1:length(amostras_simuladas)){
  
  dados_simulados = rbind(dados_simulados,
                          data.table(ID_MUNICIPIO=codigos_municipios[i,ID_MUNICIPIO],
                                     Leitura=amostras_simuladas[[i]]$Leitura,
                                     Sexo=amostras_simuladas[[i]]$Sexo))
  
}

dados_sexo_lp9 = rbind(dados_tudo[(Ano %in% c(2013,2015,2017)) & ID_SERIE==9,
                             c('ID_MUNICIPIO','Leitura','Sexo')],
                  dados_simulados)

# Calcula media por grupos
# Raca
resumo_raca_mat9 = dados_raca_mat9[,.(
  MediaMatematica_preto=mean(Matematica[Raça=='Preto'],na.rm=T),
  MediaMatematica_branco=mean(Matematica[Raça=='Branco'],na.rm=T),   
  nmatematica_preto=length(na.omit(Matematica[Raça=='Preto'])),
  nmatematica_branco=length(na.omit(Matematica[Raça=='Branco']))),
  by="ID_MUNICIPIO"]

resumo_raca_lp9 = dados_raca_lp9[,.(
  MediaLeitura_preto=mean(Leitura[Raça=='Preto'],na.rm=T),
  MediaLeitura_branco=mean(Leitura[Raça=='Branco'],na.rm=T),   
  nleitura_preto=length(na.omit(Leitura[Raça=='Preto'])),
  nleitura_branco=length(na.omit(Leitura[Raça=='Branco']))),
  by="ID_MUNICIPIO"]

# NSE
resumo_nse_mat9 = dados_nse_mat9[,.(
  MediaMatematica_NSE1=mean(Matematica[classificacao_NSE=='1'],na.rm=T),
  MediaMatematica_NSE5=mean(Matematica[classificacao_NSE=='5'],na.rm=T),   
  nmatematica_NSE1=length(na.omit(Matematica[classificacao_NSE=='1'])),
  nmatematica_NSE5=length(na.omit(Matematica[classificacao_NSE=='5']))),
  by="ID_MUNICIPIO"]

resumo_nse_lp9 = dados_nse_lp9 [,.(
  MediaLeitura_NSE1=mean(Leitura[classificacao_NSE=='1'],na.rm=T),
  MediaLeitura_NSE5=mean(Leitura[classificacao_NSE=='5'],na.rm=T),   
  nleitura_NSE1=length(na.omit(Leitura[classificacao_NSE=='1'])),
  nleitura_NSE5=length(na.omit(Leitura[classificacao_NSE=='5']))),
  by="ID_MUNICIPIO"]

# Sexo
resumo_sexo_mat9 = dados_sexo_mat9[,.(
  MediaMatematica_feminino=mean(Matematica[Sexo=='Feminino'],na.rm=T),
  MediaMatematica_masculino=mean(Matematica[Sexo=='Masculino'],na.rm=T),   
  nmatematica_feminino=length(na.omit(Matematica[Sexo=='Feminino'])),
  nmatematica_masculino=length(na.omit(Matematica[Sexo=='Masculino']))),
  by="ID_MUNICIPIO"]

resumo_sexo_lp9 = dados_sexo_lp9[,.(
  MediaLeitura_feminino=mean(Leitura[Sexo=='Feminino'],na.rm=T),
  MediaLeitura_masculino=mean(Leitura[Sexo=='Masculino'],na.rm=T),   
  nleitura_feminino=length(na.omit(Leitura[Sexo=='Feminino'])),
  nleitura_masculino=length(na.omit(Leitura[Sexo=='Masculino']))),
  by="ID_MUNICIPIO"]


# Carrega dados KL
# Raca
load(paste0(path, '/dados/kl/desigualdade_raca/kl_raca_9ano_2017.Rdata'))
# NSE
load(paste0(path, '/dados/kl/desigualdade_nse/kl_nse_9ano_2017.Rdata'))
# Sexo
load(paste0(path, '/dados/kl/desigualdade_sexo/kl_sexo_9ano_2017.Rdata'))


# Junta os dados
colnames(desigualdade_raca_nono_ano_lp)[2] = "Desigualdade.Leitura_raca"
colnames(desigualdade_raca_nono_ano_mat)[2] = "Desigualdade.Matematica_raca"

colnames(desigualdade_NSE_nono_ano_lp)[2] = "Desigualdade.Leitura_NSE"
colnames(desigualdade_NSE_nono_ano_mat)[2] = "Desigualdade.Matematica_NSE"

colnames(desigualdade_sexo_nono_ano_lp)[2] = "Desigualdade.Leitura_sexo"
colnames(desigualdade_sexo_nono_ano_mat)[2] = "Desigualdade.Matematica_sexo"

dados_kl = merge(desigualdade_raca_nono_ano_lp, desigualdade_raca_nono_ano_mat,by='ID_MUNICIPIO')
dados_kl = merge(dados_kl,desigualdade_NSE_nono_ano_lp,by='ID_MUNICIPIO')
dados_kl = merge(dados_kl,desigualdade_NSE_nono_ano_mat,by='ID_MUNICIPIO')
dados_kl = merge(dados_kl,desigualdade_sexo_nono_ano_lp,by='ID_MUNICIPIO')
dados_kl = merge(dados_kl,desigualdade_sexo_nono_ano_mat,by='ID_MUNICIPIO')

resumo_9ano = merge(resumo_raca_lp9,dados_kl,by='ID_MUNICIPIO')
resumo_9ano = merge(resumo_9ano,resumo_raca_mat9,by='ID_MUNICIPIO')
resumo_9ano = merge(resumo_9ano,resumo_nse_lp9,by='ID_MUNICIPIO')
resumo_9ano = merge(resumo_9ano,resumo_nse_mat9,by='ID_MUNICIPIO')
resumo_9ano = merge(resumo_9ano,resumo_sexo_lp9,by='ID_MUNICIPIO')
resumo_9ano = merge(resumo_9ano,resumo_sexo_mat9,by='ID_MUNICIPIO')


# Calcula diferenca de medias
resumo_9ano$Diferenca_matematica_raca = abs(resumo_9ano$MediaMatematica_branco-
                                              resumo_9ano$MediaMatematica_preto)

resumo_9ano$Diferenca_leitura_raca = abs(resumo_9ano$MediaLeitura_branco-
                                             resumo_9ano$MediaLeitura_preto)

resumo_9ano$Diferenca_matematica_NSE = abs(resumo_9ano$MediaMatematica_NSE5-
                                             resumo_9ano$MediaMatematica_NSE1)

resumo_9ano$Diferenca_leitura_NSE = abs(resumo_9ano$MediaLeitura_NSE5-
                                            resumo_9ano$MediaLeitura_NSE1)

resumo_9ano$Diferenca_matematica_sexo = abs(resumo_9ano$MediaMatematica_masculino-
                                              resumo_9ano$MediaMatematica_feminino)

resumo_9ano$Diferenca_leitura_sexo = abs(resumo_9ano$MediaLeitura_masculino-
                                           resumo_9ano$MediaLeitura_feminino)

# Limite equidade - diferenca de media maior que 5
# NSE
# MAT
pontos_corte_nse = c(resumo_9ano[abs(Diferenca_matematica_NSE)>5
                                 & Desigualdade.Matematica_NSE<0,
                                 quantile(Desigualdade.Matematica_NSE,
                                          c(0,0.05,0.25,0.5,0.75,.95))])

corte_nse_mat_nono = pontos_corte_nse[6]

# LP
pontos_corte_nse = c(resumo_9ano[abs(Diferenca_leitura_NSE)>5
                                 & Desigualdade.Leitura_NSE<0,
                                 quantile(Desigualdade.Leitura_NSE,
                                          c(0,0.05,0.25,0.5,0.75,.95))])

corte_nse_lp_nono = pontos_corte_nse[6]


# Salva pontos de corte
save(corte_nse_lp_nono,corte_nse_mat_nono,
     file=paste0(path,"/dados/faixas_kl/desigualdade/ponto_corte_faixa_9ano.Rdata"))