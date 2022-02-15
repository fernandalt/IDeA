rm(list=ls(all=T))

## Pacotes
require(data.table)

## Base de dados
wd = getwd()
path = paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios")

# Raca
load(paste0(path,"/Prop_raca_escola_co_9ano.Rdata"))
load(paste0(path,"/Prop_raca_escola_nordeste_9ano.Rdata"))
load(paste0(path,"/Prop_raca_escola_norte_9ano.Rdata"))
load(paste0(path,"/Prop_raca_escola_sudeste_9ano.Rdata"))
load(paste0(path,"/Prop_raca_escola_sul_9ano.Rdata"))

prop_raca_escola = rbind(prop_raca_escola_co,
                         prop_raca_escola_nordeste,
                         prop_raca_escola_norte,
                         prop_raca_escola_sudeste,
                         prop_raca_escola_sul)

# Sexo
load(paste0(path,"/Prop_sexo_escola_co_9ano.Rdata"))
load(paste0(path,"/Prop_sexo_escola_nordeste_9ano.Rdata"))
load(paste0(path,"/Prop_sexo_escola_norte_9ano.Rdata"))
load(paste0(path,"/Prop_sexo_escola_sudeste_9ano.Rdata"))
load(paste0(path,"/Prop_sexo_escola_sul_9ano.Rdata"))

prop_sexo_escola = rbind(prop_sexo_escola_co,
                         prop_sexo_escola_nordeste,
                         prop_sexo_escola_norte,
                         prop_sexo_escola_sudeste,
                         prop_sexo_escola_sul)


save(prop_raca_escola, prop_sexo_escola,
     file=paste0(path,'/perfis_escolas_9ano_raca_sexo.Rdata'))