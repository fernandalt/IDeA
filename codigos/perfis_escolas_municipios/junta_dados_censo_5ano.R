rm(list=ls(all=T))

## Pacotes
require(data.table)

## Base de dados
wd = getwd()
path = paste0(dirname(dirname(wd)), "/dados/perfis_escolas_municipios")

# Raca
load(paste0(path,"/Prop_raca_escola_co.Rdata"))
load(paste0(path,"/Prop_raca_escola_nordeste.Rdata"))
load(paste0(path,"/Prop_raca_escola_norte.Rdata"))
load(paste0(path,"/Prop_raca_escola_sudeste.Rdata"))
load(paste0(path,"/Prop_raca_escola_sul.Rdata"))

prop_raca_escola = rbind(prop_raca_escola_co,
                         prop_raca_escola_nordeste,
                         prop_raca_escola_norte,
                         prop_raca_escola_sudeste,
                         prop_raca_escola_sul)

# Sexo
load(paste0(path,"/Prop_sexo_escola_co.Rdata"))
load(paste0(path,"/Prop_sexo_escola_nordeste.Rdata"))
load(paste0(path,"/Prop_sexo_escola_norte.Rdata"))
load(paste0(path,"/Prop_sexo_escola_sudeste.Rdata"))
load(paste0(path,"/Prop_sexo_escola_sul.Rdata"))

prop_sexo_escola = rbind(prop_sexo_escola_co,
                         prop_sexo_escola_nordeste,
                         prop_sexo_escola_norte,
                         prop_sexo_escola_sudeste,
                         prop_sexo_escola_sul)


save(prop_raca_escola, prop_sexo_escola,
     file=paste0(path,'/perfis_escolas_5ano_raca_sexo.Rdata'))