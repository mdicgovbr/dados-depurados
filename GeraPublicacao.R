# Carregamento das bibliotecas necessarias para execução do código

  library(tidyverse)
  library(readxl)
  library(magrittr)

# Importacao dos arquivos

  vlr_total <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/01-Valor_Total-2017.csv', fileEncoding = 'UTF16LE',
             skip = 3, col.names = 'valor_total', sep = ',')

  qtde_vend_rvs_oper_17 <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/01-Qtde_Vendedores_RVS_Operações-2017.csv',
                                      fileEncoding = 'UTF16LE', skip = 3, sep = ',', col.names = c('qtde_vend_por_oper_rvs','qtde_rvs_dist_por_oper','qtde_oper_rvs'))




  serv_pos_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/02-Serviços(Posição)-2017-Valores.csv',
             fileEncoding = 'UTF16LE', skip = 3, col.names = c('cd_nbs','desc_nbs','vlr_dil_rvs (USD)'), sep = ',')

  serv_pos_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/02-Serviços(Posição)-2017-Vendedores.csv',
             fileEncoding = 'UTF16LE', skip = 3, col.names = c('cd_nbs','desc_nbs','qtde_vend'), sep = ',')




  pais_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/03-Países-2017-Valores.csv',
                         fileEncoding = 'UTF16LE', skip = 3,col.names = c('pais','','vlr_dil_rvs (USD)'),sep = ',', colClasses = c(NA,'NULL'))

  pais_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/03-Países-2017-Vendedores.csv',
                          fileEncoding = 'UTF16LE', skip = 3,col.names = c('pais','qtde_vend'),sep = ',')




  pais_serv_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/04-Países_Serviços(Posição)-2017-Valores.csv',
                              fileEncoding = 'UTF16LE', skip = 3,col.names = c('pais','cd_nbs','desc_nbs','','vlr_dil_rvs (USD)'),sep = ',', colClasses = c(NA,NA,NA,'NULL',NA))

  pais_serv_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/04-Países_Serviços(Posição)-2017-Vendedores.csv',
                               fileEncoding = 'UTF16LE', skip = 3,col.names = c('pais','cd_nbs','desc_nbs','','qtde_vend'),colClasses = c(NA,NA,NA,'NULL',NA),sep = ',')






  uf_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/05-UF-2017-Valores.csv',
                       fileEncoding = 'UTF16LE', skip = 3,col.names = c('uf','','vlr_dil_rvs(USD)'), colClasses = c(NA,'NULL',NA),sep = ',')

  uf_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/05-UF-2017-Vendedores.csv',
                        fileEncoding = 'UTF16LE', skip = 3,col.names = c('uf','','qtde_vend'),colClasses = c(NA,'NULL',NA),sep = ',')





  uf_serv_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/06-UF_Serviços(Posição)-2017-Valores.csv',
                            fileEncoding = 'UTF16LE', skip = 3,col.names = c('uf','cd_nbs','desc_nbs','','vlr_dil_rvs (USD)'), colClasses = c(NA,NA,NA,'NULL',NA),sep = ',')

  uf_serv_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/06-UF_Serviços(Posição)-2017-Vendedores.csv',
                             fileEncoding = 'UTF16LE', skip = 3,col.names = c('uf','cd_nbs','desc_nbs','qtde_vend'),sep = ',')





  uf_pais_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/07-UF_País-2017-Valores.csv',
                            fileEncoding = 'UTF16LE', skip = 3,col.names = c('uf','pais','','vlr_dil_rvs (USD)'),colClasses = c(NA,NA,'NULL',NA),sep = ',')

  uf_pais_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/07-UF_País-2017-Vendedores.csv',
                             fileEncoding = 'UTF16LE', skip = 3,col.names = c('uf','pais','','qtde_vend'),colClasses = c(NA,NA,'NULL',NA),sep = ',')



  modo_vlr <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/08-Modo-2017-Valores.csv',
                         fileEncoding = 'UTF16LE', skip = 3,col.names = c('modo_prest','','vlr_dil_rvs (RVS)'), colClasses = c(NA,'NULL',NA),sep = ',')

  modo_vend <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/publicacao/08-Modo-2017-Vendedores.csv',
                          fileEncoding = 'UTF16LE', skip = 3,col.names = c('modo_prest','','qtde_vend'), colClasses = c(NA,'NULL',NA),sep = ',')

# Manipulação dos arquivos

# Validacao do SIGILO

# valores totais

  dados <- t(qtde_vend_rvs_oper_17)
  dados_ <- t(vlr_total)

  aba_valores_totais <- rbind(dados,dados_)
  aba_valores_totais <- as.data.frame(aba_valores_totais)

  rm(dados,dados_,qtde_vend_rvs_oper_17,vlr_total)

# servicos - valores - vendedores

  serv_pos_vend <- serv_pos_vend %>%
                              filter(qtde_vend >= 4)

  aba_venda_servicos <- merge(serv_pos_vlr,serv_pos_vend,by = c('cd_nbs','desc_nbs'))

  rm(serv_pos_vend,serv_pos_vlr)

# paises - servicos - valores - vendedores

  pais_vend <- pais_vend %>%
                  filter(qtde_vend >= 4)

  aba_venda_paises <- merge(pais_vlr,pais_vend,by = 'pais')

  rm(pais_vend,pais_vlr)

# paises - valores - vendedores

  pais_serv_vend <- pais_serv_vend %>%
                        filter(qtde_vend >= 4)

  aba_paises_e_servicos <- merge(pais_serv_vlr,pais_serv_vend,by = c('pais','cd_nbs','desc_nbs'))

  rm(pais_serv_vlr,pais_serv_vend)

# uf - paises - valores - vendedores

  uf_vend <- uf_vend %>%
    filter(qtde_vend >= 4)

  aba_uf <- merge(uf_vlr,uf_vend,by = 'uf')

  rm(uf_vlr,uf_vend)

# uf - servicos - valores - vendedores

  uf_serv_vend <- uf_serv_vend %>%
                      filter(qtde_vend >= 4)

  aba_uf_e_servicos <- merge(uf_serv_vlr,uf_serv_vend,by = c('uf','cd_nbs','desc_nbs'))

  rm(uf_serv_vend,uf_serv_vlr)

# uf - valores - vendedores

  uf_pais_vend <- uf_pais_vend %>%
                    filter(qtde_vend >= 4)

  aba_pais_e_valor <- merge(uf_pais_vlr,uf_pais_vend,by = c('uf','pais'))

  rm(uf_pais_vend,uf_pais_vlr)

# modo - valores - vendedores

  modos <- merge(modo_vlr,modo_vend,by = 'modo_prest')

  rm(modo_vend,modo_vlr)

# Exclusao das variaveis de apoio

# Criar planilha de divulgacao
