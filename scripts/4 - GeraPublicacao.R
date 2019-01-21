# GERA PANILHA FORMATADA PARA DIVULGACAO DOS DADOS

# Carregamento das bibliotecas necessarias para execuÃ§Ã£o do cÃ³digo

  library(tidyverse)
  library(readxl)
  library(magrittr)
  library(xlsx)

# Importacao dos arquivos

  vlr_total <- read_xlsx('dados_publicacao/01-Valor_Total-2017.xlsx', col_names = 'VALOR_USD')
  
  qtde_vend_rvs_oper_17 <- read_xlsx(path = 'dados_publicacao/01-Qtde_Vendedores_RVS_Operações-2017.xlsx',col_names = c('QTDE_VENDEDORES','QTDE_RVS','QTDE_OPER'))
  
  serv_pos_vlr <- read_xlsx('dados_publicacao/02-Serviços(Posição)-2017-Valores.xlsx',col_names = c('CODIGO NBS','DESCRICAO NBS','VALOR_USD'),skip = 1)
  
  serv_pos_vend <- read_xlsx('dados_publicacao/02-Serviços(Posição)-2017-Vendedores.xlsx',col_names = c('CODIGO NBS','DESCRICAO NBS','QTDE_VENDEDORES'),skip = 1)
  
  pais_vlr <- read_xlsx('dados_publicacao/03-Países-2017-Valores.xlsx',col_names = c('PAIS_ADQUIRENTE','VALOR_USD'),skip = 1)
  
  pais_vend <- read_xlsx('dados_publicacao/03-Países-2017-Vendedores.xlsx',col_names = c('PAIS_ADQUIRENTE','QTDE_VENDEDORES'),skip = 1)
  
  pais_serv_vlr <- read_xlsx('dados_publicacao/04-Países_Serviços(Posição)-2017-Valores.xlsx',col_names = c('PAIS_ADQUIRENTE','CODIGO NBS','DESCRICAO NBS','VALOR_USD')
                             ,skip = 1)
  
  pais_serv_vend <- read_xlsx('dados_publicacao/04-Países_Serviços(Posição)-2017-Vendedores.xlsx',col_names = c('PAIS_ADQUIRENTE','CODIGO NBS','DESCRICAO NBS','QTDE_VENDEDORES')
                              ,skip = 1)
  
  uf_vlr <- read_xlsx('dados_publicacao/05-UF-2017-Valores.xlsx',col_names = c('UF','VALOR_USD'),skip = 1)
  
  uf_vend <- read_xlsx('dados_publicacao/05-UF-2017-Vendedores.xlsx',col_names = c('UF','QTDE_VENDEDORES'),skip = 1)
  
  uf_serv_vlr <- read_xlsx('dados_publicacao/06-UF_Serviços(Posição)-2017-Valores.xlsx',col_names = c('UF','CODIGO NBS','DESCRICAO NBS','VALOR_USD'),skip = 1)
  
  uf_serv_vend <- read_xlsx('dados_publicacao/06-UF_Serviços(Posição)-2017-Vendedores.xlsx',col_names = c('UF','CODIGO NBS','DESCRICAO NBS','QTDE_VENDEDORES'),skip = 1)
  
  uf_pais_vlr <- read_xlsx('dados_publicacao/07-UF_País-2017-Valores.xlsx',col_names = c('UF','PAIS_ADQUIRENTE','VALOR_USD'),skip = 1)
  
  uf_pais_vend <- read_xlsx('dados_publicacao/07-UF_País-2017-Vendedores.xlsx',col_names = c('UF','PAIS_ADQUIRENTE','QTDE_VENDEDORES'),skip = 1)
  
  modo_vlr <- read_xlsx('dados_publicacao/08-Modo-2017-Valores.xlsx',col_names = TRUE)
  
  modo_vlr <- modo_vlr %>% separate(`Operação - Modo de Prestação`,c('CODIGO MODO','DESCRICAO MODO'),sep = '-')
  
  colnames(modo_vlr)[3] <- 'VALOR_USD'
  
  modo_vend <- read_xlsx('dados_publicacao/08-Modo-2017-Vendedores.xlsx',col_names = TRUE)
  
  modo_vend <- modo_vend %>% separate(`Operação - Modo de Prestação`,c('CODIGO MODO','DESCRICAO MODO'),sep = '-')
  
  colnames(modo_vend)[3] <- 'QTDE_VENDEDORES' 

# Manipulacao dos arquivos(Validacao do SIGILO)
  
# VALORES TOTAIS

  venda_valores_totais <- cbind(qtde_vend_rvs_oper_17,vlr_total)
  
  venda_valores_totais <- t(venda_valores_totais)
  
  venda_valores_totais <- as.data.frame(venda_valores_totais)
  
  names(venda_valores_totais) <- c('Indicadores','Valores')
  
rm(qtde_vend_rvs_oper_17,vlr_total)

# SERVICOS - VALORES - VENDEDORES

  venda_servicos <- merge(serv_pos_vlr,serv_pos_vend,by = c('CODIGO NBS','DESCRICAO NBS'))
  
  sigilo <- venda_servicos %>%
    filter( venda_servicos$QTDE_VENDEDORES <= 3)
  
  sigilo <- sum(sigilo$VALOR_USD)
  
  sigilo <- as.data.frame(sigilo)
  
  sigilo$Outros_filtro_sigilo_consolidado = 'Outros_filtro_sigilo_consolidado'
  
  sigilo$valor <- NA
  
  sigilo$vend <- NA
  
  sigilo <- sigilo[,c(2,1,3,4)]
  
  names(sigilo) <- c('CODIGO NBS','DESCRICAO NBS','VALOR_USD','QTDE_VENDEDORES')
  
  venda_servicos <- venda_servicos %>%
    filter(venda_servicos$QTDE_VENDEDORES >= 4)
  
  venda_servicos <- rbind(venda_servicos,sigilo)
  
  rm(serv_pos_vend,serv_pos_vlr,sigilo)

# PAISES - VALORES - VENDEDORES

  venda_paises <- merge(pais_vlr,pais_vend,by = 'PAIS_ADQUIRENTE')
  
  sigilo <- venda_paises %>%
    filter(venda_paises$QTDE_VENDEDORES <= 3)
  
  sigilo <- sum(sigilo$VALOR_USD)
  
  sigilo <- as.data.frame(sigilo)
  
  sigilo$Outros_filtro_sigilo_consolidado = 'Outros_filtro_sigilo_consolidado'
  
  sigilo$valor <- NA
  
  sigilo <- sigilo[,c(2,1,3)]
  
  names(sigilo) <- c('PAIS_ADQUIRENTE','VALOR_USD','QTDE_VENDEDORES')
  
  venda_paises <- venda_paises %>%
    filter(venda_paises$QTDE_VENDEDORES >= 4)
  
  venda_paises <- rbind(venda_paises,sigilo)
  
  rm(pais_vend,pais_vlr,sigilo)

# PAISES - SERVICOS - VALORES - VENDEDORES

  venda_paises_e_servicos <- merge(pais_serv_vlr,pais_serv_vend,by = c('PAIS_ADQUIRENTE','CODIGO NBS','DESCRICAO NBS'))
  
  sigilo <- venda_paises_e_servicos %>%
    filter(venda_paises_e_servicos$QTDE_VENDEDORES <= 3)
  
  sigilo <- sum(sigilo$VALOR_USD)
  
  sigilo <- as.data.frame(sigilo)
  
  sigilo$Outros_filtro_sigilo_consolidado = 'Outros_filtro_sigilo_consolidado'
  
  sigilo$valor <- NA
  
  sigilo$a <- NA
  
  sigilo$r <- NA
  
  sigilo <- sigilo[,c(2,1,3,4,5)]
  
  names(sigilo) <- c('PAIS_ADQUIRENTE','CODIGO NBS','DESCRICAO NBS','VALOR_USD','QTDE_VENDEDORES')
  
  venda_paises_e_servicos <- venda_paises_e_servicos %>%
    filter(venda_paises_e_servicos$QTDE_VENDEDORES >= 4)
  
  venda_paises_e_servicos <- rbind(venda_paises_e_servicos,sigilo)
  
  rm(pais_serv_vend,pais_serv_vlr,sigilo)

# UF - VALORES - VENDEDORES

  venda_uf <- merge(uf_vlr,uf_vend,by = 'UF')
  
  sigilo <- venda_uf %>%
    filter(venda_uf$QTDE_VENDEDORES <= 3)
  
  sigilo <- sum(sigilo$VALOR_USD)
  
  sigilo <- as.data.frame(sigilo)
  
  sigilo$Outros_filtro_sigilo_consolidado = 'Outros_filtro_sigilo_consolidado'
  
  sigilo$valor <- NA
  
  sigilo <- sigilo[,c(2,1,3)]
  
  names(sigilo) <- c('UF','VALOR_USD','QTDE_VENDEDORES')

  venda_uf <- venda_uf %>%
    filter(venda_uf$QTDE_VENDEDORES >= 4)
  
  venda_uf <- rbind(venda_uf,sigilo)

  rm(uf_vlr,uf_vend,sigilo)

# UF - SERVICOS - VALORES - VENDEDORES

  venda_uf_servicos <- merge(uf_serv_vlr,uf_serv_vend,by = c('UF','CODIGO NBS','DESCRICAO NBS'))
  
  sigilo <- venda_uf_servicos %>%
    filter(venda_uf_servicos$QTDE_VENDEDORES <= 3)
  
  sigilo <- sum(sigilo$VALOR_USD)
  
  sigilo <- as.data.frame(sigilo)
  
  sigilo$Outros_filtro_sigilo_consolidado = 'Outros_filtro_sigilo_consolidado'

  sigilo$valor <- NA
  
  sigilo$v <- NA
  
  sigilo$va <- NA
  
  sigilo <- sigilo[,c(2,1,3,4,5)]
  
  names(sigilo) <- c('UF','CODIGO NBS','DESCRICAO NBS','VALOR_USD','QTDE_VENDEDORES')
  
  venda_uf_servicos <- venda_uf_servicos %>% 
    filter(venda_uf_servicos$QTDE_VENDEDORES >=4)
  
  venda_uf_servicos <- rbind(venda_uf_servicos,sigilo)
  
  rm(uf_serv_vend,uf_serv_vlr,sigilo)

# UF - PAISES - VALORES - VENDEDORES

  venda_uf_pais <- merge(uf_pais_vlr,uf_pais_vend,by = c('UF','PAIS_ADQUIRENTE'))

  sigilo <- venda_uf_pais %>%
    filter(venda_uf_pais$QTDE_VENDEDORES <= 3)
  
  sigilo <- sum(sigilo$VALOR_USD)
  
  sigilo <- as.data.frame(sigilo)
  
  sigilo$Outros_filtro_sigilo_consolidado = 'Outros_filtro_sigilo_consolidado'
  
  sigilo$valor <- NA
  
  sigilo$val <- NA
  
  sigilo <- sigilo[,c(2,1,3,4)]
  
  names(sigilo) <- c('UF','PAIS_ADQUIRENTE','VALOR_USD','QTDE_VENDEDORES')

  venda_uf_pais <- venda_uf_pais %>% 
    filter(venda_uf_pais$QTDE_VENDEDORES >= 4)

  venda_uf_pais <- rbind(venda_uf_pais,sigilo)  

  rm(uf_pais_vend,uf_pais_vlr,sigilo)

# modo - valores - vendedores

  venda_modos <- merge(modo_vlr,modo_vend,by = c('CODIGO MODO','DESCRICAO MODO'))

  rm(modo_vend,modo_vlr)

# Criar planilha de divulgacao

  wb = createWorkbook()
  
  sheet = createSheet(wb,'1.Venda-Valores Totais')
  
  addDataFrame(venda_valores_totais,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'2.Venda-Serviços')
  
  addDataFrame(venda_servicos,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'3.Venda-Países')
  
  addDataFrame(venda_paises,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'4.Venda-Países_&_Serviços')
  
  addDataFrame(venda_paises_e_servicos,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'5.Venda-UF')
  
  addDataFrame(venda_uf,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'6.Venda-UF_&_Serviços')
  
  addDataFrame(venda_uf_servicos,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'7.Venda-UF_&_Países')
  
  addDataFrame(venda_uf_pais,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  sheet = createSheet(wb,'8.Venda-Modos')
  
  addDataFrame(venda_modos,sheet = sheet,startColumn = 1,row.names = FALSE)
  
  saveWorkbook(wb,"divulgacao.xlsx")
  
  
