# GERA PANILHA FORMATADA PARA DIVULGACAO DOS DADOS

# Carregamento das bibliotecas necessarias para execuÃ§Ã£o do cÃ³digo

  library(tidyverse)
  library(readxl)
  library(magrittr)

# Importacao dos arquivos

  vlr_total <- read_xlsx('dados_publicacao/01-Valor_Total-2017.xlsx', col_names = TRUE)

  qtde_vend_rvs_oper_17 <- read_xlsx(path = 'dados_publicacao/01-Qtde_Vendedores_RVS_Operações-2017.xlsx',col_names = TRUE)

  serv_pos_vlr <- read_xlsx('dados_publicacao/02-Serviços(Posição)-2017-Valores.xlsx',col_names = TRUE)

  serv_pos_vend <- read_xlsx('dados_publicacao/02-Serviços(Posição)-2017-Vendedores.xlsx',col_names = TRUE)

  pais_vlr <- read_xlsx('dados_publicacao/03-Países-2017-Valores.xlsx',col_names = TRUE)

  pais_vend <- read_xlsx('dados_publicacao/03-Países-2017-Vendedores.xlsx',col_names = TRUE)

  pais_serv_vlr <- read_xlsx('dados_publicacao/04-Países_Serviços(Posição)-2017-Valores.xlsx',col_names = TRUE)

  pais_serv_vend <- read_xlsx('dados_publicacao/04-Países_Serviços(Posição)-2017-Vendedores.xlsx')

  uf_vlr <- read_xlsx('dados_publicacao/05-UF-2017-Valores.xlsx',col_names = TRUE)

  uf_vend <- read_xlsx('dados_publicacao/05-UF-2017-Vendedores.xlsx',col_names = TRUE)

  uf_serv_vlr <- read_xlsx('dados_publicacao/06-UF_Serviços(Posição)-2017-Valores.xlsx',col_names = TRUE)

  uf_serv_vend <- read_xlsx('dados_publicacao/06-UF_Serviços(Posição)-2017-Vendedores.xlsx',col_names = TRUE)

  uf_pais_vlr <- read_xlsx('dados_publicacao/07-UF_País-2017-Valores.xlsx',col_names = TRUE)

  uf_pais_vend <- read_xlsx('dados_publicacao/07-UF_País-2017-Vendedores.xlsx',col_names = TRUE)

  modo_vlr <- read_xlsx('dados_publicacao/08-Modo-2017-Valores.xlsx',col_names = TRUE)
  
  modo_vlr <- modo_vlr %>% separate(`Operação - Modo de Prestação`,c('CODIGO_MODO','DESC_MODO'),sep = '-')

  modo_vend <- read_xlsx('dados_publicacao/08-Modo-2017-Vendedores.xlsx',col_names = TRUE)

# ManipulaÃ§Ã£o dos arquivos

# Validacao do SIGILO

# valores totais

  dados <- t(qtde_vend_rvs_oper_17)
  dados_ <- t(vlr_total)

  aba_valores_totais <- rbind(dados,dados_)
  aba_valores_totais <- as.data.frame(aba_valores_totais)

  rm(dados,dados_,qtde_vend_rvs_oper_17,vlr_total)

# servicos - valores - vendedores

  serv_pos_vend <- serv_pos_vend %>%
                              filter( serv_pos_vend$`Qtde Vendedores Distintos (PF + PJ) por Operação RVS` >= 4)

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
