# Carregamento das bibliotecas necessarias

  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(statnet.common)
  library(lubridate)

# Importacao dos arquivos para depuracao

# MELHORIA: recriar codigo de leitura de arquivos com um laco for

  operacoes <- read_delim('dados/operacoes.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'),n_max = 50000)

  rvs <- read_delim('dados/rvs.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'),n_max = 50000)

  item_fat <- read_delim('dados/item_fat.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'),n_max = 50000)

  fatura <- read_delim('dados/faturas.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'),n_max = 50000)

  VENDEDORES <- read_delim('dados/vendedores.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  serv_enq_vd <- read_delim('dados/serv_enq_vd.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  taxa_venda <- read_delim('dados/taxa_venda.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

# tabelas apoio
  
  paises <- as.data.frame(read_delim('dados/paises.csv',delim = ',',col_names = TRUE),col_types = cols(.default = 'c'))
  
    paises$CODIGO <- gsub("(?<![0-9])0+", "", paises$CODIGO, perl = TRUE)
  
  moeda <- as.data.frame(read_delim('dados/moedas.csv',delim = ',',col_names = TRUE,locale = locale(encoding = "latin1"),quote = '',col_types = cols(.default = 'c')))
  
    moeda$CODIGO <- gsub('"','',moeda$CODIGO)
    
    moeda$CODIGO <- gsub("(?<![0-9])0+", "", moeda$CODIGO, perl = TRUE)

  nbs <- as.data.frame(read_delim('dados/nbs.csv',delim = ',',col_names = TRUE),col_types = cols(.default = 'c'))
  
  enquadramento <- read_delim('dados/enquadramento.csv',delim = ',',col_names = TRUE,col_types = cols(.default = 'c'))
  
  mapa <- read_delim('dados/mapeamento.csv',delim = ',',col_names = TRUE,col_types = cols(.default = 'c'))
  
  matriz <- read_delim('dados/matriz.csv',delim = ',',col_names = TRUE,col_types = cols(.default = 'c'))
  
  meses <- read_delim('dados/meses.csv',delim = ',',col_names = TRUE,col_types = cols(.default = 'c'))

##################### ############################ ##########################################

# Tratamento do arquivo taxa_venda (USAR GROUP_BY, SUMMARISE AND FIRST)
  
  taxa_venda$DATA_TAXA <- str_replace_all(string = taxa_venda$DATA_TAXA,pattern = c('\\/'='-','\\/'='-'))
  
  taxa_venda$DATA_TAXA <- dmy(taxa_venda$DATA_TAXA)
  
  taxa_venda <- taxa_venda[1:3]
  
  taxa_venda <- unique(taxa_venda[c('DATA_TAXA','MOEDA','TAXA')])
  
  taxa_venda <- taxa_venda[seq(dim(taxa_venda)[1],1),]
  
  taxa <- paste(taxa_venda$DATA_TAXA,taxa_venda$MOEDA)
  
  taxa <- cbind(taxa,taxa_venda$TAXA)
  
  nomes <- c('dt_md','TAXA')
  
  taxa <- as.data.frame(taxa)
  
  names(x = taxa) <- nomes
  
  taxa <- taxa[!duplicated(taxa$dt_md),]
  
  taxa <- taxa %>% separate(dt_md,c('DATA','MOEDA'),sep = " ")
  
  taxa$DATA <- as.Date(taxa$DATA)
  
  taxa$TAXA <- str_replace(taxa$TAXA,'\\,','.')
  
  taxa$TAXA <- as.double(taxa$TAXA)
  
  taxa$MOEDA <- as.integer(taxa$MOEDA)
  
  taxa <- taxa %>% arrange(DATA,MOEDA)
  
  rm(nomes,taxa_venda)
  
##################### ################################ ######################################

# Passo 1 - Formata CPF_CNPJ e RAZAO_SOCIAL (CONCLUIDO)

  VENDEDORES <- VENDEDORES %>% mutate(CPF_CNPJ_DESF = str_replace_all(string = VENDEDORES$CPF_CNPJ,pattern = c('\\.'='','\\/'='','\\-'='')))

  VENDEDORES <- VENDEDORES %>% mutate(CPF_CNPJ_RAZAO_SOCIAL = paste(VENDEDORES$CPF_CNPJ,VENDEDORES$RAZAO_SOCIAL,sep = ' - '))

# Passo 2 - Tipo e Id do vendedor e Marca vendedores inexistentes : RVS

  rvs <- rvs %>% mutate(TP_VENDEDOR =
                        
                        case_when(CNPJ_VENDEDOR == 'n/a' ~ 'PF',
                                  !is.na(CNPJ_VENDEDOR) ~ 'PJ')
  )

  rvs <- rvs %>% mutate( ID_VENDEDOR =
                         
                         case_when(CNPJ_VENDEDOR == 'n/a' ~ rvs$CPF_VENDEDOR,
                                   !is.na(rvs$CNPJ_VENDEDOR) ~ rvs$CNPJ_VENDEDOR)
                       
  )
  
  rvs <-  rvs %>%
          mutate(VENDEDOR_INEXISTENTE = if_else((rvs$ID_VENDEDOR %in% str_trim(VENDEDORES$CPF_CNPJ_DESF)),0,1))
  
# Passo 3 - Marca moedas inexistentes

  rvs <-  rvs %>%
    mutate(MOEDA_INEXISTENTE = if_else((rvs$MOEDA %in% moeda$CODIGO),0,1))

# Passo 4 - Marca paises inexistentes

  rvs <-  rvs %>%
    mutate(PAIS_INEXISTENTE = if_else((rvs$PAIS_ADQUIRENTE %in% paises$CODIGO),0,1))

# Passo 5 - Marca rvs inexistentes - Operacoes
  
   operacoes <- operacoes %>% 
     
     mutate(RVS_INEXISTENTE = 
   
      if_else( 0 ==  count(rvs %>% 
                             
                             select(MOEDA_INEXISTENTE,PAIS_INEXISTENTE,VENDEDOR_INEXISTENTE) %>% 
                             
                             filter(NVL(rvs$MOEDA_INEXISTENTE,0) == 0 &
                                         NVL(rvs$PAIS_INEXISTENTE,0)  == 0 &
                                         NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0 &
                                          rvs$ID_RVS %in% operacoes$ID_RVS   
                                        )
                                )
               ,1,0)
      )
   
# Passo 6 - Copia, atualiza e marca NBS inexistentes NBS
   
  operacoes <- operacoes %>% mutate(NBS_ATUALIZADA = NBS)
  
  valores <- unique(operacoes %>% select(NBS_ATUALIZADA) %>% 
                      filter(operacoes$NBS_ATUALIZADA %in% mapa$CODIGOCLASSIFICACAO_V1_0 &
                               !(operacoes$NBS_ATUALIZADA %in% nbs$CODIGO)
                             )
                    )
  
  colnames(valores)[1] <- 'CODIGOCLASSIFICACAO_V1_0'
   
  valores <- left_join(valores,mapa,'CODIGOCLASSIFICACAO_V1_0')
  
  colnames(valores)[1] <- 'NBS_ATUALIZADA'
  
  indx <- match(operacoes$NBS_ATUALIZADA,valores$NBS_ATUALIZADA,nomatch = 0)
  
  operacoes$NBS_ATUALIZADA[indx != 0] <- valores$CODIGOCLASSIFICACAO_V1_1[indx]
  
  operacoes <- operacoes %>% 
    mutate(NBS_INEXISTENTE = if_else(0 == count(nbs %>% 
                                                  select(CODIGO) %>% 
                                                  filter(nbs$CODIGO %in% operacoes$NBS_ATUALIZADA))
                                     ,1,0)
           )
  
  rm(indx,valores)
  
# Passo 7 - Marca paises inexistentes
  
  operacoes <-  operacoes %>% mutate(PAIS_INEXISTENTE = ifelse((operacoes$CD_PAIS_DESTINO %in% paises$CODIGO),0,1))
  
# Passo 8 - Calcula duracao e valor diario medio: Operacoes
  
  operacoes$DATA_CONCLUSAO_OPERACAO <- str_replace_all(string = operacoes$DATA_CONCLUSAO_OPERACAO,pattern = c('\\/'='-','\\/'='-'))
  
  operacoes$DATA_INICIO_OPERACAO <- str_replace_all(string = operacoes$DATA_INICIO_OPERACAO,pattern = c('\\/'='-','\\/'='-'))
  
  operacoes$DATA_CONCLUSAO_OPERACAO <- dmy(operacoes$DATA_CONCLUSAO_OPERACAO)
  
  operacoes$DATA_INICIO_OPERACAO <- dmy(operacoes$DATA_INICIO_OPERACAO)

  operacoes <- operacoes %>% mutate(DURACAO =  DATA_CONCLUSAO_OPERACAO - DATA_INICIO_OPERACAO + 1)
  
  operacoes$VALOR_OPERACAO <- gsub(pattern = ',','.',operacoes$VALOR_OPERACAO)
  
  operacoes$VALOR_OPERACAO_DOLAR <- gsub(pattern = ',','.',operacoes$VALOR_OPERACAO_DOLAR)
  
  operacoes <- operacoes %>% mutate(
    
    VALOR_DIARIO = as.double(VALOR_OPERACAO)/ as.double(DURACAO),
    VALOR_DIARIO_DOLAR = as.double(VALOR_OPERACAO_DOLAR) / as.double(DURACAO)
  )

# Passo 9 - Marca operacoes inexistentes

  serv_enq_vd <- serv_enq_vd %>% 
    
    mutate(OPERACAO_INEXISTENTE = if_else(0 == count(operacoes %>% 
                                                       select(RVS_INEXISTENTE,NBS_INEXISTENTE,PAIS_INEXISTENTE) %>% 
                                                       filter(NVL(operacoes$RVS_INEXISTENTE,0) == 0 &
                                                                NVL(operacoes$NBS_INEXISTENTE,0)  == 0 &
                                                                NVL(operacoes$PAIS_INEXISTENTE,0) == 0 &
                                                                operacoes$ID_OPERACAO %in% serv_enq_vd$ID_SERVICO
                                                              )
                                                     )
                                          ,1,0)
           )
  
# Passo 10 - Marca enquadramento inexistentes 
                               
   serv_enq_vd <- serv_enq_vd %>%
     mutate(ENQUAD_INEXISTENTE = ifelse(serv_enq_vd$CD_ENQUADRAMENTO %in% enquadramento$CODIGO,0,1)
            )
           
# Passo 11 - Marca RVS inexistente
         
  fatura <- fatura %>%
    mutate(RVS_INEXISTENTE = 
             if_else(0 == count(rvs %>% 
                                  select(MOEDA_INEXISTENTE,PAIS_INEXISTENTE,VENDEDOR_INEXISTENTE) %>% 
                                  filter(NVL(rvs$MOEDA_INEXISTENTE,0) == 0 &
                                           NVL(rvs$PAIS_INEXISTENTE,0)  == 0 &
                                           NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0 &
                                           rvs$ID_RVS %in% fatura$ID_RVS   
                                         )
                                )
                     ,1,0)
           )
         
# Passo 12 - Marca RF inexistente

  item_fat <- item_fat %>% 
    mutate(FATURA_INEXISTENTE = 
             if_else(0 == count(fatura %>% 
                                  select(RVS_INEXISTENTE) %>%
                                  filter(NVL(fatura$RVS_INEXISTENTE,0) == 0 &
                                           fatura$ID_FATURA %in% item_fat$ID_FATURA)
                     )
          ,1,0)
    )
  
# Passo 13 - Marca operacoes inexistentes

  item_fat <- item_fat %>% 
    
    mutate(OPERACAO_INEXISTENTE = 
             if_else(0 == count(operacoes %>% 
                                  select(RVS_INEXISTENTE,NBS_INEXISTENTE,PAIS_INEXISTENTE) %>% 
                                  filter(
                                    NVL(operacoes$RVS_INEXISTENTE,0) == 0 &
                                    NVL(operacoes$PAIS_INEXISTENTE,0)  == 0 &
                                    NVL(operacoes$NBS_INEXISTENTE,0) == 0 &
                                    fatura$ID_FATURA %in% operacoes$ID_OPERACAO 
                                    )
                                )
                     ,1,0)
           )
  
# Passo 14 - Transfere dados

# a) Copia Dados DE Importacao - RVS PARA RVS
  
  RVS <- rvs %>% 
    filter(
      NVL(rvs$MOEDA_INEXISTENTE,0) == 0 &
      NVL(rvs$PAIS_INEXISTENTE,0)  == 0 &
      NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0 &
      rvs$MOEDA %in% moeda$CODIGO 
  )
         
# b) Copia Dados DE Importacao - Operacoes PARA Operacoes
  
  OPERACOES <- operacoes %>% filter(
    NVL(operacoes$RVS_INEXISTENTE,0) == 0 &
    NVL(operacoes$NBS_INEXISTENTE,0)  == 0 &
    NVL(operacoes$PAIS_INEXISTENTE,0) == 0
    )

# c) Copia Dados DE Importacao - Enquadramentos PARA Enquadramentos
  
  ENQUADRAMENTO <- serv_enq_vd %>% filter(
    NVL(serv_enq_vd$OPERACAO_INEXISTENTE,0) == 0 &
    NVL(serv_enq_vd$ENQUAD_INEXISTENTE,0)  == 0
    )

# d) Copia Dados DE Importacao - RF PARA RF
  
  FATURA <- fatura %>% filter(
    NVL(fatura$RVS_INEXISTENTE,0) == 0
  )

# e) Copia Dados DE Importacao - Itens de FATURAMENTO PARA Itens de FATURAMENTO
  
  ITEM_FATURAMENTO <- item_fat %>% filter(
    NVL(item_fat$FATURA_INEXISTENTE,0) == 0 &
    NVL(item_fat$OPERACAO_INEXISTENTE,0)  == 0
  )
  
# ######################################################################################################################################################
  
# GERACAO DOS DADOS PARA INVESTIGACAO
  
# Gera aba -> NBS (PASSO 1)
  
# SELECT
  # '_' || DADOSOPERACOES.CODIGONBS CODIGO_NBS,NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGODESCRICAO NBS,SUM(DADOSOPERACOES.VALOROPERACAOUSD) VALOR
# FROM DADOSOPERACOES JOIN NBSV1_1_07CODIGOSCLASSIFICACAO
  # ON DADOSOPERACOES.CODIGONBS = NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGO
# WHERE DADOSOPERACOES.DATAINICIOSERVICO <
  # TO_DATE('2017-01-01', 'YYYY-MM-DD')
  # AND DADOSOPERACOES.DATACONCLUSAOSERVICO > 
  # TO_DATE('2015-12-31', 'YYYY-MM-DD')
# GROUP BY DADOSOPERACOES.CODIGONBS, NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGODESCRICAO
# ORDER BY VALOR DESC;
  
  # operacoes,nbs
  # vendedores,rvs,operacoes,nbs
  
# Gera aba -> EMPRESA (PASSO 2)
  
# SELECT 
  # DADOSOPERACOES.CODIGONBS || '-' ||
  # DADOSRVS.IDENTIFICACAOVENDEDOR CHAVE,'_' ||
  # DADOSOPERACOES.CODIGONBS CODIGO_NBS,
  # NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGODESCRICAO NBS,
  # '_' || DADOSRVS.IDENTIFICACAOVENDEDOR ID_VENDEDOR,
  # DADOSVENDEDORES.IDENTIFICACAONOME VENDEDOR,
  # SUM(DADOSOPERACOES.VALOROPERACAOUSD) VALOR,
  # AVG(DADOSOPERACOES.VALORDIARIOUSD) VALOR_DIARIO_MEDIO
# FROM DADOSVENDEDORES JOIN DADOSRVS
  # ON DADOSVENDEDORES.IDENTIFICACAO = DADOSRVS.IDENTIFICACAOVENDEDOR
  # JOIN DADOSOPERACOES
  # ON DADOSRVS.ID_RVS = DADOSOPERACOES.ID_RVS
  # JOIN NBSV1_1_07CODIGOSCLASSIFICACAO
  # ON DADOSOPERACOES.CODIGONBS = NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGO
# WHERE DADOSOPERACOES.DATAINICIOSERVICO < TO_DATE('2017-01-01', 'YYYY-MM-DD') AND DADOSOPERACOES.DATACONCLUSAOSERVICO > TO_DATE('2015-12-31', 'YYYY-MM-DD')
  # AND DADOSOPERACOES.VALORDIARIOUSD > 0
# GROUP BY DADOSOPERACOES.CODIGONBS,NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGODESCRICAO,DADOSRVS.IDENTIFICACAOVENDEDOR,DADOSVENDEDORES.IDENTIFICACAONOME
# ORDER BY VALOR DESC;