# Carregamento das bibliotecas necessarias

  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(statnet.common)
  library(lubridate)

# Importacao dos arquivos para depuracao

# MELHORIA: recriar codigo de leitura de arquivos com um laco for

  operacoes <- read_delim('dados/operacoes.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  rvs <- read_delim('dados/rvs.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  item_fat <- read_delim('dados/item_fat.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  fatura <- read_delim('dados/faturas.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  vendedores <- read_delim('dados/vendedores.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  serv_enq_vd <- read_delim('dados/serv_enq_vd.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

  taxa_venda <- read_delim('dados/taxa_venda.csv',delim = ';',col_names = TRUE,col_types = cols(.default = 'c'))

# tabelas apoio
  
  paises <- as.data.frame(read_delim('dados/paises.csv',delim = ',',col_names = TRUE),col_types = cols(.default = 'c'))
  
    paises$CODIGO <- gsub("(?<![0-9])0+", "", paises$CODIGO, perl = TRUE,col_types = cols(.default = 'c'))
  
  moeda <- as.data.frame(read_delim('dados/moedas.csv',delim = ',',col_names = TRUE,locale = locale(encoding = "latin1"),quote = '',col_types = cols(.default = 'c')))
  
    moeda$CODIGO <- gsub('"','',moeda$CODIGO)
    
    moeda$CODIGO <- gsub("(?<![0-9])0+", "", moeda$CODIGO, perl = TRUE)

  nbs <- as.data.frame(read_delim('dados/nbs.csv',delim = ',',col_names = TRUE),col_types = cols(.default = 'c'))
  
  enquad <- read_delim('dados/enquadramento.csv',delim = ',',col_names = TRUE,col_types = cols(.default = 'c'))
  
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
  
  taxa_venda <- taxa_venda %>%
    group_by(DATA_TAXA,MOEDA) %>% 
    summarise(TAXA = first(taxa_venda$TAXA))
  
##################### ################################ ######################################

# Reescrita dos codigos SQL para codigo R

# Passo 1 - Formata CPF_CNPJ e RAZAO_SOCIAL (CONCLUIDO)

  vendedores <- vendedores %>%  mutate(CPF_CNPJ_DESF = str_replace_all(string = vendedores$CPF_CNPJ,pattern = c('\\.'='','\\/'='','\\-'='')))

  vendedores <- mutate(vendedores,CPF_CNPJ_RAZAO_SOCIAL = paste(vendedores$CPF_CNPJ,vendedores$RAZAO_SOCIAL,sep = ' - '))

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
          mutate(VENDEDOR_INEXISTENTE = if_else((rvs$ID_VENDEDOR %in% str_trim(vendedores$CPF_CNPJ_DESF)),0,1))
  
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
   
  # ALTER TABLE IMPORTACAOOPERACOES ADD NBS_ATUALIZADA VARCHAR2(255);
  # UPDATE IMPORTACAOOPERACOES SET NBS_ATUALIZADA = NBS;
   
   operacoes <- operacoes %>% mutate(NBS_ATUALIZADA = NBS)
   
  # UPDATE /*+ parallel(OPER,8) */ SISCOSERVV.IMPORTACAOOPERACOES OPER
  # SET OPER.NBS_ATUALIZADA = ( SELECT MAP.CODIGOCLASSIFICACAO_V1_1
  #                             FROM SISCOSERVV.MAPEAMENTONBSV1_0_V1_1 MAP
  #                             WHERE OPER.NBS_ATUALIZADA = MAP.CODIGOCLASSIFICACAO_V1_0)
  #                             WHERE OPER.NBS_ATUALIZADA NOT IN (select NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGO from SISCOSERVV.NBSV1_1_07CODIGOSCLASSIFICACAO);
   
   
   
  operacoes <- operacoes %>% 
    mutate(NBS_INEXISTENTE = if_else(0 == count(nbs %>% filter(nbs$CODIGO %in% operacoes$NBS_ATUALIZADA)),1,0))
   
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
                                                                serv_enq_vd$ID_SERVICO %in% operacoes$ID_OPERACAO
                                                              )
                                                     )
                                          ,1,0)
           )
  
# Passo 10 - Marca enquadramento inexistentes 
                               
   serv_enq_vd <- serv_enq_vd %>%
     mutate(ENQUAD_INEXISTENTE = ifelse(serv_enq_vd$CD_ENQUADRAMENTO %in% enquad$CODIGO,0,1)
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
    
  
  
# Passo 14 - Calcula Valor Pago em USD - Itens de FATURAMENTO

  # UPDATE /*+ parallel(PGTO,8) */ siscoservv.IMPORTACAOITENSFATURAMENTO FAT
  # SET VALOR_FATURADO_DOLAR = VALOR_FATURADO * (select VALOR_OPERACAO_DOLAR/VALOR_OPERACAO
  #                                              from  siscoservv.IMPORTACAOOPERACOES OPER
  #                                              where FAT.ID_OPERACAO = Oper.ID_OPERACAO);
  
# ##################################### TRANSFERENCIA DE DADOS ###################################################

# Passo 15 - Transfere dados

# a) Copia Dados DE Importacao - VENDEDORES PARA VENDEDORES

  # --ALTER TABLE siscoservv.DADOSVENDEDORES INITRANS 10;
  # alter table  siscoservv.DADOSVENDEDORES add(IDENTIFICACAOFORMATADA  VARCHAR2(255 BYTE));
  # 
  # INSERT INTO siscoservv.DADOSVENDEDORES
  # ( IDENTIFICACAO,
  #   IDENTIFICACAOFORMATADA,
  #   NOME,
  #   IDENTIFICACAONOME,
  #   MUNICIPIO,
  #   RUA,
  #   BAIRRO,
  #   CEP,
  #   NUMERO,
  #   COMPLEMENTO )
  # SELECT /*+ parallel(t1,4) */
  # CPF_CNPJ_DESFORMATADO,
  # CPF_CNPJ,
  # RAZAO_SOCIAL,
  # CPF_CNPJ_RAZAO_SOCIAL,
  # MUNICIPIO,
  # RUA,
  # BAIRRO,
  # CEP,
  # NUMERO,
  # COMPLEMENTO
  # FROM siscoservv.IMPORTACAOVENDEDORES t1;

# b) Copia Dados DE Importacao - RVS PARA RVS

  # --ALTER TABLE siscoservv.DADOSRVS INITRANS 10;
  # 
  # INSERT INTO siscoservv.DADOSRVS
  # ( ID_RVS,
  #   RVS,
  #   CODIGOMOEDA,
  #   DATAINCLUSAORVS,
  #   TIPOVENDEDOR,
  #   IDENTIFICACAOVENDEDOR,
  #   NOMEADQUIRENTE,
  #   CODIGOPAISADQUIRENTE,
  #   UF )
  # SELECT /*+ full(t1) parallel(t1,4) */
  #   ID_RVS,
  # RVS,
  # MOEDA,
  # DATA_INCLUSAO_RVS,
  # TP_VENDEDOR,
  # ID_VENDEDOR,
  # ADQUIRENTE,
  # PAIS_ADQUIRENTE,
  # UF
  # FROM siscoservv.IMPORTACAORVS t1
  # WHERE nvl(MOEDA_INEXISTENTE,0) = 0
  # AND nvl(PAIS_INEXISTENTE,0) = 0
  # AND nvl(VENDEDOR_INEXISTENTE,0) = 0
  # AND MOEDA  in (select CODIGO from siscoservv.moeda) order by 1 ;
         
# c) Copia Dados DE Importacao - Operacoes PARA Operacoes

  # --ALTER TABLE siscoservv.DADOSOPERACOES INITRANS 10;
  # --alter table  siscoservv.DADOSOPERACOES add(  VALORDIARIOMOEDANEGOCIADA    NUMBER(38,2),  VALORDIARIOUSD               NUMBER(38,2));
  # 
  #  
  # INSERT INTO siscoservv.DADOSOPERACOES
  # ( ID_RVS,
  #   ID_OPERACAO,
  #   CODIGOPAISDESTINO,
  #   DATAINICIOSERVICO,
  #   DATACONCLUSAOSERVICO,
  #   DURACAO,
  #   MODOPRESTACAOSERVICO,
  #   CODIGONBS,
  #   VALOROPERACAOMOEDANEGOCIADA,
  #   VALOROPERACAOUSD,
  #   VALORDIARIOMOEDANEGOCIADA,
  #   VALORDIARIOUSD
  # )
  # SELECT /*+ full(t1) parallel(t1,4) */
  #   ID_RVS,
  # ID_OPERACAO,
  # CD_PAIS_DESTINO,
  # DATA_INICIO_OPERACAO,
  # DATA_CONCLUSAO_OPERACAO,
  # DURACAO,
  # MODO,
  # NBS_ATUALIZADA,
  # VALOR_OPERACAO,
  # VALOR_OPERACAO_DOLAR,
  # VALOR_DIARIO,
  # VALOR_DIARIO_DOLAR
  # FROM siscoservv.IMPORTACAOOPERACOES t1
  # WHERE nvl(RVS_INEXISTENTE,0) = 0
  # AND nvl(NBS_INEXISTENTE,0) = 0
  # AND nvl(PAIS_INEXISTENTE,0) = 0;

# d) Copia Dados DE Importacao - Enquadramentos PARA Enquadramentos

  # INSERT INTO siscoservv.ENQUADRAMENTOSDADOS
  # (ID_OPERACAO,
  #   CODIGOENQUADRAMENTO,
  #   NUMERORC)
  # SELECT ID_OPERACAO,
  # CD_ENQUADRAMENTO,
  # NR_RC_ENQUADRAMENTO
  # FROM siscoservv.IMPORTACAOENQUADRAMENTOS
  # WHERE nvl(OPERACAO_INEXISTENTE,0) = 0
  # AND nvl(ENQUADRAMENTO_INEXISTENTE,0) = 0;

# e) Copia Dados DE Importacao - RF PARA RF

  # INSERT INTO siscoservv.DADOSRF
  # ( ID_RF,
  #   ID_RVS,
  #   RF,
  #   DATAINCLUSAORF )
  # SELECT ID_RF,
  # ID_RVS,
  # NUMERO_RF,
  # DATA_INCLUSAO_RF
  # FROM siscoservv.IMPORTACAORF
  # WHERE nvl(RVS_INEXISTENTE,0) = 0;

# f) Copia Dados DE Importacao - Itens de FATURAMENTO PARA Itens de FATURAMENTO

  # INSERT INTO siscoservv.DADOSITENSFATURAMENTO
  # ( ID_ITEM_FATURAMENTO,
  #   ID_RF,
  #   ID_OPERACAO,
  #   DATAFATURAMENTO,
  #   VALORFATURADOMOEDANEGOCIADA,
  #   VALORFATURADOUSD)
  # SELECT ID_ITEM_FATURAMENTO,
  # ID_RF,
  # ID_OPERACAO,
  # DATA_FATURAMENTO,
  # VALOR_FATURADO,
  # VALOR_FATURADO_DOLAR
  # FROM siscoservv.IMPORTACAOITENSFATURAMENTO
  # WHERE nvl(RF_INEXISTENTE,0) = 0
  # AND nvl(OPERACAO_INEXISTENTE,0) = 0;

# --Passo 17

  # truncate table siscoservv.DADOSVALORESDILUIDOSMENSAIS;
  # 
  # insert into siscoservv.DADOSVALORESDILUIDOSMENSAIS
  # select  M.DATAINICIO as MES,
  # O.ID_OPERACAO ID_OPERACAO,
  # ((CASE WHEN M.DATAFIM < O.DATACONCLUSAOSERVICO
  #   THEN M.DATAFIM
  #   ELSE O.DATACONCLUSAOSERVICO
  #   END)
  #  -
  #    (CASE WHEN M.DATAINICIO > O.DATAINICIOSERVICO
  #     THEN M.DATAINICIO
  #     ELSE O.DATAINICIOSERVICO
  #     END) + 1) * O.VALORDIARIOMOEDANEGOCIADA * T.TAXA as VALORMENSALUSD
  # from SISCOSERVV.DADOSRVS R
  # inner join SISCOSERVV.DADOSOPERACOES O on R.ID_RVS = O.ID_RVS
  # inner join SISCOSERVV.TAXA_CAMBIO T on R.CODIGOMOEDA = T.MOEDA
  # inner join SISCOSERVV.MESES M on T.MES = M.DATAFIM
  # where M.DATAINICIO >= '01/10/2012'
  # and O.DATAINICIOSERVICO <= M.DATAFIM
  # and O.DATACONCLUSAOSERVICO >= M.DATAINICIO;
