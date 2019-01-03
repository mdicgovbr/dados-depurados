# Carregamento das bibliotecas necessarias

  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(statnet.common)
  library(lubridate)

# Importacao dos arquivos para depuracao

# MELHORIA: recriar codigo de leitura de arquivos com um laco for

  operacoes <- read_delim('2 - dados/operacoes.csv',delim = ';',col_names = TRUE,n_max = 20000)

  rvs <- read_delim('2 - dados/rvs.csv',delim = ';',col_names = TRUE,n_max = 20000)

  item_fat <- read_delim('2 - dados/item_fat.csv',delim = ';',col_names = TRUE,n_max = 20000)

  fatura <- read_delim('2 - dados/faturas.csv',delim = ';',col_names = TRUE,n_max = 20000)

  vendedores <- read_delim('2 - dados/vendedores.csv',delim = ';',col_names = TRUE)

  serv_enq_vd <- read_delim('2 - dados/serv_enq_vd.csv',delim = ';',col_names = TRUE)

  taxa_venda <- read_delim('2 - dados/taxa_venda.csv',delim = ';',col_names = TRUE)

# tabelas apoio
  
  paises <- as.data.frame(read_delim('2 - dados/paises.csv',delim = ',',col_names = TRUE))
  
    paises$CODIGO <- gsub("(?<![0-9])0+", "", paises$CODIGO, perl = TRUE)
  
  moeda <- as.data.frame(read_delim('2 - dados/moedas.csv',delim = ',',col_names = TRUE,locale = locale(encoding = "latin1"),quote = ''))
  
    moeda$CODIGO <- gsub('"','',moeda$CODIGO)
    
    moeda$CODIGO <- gsub("(?<![0-9])0+", "", moeda$CODIGO, perl = TRUE)

  nbs <- as.data.frame(read_delim('2 - dados/nbs.csv',delim = ',',col_names = TRUE))
  
  enquad <- read_delim('2 - dados/enquadramento.csv',delim = ',',col_names = TRUE)
  
  mapa <- read_delim('2 - dados/mapeamento.csv',delim = ',',col_names = TRUE)
  
  matriz <- read_delim('2 - dados/matriz.csv',delim = ',',col_names = TRUE)
  
  meses <- read_delim('2 - dados/meses.csv',delim = ',',col_names = TRUE)

##################### ############################ ##########################################

# Tratamento do arquivo taxa_venda (USAR GROUP_BY, SUMMARISE AND FIRST)
  
  taxa_venda$DATA_TAXA <- str_replace_all(string = taxa_venda$DATA_TAXA,pattern = c('\\/'='-','\\/'='-'))
  
  taxa_venda$DATA_TAXA <- dmy(taxa_venda$DATA_TAXA)
  
  taxa_venda <- taxa_venda[1:3]
  
  taxa_venda <- unique(taxa_venda[c('DATA_TAXA','MOEDA','TAXA')])
  
  taxa_venda <- taxa_venda[seq(dim(taxa_venda)[1],1),]
  
  taxa_venda <- taxa_venda %>%
    group_by(DATA_TAXA,MOEDA) %>% 
    summarise(TAXA = first(taxa_venda$TAXA,taxa_venda$DATA_TAXA,taxa_venda$MOEDA))
  
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
  
  rvs$ID_VENDEDOR <- ifelse(str_count(rvs$ID_VENDEDOR)==11,
                    str_pad(string = rvs$ID_VENDEDOR,width = 14,side = 'right',pad = ' '),rvs$ID_VENDEDOR)
  
  vendedores$CPF_CNPJ_DESF <- ifelse(str_count(vendedores$CPF_CNPJ_DESF)==11,
                            str_pad(string = vendedores$CPF_CNPJ_DESF,width = 14,side = 'right',pad = ' '),vendedores$CPF_CNPJ_DESF)
  
  
  rvs <-  rvs %>%
          mutate(VENDEDOR_INEXISTENTE = if_else((rvs$ID_VENDEDOR %in% vendedores$CPF_CNPJ_DESF),0,1))
  
# Passo 3 - Marca moedas inexistentes

  rvs <-  rvs %>%
    mutate(MOEDA_INEXISTENTE = if_else((rvs$MOEDA %in% moeda$CODIGO),0,1))

# Passo 4 - Marca paises inexistentes

  rvs <-  rvs %>%
    mutate(PAIS_INEXISTENTE = if_else((rvs$PAIS_ADQUIRENTE %in% paises$CODIGO),0,1))

# Passo 5 - Marca rvs inexistentes - Operacoes
   
   operacoes <- operacoes %>% 
     
     mutate(RVS_INEXISTENTE = 
   
      if_else( 0 ==  
                 ifelse(rvs$ID_RVS %in% operacoes$ID_RVS ,
        
                          count(rvs %>% 
                                  
                                  select(MOEDA_INEXISTENTE,PAIS_INEXISTENTE,VENDEDOR_INEXISTENTE) %>% 
                                  
                                  filter(NVL(rvs$MOEDA_INEXISTENTE,0) == 0 &
                                         NVL(rvs$PAIS_INEXISTENTE,0)  == 0 &
                                         NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0
                                        )
                                )
                 ,0)
               
               ,1,0)
      )
   
# Passo 6 - Copia, atualiza e marca NBS inexistentes NBS

  # UPDATE /*+ parallel(OPER,8) */ SISCOSERVV.IMPORTACAOOPERACOES OPER
  # SET OPER.NBS_ATUALIZADA = ( SELECT MAP.CODIGOCLASSIFICACAO_V1_1
  #                             FROM SISCOSERVV.MAPEAMENTONBSV1_0_V1_1 MAP
  #                             WHERE OPER.NBS_ATUALIZADA = MAP.CODIGOCLASSIFICACAO_V1_0)
  #                             WHERE OPER.NBS_ATUALIZADA NOT IN (select NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGO from SISCOSERVV.NBSV1_1_07CODIGOSCLASSIFICACAO);
   
   operacoes <- operacoes %>% 
     
     mutate(NBS_ATUALIZADA = 
   
      mapa %>% 
           
           select(CODIGOCLASSIFICACAO_V1_1) %>% 
           
           filter(operacoes$NBS_ATUALIZADA %in% mapa$CODIGOCLASSIFICACAO_V1_0 &
                  operacoes$NBS_ATUALIZADA !(nbs$CODIGO)
                  
                  )
     )
    
  # UPDATE /*+ parallel(OPER,8) */ siscoservv.IMPORTACAOOPERACOES Oper
  # SET NBS_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM siscoservv.NBSV1_1_07CODIGOSCLASSIFICACAO N
  #                   WHERE N.CODIGO =  Oper.NBS_ATUALIZADA);
   
   operacoes <- operacoes %>% 
     mutate(NBS_INEXISTENTE = if_else(0 == count(if_else(nbs$CODIGO %in% operacoes$NBS_ATUALIZADA,1,0)),1,0))

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

  # UPDATE /*+ parallel(ENQ,8) */ siscoservv.IMPORTACAOENQUADRAMENTOS ENQ
  # SET Enq.OPERACAO_INEXISTENTE = 1
  # WHERE not exists(SELECT 1
  #                  FROM siscoservv.IMPORTACAOOPERACOES Oper
  #                  WHERE nvl(Oper.RVS_INEXISTENTE,0) = 0 -- FALSE
  #                  AND nvl(Oper.NBS_INEXISTENTE,0) = 0 --FALSE
  #                  AND nvl(Oper.PAIS_INEXISTENTE,0) = 0 --FALSE
  #                  AND Enq.ID_OPERACAO = Oper.ID_OPERACAO);
  
             
                               
                               count(operacoes %>% 
                                       
                                       select(RVS_INEXISTENTE,NBS_INEXISTENTE,PAIS_INEXISTENTE) %>% 
                                       
                                       filter(NVL(operacoes$RVS_INEXISTENTE,0) == 0 &
                                              NVL(operacoes$PAIS_INEXISTENTE,0)  == 0 &
                                              NVL(operacoes$NBS_INEXISTENTE,0) == 0))

  
  

# Passo 10 - Marca enquadramento inexistentes 
                               
         serv_enq_vd <- serv_enq_vd %>% mutate(ENQUAD_INEXISTENTE = ifelse(serv_enq_vd$CD_ENQUADRAMENTO %in% enquad$CODIGO,0,1))
           
# Passo 11 - Marca RVS inexistente

  # UPDATE /*+ parallel(RF,4) */ siscoservv.IMPORTACAORF RF
  # SET RVS_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM  siscoservv.IMPORTACAORVS RVS
  #                   WHERE nvl(RVS.MOEDA_INEXISTENTE,0) = 0
  #                   AND nvl(RVS.PAIS_INEXISTENTE,0) = 0
  #                   AND nvl(RVS.VENDEDOR_INEXISTENTE,0) = 0
  #                   AND RVS.ID_RVS = RF.ID_RVS);
         
         
         count(rvs %>% 
                 
                 select(MOEDA_INEXISTENTE,VENDEDOR_INEXISTENTE,PAIS_INEXISTENTE) %>% 
                 
                 filter(NVL(operacoes$MOEDA_INEXISTENTE,0) == 0 &
                        NVL(operacoes$PAIS_INEXISTENTE,0)  == 0 &
                        NVL(operacoes$VENDEDOR_INEXISTENTE,0) == 0))

# Passo 12 - Marca RF inexistente

  # UPDATE /*+ parallel(PGTO,8) */ siscoservv.IMPORTACAOITENSFATURAMENTO FAT
  # SET RF_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM siscoservv.IMPORTACAORF RF
  #                   WHERE nvl(RF.RVS_INEXISTENTE,0) = 0
  #                   AND RF.ID_RF = FAT.ID_RF);
         
         count(fatura %>% 
                 
                 select(RVS_INEXISTENTE) %>% 
                 
                 filter(NVL(fatura$RVS_INEXISTENTE,0) == 0))
         
         
  
# Passo 13 - Marca operacoes inexistentes

  # UPDATE /*+ parallel(PGTO,8) */ siscoservv.IMPORTACAOITENSFATURAMENTO FAT
  # SET OPERACAO_INEXISTENTE = 1
  # WHERE not exists(SELECT 1
  #                  FROM  siscoservv.IMPORTACAOOPERACOES Oper
  #                   WHERE nvl(Oper.RVS_INEXISTENTE,0) = 0
  #                  AND nvl(Oper.NBS_INEXISTENTE,0) = 0
  #                  AND nvl(Oper.PAIS_INEXISTENTE,0) = 0
  #                  AND FAT.ID_OPERACAO = Oper.ID_OPERACAO);
         
         count(rvs %>% 
                 
                 select(RVS_INEXISTENTE,NBS_INEXISTENTE,PAIS_INEXISTENTE) %>% 
                 
                 filter(NVL(operacoes$RVS_INEXISTENTE,0) == 0 &
                        NVL(operacoes$PAIS_INEXISTENTE,0)  == 0 &
                        NVL(operacoes$NBS_INEXISTENTE,0) == 0))

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
         
         count(rvs %>% 
                 
                 select(all(rvs)) %>% 
                 
                 filter(NVL(rvs$MOEDA_INEXISTENTE,0) == 0 &
                          NVL(rvs$PAIS_INEXISTENTE,0)  == 0 &
                          NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0 &
                          MOEDA 
                 )
         )
  
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
