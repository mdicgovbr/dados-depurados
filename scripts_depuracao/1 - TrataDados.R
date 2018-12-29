# Carregamento das bibliotecas necessarias

  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(statnet.common)

# Importacao dos arquivos para depuracao

# MELHORIA: recriar codigo de leitura de arquivos com um laco for

  operacoes <- read_delim('2 - dados/operacoes.csv',delim = ';',col_names = TRUE)

  rvs <- read_delim('2 - dados/rvs.csv',delim = ';',col_names = TRUE)

  item_fat <- read_delim('2 - dados/item_fat.csv',delim = ';',col_names = TRUE)

  fatura <- read_delim('2 - dados/faturas.csv',delim = ';',col_names = TRUE)

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

##################### ############################ ##########################################

# Tratamento do arquivo taxa_venda
  
  taxa_venda <- read_delim('2 - dados/taxa_venda.csv',delim = ';',col_names = TRUE)

  taxa_venda <- taxa_venda[1:3]

  taxa_venda <- taxa_venda[seq(dim(taxa_venda)[1],1),]

  taxa_venda <- unique(taxa_venda[c('DATA_TAXA','MOEDA','TAXA')])
  
  taxa <- paste(taxa_venda$DATA_TAXA,taxa_venda$MOEDA)
  
  taxa <- as.data.frame(taxa)
  
  colnames(taxa)[1] <- 'dt_md'
  colnames(taxa)[2] <- 'taxa'
  
  unicos <- unique(taxa)
  
  taxa <- cbind(taxa,taxa_venda$TAXA)
  
  unicos <- unicos[unicos$dt_md %in% names(which(table(c(taxa$dt_md)) == 1)),]
  
  unicos <- ifelse(unicos[unicos$dt_md %in% names(which(table(c(taxa$dt_md)) == 1)),],taxa$taxa)
  
##################### ################################ ######################################

# Reescrita dos codigos SQL para codigo R

# Passo 1 - Formata CPF_CNPJ e RAZAO_SOCIAL (CONCLUIDO)

  vendedores$CPF_CNPJ <- str_replace_all(string = vendedores$CPF_CNPJ,pattern = c('\\.'='','\\/'='','\\-'=''))

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
  mutate(VENDEDOR_INEXISTENTE = ifelse(!(rvs$ID_VENDEDOR %in% vendedores$CPF_CNPJ),1,0))

# Passo 3 - Marca moedas inexistentes

  rvs <-  rvs %>%
    mutate(MOEDA_INEXISTENTE = ifelse(!(rvs$MOEDA %in% moeda$CODIGO),1,0))

# Passo 4 - Marca paises inexistentes

  rvs <-  rvs %>%
    mutate(PAIS_INEXISTENTE = ifelse(!(rvs$PAIS_ADQUIRENTE %in% paises$CODIGO),1,0))

# Passo 5 - Marca rvs inexistentes - Operacoes

  # UPDATE /*+ parallel(OPER,8) */ siscoservv.IMPORTACAOOPERACOES OPER
  # SET RVS_INEXISTENTE = 1
  # WHERE 0 =  (SELECT count(*)
  #             FROM siscoservv.IMPORTACAORVS RVS
  #             WHERE nvl(RVS.MOEDA_INEXISTENTE,0) = 0
  #             AND nvl(RVS.PAIS_INEXISTENTE,0) = 0
  #             AND nvl(RVS.VENDEDOR_INEXISTENTE,0) = 0
  #             AND RVS.ID_RVS = OPER.ID_RVS);

  operacoes <- operacoes %>% 
                mutate(RVS_INEXISTENTE = ifelse(
                  0 == (ifelse(rvs$ID_RVS %in% operacoes$ID_RVS,
                         rvs <- rvs %>%  case_when(NVL(rvs$MOEDA_INEXISTENTE,0) == 0,
                                                   NVL(rvs$PAIS_INEXISTENTE,0) == 0,
                                                   NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0
                         )
                  ))
                  ,1,0))

# Passo 6 - Copia, atualiza e marca NBS inexistentes NBS (IMPORTAR TABELA DE MAPEAMENTOS NBS)

  # UPDATE /*+ parallel(OPER,8) */ SISCOSERVV.IMPORTACAOOPERACOES OPER
  # SET OPER.NBS_ATUALIZADA = ( SELECT MAP.CODIGOCLASSIFICACAO_V1_1
  #                             FROM SISCOSERVV.MAPEAMENTONBSV1_0_V1_1 MAP
  #                             WHERE OPER.NBS_ATUALIZADA = MAP.CODIGOCLASSIFICACAO_V1_0)
  #                             WHERE OPER.NBS_ATUALIZADA NOT IN (select NBSV1_1_07CODIGOSCLASSIFICACAO.CODIGO from SISCOSERVV.NBSV1_1_07CODIGOSCLASSIFICACAO);
  
    operacoes <- operacoes %>% 
      mutate(NBS_ATUALIZADA = ifelse(
        0 == (ifelse(rvs$ID_RVS %in% operacoes$ID_RVS,
                     rvs <- rvs %>%  case_when(NVL(rvs$MOEDA_INEXISTENTE,0) == 0,
                                               NVL(rvs$PAIS_INEXISTENTE,0) == 0,
                                               NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0
                     )
        ))
        ,1,0))
    
    
    mapa <- mapa %>% (if(operacoes$nbs_atualizada %in% mapa$codigo) 
      which(operacoes$NBS_ATUALIZADA %!in$ (nbs$codigo)))
    
  # UPDATE /*+ parallel(OPER,8) */ siscoservv.IMPORTACAOOPERACOES Oper
  # SET NBS_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM siscoservv.NBSV1_1_07CODIGOSCLASSIFICACAO N
  #                   WHERE N.CODIGO =  Oper.NBS_ATUALIZADA);
  
  operacoes <- operacoes %>% mutate(NBS_INEXISTENTE = ifelse(
      0 == (ifelse(operacoes$nbs_atualizada %in% nbs$codigo)),1,0))


# Passo 7 - Marca paises inexistentes

  # UPDATE /*+ parallel(OPER,4) */ siscoservv.IMPORTACAOOPERACOES Oper
  # SET PAIS_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM siscoservv.PAISES
  #                   WHERE PAISES.CODIGO = Oper.CD_PAIS_DESTINO);
  
  operacoes <-  operacoes %>%
    mutate(PAIS_INEXISTENTE = ifelse(!(operacoes$CD_PAIS_DESTINO %in% paises$CODIGO),1,0))
  
# Passo 8 - Calcula duracao e valor diario medio: Operacoes

  # UPDATE /*+ parallel(OPER,8) */ siscoservv.IMPORTACAOOPERACOES OPER
  # SET DURACAO = DATA_CONCLUSAO_OPERACAO - DATA_INICIO_OPERACAO + 1;

  operacoes <- operacoes %>% mutate(duracao = DATA_CONCLUSAO_OPERACAO - DATA_INICIO_OPERACAO + 1)
  
  
  # UPDATE /*+ parallel(OPER,8) */ siscoservv.IMPORTACAOOPERACOES
  # SET VALOR_DIARIO = VALOR_OPERACAO / DURACAO,
  # VALOR_DIARIO_DOLAR = VALOR_OPERACAO_DOLAR / DURACAO;
  
  operacoes <- operacoces %>% mutate(
    
    valor_diario = valor_operacao / duracao,
    valor_diario_dolar = valor_operacao_dolar / duracao
    
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
  
  serv_enq_vd <- operacoes %>% 
    mutate(RVS_INEXISTENTE = ifelse(
      0 == (ifelse(serv_enq_vd$ID_OPERACAO %in% operacoes$ID_OPERACAO,
                   operacoes <- operacoes %>%  case_when(NVL(operacoes$MOEDA_INEXISTENTE,0) == 0,
                                             NVL(operacoes$PAIS_INEXISTENTE,0) == 0,
                                             NVL(operacoes$VENDEDOR_INEXISTENTE,0) == 0
                   )
      ))
      ,1,0))

# Passo 10 - Marca enquadramento inexistentes (IMPORTAR TABELA DE ENQUADRAMENTO)

  # UPDATE /*+ parallel(IMP,4) */ siscoservv.IMPORTACAOENQUADRAMENTOS IMP
  # SET ENQUADRAMENTO_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM siscoservv.ENQUADRAMENTOS ENQ
  #                   WHERE IMP.CD_ENQUADRAMENTO = ENQ.CODIGO);
  
  serv_enq_vd <- serv_enq_vd %>% 
    mutate(enq_inexistente = ifelse(!(serv_enq_vd$cd_enquadramento %in% enq$codigo),1,0))

# Passo 11 - Marca RVS inexistente

  # UPDATE /*+ parallel(RF,4) */ siscoservv.IMPORTACAORF RF
  # SET RVS_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM  siscoservv.IMPORTACAORVS RVS
  #                   WHERE nvl(RVS.MOEDA_INEXISTENTE,0) = 0
  #                   AND nvl(RVS.PAIS_INEXISTENTE,0) = 0
  #                   AND nvl(RVS.VENDEDOR_INEXISTENTE,0) = 0
  #                   AND RVS.ID_RVS = RF.ID_RVS);
  
  fatura <- fatura %>% 
    mutate(RVS_INEXISTENTE = ifelse(
      0 == (ifelse(rvs$ID_RVS %in% operacoes$ID_RVS,
                   rvs <- rvs %>%  case_when(NVL(rvs$MOEDA_INEXISTENTE,0) == 0,
                                             NVL(rvs$PAIS_INEXISTENTE,0) == 0,
                                             NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0
                   )
      ))
      ,1,0))

# Passo 12 - Marca RF inexistente

  # UPDATE /*+ parallel(PGTO,8) */ siscoservv.IMPORTACAOITENSFATURAMENTO FAT
  # SET RF_INEXISTENTE = 1
  # WHERE not exists (SELECT 1
  #                   FROM siscoservv.IMPORTACAORF RF
  #                   WHERE nvl(RF.RVS_INEXISTENTE,0) = 0
  #                   AND RF.ID_RF = FAT.ID_RF);
  
  item_fat <- item_fat %>% 
    mutate(RF_INEXISTENTE = ifelse(
      0 == (ifelse(fatura$ID_RF %in% item_fat$ID_RF,
                   fatura <- fatura %>%  case_when(NVL(fatura$RVS_INEXISTENTE,0) == 0
                   )
      ))
      ,1,0))

# Passo 13 - Marca operacoes inexistentes

  # UPDATE /*+ parallel(PGTO,8) */ siscoservv.IMPORTACAOITENSFATURAMENTO FAT
  # SET OPERACAO_INEXISTENTE = 1
  # WHERE not exists(SELECT 1
  #                  FROM  siscoservv.IMPORTACAOOPERACOES Oper
  #                   WHERE nvl(Oper.RVS_INEXISTENTE,0) = 0
  #                  AND nvl(Oper.NBS_INEXISTENTE,0) = 0
  #                  AND nvl(Oper.PAIS_INEXISTENTE,0) = 0
  #                  AND FAT.ID_OPERACAO = Oper.ID_OPERACAO);
  
  item_fat <- item_fat %>% 
    mutate(OPERACAO_INEXISTENTE = ifelse(
      0 == sum(ifelse(item_fat$id_operacao %in% operacoes$id_operacao,
                   operacoes <- operacoes %>%  case_when(NVL(operacoes$NBS_INEXISTENTE,0) == 0,
                                             NVL(operacoes$RVS_INEXISTENTE,0) == 0,
                                             NVL(operacoes$PAIS_INEXISTENTE,0) == 0
                   )
      ))
      ,1,0))

# Passo 14 - Calcula Valor Pago em USD - Itens de FATURAMENTO

  # UPDATE /*+ parallel(PGTO,8) */ siscoservv.IMPORTACAOITENSFATURAMENTO FAT
  # SET VALOR_FATURADO_DOLAR = VALOR_FATURADO * (select VALOR_OPERACAO_DOLAR/VALOR_OPERACAO
  #                                              from  siscoservv.IMPORTACAOOPERACOES OPER
  #                                              where FAT.ID_OPERACAO = Oper.ID_OPERACAO);
  
  operacoes <- operacoes %>% 
    mutate(RVS_INEXISTENTE = ifelse(
      0 == (ifelse(rvs$ID_RVS %in% operacoes$ID_RVS,
                   rvs <- rvs %>%  case_when(NVL(rvs$MOEDA_INEXISTENTE,0) == 0,
                                             NVL(rvs$PAIS_INEXISTENTE,0) == 0,
                                             NVL(rvs$VENDEDOR_INEXISTENTE,0) == 0
                   )
      ))
      ,1,0))

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
