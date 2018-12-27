# Carregamento das bibliotecas necessarias

library(tidyverse)
library(magrittr)
library(stringr)

# Importacao dos arquivos para depuracao

  nomes <- read.table('operacao.csv', nrows = 1,stringsAsFactors = FALSE, sep = ';')

  nomes[11] <- NULL

  dados <- read.table('operacao.csv', skip = 1,stringsAsFactors = FALSE,sep = ';')

  dados[11:13] <- NULL

  names(dados) <- nomes

operacoes <- dados

rm(dados,nomes)

  nom <- read.table('rvs.csv',nrows = 1,stringsAsFactors = FALSE,sep = ';')

  dado <- read.table('rvs.csv',skip = 1,stringsAsFactors = FALSE,sep = ';')

  dado[14:15] <- NULL

  nom[14] <- NULL

  names(dado) <- nom

rvs <- dado

rm(dado,nom)

item_fat <- read.table('item_fat.csv', sep = ';',header = TRUE,stringsAsFactors = FALSE)

fatura <- read.table('fatura.csv',sep = ';',header = TRUE,stringsAsFactors = FALSE)

vendedores <- read.csv('vendedores.csv',sep = ';', header = TRUE,stringsAsFactors = FALSE)

serv_enq_vd <- read.table('serv_enq_vd.csv',sep = ';', header = TRUE,stringsAsFactors = FALSE)

taxa_venda <- read.table('taxa_venda.csv',sep = ';',header = TRUE,stringsAsFactors = FALSE)

##################### ############################ ##########################################

# Tratamento do arquivo taxa_venda

  taxa_venda <- unique(taxa_venda)

  taxa_venda <- taxa_venda[seq(dim(taxa_venda)[1],1),]

  taxa_venda <- unique(taxa_venda[c('DATA_TAXA','MOEDA','TAXA','CPF_USUARIO')])

  # for para data
    # for para moeda
      # pego o primeiro valor para cada par data/moeda e atribuo a uma variavel x
        # descarta valores seguintes em que data e moeda sejam iguais
          # quando length(x)== 1 eu para de inserir e comeco a contagem novamente


##################### ################################ ######################################


# Reescrita dos codigos SQL para codigo R

# Passo 1 - Formata CPF_CNPJ e RAZAO_SOCIAL (CONCLUIDO)

  # definir uma regex para remover characteres especiais.

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
  
  # extrair tabela de moedas: tabelas web ou banco de dados siscoserv

# Passo 4 - Marca paises inexistentes
  
  # extrair tabela de paises: tabelas web ou banco de dados siscoserv

# Passo 5 - Marca rvs inexistentes - Operacoes

# Passo 6 - Copia, atualiza e marca NBS inexistentes NBS

# Passo 7 - Marca paises inexistentes

# Passo 8 - Calcula duracao e valor diario medio: Operacoes

# Passo 9 - Marca operacoes inexistentes

# Passo 10 - Marca enquadramento inexistentes

# Passo 11 - Marca RVS inexistente

# Passo 12 - Marca RF inexistente

# Passo 13 - Marca operacoes inexistentes