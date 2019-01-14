# Carregamento das bibliotecas necessárias para execucao do codigo

  library(tidyverse)
  library(ggplot2)
  library(DT)
  library(lubridate)

# obs: será gerado vários vários relatórios, 1 para cada empresa identificada como outlier

# Importacao e Tratamento dos dados para manipulação

###########################################################################################################################################################

# subitem_empresa_pais

  destino <- read.table(file = 'OPERACOES_EMP_DESTINO.csv',fileEncoding = 'UTF16LE',skip = 4,
                        sep = ',',col.names = c('cd_nbs','desc_nbs','emp','cnpj','pais','valor'),colClasses ="character")
  
  destino$valor <- gsub("\\.",'',destino$valor)
  
  destino$valor <- gsub(",",".",destino$valor)
  
  destino$valor <- as.double(destino$valor)

# pais$valor <- as.double(pais$valor,options(digits = 12))


# empresa_operacoes_faturamento

  operacoes <- read.table('OPERACOES_EMP.csv',fileEncoding = 'UTF16LE',skip = 3,
                          sep = ',',col.names = c('cd_nbs','desc_nbs','cnpj','emp','oper','oper_dia_ini','oper_dia_fin','vlr_oper','vlr_fat'),colClasses = "character")
  
  operacoes$vlr_oper <- gsub("\\.",'',operacoes$vlr_oper)
  operacoes$vlr_fat <- gsub("\\.",'',operacoes$vlr_fat)
  
  operacoes$vlr_oper <- gsub(",",".",operacoes$vlr_oper)
  operacoes$vlr_fat <- gsub(",",".",operacoes$vlr_fat)
  
  operacoes$vlr_oper <- as.double(operacoes$vlr_oper)
  operacoes$vlr_fat <- as.double(operacoes$vlr_fat)
  
  operacoes$vlr_oper <- as.double(operacoes$vlr_oper,options(digits = 12))
  
  operacoes$oper_dia_ini <- dmy(operacoes$oper_dia_ini)
  
  operacoes$oper_dia_fin <- dmy(operacoes$oper_dia_fin)

# subitem_todas_empresas_por_ano_operacoes

  concorrentes <- read.table('CONCORRENTES_EMP.csv',fileEncoding = 'UTF16LE',skip = 5,
                             sep = ',',col.names = c('cd_nbs','desc_nbs','cnpj','emp','vlr_2014','vlr_2015','vlr_2016'),colClasses = "character")
  
  concorrentes$vlr_2014 <- gsub("\\.",'',concorrentes$vlr_2014)
  concorrentes$vlr_2015 <- gsub("\\.",'',concorrentes$vlr_2015)
  concorrentes$vlr_2016 <- gsub("\\.",'',concorrentes$vlr_2016)
  
  concorrentes$vlr_2014 <- gsub(",",".",concorrentes$vlr_2014)
  
  concorrentes$vlr_2015 <- gsub(",",".",concorrentes$vlr_2015)
  
  concorrentes$vlr_2016 <- gsub(",",".",concorrentes$vlr_2016)
  
  concorrentes$vlr_2014 <- as.double(concorrentes$vlr_2014)
  concorrentes$vlr_2015 <- as.double(concorrentes$vlr_2015)
  concorrentes$vlr_2016 <- as.double(concorrentes$vlr_2016)

##########################################################################################################################################################

# Geracao dos relatórios investigativos:

# Formata dados para criação das tabelas de apresentação dos dados


# Tratamento do arquivo subitem_todas_empresas_por_ano_operacoes

# Adiciona coluna correspondente a soma dos valores transacionados pelas empresas em cada ano. ex: empresa x (2014 = 1, 2015 = 2, 2016 = 3)
# Adiciona linha correspondente a soma dos valores transacionados pelas empresas por ano. ex: ano 2014 = (emp1 = 200 + emp2 = 200)

  concorrentes <- cbind(concorrentes,vlr_total_ult_anos_emp = rowSums(concorrentes[5:7],na.rm = TRUE))

  concorrentes <- rbind(concorrentes,vlr_total_por_ano = colSums(concorrentes[5:8],na.rm = TRUE))

# apaga celulas desnecessarias

  concorrentes[7,c(1:4)] <- ""

  concorrentes[7,4] <- "Valor Total"

# operacoes <- operacoes[-nrow(operacoes),]

  operacoes <- operacoes[-1,]

# Cria gráfico de barras, de acordo com o valor exportado para cada pais no qual a empresa transacionou

  ggplot(destino) +
    geom_col(mapping = aes(x = pais, y = valor))

# Cria distribuicao de valores correspondentes as operações da empresa


# Cria mapa de distribuição de valores pelos países


# Apresenta dados em formato de tabela

  datatable(concorrentes,rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))

  datatable(operacoes[5:9],rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))

  datatable(destino,rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))