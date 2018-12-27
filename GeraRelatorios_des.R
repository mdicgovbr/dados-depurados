# Carregamento das bibliotecas necessárias para execucao do codigo

  library(tidyverse)
  library(ggplot2)
  library(DT)
  library(lubridate)

# obs: será gerado vários vários relatórios, 1 para cada empresa identificada como outlier

# Importacao e Tratamento dos dados para manipulação

###########################################################################################################################################################

# subitem_empresa_pais

pais <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/subitem_empresa_pais/por_paises.csv',fileEncoding = 'UTF16LE',skip = 4,
                                   sep = ',',col.names = c('cd_nbs','desc_nbs','emp','cnpj','pais','valor'),colClasses ="character")

  pais$valor <- gsub("\\.",'',pais$valor)

  pais$valor <- gsub(",",".",pais$valor)

  pais$valor <- as.double(pais$valor)

  # pais$valor <- as.double(pais$valor,options(digits = 12))


# empresa_operacoes_faturamento

oper <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/empresa_operacoes_faturamento/operacoes.csv',fileEncoding = 'UTF16LE',skip = 3,
                                   sep = ',',col.names = c('cd_nbs','desc_nbs','cnpj','emp','oper','oper_dia_ini','oper_dia_fin','vlr_oper','vlr_fat'),colClasses = "character")

  oper$vlr_oper <- gsub("\\.",'',oper$vlr_oper)
  oper$vlr_fat <- gsub("\\.",'',oper$vlr_fat)

  oper$vlr_oper <- gsub(",",".",oper$vlr_oper)
  oper$vlr_fat <- gsub(",",".",oper$vlr_fat)

  oper$vlr_oper <- as.double(oper$vlr_oper)
  oper$vlr_fat <- as.double(oper$vlr_fat)

  oper$vlr_oper <- as.double(oper$vlr_oper,options(digits = 12))

  oper$oper_dia_ini <- dmy(oper$oper_dia_ini)

  oper$oper_dia_fin <- dmy(oper$oper_dia_fin)

# subitem_todas_empresas_por_ano_operacoes

emp_por_ano <- read.table(file = '/home/erlan.mesquita/Documentos/RStudio/estatistica/subitem_todas_empresas_por_ano_operacoes/empresas_por_ano.csv',fileEncoding = 'UTF16LE',skip = 5,
                                   sep = ',',col.names = c('cd_nbs','desc_nbs','cnpj','emp','vlr_2014','vlr_2015','vlr_2016'),colClasses = "character")

  emp_por_ano$vlr_2014 <- gsub("\\.",'',emp_por_ano$vlr_2014)
  emp_por_ano$vlr_2015 <- gsub("\\.",'',emp_por_ano$vlr_2015)
  emp_por_ano$vlr_2016 <- gsub("\\.",'',emp_por_ano$vlr_2016)

  emp_por_ano$vlr_2014 <- gsub(",",".",emp_por_ano$vlr_2014)

  emp_por_ano$vlr_2015 <- gsub(",",".",emp_por_ano$vlr_2015)

  emp_por_ano$vlr_2016 <- gsub(",",".",emp_por_ano$vlr_2016)

  emp_por_ano$vlr_2014 <- as.double(emp_por_ano$vlr_2014)
  emp_por_ano$vlr_2015 <- as.double(emp_por_ano$vlr_2015)
  emp_por_ano$vlr_2016 <- as.double(emp_por_ano$vlr_2016)

##########################################################################################################################################################

# Geracao dos relatórios investigativos:

# Formata dados para criação das tabelas de apresentação dos dados


# Tratamento do arquivo subitem_todas_empresas_por_ano_operacoes

# Adiciona coluna correspondente a soma dos valores transacionados pelas empresas em cada ano. ex: empresa x (2014 = 1, 2015 = 2, 2016 = 3)
# Adiciona linha correspondente a soma dos valores transacionados pelas empresas por ano. ex: ano 2014 = (emp1 = 200 + emp2 = 200)

  emp_por_ano <- cbind(emp_por_ano,vlr_total_ult_anos_emp = rowSums(emp_por_ano[5:7],na.rm = TRUE))

  emp_por_ano <- rbind(emp_por_ano,vlr_total_por_ano = colSums(emp_por_ano[5:8],na.rm = TRUE))

  # apaga celulas desnecessarias

    emp_por_ano[7,c(1:4)] <- ""

    emp_por_ano[7,4] <- "Valor Total"

    oper <- oper[-nrow(oper),]

# Cria gráfico de barras, de acordo com o valor exportado para cada pais no qual a empresa transacionou

  ggplot(data = pais) +
    geom_col(mapping = aes(x = pais, y = valor))

# Cria distribuicao de valores correspondentes as operações da empresa

  plot(oper$vlr_oper)

# Cria mapa de distribuição de valores pelos países


# Apresenta dados em formato de tabela

datatable(emp_por_ano,rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))

datatable(oper,rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))

datatable(pais,rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))





