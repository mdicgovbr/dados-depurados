# Carregamento das bibliotecas necessárias para execucao do codigo

  library(tidyverse)
  library(ggplot2)
  library(DT)
  library(lubridate)

# Importacao e Tratamento dos dados para manipulação

# Leitura dos arquivos em várias variáveis

# Tratamento dos arquivos

# Gerar relatorio investigativo para cada empresa em .Rmd

# Cria livro de investigacao com os relatorios criados (passo anterior), usando o formato bookdown.
  
# subitem_empresa_pais
  
  teste <- read_delim(file = 'invest/operacoes/1) 1_0504_90_00-86_846_847-0003-79-ALLINK TRANSPORTES INTERNACIONAIS LTDA.csv'
                    ,col_names = TRUE,col_types = cols(.default = 'c'),quote = "",trim_ws = TRUE,delim = ",")
  
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
  
  operacoes <- operacoes[-nrow(operacoes),]
  
  operacoes <- operacoes[-1,]

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
  
  concorrentes <- cbind(concorrentes,vlr_total_ult_anos_emp = rowSums(concorrentes[5:7],na.rm = TRUE))
  
  concorrentes <- rbind(concorrentes,vlr_total_por_ano = colSums(concorrentes[5:8],na.rm = TRUE))
  
  concorrentes[7,c(1:4)] <- ""
  
  concorrentes[7,4] <- "Valor Total"

# Cria gráfico de barras, de acordo com o valor exportado para cada pais no qual a empresa transacionou

  ggplot(destino) +
    geom_col(mapping = aes(x = pais, y = valor))

# Cria distribuicao de valores correspondentes as operações da empresa


# Cria mapa de distribuição de valores pelos países


# Apresenta dados em formato de tabela

  datatable(operacoes[5:9],rownames = FALSE, editable = TRUE,filter = 'top',options = list(pageLength = 10, autoWidth = FALSE))
  