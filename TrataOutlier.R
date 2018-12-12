# Estatisticas do Comercio Exterior de Servicos

# Importar bibliotecas

  library(tidyverse)
  library(magrittr)
  library(readxl)

# Formatar dados
  
  nbs_aq <- read_xlsx('NBS.xlsx')
  emp_aq <- read_xlsx('EMPRESAS.xlsx')
  names(nbs_aq) <- c('cd_nbs','desc_nbs','vlr_tot_nbs')
  names(emp_aq) <- c('nbs_cpf/cnpj','cd_nbs','desc_nbs','cpf_cnpj','cnpj_nome','vlr_tot_emp','vlr_dia_med_emp')
  
# adcionar seguintes colunas na variavel nbs_aquisicao:
  # desvio_padrao_nbs
  # valor_diario_medio_nbs
  
    nbs <- emp_aq %>% 
        group_by(cd_nbs)%>%
        summarise(vlr_dia_med_nbs = mean(vlr_dia_med_emp,na.rm = FALSE),
        desv_pdr_nbs = sd(vlr_dia_med_emp,na.rm = FALSE))

    nbs_aquisicao <- merge(nbs_aq,nbs,by = 'cd_nbs')

    rm(nbs,nbs_aq)

# adcionar seguintes colunas na variavel empresas_aquisicao:
    
  # VALOR_TOTAL_NBS (valor em nbs_aquisicao)
  # VALOR_DIARIO_MEDIO_NBS (valor em nbs_aquisicao)
  # DESVIO_PADRAO_NBS (valor em nbs_aquisicao)

empresas_aquisicao <- nbs_aquisicao %>%
                      select(-desc_nbs)%>%
                      right_join(emp_aq,by = "cd_nbs")
rm(emp_aq)
    
  # z_norm = (vlr_dia_med_emp - vlr_dia_med_nbs)/desv_pdr_nbs,  
  # part =vlr_tot_emp/vlr_tot_nbs,
  # maior_0.10 = if_else(part > 0.1 & vlr_tot_nbs > 1000000,TRUE,FALSE),
  # avalia = if_else(part > 0.9 | z_norm > 2, TRUE,FALSE)

empresas_aquisicao <- empresas_aquisicao %>%
              mutate(z_norm = (vlr_dia_med_emp - vlr_dia_med_nbs)/desv_pdr_nbs,
                part =vlr_tot_emp/vlr_tot_nbs,
                maior_0.10 = if_else(part > 0.1 & vlr_tot_nbs > 1000000,TRUE,FALSE),
                avalia = if_else(part > 0.9 | z_norm > 2, TRUE,FALSE))

# selecionar outliers

provaveis_outliers <- empresas_aquisicao %>%
                        filter(avalia == TRUE ,maior_0.10 == TRUE)

# Importar relatorios investigativos

# gerar Dashboard investigativo

# selecionar dados CONFIRMADOS como outliers

