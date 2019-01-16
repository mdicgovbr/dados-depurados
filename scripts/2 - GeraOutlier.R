# IDENFITICACAO DOS POSSIVEIS OUTLIERS

# Carregamento das bibliotecas necessarias

  library(tidyverse)
  library(magrittr)

# Formatar dados

  names(NBS) <- c('cd_nbs','desc_nbs','vlr_tot_nbs')
  names(EMPRESAS) <- c('cd_nbs','desc_nbs','cpf_cnpj','cpf_cnpj/nome','vlr_tot_emp','vlr_dia_med_emp')

# adcionar seguintes colunas na variavel nbs_aquisicao:
  # desvio_padrao_nbs
  # valor_diario_medio_nbs

    nbs <- EMPRESAS %>%
        group_by(cd_nbs)%>%
        summarise(vlr_dia_med_nbs = mean(vlr_dia_med_emp,na.rm = FALSE),
        desv_pdr_nbs = sd(vlr_dia_med_emp,na.rm = FALSE))

    NBS <- merge(NBS,nbs,by = 'cd_nbs')

    rm(nbs)

# adcionar seguintes colunas na variavel empresas_aquisicao:

  # vlr_tot_nbs
  # vlr_dia_med_nbs (valor em nbs_aquisicao)
  # desv_pdr_nbs (valor em nbs_aquisicao)
  # z_norm = (vlr_dia_med_nbs - vlr_dia_med_emp)/desv_pdr_nbs,
  # part =vlr_tot_emp/vlr_tot_nbs,
  # maior_0.10 = if_else(part > 0.1 & vlr_tot_nbs > 1000000,TRUE,FALSE),
  # avalia = if_else(part > 0.9 | z_norm > 2, TRUE,FALSE)

  EMPRESAS <- NBS %>%
    select(-desc_nbs)%>%
    right_join(EMPRESAS,by = "cd_nbs")
  
  EMPRESAS <- EMPRESAS %>%
    mutate(z_norm = (vlr_dia_med_emp - vlr_dia_med_nbs)/desv_pdr_nbs,
           part =vlr_tot_emp/vlr_tot_nbs,
           maior_0.10 = if_else(part > 0.1 & vlr_tot_nbs > 1000000,TRUE,FALSE),
           avalia = if_else(part > 0.9 | z_norm > 2, TRUE,FALSE))

# selecionar outliers

  provaveis_outliers <- EMPRESAS %>%
                        filter(avalia == TRUE ,maior_0.10 == TRUE)


