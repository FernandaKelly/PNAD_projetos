##############################################
#          PNAD Contínua
# Dados: 2012 - 2026 (UF)
##############################################
#            ANUAL               #
##################################
#    LEITURA DE PACOTES
##################################
options(timeout = 600) 
# install.packages("PNADcIBGE")
# install.packages("survey")
library(PNADcIBGE)
library(survey)
library(foreign)
library(srvyr)
library(reactable)
library(purrr)
library(readxl)
library(tidyverse)

#####################################################
#        CONFIGURANDO DIRETÓRIO DE DADOS
#####################################################

library(here)
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

#####################################################

################################################
# LEITURA DA BASE DE DADOS
################################################

abas <- c(
  "N PRINCIPAL",
  "HABITUAL PRINCIPAL",
  "EFETIVA PRINCIPAL",
  "N SECUNDÁRIO",
  "HABITUAL SECUNDÁRIO",
  "EFETIVA SECUNDÁRIO"
)

tabPSANO_UF <- map_dfr(
  abas,
  ~ read_excel("Dados/PRODUTIVIDADE/tabPSANOS_UF.xlsx", sheet = .x),
  .id = "tipo_principal"
)


################################################
# SOMA DOS IGUAIS
################################################
table_atividades <- tabPSANO_UF %>% 
  dplyr::mutate(atividade = dplyr::coalesce(cod_SCN_SEC, cod_SCN_PR)
  ) %>% 
  dplyr::group_by(atividade, Ano, UF) %>% 
  dplyr::summarise(
    soma_N             = sum(freq, na.rm = TRUE),
    qtd_horasHabituais = sum(Qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(Qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  )
###################################################################
# GRUPOS: INDÚSTRIA E SERVIÇOS
###################################################################
grupos <- list(
  "industria" = c(2, 3, 4, 5),
  "servicos"  = c(6, 7, 8, 9, 10, 11, 12)
)

table_grupos <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$industria ~ "industria",
      atividade %in% grupos$servicos  ~ "servicos",
      #atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, UF) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: TOTAL
###################################################################
grupos <- list(
  "total" = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

table_grupos_Total <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, UF) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)

###################################################################
# EMPILHAMENTO
###################################################################
table_ANOUF_PS <- dplyr::bind_rows(
  table_atividades %>%
    dplyr::mutate(
      atividade  = as.character(atividade)
    ), table_grupos, table_grupos_Total)


################################################
# Excel
################################################
# sheets <- list(
#                "P&S ANO UF" = table_ANORS_PS,
#                "P&S ANO BR" = table_ANOBR_PS
# )

writexl::write_xlsx(table_ANOUF_PS, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS_UF.xlsx"))


