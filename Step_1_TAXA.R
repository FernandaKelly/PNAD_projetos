#####################################################
# ENTREGA: TAXAS
# AGRUPADO: --
# ANOS: 2012 - 2024
# PERÍODO: TRIMESTRAL
#####################################################
options(timeout = 600) 
options(scipen = 999)
# install.packages("PNADcIBGE")
# install.packages("survey")
library(PNADcIBGE)
library(survey)
library(foreign)
library(srvyr)
library(reactable)
library(purrr)
library(tidyverse)
library(zoo)
library(slider)
#####################################################
#        CONFIGURANDO DIRETÓRIO DE DADOS
#####################################################

library(here)
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados")
#####################################################

#####################################################
# RIO GRANDE DO SUL
#####################################################
#                     INTERANUAL
#####################################################
table_PS_9 <- readxl::read_excel("Dados/table_PS_9.xlsx", 
                                 sheet = "Indicador TRI RS")
#####################################################

dadosRS_TAXA_INTERANUAL_RS <- table_PS_9 %>% 
  dplyr::arrange(atividade, Trimestre, Ano) %>%
  dplyr::group_by(atividade, Trimestre) %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()

#####################################################
#              IMEDIATAMENTE ANTERIOR
#####################################################
table_PS_SAZ_6 <- readxl::read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                 sheet = "P&S SAZ_VAR RS")
#####################################################

dadosRS_IMED_ANTERIOR_RS <- table_PS_SAZ_6 %>% 
  dplyr::arrange(atividade, Ano, Trimestre) %>%
  dplyr::group_by(atividade) %>%
  dplyr::mutate(
    taxa_IMED_ANTERIOR_N = 100 * (indicador_N - lag(indicador_N)) / lag(indicador_N),
    taxa_IMED_ANTERIOR_qtd_HHabituais = 100 * (indicador_qtd_horasHabituais - lag(indicador_qtd_horasHabituais)) / lag(indicador_qtd_horasHabituais),
    taxa_IMED_ANTERIOR_qtd_HEfetivas = 100 * (indicador_qtd_horasEfetivas - lag(indicador_qtd_horasEfetivas)) / lag(indicador_qtd_horasEfetivas),
  ) %>%
  ungroup()
#####################################################
#             ACUMULADA EM 4 TRI
#####################################################
table_PS_MM_2 <- readxl::read_excel("Dados/table_PS_MM_2.xlsx", 
                                    sheet = "MM4 RS TRI")
#####################################################

dadosRS_ACUMULADA_RS <- table_PS_MM_2 %>% 
  dplyr::arrange(atividade, Ano, Trimestre) %>%
  dplyr::group_by(atividade) %>%
  dplyr::mutate(
    taxa_ACUMULADA_N = 100 * (indicadorVA_N_MM4 - lag(indicadorVA_N_MM4)) / lag(indicadorVA_N_MM4),
    taxa_ACUMULADA_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais_MM4 - lag(indicadorVA_qtd_HHabituais_MM4)) / lag(indicadorVA_qtd_HHabituais_MM4),
    taxa_ACUMULADA_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas_MM4 - lag(indicadorVA_qtd_HEfetivas_MM4)) / lag(indicadorVA_qtd_HEfetivas_MM4),
  ) %>%
  ungroup()


#####################################################
# BRASIL
#####################################################
#                     INTERANUAL
#####################################################
table_PS_9 <- readxl::read_excel("Dados/table_PS_9.xlsx", 
                                 sheet = "Indicador TRI BR")
#####################################################

dadosRS_TAXA_INTERANUAL_BR <- table_PS_9 %>% 
  dplyr::arrange(atividade, Trimestre, Ano) %>%
  dplyr::group_by(atividade, Trimestre) %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()

#####################################################
#              IMEDIATAMENTE ANTERIOR
#####################################################
table_PS_SAZ_6 <- readxl::read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                     sheet = "P&S SAZ_VAR BR")
#####################################################

dadosRS_IMED_ANTERIOR_BR <- table_PS_SAZ_6 %>% 
  dplyr::arrange(atividade, Ano, Trimestre) %>%
  dplyr::group_by(atividade) %>%
  dplyr::mutate(
    taxa_IMED_ANTERIOR_N = 100 * (indicador_N - lag(indicador_N)) / lag(indicador_N),
    taxa_IMED_ANTERIOR_qtd_HHabituais = 100 * (indicador_qtd_horasHabituais - lag(indicador_qtd_horasHabituais)) / lag(indicador_qtd_horasHabituais),
    taxa_IMED_ANTERIOR_qtd_HEfetivas = 100 * (indicador_qtd_horasEfetivas - lag(indicador_qtd_horasEfetivas)) / lag(indicador_qtd_horasEfetivas),
  ) %>%
  ungroup()
#####################################################
#             ACUMULADA EM 4 TRI
#####################################################
table_PS_MM_2 <- readxl::read_excel("Dados/table_PS_MM_2.xlsx", 
                                    sheet = "MM4 BR TRI")
#####################################################

dadosRS_ACUMULADA_BR <- table_PS_MM_2 %>% 
  dplyr::arrange(atividade, Ano, Trimestre) %>%
  dplyr::group_by(atividade) %>%
  dplyr::mutate(
    taxa_ACUMULADA_N = 100 * (indicadorVA_N_MM4 - lag(indicadorVA_N_MM4)) / lag(indicadorVA_N_MM4),
    taxa_ACUMULADA_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais_MM4 - lag(indicadorVA_qtd_HHabituais_MM4)) / lag(indicadorVA_qtd_HHabituais_MM4),
    taxa_ACUMULADA_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas_MM4 - lag(indicadorVA_qtd_HEfetivas_MM4)) / lag(indicadorVA_qtd_HEfetivas_MM4),
  ) %>%
  ungroup()


###################################
#            EXCEL
###################################

sheets <- list("dadosRS_TAXA_INTERANUAL_RS"   = dadosRS_TAXA_INTERANUAL_RS,
               "dadosRS_IMED_ANTERIOR_RS"   = dadosRS_IMED_ANTERIOR_RS,
               "dadosRS_ACUMULADA_RS"   = dadosRS_ACUMULADA_RS,
               "dadosRS_TAXA_INTERANUAL_BR"   = dadosRS_TAXA_INTERANUAL_BR,
               "dadosRS_IMED_ANTERIOR_BR"   = dadosRS_IMED_ANTERIOR_BR,
               "dadosRS_ACUMULADA_BR"   = dadosRS_ACUMULADA_BR)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_TAXA_1.xlsx"))









