#####################################################
# ENTREGA: TABELA AUXILIAR DO PBI
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
library(readxl)
#####################################################
#        CONFIGURANDO DIRETÓRIO DE DADOS
#####################################################

library(here)
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/DadosPRODUTIVIDADE/")


################################################
#                  TRIMESTRAL
################################################

#####################################################
#              LEITURA DE BASES
#####################################################

table_PROD_RS_1_va <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                  sheet = "va") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("va"),
                nivel  = rep("RS"))


table_PROD_RS_1_n <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                 sheet = "n") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("n"),
                nivel  = rep("RS"))


table_PROD_RS_1_hb <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                sheet = "hb") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("hb"),
                nivel  = rep("RS"))


table_PROD_RS_1_ef <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                 sheet = "ef") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("ef"),
                nivel  = rep("RS"))

table_PROD_RS_1_va_sa <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                 sheet = "va_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("va_sa"),
                nivel  = rep("RS"))


table_PROD_RS_1_n_sa <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                    sheet = "n_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("n_sa"),
                nivel  = rep("RS"))


table_PROD_RS_1_hb_sa <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                   sheet = "hb_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("hb_sa"),
                nivel  = rep("RS"))

table_PROD_RS_1_ef_sa <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                    sheet = "ef_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("ef_sa"),
                nivel  = rep("RS"))

table_PROD_RS_1_va_MM4 <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                    sheet = "va_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("va_MM4"),
                nivel  = rep("RS"))

table_PROD_RS_1_n_MM4 <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                     sheet = "n_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("n_MM4"),
                nivel  = rep("RS"))


table_PROD_RS_1_hb_MM4 <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                    sheet = "hb_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("hb_MM4"),
                nivel  = rep("RS"))


table_PROD_RS_1_ef_MM4 <- read_excel("Dados/table_PROD_RS_aux.xlsx", 
                                     sheet = "ef_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("ef_MM4"),
                nivel  = rep("RS"))

###

table_PROD_BR_1_va <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                 sheet = "va") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("va"),
                nivel  = rep("BR"))


table_PROD_BR_1_n <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                sheet = "n") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("n"),
                nivel  = rep("BR"))


table_PROD_BR_1_hb <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                 sheet = "hb") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("hb"),
                nivel  = rep("BR"))


table_PROD_BR_1_ef <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                 sheet = "ef") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("ef"),
                nivel  = rep("BR"))

table_PROD_BR_1_va_sa <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                    sheet = "va_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("va_sa"),
                nivel  = rep("BR"))


table_PROD_BR_1_n_sa <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                   sheet = "n_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("n_sa"),
                nivel  = rep("BR"))


table_PROD_BR_1_hb_sa <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                    sheet = "hb_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("hb_sa"),
                nivel  = rep("BR"))

table_PROD_BR_1_ef_sa <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                    sheet = "ef_sa") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("ef_sa"),
                nivel  = rep("BR"))

table_PROD_BR_1_va_MM4 <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                     sheet = "va_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("va_MM4"),
                nivel  = rep("BR"))

table_PROD_BR_1_n_MM4 <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                    sheet = "n_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("n_MM4"),
                nivel  = rep("BR"))


table_PROD_BR_1_hb_MM4 <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                     sheet = "hb_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("hb_MM4"),
                nivel  = rep("BR"))


table_PROD_BR_1_ef_MM4 <- read_excel("Dados/table_PROD_BR_aux.xlsx", 
                                     sheet = "ef_MM4") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = rep("ef_MM4"),
                nivel  = rep("BR"))


#################################################
#              EMPILHAMENTO
#################################################


table_PBI <- table_PROD_RS_1_va %>% 
  dplyr::bind_rows(table_PROD_RS_1_n, table_PROD_RS_1_hb, table_PROD_RS_1_ef,
                   table_PROD_RS_1_va_sa, table_PROD_RS_1_n_sa, table_PROD_RS_1_hb_sa, table_PROD_RS_1_ef_sa,
                   table_PROD_RS_1_va_MM4, table_PROD_RS_1_n_MM4, table_PROD_RS_1_hb_MM4, table_PROD_RS_1_ef_MM4,
                   
                   table_PROD_BR_1_va,    table_PROD_BR_1_n,     table_PROD_BR_1_hb,     table_PROD_BR_1_ef,
                   table_PROD_BR_1_va_sa, table_PROD_BR_1_n_sa,  table_PROD_BR_1_hb_sa,  table_PROD_BR_1_ef_sa,
                   table_PROD_BR_1_va_MM4,table_PROD_BR_1_n_MM4, table_PROD_BR_1_hb_MM4, table_PROD_BR_1_ef_MM4) %>% 
  tidyr::separate_wider_delim("PERÍODO", delim = ".", names = c("ano", "tri"))

#################################################
#                EXCEL
#################################################

writexl::write_xlsx(table_PBI, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PROD_TRIM_PBI.xlsx"))




################################################
#           ANUAL
################################################


#####################################################
#              LEITURA DE BASES
#####################################################

table_PROD_RS_1_va <- read_excel("Dados/table_PROD_RS_ANUAL_aux.xlsx", 
                                 sheet = "va") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "va",
                nivel  = "RS")


table_PROD_RS_1_n <- read_excel("Dados/table_PROD_RS_ANUAL_aux.xlsx", 
                                sheet = "n") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "n",
                nivel  = "RS")


table_PROD_RS_1_hb <- read_excel("Dados/table_PROD_RS_ANUAL_aux.xlsx", 
                                 sheet = "hb") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "hb",
                nivel  = "RS")


table_PROD_RS_1_ef <- read_excel("Dados/table_PROD_RS_ANUAL_aux.xlsx", 
                                 sheet = "ef") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "ef",
                nivel  = "RS")

###

table_PROD_BR_1_va <- read_excel("Dados/table_PROD_BR_ANUAL_aux.xlsx", 
                                 sheet = "va") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "va" ,
                nivel  = "BR" )


table_PROD_BR_1_n <- read_excel("Dados/table_PROD_BR_ANUAL_aux.xlsx", 
                                sheet = "n") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "n",  
                nivel  = "BR" )


table_PROD_BR_1_hb <- read_excel("Dados/table_PROD_BR_ANUAL_aux.xlsx", 
                                 sheet = "hb") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "hb", 
                nivel  = "BR" )


table_PROD_BR_1_ef <- read_excel("Dados/table_PROD_BR_ANUAL_aux.xlsx", 
                                 sheet = "ef") %>% 
  tidyr::pivot_longer(
    cols = c("Agropecuária":"SETOR MERCANTIL NÃO-AGRÍCOLA-FINANCEIRO"),
    names_to = "atividade",
    values_to = "valor"
  ) %>% 
  dplyr::mutate(indice = "ef", 
                nivel  = "BR" )




#################################################
#              EMPILHAMENTO
#################################################


table_BR_ANUAL <- table_PROD_BR_1_va %>% 
  dplyr::bind_rows(table_PROD_BR_1_n,     table_PROD_BR_1_hb,     table_PROD_BR_1_ef) %>% 
  dplyr::mutate("PERÍODO"= base::as.character(PERÍODO))

table_RS_ANUAL <- table_PROD_RS_1_va %>%
  dplyr::bind_rows(table_PROD_RS_1_n, table_PROD_RS_1_hb, table_PROD_RS_1_ef) 

table_PBI_ANUAL <-  table_RS_ANUAL %>% 
                    bind_rows(table_BR_ANUAL)

#################################################
#                EXCEL
#################################################

writexl::write_xlsx(table_PBI_ANUAL, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PROD_ANUAL_PBI.xlsx"))



