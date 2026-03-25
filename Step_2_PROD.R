#####################################################
# ENTREGA: INDICADOR DE PRODUTIVIDADE
# AGRUPADO: --
# ANOS: 2012 - 2024
# PERÍODO: ANUAL
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
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados")

#####################################################
#                LEITURA DAS BASES
#####################################################
#####################################################
#              RIO GRANDE DO SUL
#####################################################

table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador ANO RS") %>% 
  dplyr::select(atividade, Ano, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
  dplyr::mutate(atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")
                )  %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)
  
###

prod_n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  
  
  
#####################################################
#                     INTERANUAL
#####################################################

dadosRS_TAXA_INTERANUAL_RS <- table_PS_9_RS %>% 
  dplyr::arrange(atividade, "PERÍODO") %>%
  dplyr::group_by(atividade, "PERÍODO") %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()  
  
###

tx_prod_n <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  
  
  
#####################################################
#                        EXCEL
#####################################################

sheets <- list("prod_n"          = prod_n,
               "prod_hb"         = prod_hb,
               "prod_ef"         = prod_ef,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD__ANUAL_RS_1.xlsx"))

#####################################################
#              RIO GRANDE DO SUL
#####################################################

table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador ANO RS") %>% 
  dplyr::select(atividade, Ano, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
  dplyr::mutate(atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")
  )  %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)

###

prod_n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                     INTERANUAL
#####################################################

dadosRS_TAXA_INTERANUAL_RS <- table_PS_9_RS %>% 
  dplyr::arrange(atividade, "PERÍODO") %>%
  dplyr::group_by(atividade, "PERÍODO") %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()  

###

tx_prod_n <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                        EXCEL
#####################################################

sheets <- list("prod_n"          = prod_n,
               "prod_hb"         = prod_hb,
               "prod_ef"         = prod_ef,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD__ANUAL_RS_1.xlsx"))

#####################################################
#              RIO GRANDE DO SUL
#####################################################

table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador ANO RS") %>% 
  dplyr::select(atividade, Ano, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
  dplyr::mutate(atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")
  )  %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)

###

prod_n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                     INTERANUAL
#####################################################

dadosRS_TAXA_INTERANUAL_RS <- table_PS_9_RS %>% 
  dplyr::arrange(atividade, "PERÍODO") %>%
  dplyr::group_by(atividade, "PERÍODO") %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()  

###

tx_prod_n <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                        EXCEL
#####################################################

sheets <- list("prod_n"          = prod_n,
               "prod_hb"         = prod_hb,
               "prod_ef"         = prod_ef,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD__ANUAL_RS_1.xlsx"))


#####################################################
#              RIO GRANDE DO SUL
#####################################################

table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador ANO RS") %>% 
  dplyr::select(atividade, Ano, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
  dplyr::mutate(atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")
  )  %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)

###

prod_n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                     INTERANUAL
#####################################################

dadosRS_TAXA_INTERANUAL_RS <- table_PS_9_RS %>% 
  dplyr::arrange(atividade, "PERÍODO") %>%
  dplyr::group_by(atividade, "PERÍODO") %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()  

###

tx_prod_n <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                        EXCEL
#####################################################

sheets <- list("prod_n"          = prod_n,
               "prod_hb"         = prod_hb,
               "prod_ef"         = prod_ef,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD__ANUAL_RS_1.xlsx"))

#####################################################
#              RIO GRANDE DO SUL
#####################################################

table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador ANO RS") %>% 
  dplyr::select(atividade, Ano, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
  dplyr::mutate(atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")
  )  %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)

###

prod_n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                     INTERANUAL
#####################################################

dadosRS_TAXA_INTERANUAL_RS <- table_PS_9_RS %>% 
  dplyr::arrange(atividade, "PERÍODO") %>%
  dplyr::group_by(atividade, "PERÍODO") %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()  

###

tx_prod_n <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef <- dadosRS_TAXA_INTERANUAL_RS %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                        EXCEL
#####################################################

sheets <- list("prod_n"          = prod_n,
               "prod_hb"         = prod_hb,
               "prod_ef"         = prod_ef,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD__ANUAL_RS_1.xlsx"))

#####################################################
#               Brasil
#####################################################

table_PS_9_BR <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador ANO BR") %>% 
  dplyr::select(atividade, Ano, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
  dplyr::mutate(atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")
  )  %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)

###

prod_n <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                     INTERANUAL
#####################################################

dadosRS_TAXA_INTERANUAL_BR <- table_PS_9_BR %>% 
  dplyr::arrange(atividade, "PERÍODO") %>%
  dplyr::group_by(atividade, "PERÍODO") %>%
  dplyr::mutate(
    taxa_interanual_N = 100 * (indicadorVA_N - lag(indicadorVA_N)) / lag(indicadorVA_N),
    taxa_interanual_qtd_HHabituais = 100 * (indicadorVA_qtd_HHabituais - lag(indicadorVA_qtd_HHabituais)) / lag(indicadorVA_qtd_HHabituais),
    taxa_interanual_qtd_HEfetivas = 100 * (indicadorVA_qtd_HEfetivas - lag(indicadorVA_qtd_HEfetivas)) / lag(indicadorVA_qtd_HEfetivas),
  ) %>%
  ungroup()  

###

tx_prod_n <- dadosRS_TAXA_INTERANUAL_BR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb <- dadosRS_TAXA_INTERANUAL_BR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef <- dadosRS_TAXA_INTERANUAL_BR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")  


#####################################################
#                        EXCEL
#####################################################

sheets <- list("prod_n"          = prod_n,
               "prod_hb"         = prod_hb,
               "prod_ef"         = prod_ef,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD__ANUAL_BR_1.xlsx"))
