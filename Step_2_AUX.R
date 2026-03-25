#####################################################
# ENTREGA: TABELA AUXILIAR
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

######################################################################
# TABELA AUXILIAR COM AJUSTE, SEM AJUSTE, E CV
######################################################################
#                         RIO GRANDE DO SUL
######################################################################
table_PS_9_RS <- readxl::read_excel("Dados/table_PS_9.xlsx", 
                                    sheet = "Indicador ANO RS") %>% 
  dplyr::select(atividade:VA_RS_soma) %>% 
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
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")) %>% 
                
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS_soma, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas) %>% 
  dplyr::mutate(qtd_horasHabituais  = qtd_horasHabituais*51.6,
                qtd_horasEfetivas   = qtd_horasEfetivas*51.6)


va <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS_soma) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS_soma) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

######################################################################
# CV: PRINCIPAL
######################################################################

tabPSANOS_RS_10_n <- read_excel("Dados/tabPSANOS_RS_10.xlsx", 
                                         sheet = "N PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, freq_cv) %>% 
  dplyr::mutate(cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
                                              cod_SCN_PR == 2~ "Indústria extrativa",
                                              cod_SCN_PR == 3~ "Indústria de transformação",
                                              cod_SCN_PR == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_PR == 5~ "Construção",
                                              cod_SCN_PR == 6~ "Comércio",
                                              cod_SCN_PR == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_PR == 8~ "Informação e comunicação",
                                              cod_SCN_PR == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_PR == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_PR == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_p_cv <- tabPSANOS_RS_10_n %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")




tabPSANOS_RS_10_hb <- read_excel("Dados/tabPSANOS_RS_10.xlsx", 
                                sheet = "HABITUAL PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
                                              cod_SCN_PR == 2~ "Indústria extrativa",
                                              cod_SCN_PR == 3~ "Indústria de transformação",
                                              cod_SCN_PR == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_PR == 5~ "Construção",
                                              cod_SCN_PR == 6~ "Comércio",
                                              cod_SCN_PR == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_PR == 8~ "Informação e comunicação",
                                              cod_SCN_PR == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_PR == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_PR == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_p_cv <- tabPSANOS_RS_10_hb %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")


tabPSANOS_RS_10_ef <- read_excel("Dados/tabPSANOS_RS_10.xlsx", 
                                 sheet = "EFETIVA PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
                                              cod_SCN_PR == 2~ "Indústria extrativa",
                                              cod_SCN_PR == 3~ "Indústria de transformação",
                                              cod_SCN_PR == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_PR == 5~ "Construção",
                                              cod_SCN_PR == 6~ "Comércio",
                                              cod_SCN_PR == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_PR == 8~ "Informação e comunicação",
                                              cod_SCN_PR == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_PR == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_PR == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_p_cv <- tabPSANOS_RS_10_ef %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasEfetivas_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")


######################################################################
# CV: SECUNDARIO
######################################################################

tabPSANOS_RS_10_n <- read_excel("Dados/tabPSANOS_RS_10.xlsx", 
                                sheet = "N SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, freq_cv) %>% 
  dplyr::mutate(cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
                                              cod_SCN_SEC == 2~ "Indústria extrativa",
                                              cod_SCN_SEC == 3~ "Indústria de transformação",
                                              cod_SCN_SEC == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_SEC == 5~ "Construção",
                                              cod_SCN_SEC == 6~ "Comércio",
                                              cod_SCN_SEC == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_SEC == 8~ "Informação e comunicação",
                                              cod_SCN_SEC == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_SEC == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_SEC == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_s_cv <- tabPSANOS_RS_10_n %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")




tabPSANOS_RS_10_hb <- read_excel("Dados/tabPSANOS_RS_10.xlsx", 
                                 sheet = "HABITUAL SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
                                              cod_SCN_SEC == 2~ "Indústria extrativa",
                                              cod_SCN_SEC == 3~ "Indústria de transformação",
                                              cod_SCN_SEC == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_SEC == 5~ "Construção",
                                              cod_SCN_SEC == 6~ "Comércio",
                                              cod_SCN_SEC == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_SEC == 8~ "Informação e comunicação",
                                              cod_SCN_SEC == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_SEC == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_SEC == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_s_cv <- tabPSANOS_RS_10_hb %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")


tabPSANOS_RS_10_ef <- read_excel("Dados/tabPSANOS_RS_10.xlsx", 
                                 sheet = "EFETIVA SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
                                              cod_SCN_SEC == 2~ "Indústria extrativa",
                                              cod_SCN_SEC == 3~ "Indústria de transformação",
                                              cod_SCN_SEC == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_SEC == 5~ "Construção",
                                              cod_SCN_SEC == 6~ "Comércio",
                                              cod_SCN_SEC == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_SEC == 8~ "Informação e comunicação",
                                              cod_SCN_SEC == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_SEC == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_SEC == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_s_cv <- tabPSANOS_RS_10_ef %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = Qtd_horasEfetivas_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")

#####################################################
#                        EXCEL
#####################################################

sheets <- list("va"          = va,
               "n"           = n,
               "hb"          = hb,
               "ef"          = ef,
               "n_p_cv"      = n_p_cv, 
               "hb_p_cv"     = hb_p_cv,
               "ef_p_cv"     = ef_p_cv,
               "n_s_cv"      = n_s_cv, 
               "hb_s_cv"     = hb_s_cv,
               "ef_s_cv"     = ef_s_cv
)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_RS_ANUAL_1_aux.xlsx"))


######################################################################
#                         BRASIL
######################################################################
table_PS_9_BR <- readxl::read_excel("Dados/table_PS_9.xlsx", 
                                    sheet = "Indicador ANO BR") %>% 
  dplyr::select(atividade:VA_BR_soma) %>% 
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
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA")) %>% 
  
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR_soma, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas) %>% 
  dplyr::mutate(qtd_horasHabituais  = qtd_horasHabituais*51.6,
                qtd_horasEfetivas   = qtd_horasEfetivas*51.6)


va <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR_soma) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_BR_soma) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

n <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

hb <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

ef <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

######################################################################
# CV: PRINCIPAL
######################################################################

tabPSANOS_BR_5_n <- read_excel("Dados/tabPSANOS_BR_5.xlsx", 
                                sheet = "N PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, freq_cv) %>% 
  dplyr::mutate(cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
                                              cod_SCN_PR == 2~ "Indústria extrativa",
                                              cod_SCN_PR == 3~ "Indústria de transformação",
                                              cod_SCN_PR == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_PR == 5~ "Construção",
                                              cod_SCN_PR == 6~ "Comércio",
                                              cod_SCN_PR == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_PR == 8~ "Informação e comunicação",
                                              cod_SCN_PR == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_PR == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_PR == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_p_cv <- tabPSANOS_BR_5_n %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")




tabPSANOS_BR_5_hb <- read_excel("Dados/tabPSANOS_BR_5.xlsx", 
                                 sheet = "HABITUAL PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
                                              cod_SCN_PR == 2~ "Indústria extrativa",
                                              cod_SCN_PR == 3~ "Indústria de transformação",
                                              cod_SCN_PR == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_PR == 5~ "Construção",
                                              cod_SCN_PR == 6~ "Comércio",
                                              cod_SCN_PR == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_PR == 8~ "Informação e comunicação",
                                              cod_SCN_PR == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_PR == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_PR == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_p_cv <- tabPSANOS_BR_5_hb %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")


tabPSANOS_BR_5_ef <- read_excel("Dados/tabPSANOS_BR_5.xlsx", 
                                 sheet = "EFETIVA PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
                                              cod_SCN_PR == 2~ "Indústria extrativa",
                                              cod_SCN_PR == 3~ "Indústria de transformação",
                                              cod_SCN_PR == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                              cod_SCN_PR == 5~ "Construção",
                                              cod_SCN_PR == 6~ "Comércio",
                                              cod_SCN_PR == 7~ "Transporte, armazenagem e correio",
                                              cod_SCN_PR == 8~ "Informação e comunicação",
                                              cod_SCN_PR == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                              cod_SCN_PR == 10 ~ "Atividades imobiliárias",
                                              cod_SCN_PR == 11 ~ "Outras atividades de serviços",
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_p_cv <- tabPSANOS_BR_5_ef %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasEfetivas_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")


######################################################################
# CV: SECUNDARIO
######################################################################

tabPSANOS_BR_5_n <- read_excel("Dados/tabPSANOS_BR_5.xlsx", 
                                sheet = "N SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, freq_cv) %>% 
  dplyr::mutate(cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
                                               cod_SCN_SEC == 2~ "Indústria extrativa",
                                               cod_SCN_SEC == 3~ "Indústria de transformação",
                                               cod_SCN_SEC == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                               cod_SCN_SEC == 5~ "Construção",
                                               cod_SCN_SEC == 6~ "Comércio",
                                               cod_SCN_SEC == 7~ "Transporte, armazenagem e correio",
                                               cod_SCN_SEC == 8~ "Informação e comunicação",
                                               cod_SCN_SEC == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                               cod_SCN_SEC == 10 ~ "Atividades imobiliárias",
                                               cod_SCN_SEC == 11 ~ "Outras atividades de serviços",
                                               cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_s_cv <- tabPSANOS_BR_5_n %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")




tabPSANOS_BR_5_hb <- read_excel("Dados/tabPSANOS_BR_5.xlsx", 
                                 sheet = "HABITUAL SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
                                               cod_SCN_SEC == 2~ "Indústria extrativa",
                                               cod_SCN_SEC == 3~ "Indústria de transformação",
                                               cod_SCN_SEC == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                               cod_SCN_SEC == 5~ "Construção",
                                               cod_SCN_SEC == 6~ "Comércio",
                                               cod_SCN_SEC == 7~ "Transporte, armazenagem e correio",
                                               cod_SCN_SEC == 8~ "Informação e comunicação",
                                               cod_SCN_SEC == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                               cod_SCN_SEC == 10 ~ "Atividades imobiliárias",
                                               cod_SCN_SEC == 11 ~ "Outras atividades de serviços",
                                               cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_s_cv <- tabPSANOS_BR_5_hb %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")


tabPSANOS_BR_5_ef <- read_excel("Dados/tabPSANOS_BR_5.xlsx", 
                                 sheet = "EFETIVA SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
                                               cod_SCN_SEC == 2~ "Indústria extrativa",
                                               cod_SCN_SEC == 3~ "Indústria de transformação",
                                               cod_SCN_SEC == 4~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                               cod_SCN_SEC == 5~ "Construção",
                                               cod_SCN_SEC == 6~ "Comércio",
                                               cod_SCN_SEC == 7~ "Transporte, armazenagem e correio",
                                               cod_SCN_SEC == 8~ "Informação e comunicação",
                                               cod_SCN_SEC == 9~ "Atividades financeiras, de seguros e serviços relacionados",
                                               cod_SCN_SEC == 10 ~ "Atividades imobiliárias",
                                               cod_SCN_SEC == 11 ~ "Outras atividades de serviços",
                                               cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social")
                
  ) %>% 
  dplyr::rename("PERÍODO" = Ano) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_s_cv <- tabPSANOS_BR_5_ef %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = Qtd_horasEfetivas_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")

#####################################################
#                        EXCEL
#####################################################

sheets <- list("va"          = va,
               "n"           = n,
               "hb"          = hb,
               "ef"          = ef,
               "n_p_cv"      = n_p_cv, 
               "hb_p_cv"     = hb_p_cv,
               "ef_p_cv"     = ef_p_cv,
               "n_s_cv"      = n_s_cv, 
               "hb_s_cv"     = hb_s_cv,
               "ef_s_cv"     = ef_s_cv
)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_BR_ANUAL_1_aux.xlsx"))


