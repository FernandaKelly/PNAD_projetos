#####################################################
# ENTREGA: TABELA AUXILIAR
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
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados")

######################################################################
# TABELA AUXILIAR COM AJUSTE, SEM AJUSTE, MÉDIA MÓVEL E CV
######################################################################
#                         RIO GRANDE DO SUL
######################################################################
table_PS_9_RS <- readxl::read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador TRI RS") %>% 
  dplyr::select(atividade:VA_RS) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
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
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas) %>% 
  dplyr::mutate(qtd_horasHabituais  = qtd_horasHabituais*12.9,
                qtd_horasEfetivas   = qtd_horasEfetivas*12.9)


va <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

###

table_PS_SAZ_6_RS <- readxl::read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                sheet = "P&S SAZ_VAR RS") %>% 
  dplyr::select(atividade:VA_RS) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
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
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas)


va_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


n_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


###

table_PS_MM_3_RS <- readxl::read_excel("Dados/table_PS_MM_3.xlsx", 
                                           sheet = "MM4 RS TRI") %>% 
  dplyr::select(atividade, Ano, Trimestre, soma_N_MM4, qtd_horasHabituais_MM4, qtd_horasEfetivas_MM4, VA_RS_MM4) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
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
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", soma_N_MM4, qtd_horasHabituais_MM4, qtd_horasEfetivas_MM4, VA_RS_MM4)


va_MM4 <- table_PS_MM_3_RS %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


n_MM4 <- table_PS_MM_3_RS %>% 
  dplyr::select(atividade, "PERÍODO", soma_N_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb_MM4 <- table_PS_MM_3_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef_MM4 <- table_PS_MM_3_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

###

tabPSTRI_RS_4_n <- read_excel("Dados/tabPSTRI_RS_4.xlsx", 
                                                sheet = "N PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Trimestre, freq_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
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
                                             cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_p_cv <- tabPSTRI_RS_4_n %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_RS_4_hb <- read_excel("Dados/tabPSTRI_RS_4.xlsx", 
                              sheet = "HABITUAL PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Trimestre, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
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
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_p_cv <- tabPSTRI_RS_4_hb %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_RS_4_ef <- read_excel("Dados/tabPSTRI_RS_4.xlsx", 
                               sheet = "EFETIVA PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Trimestre, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
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
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_p_cv <- tabPSTRI_RS_4_ef %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasEfetivas_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



###


tabPSTRI_RS_4_n <- read_excel("Dados/tabPSTRI_RS_4.xlsx", 
                              sheet = "N SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Trimestre, freq_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
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
                                              cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_s_cv <- tabPSTRI_RS_4_n %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_RS_4_hb <- read_excel("Dados/tabPSTRI_RS_4.xlsx", 
                               sheet = "HABITUAL SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Trimestre, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
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
                                              cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_s_cv <- tabPSTRI_RS_4_hb %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_RS_4_ef <- read_excel("Dados/tabPSTRI_RS_4.xlsx", 
                               sheet = "EFETIVA SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Trimestre, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
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
                                              cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_s_cv <- tabPSTRI_RS_4_ef %>% 
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
               "va_sa"       = va_sa,
               "n_sa"        = n_sa,
               "hb_sa"       = hb_sa,
               "ef_sa"       = ef_sa,
               "n_p_cv"      = n_p_cv, 
               "hb_p_cv"     = hb_p_cv,
               "ef_p_cv"     = ef_p_cv,
               "n_s_cv"      = n_s_cv, 
               "hb_s_cv"     = hb_s_cv,
               "ef_s_cv"     = ef_s_cv
)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_RS_1_aux.xlsx"))


######################################################################
#                             BRASIL
######################################################################

table_PS_9_BR <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador TRI BR") %>% 
  dplyr::select(atividade:VA_BR) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
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
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas) %>% 
  dplyr::mutate(qtd_horasHabituais = qtd_horasHabituais*12.9,
                qtd_horasEfetivas   = qtd_horasEfetivas*12.9)


va <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_BR) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

n <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")



table_PS_SAZ_6_BR <- read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                sheet = "P&S SAZ_VAR BR") %>% 
  dplyr::select(atividade:VA_BR) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
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
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas)


va_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_BR) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


n_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


###

tabPSTRI_BR_4_n <- read_excel("Dados/tabPSTRI_BR_4.xlsx", 
                              sheet = "N PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Trimestre, freq_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
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
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_p_cv <- tabPSTRI_BR_4_n %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_BR_4_hb <- read_excel("Dados/tabPSTRI_BR_4.xlsx", 
                               sheet = "HABITUAL PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Trimestre, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
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
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_p_cv <- tabPSTRI_BR_4_hb %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_BR_4_ef <- read_excel("Dados/tabPSTRI_BR_4.xlsx", 
                               sheet = "EFETIVA PRINCIPAL") %>% 
  dplyr::filter(cod_SCN_PR != 0) %>% 
  dplyr::select(cod_SCN_PR, Ano, Trimestre, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_PR = dplyr::case_when(cod_SCN_PR == 1~ "Agropecuária",
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
                                              cod_SCN_PR == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_p_cv <- tabPSTRI_BR_4_ef %>% 
  dplyr::select(cod_SCN_PR, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_PR,
                     values_from = Qtd_horasEfetivas_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



###


tabPSTRI_BR_4_n <- read_excel("Dados/tabPSTRI_BR_4.xlsx", 
                              sheet = "N SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Trimestre, freq_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
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
                                               cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  dplyr::mutate(freq_cv = freq_cv*100)


n_s_cv <- tabPSTRI_BR_4_n %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", freq_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = freq_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_BR_4_hb <- read_excel("Dados/tabPSTRI_BR_4.xlsx", 
                               sheet = "HABITUAL SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Trimestre, Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
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
                                               cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  dplyr::mutate(Qtd_horasHabituais_cv = Qtd_horasHabituais_cv*100)


hb_s_cv <- tabPSTRI_BR_4_hb %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasHabituais_cv) %>% 
  tidyr::pivot_wider(names_from  = cod_SCN_SEC,
                     values_from = Qtd_horasHabituais_cv) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção")



tabPSTRI_BR_4_ef <- read_excel("Dados/tabPSTRI_BR_4.xlsx", 
                               sheet = "EFETIVA SECUNDÁRIO") %>% 
  dplyr::filter(cod_SCN_SEC != 0) %>% 
  dplyr::select(cod_SCN_SEC, Ano, Trimestre, Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 | Trimestre == "Trimestre_1" ~ "I",
                                             Trimestre == 2 | Trimestre == "Trimestre_2" ~ "II",
                                             Trimestre == 3 | Trimestre == "Trimestre_3" ~ "III",
                                             Trimestre == 4 | Trimestre == "Trimestre_4" ~ "IV"),
                
                cod_SCN_SEC = dplyr::case_when(cod_SCN_SEC == 1~ "Agropecuária",
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
                                               cod_SCN_SEC == 12 ~ "Adm., defesa, saúde e educação públicas e seguridade social"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(cod_SCN_SEC, "PERÍODO", Qtd_horasEfetivas_cv) %>% 
  dplyr::mutate(Qtd_horasEfetivas_cv = Qtd_horasEfetivas_cv*100)


ef_s_cv <- tabPSTRI_BR_4_ef %>% 
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
               "va_sa"       = va_sa,
               "n_sa"        = n_sa,
               "hb_sa"       = hb_sa,
               "ef_sa"       = ef_sa,
               "n_p_cv"      = n_p_cv, 
               "hb_p_cv"     = hb_p_cv,
               "ef_p_cv"     = ef_p_cv,
               "n_s_cv"      = n_s_cv, 
               "hb_s_cv"     = hb_s_cv,
               "ef_s_cv"     = ef_s_cv
)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_BR_1_aux.xlsx"))











