#####################################################
# ENTREGA: INDICADOR DE PRODUTIVIDADE
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
#                LEITURA DAS BASES
#####################################################

#####################################################
#              RIO GRANDE DO SUL
#####################################################

table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                         sheet = "Indicador TRI RS") %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
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

table_PS_SAZ_6_RS <- read_excel("Dados/table_PS_SAZ_6.xlsx", 
                             sheet = "P&S SAZ_VAR RS") %>% 
  dplyr::select(atividade, Ano, Trimestre, indicador_N, indicador_qtd_horasHabituais, indicador_qtd_horasEfetivas) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", indicador_N, indicador_qtd_horasHabituais,
                indicador_qtd_horasEfetivas)


prod_n_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicador_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicador_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicador_qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicador_qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicador_qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicador_qtd_horasEfetivas) %>% 
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

table_PS_MM_2_RS <- read_excel("Dados/table_PS_MM_3.xlsx", 
                            sheet = "MM4 RS TRI") %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N_MM4, indicadorVA_qtd_HHabituais_MM4, indicadorVA_qtd_HEfetivas_MM4) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N_MM4, indicadorVA_qtd_HHabituais_MM4,
                indicadorVA_qtd_HEfetivas_MM4)

prod_n_4 <- table_PS_MM_2_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb_4 <- table_PS_MM_2_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef_4 <- table_PS_MM_2_RS %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas_MM4) %>% 
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

table_TAXA_1_INTERANUAL <- read_excel("Dados/table_TAXA_2.xlsx", 
                           sheet = "dadosRS_TAXA_INTERANUAL_RS") %>% 
  dplyr::select(atividade, Ano, Trimestre, taxa_interanual_N, taxa_interanual_qtd_HHabituais, taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::arrange(Ano) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N, taxa_interanual_qtd_HHabituais,
                taxa_interanual_qtd_HEfetivas)

tx_prod_n <- table_TAXA_1_INTERANUAL %>% 
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

tx_prod_hb <- table_TAXA_1_INTERANUAL %>% 
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

tx_prod_ef <- table_TAXA_1_INTERANUAL %>% 
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

table_TAXA_1_IMED_ANTERIOR <- read_excel("Dados/table_TAXA_2.xlsx", 
                           sheet = "dadosRS_IMED_ANTERIOR_RS") %>% 
  dplyr::select(atividade, Ano, Trimestre, taxa_IMED_ANTERIOR_N, taxa_IMED_ANTERIOR_qtd_HHabituais, taxa_IMED_ANTERIOR_qtd_HEfetivas) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_N, taxa_IMED_ANTERIOR_qtd_HHabituais,
                taxa_IMED_ANTERIOR_qtd_HEfetivas)

tx_prod_n_sa <- table_TAXA_1_IMED_ANTERIOR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_IMED_ANTERIOR_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb_sa <- table_TAXA_1_IMED_ANTERIOR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_IMED_ANTERIOR_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef_sa <- table_TAXA_1_IMED_ANTERIOR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_IMED_ANTERIOR_qtd_HEfetivas) %>% 
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

table_TAXA_1_ACUMULADA <- read_excel("Dados/table_TAXA_2.xlsx", 
                           sheet = "dadosRS_ACUMULADA_RS") %>% 
  dplyr::select(atividade, Ano, Trimestre, taxa_ACUMULADA_N, taxa_ACUMULADA_qtd_HHabituais, taxa_ACUMULADA_qtd_HEfetivas) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_N, taxa_ACUMULADA_qtd_HHabituais,
                taxa_ACUMULADA_qtd_HEfetivas)

tx_prod_n_4 <- table_TAXA_1_ACUMULADA %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_ACUMULADA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb_4 <- table_TAXA_1_ACUMULADA %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_ACUMULADA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef_4 <- table_TAXA_1_ACUMULADA %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_ACUMULADA_qtd_HEfetivas) %>% 
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
               "prod_n_sa"       = prod_n_sa,
               "prod_hb_sa"      = prod_hb_sa,
               "prod_ef_sa"      = prod_ef_sa,
               "prod_n_4"        = prod_n_4,
               "prod_hb_4"       = prod_hb_4,
               "prod_ef_4"       = prod_ef_4,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef,
               "tx_prod_n_sa"    = tx_prod_n_sa,
               "tx_prod_hb_sa"   = tx_prod_hb_sa,
               "tx_prod_ef_sa"   = tx_prod_ef_sa,
               "tx_prod_n_4"     = tx_prod_n_4,
               "tx_prod_hb_4"    = tx_prod_hb_4,
               "tx_prod_ef_4"    = tx_prod_ef_4                  )
               

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_RS_2.xlsx"))

#####################################################
rm(list = ls())
#####################################################


#####################################################
#                        BRASIL
#####################################################


table_PS_9_BR <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador TRI BR") %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N, indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas) %>% 
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
  dplyr::select(atividade, "PERÍODO", indicadorVA_N, indicadorVA_qtd_HHabituais,
                indicadorVA_qtd_HEfetivas)

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

table_PS_SAZ_6_BR <- read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                sheet = "P&S SAZ_VAR BR") %>% 
  dplyr::select(atividade, Ano, Trimestre, indicador_N, indicador_qtd_horasHabituais, indicador_qtd_horasEfetivas) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", indicador_N, indicador_qtd_horasHabituais,
                indicador_qtd_horasEfetivas)


prod_n_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicador_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicador_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicador_qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicador_qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicador_qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicador_qtd_horasEfetivas) %>% 
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

table_PS_MM_2_BR <- read_excel("Dados/table_PS_MM_3.xlsx", 
                               sheet = "MM4 BR TRI") %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N_MM4, indicadorVA_qtd_HHabituais_MM4, indicadorVA_qtd_HEfetivas_MM4) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N_MM4, indicadorVA_qtd_HHabituais_MM4,
                indicadorVA_qtd_HEfetivas_MM4)

prod_n_4 <- table_PS_MM_2_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_N_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_N_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_hb_4 <- table_PS_MM_2_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HHabituais_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HHabituais_MM4) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

prod_ef_4 <- table_PS_MM_2_BR %>% 
  dplyr::select(atividade, "PERÍODO", indicadorVA_qtd_HEfetivas_MM4) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = indicadorVA_qtd_HEfetivas_MM4) %>% 
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

table_TAXA_1_INTERANUAL <- read_excel("Dados/table_TAXA_2.xlsx", 
                                      sheet = "dadosRS_TAXA_INTERANUAL_BR") %>% 
  dplyr::select(atividade, Ano, Trimestre, taxa_interanual_N, taxa_interanual_qtd_HHabituais, taxa_interanual_qtd_HEfetivas) %>% 
  dplyr::arrange(Ano) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", taxa_interanual_N, taxa_interanual_qtd_HHabituais,
                taxa_interanual_qtd_HEfetivas)

tx_prod_n <- table_TAXA_1_INTERANUAL %>% 
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

tx_prod_hb <- table_TAXA_1_INTERANUAL %>% 
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

tx_prod_ef <- table_TAXA_1_INTERANUAL %>% 
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

table_TAXA_1_IMED_ANTERIOR <- read_excel("Dados/table_TAXA_2.xlsx", 
                                         sheet = "dadosRS_IMED_ANTERIOR_BR") %>% 
  dplyr::select(atividade, Ano, Trimestre, taxa_IMED_ANTERIOR_N, taxa_IMED_ANTERIOR_qtd_HHabituais, taxa_IMED_ANTERIOR_qtd_HEfetivas) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_N, taxa_IMED_ANTERIOR_qtd_HHabituais,
                taxa_IMED_ANTERIOR_qtd_HEfetivas)

tx_prod_n_sa <- table_TAXA_1_IMED_ANTERIOR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_IMED_ANTERIOR_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb_sa <- table_TAXA_1_IMED_ANTERIOR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_IMED_ANTERIOR_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef_sa <- table_TAXA_1_IMED_ANTERIOR %>% 
  dplyr::select(atividade, "PERÍODO", taxa_IMED_ANTERIOR_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_IMED_ANTERIOR_qtd_HEfetivas) %>% 
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

table_TAXA_1_ACUMULADA <- read_excel("Dados/table_TAXA_2.xlsx", 
                                     sheet = "dadosRS_ACUMULADA_BR") %>% 
  dplyr::select(atividade, Ano, Trimestre, taxa_ACUMULADA_N, taxa_ACUMULADA_qtd_HHabituais, taxa_ACUMULADA_qtd_HEfetivas) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = "."))  %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_N, taxa_ACUMULADA_qtd_HHabituais,
                taxa_ACUMULADA_qtd_HEfetivas)

tx_prod_n_4 <- table_TAXA_1_ACUMULADA %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_ACUMULADA_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_hb_4 <- table_TAXA_1_ACUMULADA %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_qtd_HHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_ACUMULADA_qtd_HHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                  "Construção", "INDÚSTRIA", "Comércio", "Transporte, armazenagem e correio",
                  "Informação e comunicação",
                  "Atividades financeiras, de seguros e serviços relacionados",
                  "Atividades imobiliárias",
                  "Outras atividades de serviços",
                  "Adm., defesa, saúde e educação públicas e seguridade social",
                  "SERVIÇOS","TOTAL","SETOR EMPRESARIAL NÃO-AGRÍCOLA")

tx_prod_ef_4 <- table_TAXA_1_ACUMULADA %>% 
  dplyr::select(atividade, "PERÍODO", taxa_ACUMULADA_qtd_HEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = taxa_ACUMULADA_qtd_HEfetivas) %>% 
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
               "prod_n_sa"       = prod_n_sa,
               "prod_hb_sa"      = prod_hb_sa,
               "prod_ef_sa"      = prod_ef_sa,
               "prod_n_4"        = prod_n_4,
               "prod_hb_4"       = prod_hb_4,
               "prod_ef_4"       = prod_ef_4,
               "tx_prod_n"       = tx_prod_n,
               "tx_prod_hb"      = tx_prod_hb,
               "tx_prod_ef"      = tx_prod_ef,
               "tx_prod_n_sa"    = tx_prod_n_sa,
               "tx_prod_hb_sa"   = tx_prod_hb_sa,
               "tx_prod_ef_sa"   = tx_prod_ef_sa,
               "tx_prod_n_4"     = tx_prod_n_4,
               "tx_prod_hb_4"    = tx_prod_hb_4,
               "tx_prod_ef_4"    = tx_prod_ef_4                  )


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_BR_2.xlsx"))
