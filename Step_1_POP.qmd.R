options(timeout = 600) 
options(scipen = 999)

library(PNADcIBGE)
library(survey)
library(foreign)
library(srvyr)
library(reactable)
library(purrr)
library(janitor)
library(gtsummary)


load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2012.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2013.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2014.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2015.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2016.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2017.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2018.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2019.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc5VISITA_2020.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc5VISITA_2021.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc5VISITA_2022.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2023.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2024.RData")

load("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Visitas/dadosPNADc1VISITA_2025.RData")



dadosPNADc1VISITA_2012  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2012) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2012)

dadosPNADc1VISITA_2013  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2013) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2013)

dadosPNADc1VISITA_2014  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2014) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2014)

dadosPNADc1VISITA_2015  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2015) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2015)

dadosPNADc1VISITA_2016  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2016) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2016)

dadosPNADc1VISITA_2017  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2017) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2017)

dadosPNADc1VISITA_2018  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2018) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2018)

dadosPNADc1VISITA_2019  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2019) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2019)

dadosPNADc1VISITA_2015  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2015) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2015)

dadosPNADc5VISITA_2020  <- PNADcIBGE::pnadc_design(dadosPNADc5VISITA_2020) %>% 
  srvyr::as_survey(dadosPNADc5VISITA_2020)

dadosPNADc5VISITA_2021  <- PNADcIBGE::pnadc_design(dadosPNADc5VISITA_2021) %>% 
  srvyr::as_survey(dadosPNADc5VISITA_2021)

dadosPNADc5VISITA_2022  <- PNADcIBGE::pnadc_design(dadosPNADc5VISITA_2022) %>% 
  srvyr::as_survey(dadosPNADc5VISITA_2022)

dadosPNADc1VISITA_2023  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2023) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2023)

dadosPNADc1VISITA_2024  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2024) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2024)

dadosPNADc1VISITA_2025  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2025) %>% 
  srvyr::as_survey(dadosPNADc1VISITA_2025)



###############################################

table_12_V1030 <- dadosPNADc1VISITA_2012 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2012)

table_12_V1034 <- dadosPNADc1VISITA_2012 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2012)

table_12_pop_idade <- dadosPNADc1VISITA_2012 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2012)

table_12_fdt <- dadosPNADc1VISITA_2012 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2012)

table_12_cond_ocup <- dadosPNADc1VISITA_2012 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2012)

table_12_potencial <- dadosPNADc1VISITA_2012 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2012)

table_12_ppa <- dadosPNADc1VISITA_2012 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2012)

###############################################



###############################################

table_13_V1030 <- dadosPNADc1VISITA_2013 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2013)

table_13_V1034 <- dadosPNADc1VISITA_2013 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2013)

table_13_pop_idade <- dadosPNADc1VISITA_2013 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2013)

table_13_fdt <- dadosPNADc1VISITA_2013 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2013)

table_13_cond_ocup <- dadosPNADc1VISITA_2013 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2013)

table_13_potencial <- dadosPNADc1VISITA_2013 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2013)

table_13_ppa <- dadosPNADc1VISITA_2013 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2013)

###############################################



###############################################

table_14_V1030 <- dadosPNADc1VISITA_2014 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2014)

table_14_V1034 <- dadosPNADc1VISITA_2014 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2014)

table_14_pop_idade <- dadosPNADc1VISITA_2014 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2014)

table_14_fdt <- dadosPNADc1VISITA_2014 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2014)

table_14_cond_ocup <- dadosPNADc1VISITA_2014 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2014)

table_14_potencial <- dadosPNADc1VISITA_2014 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2014)

table_14_ppa <- dadosPNADc1VISITA_2014 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2014)

###############################################



###############################################

table_15_V1030 <- dadosPNADc1VISITA_2015 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2015)

table_15_V1034 <- dadosPNADc1VISITA_2015 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2015)

table_15_pop_idade <- dadosPNADc1VISITA_2015 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2015)

table_15_fdt <- dadosPNADc1VISITA_2015 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2015)

table_15_cond_ocup <- dadosPNADc1VISITA_2015 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2015)

table_15_potencial <- dadosPNADc1VISITA_2015 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2015)

table_15_ppa <- dadosPNADc1VISITA_2015 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2015)

###############################################



###############################################

table_16_V1030 <- dadosPNADc1VISITA_2016 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2016)

table_16_V1034 <- dadosPNADc1VISITA_2016 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2016)

table_16_pop_idade <- dadosPNADc1VISITA_2016 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2016)

table_16_fdt <- dadosPNADc1VISITA_2016 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2016)

table_16_cond_ocup <- dadosPNADc1VISITA_2016 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2016)

table_16_potencial <- dadosPNADc1VISITA_2016 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2016)

table_16_ppa <- dadosPNADc1VISITA_2016 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2016)

###############################################



###############################################

table_17_V1030 <- dadosPNADc1VISITA_2017 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2017)

table_17_V1034 <- dadosPNADc1VISITA_2017 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2017)

table_17_pop_idade <- dadosPNADc1VISITA_2017 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2017)

table_17_fdt <- dadosPNADc1VISITA_2017 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2017)

table_17_cond_ocup <- dadosPNADc1VISITA_2017 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2017)

table_17_potencial <- dadosPNADc1VISITA_2017 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2017)

table_17_ppa <- dadosPNADc1VISITA_2017 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2017)

###############################################



###############################################

table_18_V1030 <- dadosPNADc1VISITA_2018 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2018)

table_18_V1034 <- dadosPNADc1VISITA_2018 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2018)

table_18_pop_idade <- dadosPNADc1VISITA_2018 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2018)

table_18_fdt <- dadosPNADc1VISITA_2018 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2018)

table_18_cond_ocup <- dadosPNADc1VISITA_2018 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2018)

table_18_potencial <- dadosPNADc1VISITA_2018 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2018)

table_18_ppa <- dadosPNADc1VISITA_2018 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2018)

###############################################



###############################################

table_19_V1030 <- dadosPNADc1VISITA_2019 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2019)

table_19_V1034 <- dadosPNADc1VISITA_2019 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2019)

table_19_pop_idade <- dadosPNADc1VISITA_2019 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2019)

table_19_fdt <- dadosPNADc1VISITA_2019 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2019)

table_19_cond_ocup <- dadosPNADc1VISITA_2019 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2019)

table_19_potencial <- dadosPNADc1VISITA_2019 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2019)

table_19_ppa <- dadosPNADc1VISITA_2019 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2019)

###############################################



###############################################

table_20_V1030 <- dadosPNADc5VISITA_2020 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2020)

table_20_V1034 <- dadosPNADc5VISITA_2020 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2020)

table_20_pop_idade <- dadosPNADc5VISITA_2020 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2020)

table_20_fdt <- dadosPNADc5VISITA_2020 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2020)

table_20_cond_ocup <- dadosPNADc5VISITA_2020 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2020)

table_20_potencial <- dadosPNADc5VISITA_2020 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2020)

table_20_ppa <- dadosPNADc5VISITA_2020 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2020)

###############################################



###############################################

table_21_V1030 <- dadosPNADc5VISITA_2021 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2021)

table_21_V1034 <- dadosPNADc5VISITA_2021 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2021)

table_21_pop_idade <- dadosPNADc5VISITA_2021 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2021)

table_21_fdt <- dadosPNADc5VISITA_2021 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2021)

table_21_cond_ocup <- dadosPNADc5VISITA_2021 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2021)

table_21_potencial <- dadosPNADc5VISITA_2021 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2021)

table_21_ppa <- dadosPNADc5VISITA_2021 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2021)

###############################################



###############################################

table_22_V1030 <- dadosPNADc5VISITA_2022 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2022)

table_22_V1034 <- dadosPNADc5VISITA_2022 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2022)

table_22_pop_idade <- dadosPNADc5VISITA_2022 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2022)

table_22_fdt <- dadosPNADc5VISITA_2022 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2022)

table_22_cond_ocup <- dadosPNADc5VISITA_2022 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2022)

table_22_potencial <- dadosPNADc5VISITA_2022 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2022)

table_22_ppa <- dadosPNADc5VISITA_2022 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2022)

###############################################



###############################################

table_23_V1030 <- dadosPNADc1VISITA_2023 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2023)

table_23_V1034 <- dadosPNADc1VISITA_2023 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2023)

table_23_pop_idade <- dadosPNADc1VISITA_2023 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2023)

table_23_fdt <- dadosPNADc1VISITA_2023 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2023)

table_23_cond_ocup <- dadosPNADc1VISITA_2023 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2023)

table_23_potencial <- dadosPNADc1VISITA_2023 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2023)

table_23_ppa <- dadosPNADc1VISITA_2023 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2023)

###############################################



###############################################

table_24_V1030 <- dadosPNADc1VISITA_2024 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2024)

table_24_V1034 <- dadosPNADc1VISITA_2024 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2024)

table_24_pop_idade <- dadosPNADc1VISITA_2024 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2024)

table_24_fdt <- dadosPNADc1VISITA_2024 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2024)

table_24_cond_ocup <- dadosPNADc1VISITA_2024 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2024)

table_24_potencial <- dadosPNADc1VISITA_2024 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2024)

table_24_ppa <- dadosPNADc1VISITA_2024 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2024)

###############################################




###############################################

table_25_V1030 <- dadosPNADc1VISITA_2025 %>% 
  dplyr::group_by(UF, V1030) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2025)

table_25_V1034 <- dadosPNADc1VISITA_2025 %>% 
  dplyr::group_by(UF, V1034) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype = "cv")) %>% 
  dplyr::mutate(ano = 2025)

table_25_pop_idade <- dadosPNADc1VISITA_2025 %>% 
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2025)

table_25_fdt <- dadosPNADc1VISITA_2025 %>% 
  dplyr::group_by(UF, VD4001) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4001))) %>% 
  tidyr::pivot_wider(names_from = VD4001,
                     values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2025)

table_25_cond_ocup <- dadosPNADc1VISITA_2025 %>% 
  dplyr::group_by(UF, VD4002) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::filter(!(is.na(VD4002))) %>%
  tidyr::pivot_wider(names_from = VD4002, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2025)

table_25_potencial <- dadosPNADc1VISITA_2025 %>% 
  dplyr::group_by(UF, VD4003) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv"))  %>% 
  dplyr::filter(!(is.na(VD4003))) %>%
  tidyr::pivot_wider(names_from = VD4003, values_from = c(freq, freq_cv)) %>% 
  dplyr::mutate(ano = 2025)

table_25_ppa <- dadosPNADc1VISITA_2025 %>% 
  dplyr::filter(V2009 >= 14 & V2009 <= 65) %>%   
  dplyr::group_by(UF, V2009) %>%
  dplyr::summarise(
    freq = srvyr::survey_total(vartype  = "cv")) %>% 
  dplyr::group_by(UF) %>% 
  dplyr::summarise(soma_idade = sum(freq)) %>% 
  dplyr::mutate(ano = 2025)

###############################################



#################################################
#                 EMPILHAMENTO
#################################################


table_V1030 <- table_12_V1030 %>% 
  dplyr::bind_rows(table_13_V1030 , 
                   table_14_V1030 , 
                   table_15_V1030 , 
                   table_16_V1030 , 
                   table_17_V1030 , 
                   table_18_V1030 , 
                   table_19_V1030 , 
                   table_20_V1030 , 
                   table_21_V1030 , 
                   table_22_V1030 , 
                   table_23_V1030 , 
                   table_24_V1030,
                   table_25_V1030)



table_V1034 <- table_12_V1034 %>% 
  dplyr::bind_rows(table_13_V1034 , 
                   table_14_V1034 , 
                   table_15_V1034 , 
                   table_16_V1034 , 
                   table_17_V1034 , 
                   table_18_V1034 , 
                   table_19_V1034 , 
                   table_20_V1034 , 
                   table_21_V1034 , 
                   table_22_V1034 , 
                   table_23_V1034 , 
                   table_24_V1034,
                   table_25_V1034)

table_pop_idade <- table_12_pop_idade %>% 
  dplyr::bind_rows(table_13_pop_idade , 
                   table_14_pop_idade , 
                   table_15_pop_idade , 
                   table_16_pop_idade , 
                   table_17_pop_idade , 
                   table_18_pop_idade , 
                   table_19_pop_idade , 
                   table_20_pop_idade , 
                   table_21_pop_idade , 
                   table_22_pop_idade , 
                   table_23_pop_idade , 
                   table_24_pop_idade,
                   table_25_pop_idade) %>% 
  dplyr::rename("freq" = soma_idade)

table_fdt <- table_12_fdt %>% 
  dplyr::bind_rows(table_13_fdt , 
                   table_14_fdt , 
                   table_15_fdt , 
                   table_16_fdt , 
                   table_17_fdt , 
                   table_18_fdt , 
                   table_19_fdt , 
                   table_20_fdt , 
                   table_21_fdt , 
                   table_22_fdt , 
                   table_23_fdt , 
                   table_24_fdt,
                   table_25_fdt)

table_cond_ocup <- table_12_cond_ocup %>% 
  dplyr::bind_rows(table_13_cond_ocup , 
                   table_14_cond_ocup , 
                   table_15_cond_ocup , 
                   table_16_cond_ocup , 
                   table_17_cond_ocup , 
                   table_18_cond_ocup , 
                   table_19_cond_ocup , 
                   table_20_cond_ocup , 
                   table_21_cond_ocup , 
                   table_22_cond_ocup , 
                   table_23_cond_ocup , 
                   table_24_cond_ocup,
                   table_25_cond_ocup)

table_potencial <- table_12_potencial %>% 
  dplyr::bind_rows(table_13_potencial , 
                   table_14_potencial , 
                   table_15_potencial , 
                   table_16_potencial , 
                   table_17_potencial , 
                   table_18_potencial , 
                   table_19_potencial , 
                   table_20_potencial , 
                   table_21_potencial , 
                   table_22_potencial , 
                   table_23_potencial , 
                   table_24_potencial,
                   table_25_potencial)

table_ppa <- table_12_ppa %>% 
  dplyr::bind_rows(table_13_ppa , 
                   table_14_ppa , 
                   table_15_ppa , 
                   table_16_ppa , 
                   table_17_ppa , 
                   table_18_ppa , 
                   table_19_ppa , 
                   table_20_ppa , 
                   table_21_ppa , 
                   table_22_ppa , 
                   table_23_ppa , 
                   table_24_ppa,
                   table_25_ppa) %>% 
  dplyr::rename("pop_14_65" = soma_idade)






sheets <- list("V1030"     = table_V1030,
               #"V1034"     = table_V1034,
               "pop" = table_pop_idade,
               "fdt"       = table_fdt,
               "cond_ocup" = table_cond_ocup,
               "potencial" = table_potencial,
               "pop_14_65" = table_ppa)



writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/tablePOP_2.xlsx"))

