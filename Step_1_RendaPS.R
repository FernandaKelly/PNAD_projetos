#####################################################
# ENTREGA: Principal, Secundário & Renda (2019-2025)
# AGRUPADO: RENDA
# ANOS: 2012 - 2025
# DADOS: RIO GRANDE DO SUL
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
#####################################################
#        CONFIGURANDO DIRETÓRIO DE DADOS
#####################################################
library(here)
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos")

#####################################################



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





dadosPNADc1VISITA_2012  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2012)
dadosPNADc1VISITA_2012 <- srvyr::as_survey(dadosPNADc1VISITA_2012)

dadosPNADc1VISITA_2012 <- dadosPNADc1VISITA_2012 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2012 <- dadosPNADc1VISITA_2012 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2012 <- dadosPNADc1VISITA_2012 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2012 <- dadosPNADc1VISITA_2012 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))





dadosPNADc1VISITA_2013  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2013)
dadosPNADc1VISITA_2013 <- srvyr::as_survey(dadosPNADc1VISITA_2013)

dadosPNADc1VISITA_2013 <- dadosPNADc1VISITA_2013 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2013 <- dadosPNADc1VISITA_2013 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2013 <- dadosPNADc1VISITA_2013 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2013 <- dadosPNADc1VISITA_2013 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))






dadosPNADc1VISITA_2014  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2014)
dadosPNADc1VISITA_2014 <- srvyr::as_survey(dadosPNADc1VISITA_2014)

dadosPNADc1VISITA_2014 <- dadosPNADc1VISITA_2014 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2014 <- dadosPNADc1VISITA_2014 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2014 <- dadosPNADc1VISITA_2014 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2014 <- dadosPNADc1VISITA_2014 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))






dadosPNADc1VISITA_2015  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2015)
dadosPNADc1VISITA_2015 <- srvyr::as_survey(dadosPNADc1VISITA_2015)

dadosPNADc1VISITA_2015 <- dadosPNADc1VISITA_2015 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2015 <- dadosPNADc1VISITA_2015 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2015 <- dadosPNADc1VISITA_2015 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2015 <- dadosPNADc1VISITA_2015 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))





dadosPNADc1VISITA_2016  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2016)
dadosPNADc1VISITA_2016 <- srvyr::as_survey(dadosPNADc1VISITA_2016)

dadosPNADc1VISITA_2016 <- dadosPNADc1VISITA_2016 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2016 <- dadosPNADc1VISITA_2016 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2016 <- dadosPNADc1VISITA_2016 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2016 <- dadosPNADc1VISITA_2016 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))




dadosPNADc1VISITA_2017  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2017)
dadosPNADc1VISITA_2017 <- srvyr::as_survey(dadosPNADc1VISITA_2017)

dadosPNADc1VISITA_2017 <- dadosPNADc1VISITA_2017 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2017 <- dadosPNADc1VISITA_2017 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2017 <- dadosPNADc1VISITA_2017 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2017 <- dadosPNADc1VISITA_2017 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))




dadosPNADc1VISITA_2018  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2018)
dadosPNADc1VISITA_2018 <- srvyr::as_survey(dadosPNADc1VISITA_2018)

dadosPNADc1VISITA_2018 <- dadosPNADc1VISITA_2018 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2018 <- dadosPNADc1VISITA_2018 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2018 <- dadosPNADc1VISITA_2018 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2018 <- dadosPNADc1VISITA_2018 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))





dadosPNADc1VISITA_2019  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2019)
dadosPNADc1VISITA_2019 <- srvyr::as_survey(dadosPNADc1VISITA_2019)

dadosPNADc1VISITA_2019 <- dadosPNADc1VISITA_2019 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2019 <- dadosPNADc1VISITA_2019 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2019 <- dadosPNADc1VISITA_2019 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2019 <- dadosPNADc1VISITA_2019 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))





dadosPNADc5VISITA_2020  <- PNADcIBGE::pnadc_design(dadosPNADc5VISITA_2020)
dadosPNADc5VISITA_2020 <- srvyr::as_survey(dadosPNADc5VISITA_2020)

dadosPNADc5VISITA_2020 <- dadosPNADc5VISITA_2020 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2020 <- dadosPNADc5VISITA_2020 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc5VISITA_2020 <- dadosPNADc5VISITA_2020 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2020 <- dadosPNADc5VISITA_2020 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))



dadosPNADc5VISITA_2021  <- PNADcIBGE::pnadc_design(dadosPNADc5VISITA_2021)
dadosPNADc5VISITA_2021 <- srvyr::as_survey(dadosPNADc5VISITA_2021)

dadosPNADc5VISITA_2021 <- dadosPNADc5VISITA_2021 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2021 <- dadosPNADc5VISITA_2021 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc5VISITA_2021 <- dadosPNADc5VISITA_2021 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2021 <- dadosPNADc5VISITA_2021 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))





dadosPNADc5VISITA_2022  <- PNADcIBGE::pnadc_design(dadosPNADc5VISITA_2022)
dadosPNADc5VISITA_2022 <- srvyr::as_survey(dadosPNADc5VISITA_2022)

dadosPNADc5VISITA_2022 <- dadosPNADc5VISITA_2022 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2022 <- dadosPNADc5VISITA_2022 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc5VISITA_2022 <- dadosPNADc5VISITA_2022 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2022 <- dadosPNADc5VISITA_2022 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))



dadosPNADc1VISITA_2023  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2023)
dadosPNADc1VISITA_2023 <- srvyr::as_survey(dadosPNADc1VISITA_2023)

dadosPNADc1VISITA_2023 <- dadosPNADc1VISITA_2023 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2023 <- dadosPNADc1VISITA_2023 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2023 <- dadosPNADc1VISITA_2023 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2023 <- dadosPNADc1VISITA_2023 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))




dadosPNADc1VISITA_2024  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2024)
dadosPNADc1VISITA_2024 <- srvyr::as_survey(dadosPNADc1VISITA_2024)

dadosPNADc1VISITA_2024 <- dadosPNADc1VISITA_2024 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2024 <- dadosPNADc1VISITA_2024 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2024 <- dadosPNADc1VISITA_2024 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2024 <- dadosPNADc1VISITA_2024 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))



dadosPNADc1VISITA_2025  <- PNADcIBGE::pnadc_design(dadosPNADc1VISITA_2025)
dadosPNADc1VISITA_2025 <- srvyr::as_survey(dadosPNADc1VISITA_2025)

dadosPNADc1VISITA_2025 <- dadosPNADc1VISITA_2025 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(VD4017 >= 0)

table_1RP_2025 <- dadosPNADc1VISITA_2025 %>%  
  dplyr::group_by(V4013, V4012, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(VD4017,
                                                   na.rm = TRUE))

dadosPNADc1VISITA_2025 <- dadosPNADc1VISITA_2025 %>% 
  dplyr::filter(UF == "Rio Grande do Sul" & VD4002 == "Pessoas ocupadas") %>% 
  dplyr:::filter(V405112 >= 0)

table_1RS_2025 <- dadosPNADc1VISITA_2025 %>%  
  dplyr::mutate(Qtd_renda = base::sum(VD4017, V405112, V405122, na.rm = TRUE)) %>% 
  dplyr::group_by(V4044, V4043, Ano) %>% 
  dplyr::summarise(freq = srvyr::survey_total(),
                   Qtd_renda = srvyr::survey_total(Qtd_renda,
                                                   na.rm = TRUE))





table_1RP <- table_1RP_2012 %>% 
  dplyr::bind_rows(table_1RP_2013,
                   table_1RP_2014,
                   table_1RP_2015,
                   table_1RP_2016,
                   table_1RP_2017,
                   table_1RP_2018,
                   table_1RP_2019,
                   table_1RP_2020,
                   table_1RP_2021,
                   table_1RP_2022,
                   table_1RP_2023,
                   table_1RP_2024,
                   table_1RP_2025) 

table_1RS <- table_1RS_2012 %>% 
  dplyr::bind_rows(table_1RS_2013,
                   table_1RS_2014,
                   table_1RS_2015,
                   table_1RS_2016,
                   table_1RS_2017,
                   table_1RS_2018,
                   table_1RS_2019,
                   table_1RS_2020,
                   table_1RS_2021,
                   table_1RS_2022,
                   table_1RS_2023,
                   table_1RS_2024,
                   table_1RS_2025) %>% 
  dplyr::rename("V4013" = V4044,
                "V4012" = V4043)


table_RENDA_PS <- table_1RP %>% 
  dplyr::bind_rows(table_1RS) %>% 
  dplyr::rename("CNAE" = V4013,
                "OCUPACAO" = V4012) %>% 
  dplyr::group_by(CNAE, OCUPACAO, Ano) %>% 
  dplyr::summarise(freq_N = base::sum(freq),
                   Qtd_Renda = base::sum(Qtd_renda))



writexl::write_xlsx(table_RENDA_PS, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_Renda_12_25_1.xlsx"))




