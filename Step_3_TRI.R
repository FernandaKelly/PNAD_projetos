#####################################################
# ENTREGA: MÉDIA MÓVEL
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
# RIO GRANDE DO SUL
#####################################################

table_PS_8 <- readxl::read_excel("table_PS_8.xlsx", sheet = "Indicador TRI RS")

dadosRS_MM <- table_PS_8 %>% 
  dplyr::group_by(atividade) %>% 
  dplyr::mutate(
                soma_N_MM4 = slider::slide_sum(soma_N, before = 3, complete = TRUE),
                qtd_horasHabituais_MM4 = slider::slide_sum(qtd_horasHabituais, before = 3, complete = TRUE),
                qtd_horasEfetivas_MM4 = slider::slide_sum(qtd_horasEfetivas, before = 3, complete = TRUE),
                VA_RS_MM4 = slider::slide_sum(VA_RS, before = 3, complete = TRUE),
                
                indicadorVA_N_MM4 = VA_RS_MM4/soma_N_MM4,
                indicadorVA_qtd_HHabituais_MM4 = VA_RS_MM4/(qtd_horasHabituais_MM4*12.9),
                indicadorVA_qtd_HEfetivas_MM4 = VA_RS_MM4/(qtd_horasEfetivas_MM4*12.9))

#####################################################
# BRASIL
#####################################################
  
table_PS_8 <- readxl::read_excel("table_PS_8.xlsx", sheet = "Indicador TRI BR")

dadosBR_MM <- table_PS_8 %>% 
  dplyr::group_by(atividade) %>% 
  dplyr::mutate(
    soma_N_MM4 = slider::slide_sum(soma_N, before = 3, complete = TRUE),
    qtd_horasHabituais_MM4 = slider::slide_sum(qtd_horasHabituais, before = 3, complete = TRUE),
    qtd_horasEfetivas_MM4 = slider::slide_sum(qtd_horasEfetivas, before = 3, complete = TRUE),
    VA_BR_MM4 = slider::slide_sum(VA_BR, before = 3, complete = TRUE),
    
    indicadorVA_N_MM4 = VA_BR_MM4/soma_N_MM4,
    indicadorVA_qtd_HHabituais_MM4 = VA_BR_MM4/(qtd_horasHabituais_MM4*12.9),
    indicadorVA_qtd_HEfetivas_MM4 = VA_BR_MM4/(qtd_horasEfetivas_MM4*12.9))

#####################################################

sheets <- list("MM4 RS TRI"   = dadosRS_MM,
               "MM4 BR TRI"   = dadosBR_MM)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_MM_1.xlsx"))
