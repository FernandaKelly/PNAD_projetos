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
                soma_N_MM = slider::slide_sum(soma_N, before = 3, complete = TRUE),
                qtd_horasHabituais_MM = slider::slide_sum(qtd_horasHabituais, before = 3, complete = TRUE),
                qtd_horasEfetivas_MM = slider::slide_sum(qtd_horasEfetivas, before = 3, complete = TRUE),
                VA_RS_MM = slider::slide_sum(VA_RS, before = 3, complete = TRUE),
                
                indicadorVA_N_MM4 = VA_RS_MM/soma_N_MM,
                indicadorVA_qtd_HHabituais_MM4 = VA_RS_MM/(qtd_horasHabituais_MM*12.9),
                indicadorVA_qtd_HEfetivas_MM4 = VA_RS_MM/(qtd_horasEfetivas_MM*12.9))

#####################################################
# BRASIL
#####################################################
  
table_PS_8 <- readxl::read_excel("table_PS_8.xlsx", sheet = "Indicador TRI BR")

dadosBR_MM <- table_PS_8 %>% 
  dplyr::group_by(atividade) %>% 
  dplyr::mutate(
    soma_N_MM = slider::slide_sum(soma_N, before = 3, complete = TRUE),
    qtd_horasHabituais_MM = slider::slide_sum(qtd_horasHabituais, before = 3, complete = TRUE),
    qtd_horasEfetivas_MM = slider::slide_sum(qtd_horasEfetivas, before = 3, complete = TRUE),
    VA_BR_MM = slider::slide_sum(VA_BR, before = 3, complete = TRUE),
    
    indicadorVA_N_MM4 = VA_BR_MM/soma_N_MM,
    indicadorVA_qtd_HHabituais_MM4 = VA_BR_MM/(qtd_horasHabituais_MM*12.9),
    indicadorVA_qtd_HEfetivas_MM4 = VA_BR_MM/(qtd_horasEfetivas_MM*12.9))
