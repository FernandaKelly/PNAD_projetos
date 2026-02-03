#####################################################
# ENTREGA: MÉDIA MÓVEL
# AGRUPADO: --
# ANOS: 2012 - 2024
# DADOS: RIO GRANDE DO SUL
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

table_PS_8 <- readxl::read_excel("table_PS_8.xlsx", sheet = "Indicador TRI RS")



teste1 <- table_PS_8 %>% 
  dplyr::select(atividade, Ano, Trimestre, soma_N, qtd_horasHabituais, qtd_horasEfetivas, VA_RS) 

teste1 %>% 
  #dplyr::group_by(atividade) %>% 
  dplyr::mutate(#teste_var = zoo::rollsum(soma_N, k = 3),
                soma_N_MM = slider::slide_sum(soma_N, before = 3, complete = TRUE),
                qtd_horasHabituais_MM = slider::slide_sum(qtd_horasHabituais, before = 3, complete = TRUE),
                qtd_horasEfetivas_MM = slider::slide_sum(qtd_horasEfetivas, before = 3, complete = TRUE),
                VA_RS_MM = slider::slide_sum(VA_RS, before = 3, complete = TRUE))
  
