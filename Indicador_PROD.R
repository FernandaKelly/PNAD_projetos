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
library(readxl)
#####################################################
#        CONFIGURANDO DIRETÓRIO DE DADOS
#####################################################

library(here)
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos")

#####################################################
#        DADOS: PRINCIPAL, SECUNDÁRIO
#####################################################


#####################################################
#        DADOS: PRINCIPAL & SECUNDÁRIO (PS)
#####################################################


#####################################################
#        DADOS: SAZONALIDADE (SAZONAL)
#####################################################


#####################################################
#        DADOS: MÉDIA MÓVEL (MM)
#####################################################


#####################################################
#        DADOS: TAXA (TAXA)
#####################################################


#####################################################
#        DADOS: INDICADOR DE PRODUTIVIDADE
#####################################################


#####################################################
#        DADOS: AUXILIARES
#####################################################


source("PNAD_projetos/Step_2_PS.R")
source("scripts/limpeza.R")
source("scripts/processamento.R")