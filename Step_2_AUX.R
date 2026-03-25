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