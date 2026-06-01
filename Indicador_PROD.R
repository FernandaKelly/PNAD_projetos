#####################################################
# ENTREGA: INDICADOR DE PRODUTIVIDADE
# AGRUPADO: --
# ANOS: 2012 - 2025
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
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos")

#####################################################
#        DADOS: PRINCIPAL, SECUNDÁRIO
#####################################################

source("PNAD_projetos/Step_1_TRI_RS.R")
source("PNAD_projetos/Step_2_TRI_BR.R")
source("PNAD_projetos/Step_1_ANO_RS.R")
source("PNAD_projetos/Step_2_ANO_BR.R")

#####################################################
#        DADOS: PRINCIPAL & SECUNDÁRIO (PS)
#####################################################

source("PNAD_projetos/Step_1_PS.R")

#####################################################
#        DADOS: SAZONALIDADE (SAZONAL)
#####################################################

source("PNAD_projetos/Step_1_SAZONAL.R")

#####################################################
#        DADOS: MÉDIA MÓVEL (MM)
#####################################################

source("PNAD_projetos/Step_1_MM.R")

#####################################################
#        DADOS: TAXA (TAXA)
#####################################################

source("PNAD_projetos/Step_1_TAXA.R")

#####################################################
#   DADOS: INDICADOR DE PRODUTIVIDADE TRIMESTRAL
#####################################################

source("PNAD_projetos/Step_1_PROD.R")
source("PNAD_projetos/Step_1_AUX.R")

#####################################################
#   DADOS: INDICADOR DE PRODUTIVIDADE ANUAL
#####################################################

source("PNAD_projetos/Step_2_PROD.R")
source("PNAD_projetos/Step_2_AUX.R")
