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
#    DADOS: PRINCIPAL, SECUNDÁRIO TRIMESTRAL RS
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_TRI_RS.R")

rm(list = ls())
#####################################################
#   DADOS: PRINCIPAL, SECUNDÁRIO TRIMESTRAL BR
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_2_TRI_BR.R")

rm(list = ls())
#####################################################
#        DADOS: PRINCIPAL, SECUNDÁRIO ANUAL RS
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_ANO_RS.R")

rm(list = ls())
#####################################################
#        DADOS: PRINCIPAL, SECUNDÁRIO ANUAL BR
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_2_ANO_BR.R")

rm(list = ls())
#####################################################
#        DADOS: PRINCIPAL & SECUNDÁRIO (PS)
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_PS.R")

rm(list = ls())
#####################################################
#        DADOS: SAZONALIDADE (SAZONAL)
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_SAZONAL.R")

rm(list = ls())
#####################################################
#        DADOS: MÉDIA MÓVEL (MM)
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_MM.R")

rm(list = ls())
#####################################################
#        DADOS: TAXA (TAXA)
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_TAXA.R")

rm(list = ls())
#####################################################
#   DADOS: INDICADOR DE PRODUTIVIDADE TRIMESTRAL
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_PROD.R")

rm(list = ls())
#####################################################
#   DADOS: INDICADOR DE PRODUTIVIDADE TRIMESTRAL AUX
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_1_AUX.R")

rm(list = ls())
#####################################################
#   DADOS: INDICADOR DE PRODUTIVIDADE ANUAL
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_2_PROD.R")

rm(list = ls())
#####################################################
#   DADOS: INDICADOR DE PRODUTIVIDADE ANUAL AUX
#####################################################
library(here)
setwd("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

source("PNAD_projetos/Step_2_AUX.R")

rm(list = ls())