######################################################################
# REAJUSTE DOS INDICADORES
######################################################################

library(seasonal)
library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)

######################################################################
# RIO GRANDE DO SUL
######################################################################
table_PS_RS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS.xlsx", 
                         sheet = "Indicador TRI RS") %>% 
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N,  indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas)

table_PS_RS_ind <- table_PS_RS %>% 
  tidyr::pivot_wider(names_from = atividade, values_from = c("indicadorVA_N",
                                                             "indicadorVA_qtd_HHabituais",
                                                             "indicadorVA_qtd_HEfetivas")) %>% 
  dplyr::mutate(ano =  base::paste0(Ano, ".", Trimestre)) %>% 
  dplyr::select(-c("Ano", "Trimestre")) %>% 
  dplyr::relocate(ano)


table_PS_RS_ind <- stats::ts(table_PS_RS_ind[,-1],
               start = c(2012,1),
               freq = 4)

lista<-list()

for(i in 1:ncol(table_PS_RS_ind)){
  lista[[i]]<-table_PS_RS_ind[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start = start(table_PS_RS_ind), freq = 4),
                                                         transform.function = "auto",
                                                         regression.aictest = c("td", "easter"),
                                                         pickmdl.method="best",
                                                         pickmdl.identify="all",
                                                         outlier.types="all",
                                                         x11="", 
                                                         forecast.maxlead=6,
                                                         forecast.maxback=0,
                                                         estimate.maxiter = 30000)))

names(agreg_SA) <- colnames(table_PS_RS_ind)
pib_SA <- lapply(agreg_SA, final)
pib_SA <- do.call(cbind, pib_SA)
pib_SA <- base::as.data.frame(pib_SA)



table_PS_RS_ind <- pib_SA %>%
  tidyr::pivot_longer(
    cols = starts_with(c("indicadorVA_")),
    names_to = c(".value", "atividade"),
    names_sep = "_(?=[^_]+$)"
  ) %>% 
  dplyr::mutate(indicadorVA_N = base::ifelse(is.na(indicadorVA_N), indicadorVA_N_merc_n, indicadorVA_N),
                indicadorVA_N = base::ifelse(is.na(indicadorVA_N), indicadorVA_N_merc_n_agr, indicadorVA_N),
                
                indicadorVA_qtd_HHabituais = base::ifelse(is.na(indicadorVA_qtd_HHabituais), indicadorVA_qtd_HHabituais_merc_n, indicadorVA_qtd_HHabituais),
                indicadorVA_qtd_HHabituais = base::ifelse(is.na(indicadorVA_qtd_HHabituais), indicadorVA_qtd_HHabituais_merc_n_agr, indicadorVA_qtd_HHabituais),
                
                indicadorVA_qtd_HEfetivas = base::ifelse(is.na(indicadorVA_qtd_HEfetivas), indicadorVA_qtd_HEfetivas_merc_n, indicadorVA_qtd_HEfetivas),
                indicadorVA_qtd_HEfetivas = base::ifelse(is.na(indicadorVA_qtd_HEfetivas), indicadorVA_qtd_HEfetivas_merc_n_agr, indicadorVA_qtd_HEfetivas)) %>% 
  dplyr::select(-c(indicadorVA_N_merc_n, indicadorVA_N_merc_n_agr,
                   indicadorVA_qtd_HHabituais_merc_n, indicadorVA_qtd_HHabituais_merc_n_agr,
                   indicadorVA_qtd_HEfetivas_merc_n, indicadorVA_qtd_HEfetivas_merc_n_agr)) %>% 
  dplyr::mutate(Ano = rep(2012:2025,  each = 72),
                Trimestre = rep(
                  rep(1:4, each = 18),
                  times = 14
                )) %>% 
  dplyr::relocate(atividade, Ano, Trimestre) 



######################################################################
# BRASIL
######################################################################
table_PS_BR <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS.xlsx", 
                          sheet = "Indicador TRI BR") %>%   
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N,  indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas)


table_PS_BR_ind <- table_PS_BR %>% 
  tidyr::pivot_wider(names_from = atividade, values_from = c("indicadorVA_N",
                                                             "indicadorVA_qtd_HHabituais",
                                                             "indicadorVA_qtd_HEfetivas")) %>% 
  dplyr::mutate(ano =  base::paste0(Ano, ".", Trimestre)) %>% 
  dplyr::select(-c("Ano", "Trimestre")) %>% 
  dplyr::relocate(ano)


table_PS_BR_ind <- stats::ts(table_PS_BR_ind[,-1],
                             start = c(2012,1),
                             freq = 4)

lista<-list()

for(i in 1:ncol(table_PS_BR_ind)){
  lista[[i]]<-table_PS_BR_ind[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(table_PS_BR_ind),freq=4),
                                                         transform.function = "auto",
                                                         regression.aictest = c("td", "easter"),
                                                         pickmdl.method="best",
                                                         pickmdl.identify="all",
                                                         outlier.types="all",
                                                         x11="", 
                                                         forecast.maxlead=6,
                                                         forecast.maxback=0,
                                                         estimate.maxiter = 30000)))

names(agreg_SA) <- colnames(table_PS_BR_ind)
pib_SA <- lapply(agreg_SA,final)
pib_SA <- do.call(cbind,pib_SA)
pib_SA <- base::as.data.frame(pib_SA)

table_PS_BR_ind <- pib_SA %>%
  tidyr::pivot_longer(
    cols = starts_with(c("indicadorVA_")),
    names_to = c(".value", "atividade"),
    names_sep = "_(?=[^_]+$)"
  ) %>% 
  dplyr::mutate(indicadorVA_N = base::ifelse(is.na(indicadorVA_N), indicadorVA_N_merc_n, indicadorVA_N),
                indicadorVA_N = base::ifelse(is.na(indicadorVA_N), indicadorVA_N_merc_n_agr, indicadorVA_N),
                
                indicadorVA_qtd_HHabituais = base::ifelse(is.na(indicadorVA_qtd_HHabituais), indicadorVA_qtd_HHabituais_merc_n, indicadorVA_qtd_HHabituais),
                indicadorVA_qtd_HHabituais = base::ifelse(is.na(indicadorVA_qtd_HHabituais), indicadorVA_qtd_HHabituais_merc_n_agr, indicadorVA_qtd_HHabituais),
                
                indicadorVA_qtd_HEfetivas = base::ifelse(is.na(indicadorVA_qtd_HEfetivas), indicadorVA_qtd_HEfetivas_merc_n, indicadorVA_qtd_HEfetivas),
                indicadorVA_qtd_HEfetivas = base::ifelse(is.na(indicadorVA_qtd_HEfetivas), indicadorVA_qtd_HEfetivas_merc_n_agr, indicadorVA_qtd_HEfetivas)) %>% 
  dplyr::select(-c(indicadorVA_N_merc_n, indicadorVA_N_merc_n_agr,
                   indicadorVA_qtd_HHabituais_merc_n, indicadorVA_qtd_HHabituais_merc_n_agr,
                   indicadorVA_qtd_HEfetivas_merc_n, indicadorVA_qtd_HEfetivas_merc_n_agr)) %>% 
  dplyr::mutate(Ano = rep(2012:2025,  each = 72),
                Trimestre = rep(
                  rep(1:4, each = 18),
                  times = 14
                )) %>% 
  dplyr::relocate(atividade, Ano, Trimestre) 

######################################################################
# REAJUSTE DAS VARIÁVEIS
######################################################################
######################################################################
# RIO GRANDE DO SUL
######################################################################
table_PS_RS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS.xlsx", 
                          sheet = "Indicador TRI RS") %>% 
  dplyr::mutate(atividade = base::ifelse(atividade == "total_exc", "totalExc", atividade )) %>%  
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::select(atividade, Ano, Trimestre, VA_RS, soma_N,  qtd_horasHabituais, qtd_horasEfetivas) %>% 
  dplyr::mutate(qtd_horasHabituais = qtd_horasHabituais*12.9,
                qtd_horasEfetivas  = qtd_horasEfetivas*12.9)

table_PS_RS_var <- table_PS_RS %>% 
  tidyr::pivot_wider(names_from = atividade, values_from = c("soma_N",
                                                             "qtd_horasHabituais",
                                                             "qtd_horasEfetivas",
                                                             "VA_RS")) %>% 
  dplyr::mutate(ano =  base::paste0(Ano, ".", Trimestre)) %>% 
  dplyr::select(-c("Ano", "Trimestre")) %>% 
  dplyr::relocate(ano)


table_PS_RS_var <- stats::ts(table_PS_RS_var[,-1],
                             start = c(2012,1),
                             freq = 4)

lista<-list()

for(i in 1:ncol(table_PS_RS_var)){
  lista[[i]]<-table_PS_RS_var[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(table_PS_RS_var), freq = 4),
                                                         transform.function = "auto",
                                                         regression.aictest = c("td", "easter"),
                                                         pickmdl.method="best",
                                                         pickmdl.identify="all",
                                                         outlier.types="all",
                                                         x11="", 
                                                         forecast.maxlead=6,
                                                         forecast.maxback=0,
                                                         estimate.maxiter = 30000)))

names(agreg_SA) <- colnames(table_PS_RS_var)
pib_SA <- lapply(agreg_SA,final)
pib_SA <- do.call(cbind,pib_SA)
pib_SA <- base::as.data.frame(pib_SA)

table_PS_RS_var <- pib_SA %>%
  tidyr::pivot_longer(
    cols = starts_with(c("soma_N_","qtd_horasHabituais","qtd_horasEfetivas_", "VA_")),
    names_to = c(".value", "atividade"),
    names_sep = "_(?=[^_]+$)"
  ) %>% 
  dplyr::mutate(VA_RS = base::ifelse(is.na(VA_RS), VA_RS_merc_n, VA_RS),
                VA_RS = base::ifelse(is.na(VA_RS), VA_RS_merc_n_agr, VA_RS),
                
                soma_N = base::ifelse(is.na(soma_N), soma_N_merc_n, soma_N),
                soma_N = base::ifelse(is.na(soma_N), soma_N_merc_n_agr, soma_N),
                
                qtd_horasHabituais = base::ifelse(is.na(qtd_horasHabituais), qtd_horasHabituais_merc_n, qtd_horasHabituais),
                qtd_horasHabituais = base::ifelse(is.na(qtd_horasHabituais), qtd_horasHabituais_merc_n_agr, qtd_horasHabituais),
                
                qtd_horasEfetivas = base::ifelse(is.na(qtd_horasEfetivas), qtd_horasEfetivas_merc_n, qtd_horasEfetivas),
                qtd_horasEfetivas = base::ifelse(is.na(qtd_horasEfetivas), qtd_horasEfetivas_merc_n_agr, qtd_horasEfetivas)
                
                ) %>% 
  
  dplyr::mutate(indicador_N = VA_RS/soma_N,
                indicador_qtd_horasHabituais = VA_RS/qtd_horasHabituais,
                indicador_qtd_horasEfetivas = VA_RS/qtd_horasEfetivas)  %>% 
  
  dplyr::select(-c(soma_N_merc_n, soma_N_merc_n_agr,
                   qtd_horasHabituais_merc_n, qtd_horasHabituais_merc_n_agr,
                   qtd_horasEfetivas_merc_n, qtd_horasEfetivas_merc_n_agr,
                   VA_RS_merc_n, VA_RS_merc_n_agr)) %>% 
  dplyr::mutate(Ano = rep(2012:2025,  each = 72),
                Trimestre = rep(
                  rep(1:4, each = 18),
                  times = 14
                )) %>% 
  dplyr::relocate(atividade, Ano, Trimestre) 
# %>%
#   dplyr::left_join(table_PS_RS %>% 
#                      dplyr::select(atividade, Ano, Trimestre),
#                    by = "atividade",
#                    relationship = "one-to-one") 
######################################################################
# BRASIL
######################################################################
table_PS_BR <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS.xlsx", 
                          sheet = "Indicador TRI BR") %>% 
  dplyr::mutate(atividade = base::ifelse(atividade == "total_exc", "totalExc", atividade )) %>%  
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::select(atividade, Ano, Trimestre, VA_BR, soma_N,  qtd_horasHabituais, qtd_horasEfetivas) %>% 
  dplyr::mutate(qtd_horasHabituais = qtd_horasHabituais*12.9,
                qtd_horasEfetivas  = qtd_horasEfetivas*12.9)

table_PS_BR_var <- table_PS_BR %>% 
  tidyr::pivot_wider(names_from = atividade, values_from = c("soma_N",
                                                             "qtd_horasHabituais",
                                                             "qtd_horasEfetivas",
                                                             "VA_BR")) %>% 
  dplyr::mutate(ano =  base::paste0(Ano, ".", Trimestre)) %>% 
  dplyr::select(-c("Ano", "Trimestre")) %>% 
  dplyr::relocate(ano)


table_PS_BR_var <- stats::ts(table_PS_BR_var[,-1],
                             start = c(2012,1),
                             freq = 4)

lista<-list()

for(i in 1:ncol(table_PS_BR_var)){
  lista[[i]]<-table_PS_BR_var[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(table_PS_BR_var),freq=4),
                                                         transform.function = "auto",
                                                         regression.aictest = c("td", "easter"),
                                                         pickmdl.method="best",
                                                         pickmdl.identify="all",
                                                         outlier.types="all",
                                                         x11="", 
                                                         forecast.maxlead=6,
                                                         forecast.maxback=0,
                                                         estimate.maxiter = 30000)))

names(agreg_SA) <- colnames(table_PS_BR_var)
pib_SA <- lapply(agreg_SA,final)
pib_SA <- do.call(cbind,pib_SA)
pib_SA <- base::as.data.frame(pib_SA)

table_PS_BR_var <- pib_SA %>%
  tidyr::pivot_longer(
    cols = starts_with(c("soma_N_","qtd_horasHabituais","qtd_horasEfetivas_", "VA_")),
    names_to = c(".value", "atividade"),
    names_sep = "_(?=[^_]+$)"
  ) %>% 

  dplyr::mutate(VA_BR = base::ifelse(is.na(VA_BR), VA_BR_merc_n, VA_BR),
                VA_BR = base::ifelse(is.na(VA_BR), VA_BR_merc_n_agr, VA_BR),
                
                soma_N = base::ifelse(is.na(soma_N), soma_N_merc_n, soma_N),
                soma_N = base::ifelse(is.na(soma_N), soma_N_merc_n_agr, soma_N),
                
                qtd_horasHabituais = base::ifelse(is.na(qtd_horasHabituais), qtd_horasHabituais_merc_n, qtd_horasHabituais),
                qtd_horasHabituais = base::ifelse(is.na(qtd_horasHabituais), qtd_horasHabituais_merc_n_agr, qtd_horasHabituais),
                
                qtd_horasEfetivas = base::ifelse(is.na(qtd_horasEfetivas), qtd_horasEfetivas_merc_n, qtd_horasEfetivas),
                qtd_horasEfetivas = base::ifelse(is.na(qtd_horasEfetivas), qtd_horasEfetivas_merc_n_agr, qtd_horasEfetivas)
                ) %>% 
  
  dplyr::mutate(indicador_N = VA_BR/soma_N,
                indicador_qtd_horasHabituais = VA_BR/qtd_horasHabituais,
                indicador_qtd_horasEfetivas = VA_BR/qtd_horasEfetivas) %>% 
  
  dplyr::select(-c(soma_N_merc_n, soma_N_merc_n_agr,
                   qtd_horasHabituais_merc_n, qtd_horasHabituais_merc_n_agr,
                   qtd_horasEfetivas_merc_n, qtd_horasEfetivas_merc_n_agr,
                   VA_BR_merc_n, VA_BR_merc_n_agr)) %>% 
  dplyr::mutate(Ano = rep(2012:2025,  each = 72),
                Trimestre = rep(
                  rep(1:4, each = 18),
                  times = 14
                )) %>% 
  dplyr::relocate(atividade, Ano, Trimestre) 

######################################################################
# Excel
######################################################################
sheets <- list("P&S SAZ_VAR BR" = table_PS_BR_var,
               "P&S SAZ_VAR RS" = table_PS_RS_var, 
               "P&S SAZ_IND BR" = table_PS_BR_ind,
               "P&S SAZ_IND RS" = table_PS_RS_ind
)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS_SAZ.xlsx"))
# ######################################################################
# # PLOT
# ######################################################################
# 
# options(timeout = 600) 
# options(scipen = 999)
# library(esquisse)
# #esquisse::esquisser(viewer = "browser")
# ######################################################################
# # Para a construção dos plot foi necessário agregar os dados da sazonalidade feita diretamente no indicador e
# # os dados em que fizemos essa sazonalidade por variável e, por isso, foi criado a sheet DIR_SEP
# ######################################################################
# 
# table_PS_SAZ_5_DIR_SEP_RS <- read_excel("Dados/table_PS_SAZ_5.xlsx", 
#                                              sheet = "DIR_SEP_RS")
# 
# table_PS_SAZ_5_DIR_SEP_BR <- read_excel("Dados/table_PS_SAZ_5.xlsx", 
#                                              sheet = "DIR_SEP_BR")
# 
# ############################
# # HORAS HABITUAIS
# ############################
# # RS
# ############################
# 
# ggplot(table_PS_SAZ_5_DIR_SEP_RS) +
#   aes(x = Ano) +
#   
#   geom_line(
#     aes(y = indicador_qtd_horasHabituais,
#         colour = "Separado RS"),
#     linewidth = 1
#   ) +
#   
#   geom_line(
#     aes(y = indicadorVA_qtd_HHabituais,
#         colour = "Direto RS"),
#     linewidth = 1
#   ) +
#   
#   scale_colour_manual(
#     name = "",
#     breaks = c("Separado RS", "Direto RS"),
#     values = c(
#       "Separado RS" = "#112446",
#       "Direto RS"  = "#E4003A"
#     )
#   )  +
#   
#   facet_wrap(
#     vars(atividade),
#     scales = "free"
#   ) +
#   
#   theme_minimal()
# 
# ############################
# # HORAS HABITUAIS
# ############################
# # BR
# ############################
# 
# ggplot(table_PS_SAZ_5_DIR_SEP_BR) +
#   aes(x = Ano) +
#   
#   geom_line(
#     aes(y = indicador_qtd_horasHabituais,
#         colour = "Separado BR"),
#     linewidth = 1
#   ) +
#   
#   geom_line(
#     aes(y = indicadorVA_qtd_HHabituais,
#         colour = "Direto BR"),
#     linewidth = 1
#   ) +
#   
#   scale_colour_manual(
#     name = "",
#     breaks = c("Separado BR", "Direto BR"),
#     values = c(
#       "Separado BR" = "#112446",
#       "Direto BR"  = "#E4003A"
#     )
#   )  +
#   
#   facet_wrap(
#     vars(atividade),
#     scales = "free"
#   ) +
#   
#   theme_minimal()
# ############################
# # HORAS EFETIVAS
# ############################
# # RS
# ############################
# ggplot(table_PS_SAZ_5_DIR_SEP_RS) +
#   aes(x = Ano) +
#   
#   geom_line(
#     aes(y = indicador_qtd_horasEfetivas,
#         colour = "Separado RS"),
#     linewidth = 1
#   ) +
#   
#   geom_line(
#     aes(y = indicadorVA_qtd_HEfetivas,
#         colour = "Direto RS"),
#     linewidth = 1
#   ) +
#   
#   scale_colour_manual(
#     name = "",
#     breaks = c("Separado RS", "Direto RS"),
#     values = c(
#       "Separado RS" = "#112446",
#       "Direto RS"  = "#E4003A"
#     )
#   )  +
#   
#   facet_wrap(
#     vars(atividade),
#     scales = "free"
#   ) +
#   
#   theme_minimal()
# ############################
# # HORAS EFETIVAS
# ############################
# # BR
# ############################
# ggplot(table_PS_SAZ_5_DIR_SEP_BR) +
#   aes(x = Ano) +
#   
#   geom_line(
#     aes(y = indicador_qtd_horasEfetivas,
#         colour = "Separado BR"),
#     linewidth = 1
#   ) +
#   
#   geom_line(
#     aes(y = indicadorVA_qtd_HEfetivas,
#         colour = "Direto BR"),
#     linewidth = 1
#   ) +
#   
#   scale_colour_manual(
#     name = "",
#     breaks = c("Separado BR", "Direto BR"),
#     values = c(
#       "Separado BR" = "#112446",
#       "Direto BR"  = "#E4003A"
#     )
#   )  +
#   
#   facet_wrap(
#     vars(atividade),
#     scales = "free"
#   ) +
#   
#   theme_minimal()
######################################################################
# EXEMPLO IBGE
######################################################################
# pib<-readxl::read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/PIB_Sem_Ajuste.xlsx",
#                         sheet =1)
# 
# pib<-stats::ts(pib[,-1],
#                start = c(2012,1),
#                freq = 4)
# 
# lista<-list()
# 
# for(i in 1:ncol(pib)){
#   lista[[i]]<-pib[,i]
# }
# 
# 
# agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(pib),freq=4),
#                                              transform.function = "auto",
#                                              regression.aictest = c("td", "easter"),
#                                              pickmdl.method="best",
#                                              pickmdl.identify="all",
#                                              outlier.types="all",
#                                              x11="", 
#                                              forecast.maxlead=6,
#                                              forecast.maxback=0,
#                                              estimate.maxiter = 30000)))
# 
# names(agreg_SA)<-colnames(pib)
# pib_SA<-lapply(agreg_SA, final)
# pib_SA<-do.call(cbind, pib_SA)

#write.table(pib_SA,"PIB_Com_Ajuste_Sazonal.csv",sep=";",dec=",",row.names=F)