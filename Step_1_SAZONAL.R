library(seasonal)
library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)

######################################################################
# EXEMPLO IBGE
######################################################################
pib<-readxl::read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/PIB_Sem_Ajuste.xlsx",
                        sheet =1)

pib<-stats::ts(pib[,-1],
               start = c(2012,1),
               freq = 4)

lista<-list()

for(i in 1:ncol(pib)){
  lista[[i]]<-pib[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(pib),freq=4),
                                             transform.function = "auto",
                                             regression.aictest = c("td", "easter"),
                                             pickmdl.method="best",
                                             pickmdl.identify="all",
                                             outlier.types="all",
                                             x11="", 
                                             forecast.maxlead=6,
                                             forecast.maxback=0,
                                             estimate.maxiter = 30000)))

names(agreg_SA)<-colnames(pib)

pib_SA<-lapply(agreg_SA, final)

pib_SA<-do.call(cbind, pib_SA)

write.table(pib_SA,"PIB_Com_Ajuste_Sazonal.csv",sep=";",dec=",",row.names=F)
######################################################################
# REAJUSTE DOS INDICADORES
######################################################################
######################################################################
# RIO GRANDE DO SUL
######################################################################
table_PS_RS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_9.xlsx", 
                         sheet = "Indicador TRI RS") %>% 
  dplyr::mutate(atividade = base::ifelse(atividade == "total_exc", "totalExc", atividade )) %>% 
  #dplyr::filter(Ano != 2025) %>% 
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
  ) 


#utils::write.table(table_PS_RS_ind,"PIB_Com_Ajuste_Sazonal.csv",sep=";",dec=",",row.names=F)
# %>%
#   dplyr::left_join(table_PS_RS %>% 
#                      dplyr::select(atividade, Ano, Trimestre),
#                    by = "atividade",
#                    relationship = "one-to-one") 
######################################################################
# BRASIL
######################################################################
table_PS_BR <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_9.xlsx", 
                          sheet = "Indicador TRI BR") %>% 
  dplyr::mutate(atividade = base::ifelse(atividade == "total_exc", "totalExc", atividade )) %>%  
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
  )
######################################################################
# REAJUSTE DAS VARIÁVEIS
######################################################################
######################################################################
# RIO GRANDE DO SUL
######################################################################
table_PS_RS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_9.xlsx", 
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
  dplyr::mutate(indicador_N = VA_RS/soma_N,
                indicador_qtd_horasHabituais = VA_RS/qtd_horasHabituais,
                indicador_qtd_horasEfetivas = VA_RS/qtd_horasEfetivas)
# %>%
#   dplyr::left_join(table_PS_RS %>% 
#                      dplyr::select(atividade, Ano, Trimestre),
#                    by = "atividade",
#                    relationship = "one-to-one") 
######################################################################
# BRASIL
######################################################################
table_PS_BR <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_9.xlsx", 
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
  dplyr::mutate(indicador_N = VA_BR/soma_N,
                indicador_qtd_horasHabituais = VA_BR/qtd_horasHabituais,
                indicador_qtd_horasEfetivas = VA_BR/qtd_horasEfetivas)

######################################################################
# Excel
######################################################################
sheets <- list("P&S SAZ_VAR BR" = table_PS_BR_var,
               "P&S SAZ_VAR RS" = table_PS_RS_var, 
               "P&S SAZ_IND BR" = table_PS_BR_ind,
               "P&S SAZ_IND RS" = table_PS_RS_ind
)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_SAZ_6.xlsx"))
######################################################################
# PLOT
######################################################################

options(timeout = 600) 
options(scipen = 999)
library(esquisse)
#esquisse::esquisser(viewer = "browser")

table_PS_SAZ_5_DIR_SEP_RS <- read_excel("Dados/table_PS_SAZ_5.xlsx", 
                                             sheet = "DIR_SEP_RS")

table_PS_SAZ_5_DIR_SEP_BR <- read_excel("Dados/table_PS_SAZ_5.xlsx", 
                                             sheet = "DIR_SEP_BR")

############################
# HORAS HABITUAIS
############################
# RS
############################

ggplot(table_PS_SAZ_5_DIR_SEP_RS) +
  aes(x = Ano) +
  
  geom_line(
    aes(y = indicador_qtd_horasHabituais,
        colour = "Separado RS"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = indicadorVA_qtd_HHabituais,
        colour = "Direto RS"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    breaks = c("Separado RS", "Direto RS"),
    values = c(
      "Separado RS" = "#112446",
      "Direto RS"  = "#E4003A"
    )
  )  +
  
  facet_wrap(
    vars(atividade),
    scales = "free"
  ) +
  
  theme_minimal()

############################
# HORAS HABITUAIS
############################
# BR
############################

ggplot(table_PS_SAZ_5_DIR_SEP_BR) +
  aes(x = Ano) +
  
  geom_line(
    aes(y = indicador_qtd_horasHabituais,
        colour = "Separado BR"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = indicadorVA_qtd_HHabituais,
        colour = "Direto BR"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    breaks = c("Separado BR", "Direto BR"),
    values = c(
      "Separado BR" = "#112446",
      "Direto BR"  = "#E4003A"
    )
  )  +
  
  facet_wrap(
    vars(atividade),
    scales = "free"
  ) +
  
  theme_minimal()
############################
# HORAS EFETIVAS
############################
# RS
############################
ggplot(table_PS_SAZ_5_DIR_SEP_RS) +
  aes(x = Ano) +
  
  geom_line(
    aes(y = indicador_qtd_horasEfetivas,
        colour = "Separado RS"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = indicadorVA_qtd_HEfetivas,
        colour = "Direto RS"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    breaks = c("Separado RS", "Direto RS"),
    values = c(
      "Separado RS" = "#112446",
      "Direto RS"  = "#E4003A"
    )
  )  +
  
  facet_wrap(
    vars(atividade),
    scales = "free"
  ) +
  
  theme_minimal()
############################
# HORAS EFETIVAS
############################
# BR
############################
ggplot(table_PS_SAZ_5_DIR_SEP_BR) +
  aes(x = Ano) +
  
  geom_line(
    aes(y = indicador_qtd_horasEfetivas,
        colour = "Separado BR"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = indicadorVA_qtd_HEfetivas,
        colour = "Direto BR"),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = "",
    breaks = c("Separado BR", "Direto BR"),
    values = c(
      "Separado BR" = "#112446",
      "Direto BR"  = "#E4003A"
    )
  )  +
  
  facet_wrap(
    vars(atividade),
    scales = "free"
  ) +
  
  theme_minimal()
######################################################################
# TABELA AUXILIAR COM AJUSTE E SEM AJUSTE
######################################################################
#                         RIO GRANDE DO SUL
######################################################################
table_PS_9_RS <- read_excel("Dados/table_PS_9.xlsx", 
                              sheet = "Indicador TRI RS") %>% 
  dplyr::select(atividade:VA_RS) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas)


va <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

n <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef <- table_PS_9_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")



table_PS_SAZ_6_RS <- read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                sheet = "P&S SAZ_VAR RS") %>% 
  dplyr::select(atividade:VA_RS) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas)


va_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", VA_RS) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


n_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef_sa <- table_PS_SAZ_6_RS %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


#####################################################
#                        EXCEL
#####################################################

sheets <- list("va"          = va,
               "n"           = n,
               "hb"          = hb,
               "ef"          = ef,
               "va_sa"       = va_sa,
               "n_sa"        = n_sa,
               "hb_sa"       = hb_sa,
               "ef_sa"       = ef_sa
                                )


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_RS_1_aux.xlsx"))


######################################################################
#                             BRASIL
######################################################################

table_PS_9_BR <- read_excel("Dados/table_PS_9.xlsx", 
                            sheet = "Indicador TRI BR") %>% 
  dplyr::select(atividade:VA_BR) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "total_exc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas)


va <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_RS) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

n <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef <- table_PS_9_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")



table_PS_SAZ_6_BR <- read_excel("Dados/table_PS_SAZ_6.xlsx", 
                                sheet = "P&S SAZ_VAR BR") %>% 
  dplyr::select(atividade:VA_BR) %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == 1 ~ "I",
                                             Trimestre == 2 ~ "II",
                                             Trimestre == 3 ~ "III",
                                             Trimestre == 4 ~ "IV"),
                
                atividade = dplyr::case_when(atividade == "1"~ "Agropecuária",
                                             atividade == "2"~ "Indústria extrativa",
                                             atividade == "3"~ "Indústria de transformação",
                                             atividade == "4"~ "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação",
                                             atividade == "5"~ "Construção",
                                             atividade == "industria" ~ "INDÚSTRIA",
                                             atividade == "6"~ "Comércio",
                                             atividade == "7"~ "Transporte, armazenagem e correio",
                                             atividade == "8"~ "Informação e comunicação",
                                             atividade == "9"~ "Atividades financeiras, de seguros e serviços relacionados",
                                             atividade == "10" ~ "Atividades imobiliárias",
                                             atividade == "11" ~ "Outras atividades de serviços",
                                             atividade == "12" ~ "Adm., defesa, saúde e educação públicas e seguridade social",
                                             atividade == "servicos" ~ "SERVIÇOS",
                                             atividade == "total" ~ "TOTAL",
                                             atividade == "totalExc" ~ "SETOR EMPRESARIAL NÃO-AGRÍCOLA"),
                "PERÍODO" = paste(Ano, Trimestre, sep = ".")
                
  ) %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR, soma_N, qtd_horasHabituais,
                qtd_horasEfetivas)


va_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", VA_BR) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = VA_BR) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


n_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", soma_N) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = soma_N) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

hb_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasHabituais) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasHabituais) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")

ef_sa <- table_PS_SAZ_6_BR %>% 
  dplyr::select(atividade, "PERÍODO", qtd_horasEfetivas) %>% 
  tidyr::pivot_wider(names_from  = atividade,
                     values_from = qtd_horasEfetivas) %>% 
  dplyr::relocate("PERÍODO", "Agropecuária", "Indústria extrativa", "Indústria de transformação",
                  "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação", "Construção", "INDÚSTRIA")


#####################################################
#                        EXCEL
#####################################################

sheets <- list("va"          = va,
               "n"           = n,
               "hb"          = hb,
               "ef"          = ef,
               "va_sa"       = va_sa,
               "n_sa"        = n_sa,
               "hb_sa"       = hb_sa,
               "ef_sa"       = ef_sa
)


writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PROD_BR_1_aux.xlsx"))











