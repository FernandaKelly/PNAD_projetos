##############################################
#          PNAD Contínua
# Primário e Secundário: Ano/Trimestre - RS/BR
##############################################

##################################
#    LEITURA DE PACOTES
##################################
library(PNADcIBGE)
library(survey)
library(foreign)
library(srvyr)
library(reactable)
library(purrr)
library(readxl)

options(timeout = 600) 
options(scipen = 999)
##################################
# Dados: PIB
##################################
PIB_RSVA <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/PIB_VA.xlsx", 
                       sheet = "VA_RS") 

total_exc <- PIB_RSVA %>%
  dplyr::filter(atividade %in% c("2","3","4","5","6","7","8","9","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "total_exc",
    VA_RS = sum(VA_RS, na.rm = TRUE),
    .groups = "drop"
  )

PIB_RSVA <- dplyr::bind_rows(PIB_RSVA, total_exc) 

PIB_RSVA_ANO <- PIB_RSVA %>%  
  
  dplyr::group_by(atividade, ano) %>% 
  dplyr::mutate(VA_RS = base::as.numeric(VA_RS),
                VA_RS_soma = base::sum(VA_RS, na.rm = TRUE)) %>% 
  dplyr::select(-c(tri, VA_RS)) %>%
  dplyr::distinct()

PIB_BRVA <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/PIB_VA.xlsx", 
                       sheet = "VA_BR")

total_exc <- PIB_BRVA %>%
  dplyr::filter(atividade %in% c("2","3","4","5","6","7","8","9","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "total_exc",
    VA_BR = sum(VA_BR, na.rm = TRUE),
    .groups = "drop"
  )

PIB_BRVA <- dplyr::bind_rows(PIB_BRVA, total_exc) 

PIB_BRVA_ANO <- PIB_BRVA %>%  
  
  dplyr::group_by(atividade, ano) %>% 
  dplyr::mutate(VA_BR = base::as.numeric(VA_BR),
                VA_BR_soma = base::sum(VA_BR, na.rm = TRUE)) %>% 
  dplyr::select(-c(tri, VA_BR)) %>%
  dplyr::distinct()


################################################
## Trimestre
### Rio Grande do Sul (RS)
################################################

################################################
# LEITURA DA BASE DE DADOS
################################################

abas <- c(
  "N PRINCIPAL",
  "HABITUAL PRINCIPAL",
  "EFETIVA PRINCIPAL",
  "N SECUNDÁRIO",
  "HABITUAL SECUNDÁRIO",
  "EFETIVA SECUNDÁRIO"
)

tabPSTRI_RS <- map_dfr(
  abas,
  ~ read_excel("Dados/tabPSTRI_RS_4.xlsx", sheet = .x),
  .id = "tipo_principal"
)


################################################
# SOMA DOS IGUAIS
################################################

# table_TRIRS_PS <- tabPSTRI_RS %>% 
#   dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ "1",
#                                              Trimestre == "Trimestre_2" ~ "2",
#                                              Trimestre == "Trimestre_3" ~ "3",
#                                              Trimestre == "Trimestre_4" ~ "4",
#                                              TRUE ~ Trimestre),
#                 Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4")),
#                 atividade = base::ifelse(is.na(cod_SCN_SEC), cod_SCN_PR, cod_SCN_SEC)) %>% 
#   #dplyr::filter(Ano == 2012 & Trimestre == "1") %>% 
#   dplyr::group_by(atividade, Ano, Trimestre) %>% 
#   dplyr::mutate(soma_N             = base::sum(freq,
#                                                na.rm = TRUE),
#                 qtd_horasHabituais = base::sum(Qtd_horasHabituais,
#                                                na.rm = TRUE),
#                 qtd_horasEfetivas  = base::sum(Qtd_horasEfetivas,
#                                                na.rm = TRUE) ) %>% 
#   dplyr::select(atividade, Ano, Trimestre, soma_N, qtd_horasHabituais, qtd_horasEfetivas) %>% 
#   dplyr::distinct()

table_atividades <- tabPSTRI_RS %>% 
  dplyr::mutate(
    Trimestre = dplyr::recode(
      Trimestre,
      "Trimestre_1" = "1",
      "Trimestre_2" = "2",
      "Trimestre_3" = "3",
      "Trimestre_4" = "4"
    ),
    Trimestre = factor(Trimestre, levels = c("1", "2", "3", "4")),
    atividade = dplyr::coalesce(cod_SCN_SEC, cod_SCN_PR)
  ) %>% 
  dplyr::group_by(atividade, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(freq, na.rm = TRUE),
    qtd_horasHabituais = sum(Qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(Qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  )

###################################################################
# GRUPOS: INDÚSTRIA E SERVIÇOS
###################################################################
grupos <- list(
  "industria" = c(2, 3, 4, 5),
  "servicos"  = c(6, 7, 8, 9, 10, 11, 12)
)

table_grupos <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$industria ~ "industria",
      atividade %in% grupos$servicos  ~ "servicos",
      #tividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: TOTAL
###################################################################
grupos <- list(
  "total" = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

table_grupos_Total <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: EXCLUÍDOS
###################################################################
grupos <- list(
  "total_exc" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_Total_exc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total_exc     ~ "total_exc",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# EMPILHAMENTO
###################################################################
table_TRIRS_PS <- dplyr::bind_rows(
  table_atividades %>%
    dplyr::mutate(
      atividade  = as.character(atividade)
    ), table_grupos, table_grupos_Total, table_grupos_Total_exc)



################################################
#### Indicador
################################################


table_TRIRS_PS_indicador <- table_TRIRS_PS %>%
  dplyr::filter(atividade != 0) %>%
  dplyr::left_join(PIB_RSVA %>% 
                     dplyr::mutate(tri = base::factor(tri, levels = c("1", "2", "3", "4"))),
                   by = c("atividade" = "atividade",
                          "Ano" = "ano",
                          "Trimestre" = "tri")) %>% 
  dplyr::group_by(atividade, Ano, Trimestre) %>% 
  dplyr::mutate(
    #VA_RS =  gsub(",", ".", VA_RS),
    VA_RS = base::as.numeric(VA_RS),
    
    indicadorVA_N = VA_RS/soma_N,
    indicadorVA_qtd_HHabituais = VA_RS/(qtd_horasHabituais*12.9),
    indicadorVA_qtd_HEfetivas = VA_RS/(qtd_horasEfetivas*12.9))

################################################
### Brasil (BR)
################################################

################################################
# LEITURA DA BASE DE DADOS
################################################

abas <- c(
  "N PRINCIPAL",
  "HABITUAL PRINCIPAL",
  "EFETIVA PRINCIPAL",
  "N SECUNDÁRIO",
  "HABITUAL SECUNDÁRIO",
  "EFETIVA SECUNDÁRIO"
)

tabPSTRI_BR <- map_dfr(
  abas,
  ~ read_excel("Dados/tabPSTRI_BR_4.xlsx", sheet = .x),
  .id = "tipo_principal"
)


################################################
# SOMA DOS IGUAIS
################################################
table_atividades <- tabPSTRI_BR %>% 
  dplyr::mutate(
    Trimestre = dplyr::recode(
      Trimestre,
      "Trimestre_1" = "1",
      "Trimestre_2" = "2",
      "Trimestre_3" = "3",
      "Trimestre_4" = "4"
    ),
    Trimestre = factor(Trimestre, levels = c("1", "2", "3", "4")),
    atividade = dplyr::coalesce(cod_SCN_SEC, cod_SCN_PR)
  ) %>% 
  dplyr::group_by(atividade, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(freq, na.rm = TRUE),
    qtd_horasHabituais = sum(Qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(Qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  )
###################################################################
# GRUPOS: INDÚSTRIA E SERVIÇOS
###################################################################
grupos <- list(
  "industria" = c(2, 3, 4, 5),
  "servicos"  = c(6, 7, 8, 9, 10, 11, 12)
)

table_grupos <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$industria ~ "industria",
      atividade %in% grupos$servicos  ~ "servicos",
      #atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: TOTAL
###################################################################
grupos <- list(
  "total" = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

table_grupos_Total <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: EXCLUÍDOS
###################################################################
grupos <- list(
  "total_exc" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_Total_exc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total_exc     ~ "total_exc",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano, Trimestre) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# EMPILHAMENTO
###################################################################
table_TRIBR_PS <- dplyr::bind_rows(
  table_atividades %>%
    dplyr::mutate(
      atividade  = as.character(atividade)
    ), table_grupos, table_grupos_Total, table_grupos_Total_exc)

################################################
#### Indicador
################################################


table_TRIBR_PS_indicador <- table_TRIBR_PS %>%
  dplyr::filter(atividade != 0) %>%
  dplyr::left_join(PIB_BRVA %>% 
                     dplyr::mutate(tri = base::factor(tri, levels = c("1", "2", "3", "4")),
                                   ano = as.double(ano),
                                   atividade = base::as.character(atividade)),
                   by = c("atividade" = "atividade",
                          "Ano" = "ano",
                          "Trimestre" = "tri")) %>% 
  dplyr::group_by(atividade, Ano, Trimestre) %>% 
  dplyr::mutate(
    #VA_BR =  gsub(",", ".", VA_BR),
    VA_BR = base::as.numeric(VA_BR),
    indicadorVA_N = VA_BR/soma_N,
    indicadorVA_qtd_HHabituais = VA_BR/(qtd_horasHabituais*12.9),
    indicadorVA_qtd_HEfetivas = VA_BR/(qtd_horasEfetivas*12.9))



################################################
## Ano
### Rio Grande do Sul (RS)
################################################

################################################
# LEITURA DA BASE DE DADOS
################################################

abas <- c(
  "N PRINCIPAL",
  "HABITUAL PRINCIPAL",
  "EFETIVA PRINCIPAL",
  "N SECUNDÁRIO",
  "HABITUAL SECUNDÁRIO",
  "EFETIVA SECUNDÁRIO"
)

tabPSANO_RS <- map_dfr(
  abas,
  ~ read_excel("Dados/tabPSANOS_RS_10.xlsx", sheet = .x),
  .id = "tipo_principal"
)


################################################
# SOMA DOS IGUAIS
################################################
table_atividades <- tabPSANO_RS %>% 
  dplyr::mutate(atividade = dplyr::coalesce(cod_SCN_SEC, cod_SCN_PR)
  ) %>% 
  dplyr::group_by(atividade, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(freq, na.rm = TRUE),
    qtd_horasHabituais = sum(Qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(Qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  )
###################################################################
# GRUPOS: INDÚSTRIA E SERVIÇOS
###################################################################
grupos <- list(
  "industria" = c(2, 3, 4, 5),
  "servicos"  = c(6, 7, 8, 9, 10, 11, 12)
)

table_grupos <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$industria ~ "industria",
      atividade %in% grupos$servicos  ~ "servicos",
      #atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: TOTAL
###################################################################
grupos <- list(
  "total" = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

table_grupos_Total <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: EXCLUÍDOS
###################################################################
grupos <- list(
  "total_exc" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_Total_exc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total_exc     ~ "total_exc",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# EMPILHAMENTO
###################################################################
table_ANORS_PS <- dplyr::bind_rows(
  table_atividades %>%
    dplyr::mutate(
      atividade  = as.character(atividade)
    ), table_grupos, table_grupos_Total, table_grupos_Total_exc)

################################################
#### Indicador
################################################


table_ANORS_PS_indicador <- table_ANORS_PS %>%
  dplyr::filter(atividade != 0) %>%
  dplyr::filter(Ano != 2025) %>% 
  dplyr::left_join(PIB_RSVA_ANO,
                   by = c("atividade" = "atividade",
                          "Ano" = "ano")) %>% 
  
  dplyr::group_by(atividade, Ano) %>% 
  dplyr::mutate(indicadorVA_N = VA_RS_soma/soma_N,
                indicadorVA_qtd_HHabituais = VA_RS_soma/(qtd_horasHabituais*51.6),
                indicadorVA_qtd_HEfetivas = VA_RS_soma/(qtd_horasEfetivas*51.6))

################################################
### Brasil (BR)
################################################

################################################
# LEITURA DA BASE DE DADOS
################################################

abas <- c(
  "N PRINCIPAL",
  "HABITUAL PRINCIPAL",
  "EFETIVA PRINCIPAL",
  "N SECUNDÁRIO",
  "HABITUAL SECUNDÁRIO",
  "EFETIVA SECUNDÁRIO"
)

tabPSANO_BR <- map_dfr(
  abas,
  ~ read_excel("Dados/tabPSANOS_BR_5.xlsx", sheet = .x),
  .id = "tipo_principal"
)


################################################
# SOMA DOS IGUAIS
################################################
table_atividades <- tabPSANO_BR %>% 
  dplyr::mutate(atividade = dplyr::coalesce(cod_SCN_SEC, cod_SCN_PR)
  ) %>% 
  dplyr::group_by(atividade, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(freq, na.rm = TRUE),
    qtd_horasHabituais = sum(Qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(Qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  )
###################################################################
# GRUPOS: INDÚSTRIA E SERVIÇOS
###################################################################
grupos <- list(
  "industria" = c(2, 3, 4, 5),
  "servicos"  = c(6, 7, 8, 9, 10, 11, 12)
)

table_grupos <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$industria ~ "industria",
      atividade %in% grupos$servicos  ~ "servicos",
      #atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: TOTAL
###################################################################
grupos <- list(
  "total" = c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

table_grupos_Total <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total     ~ "total",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# GRUPOS: EXCLUÍDOS
###################################################################
grupos <- list(
  "total_exc" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_Total_exc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$total_exc     ~ "total_exc",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(categoria)) %>% 
  dplyr::group_by(categoria, Ano) %>% 
  dplyr::summarise(
    soma_N             = sum(soma_N, na.rm = TRUE),
    qtd_horasHabituais = sum(qtd_horasHabituais, na.rm = TRUE),
    qtd_horasEfetivas  = sum(qtd_horasEfetivas, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  dplyr::rename("atividade" = categoria)
###################################################################
# EMPILHAMENTO
###################################################################
table_ANOBR_PS <- dplyr::bind_rows(
  table_atividades %>%
    dplyr::mutate(
      atividade  = as.character(atividade)
    ), table_grupos, table_grupos_Total, table_grupos_Total_exc)


################################################
#### Indicador
################################################

table_ANOBR_PS_indicador <- table_ANOBR_PS %>%
  dplyr::mutate(Ano = base::as.double(Ano)) %>% 
  dplyr::filter(atividade != 0) %>%
  dplyr::filter(Ano != 2025) %>%
  dplyr::left_join(PIB_BRVA_ANO, 
                   by = c("atividade" = "atividade",
                          "Ano" = "ano")) %>% 
  dplyr::group_by(atividade, Ano) %>% 
  dplyr::mutate(indicadorVA_N = VA_BR_soma/soma_N,
                indicadorVA_qtd_HHabituais = VA_BR_soma/(qtd_horasHabituais*51.6),
                indicadorVA_qtd_HEfetivas = VA_BR_soma/(qtd_horasEfetivas*51.6)) 

################################################
# Excel
################################################
sheets <- list("P&S TRI RS" = table_TRIRS_PS,
               "P&S TRI BR" = table_TRIBR_PS, 
               "P&S ANO RS" = table_ANORS_PS,
               "P&S ANO BR" = table_ANOBR_PS,
               
               "Indicador TRI RS" = table_TRIRS_PS_indicador,
               "Indicador TRI BR" = table_TRIBR_PS_indicador, 
               "Indicador ANO RS" = table_ANORS_PS_indicador,
               "Indicador ANO BR" = table_ANOBR_PS_indicador
)

writexl::write_xlsx(sheets, 
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_9.xlsx"))


