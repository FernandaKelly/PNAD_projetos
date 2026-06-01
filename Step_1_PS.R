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
PIB_VA_RS_trimestral <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/va_trimestral_final.xlsx",
                                   col_names = FALSE,
                                   sheet = "aba_final_RS") %>% 
  dplyr::slice(3:n()) %>% 
  dplyr::rename("ano_tri"         = "...1",
                "1"               = "...2",
                "2"               = "...3",
                "3"               = "...4",
                "4"               = "...5",
                "5"               = "...6",
                "industria"       = "...7",
                "6"               = "...8",
                "7"               = "...9",
                "8"               = "...10",
                "9"               = "...11",
                "10"              = "...12",
                "11"              = "...13",
                "12"              = "...14",
                "servicos"        = "...15",
                "total"           = "...16") %>% 
  tidyr::pivot_longer(cols = c("1", "2", "3",
                               "4", "5", "industria",
                               "6", "7", "8", "9",
                               "10", "11", "12",
                               "servicos", "total"),
                      names_to = "atividade",
                      values_to = "VA_RS") %>% 
  tidyr::separate(ano_tri, into = c("ano", "tri"), sep = "\\.") %>% 
  dplyr::mutate(VA_RS = base::as.numeric(VA_RS),
                VA_RS = VA_RS*1000000,
                tri = dplyr::recode(tri,
                                    `I`   = "1",
                                    `II`  = "2",
                                    `III` = "3",
                                    `IV`  = "4")) 


merc <- PIB_VA_RS_trimestral %>%
  dplyr::filter(atividade %in% c("1", "2","3","4","5","6","7","8","9","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "merc",
    VA_RS = sum(VA_RS, na.rm = TRUE),
    .groups = "drop"
  )

merc_n_agr <- PIB_VA_RS_trimestral %>%
  dplyr::filter(atividade %in% c("2","3","4","5","6","7","8","9","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "merc_n_agr",
    VA_RS = sum(VA_RS, na.rm = TRUE),
    .groups = "drop"
  )

merc_n_agr_fin <- PIB_VA_RS_trimestral %>%
  dplyr::filter(atividade %in% c("2","3","4","5","6","7","8","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "merc_n_agr_fin",
    VA_RS = sum(VA_RS, na.rm = TRUE),
    .groups = "drop"
  )


PIB_RSVA <- dplyr::bind_rows(PIB_VA_RS_trimestral, merc, merc_n_agr, merc_n_agr_fin) 


PIB_RSVA_ANO <- PIB_RSVA %>%  
  
  dplyr::group_by(atividade, ano) %>% 
  dplyr::mutate(VA_RS = base::as.numeric(VA_RS),
                VA_RS_soma = base::sum(VA_RS, na.rm = TRUE)) %>% 
  dplyr::select(-c(tri, VA_RS)) %>%
  dplyr::distinct()
##################################
PIB_VA_BR_trimestral <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/va_trimestral_final.xlsx",
                                   col_names = FALSE,
                                   sheet = "aba_final_BR") %>% 
  dplyr::slice(3:n()) %>% 
  dplyr::rename("ano_tri"         = "...1",
                "1"               = "...2",
                "2"               = "...3",
                "3"               = "...4",
                "4"               = "...5",
                "5"               = "...6",
                "industria"       = "...7",
                "6"               = "...8",
                "7"               = "...9",
                "8"               = "...10",
                "9"               = "...11",
                "10"              = "...12",
                "11"              = "...13",
                "12"              = "...14",
                "servicos"        = "...15",
                "total"           = "...16") %>% 
  tidyr::pivot_longer(cols = c("1", "2", "3",
                               "4", "5", "industria",
                               "6", "7", "8", "9",
                               "10", "11", "12",
                                "servicos", "total"),
                      names_to = "atividade",
                      values_to = "VA_BR") %>% 
  tidyr::separate(ano_tri, into = c("ano", "tri"), sep = "\\.") %>% 
  dplyr::mutate(VA_BR = base::as.numeric(VA_BR),
                VA_BR = VA_BR*1000000,
                tri = dplyr::recode(tri,
                                    `I`   = "1",
                                    `II`  = "2",
                                    `III` = "3",
                                    `IV`  = "4")) 



merc <- PIB_VA_BR_trimestral %>%
  dplyr::filter(atividade %in% c("1", "2","3","4","5","6","7","8","9","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "merc",
    VA_BR = sum(VA_BR, na.rm = TRUE),
    .groups = "drop"
  )

merc_n_agr <- PIB_VA_BR_trimestral %>%
  dplyr::filter(atividade %in% c("2","3","4","5","6","7","8","9","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "merc_n_agr",
    VA_BR = sum(VA_BR, na.rm = TRUE),
    .groups = "drop"
  )

merc_n_agr_fin <- PIB_VA_BR_trimestral %>%
  dplyr::filter(atividade %in% c("2","3","4","5","6","7","8","11")) %>%
  dplyr::group_by(ano, tri) %>%
  dplyr::summarise(
    atividade = "merc_n_agr_fin",
    VA_BR = sum(VA_BR, na.rm = TRUE),
    .groups = "drop"
  )


PIB_BRVA <- dplyr::bind_rows(PIB_VA_BR_trimestral, merc, merc_n_agr, merc_n_agr_fin) 


PIB_BRVA_ANO <- PIB_BRVA %>%  
  
  dplyr::group_by(atividade, ano) %>% 
  dplyr::mutate(VA_BR = base::as.numeric(VA_BR),
                VA_BR_soma = base::sum(VA_BR, na.rm = TRUE)) %>% 
  dplyr::select(-c(tri, VA_BR)) %>%
  dplyr::distinct()

################################################
#############    TRIMESTRAL   ##################
###         Rio Grande do Sul (RS)           ###
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
  ~ read_excel("Dados/PRODUTIVIDADE/tabPSTRI_RS.xlsx", sheet = .x),
  .id = "tipo_principal"
)


################################################
# SOMA DOS IGUAIS
################################################

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
# GRUPOS: merc, merc_n_agr, merc_n_agr_fin
###################################################################

########
# merc
########
grupos <- list(
  "merc" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc     ~ "merc",
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
########
# merc_n_agr
########
grupos <- list(
  "merc_n_agr" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc_n_agr <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr     ~ "merc_n_agr",
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
########
# merc_n_agr_fin
########
grupos <- list(
  "merc_n_agr_fin" = c(2, 3, 4, 5, 6, 7, 8, 11)
)

table_grupos_merc_n_agr_fin <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr_fin     ~ "merc_n_agr_fin",
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
    ), table_grupos, table_grupos_Total, table_grupos_merc, table_grupos_merc_n_agr, table_grupos_merc_n_agr_fin)



################################################
#### Indicador
################################################

table_TRIRS_PS_indicador <- table_TRIRS_PS %>%
  dplyr::filter(atividade != 0) %>%
  dplyr::left_join(PIB_RSVA %>% 
                     dplyr::mutate(tri = base::factor(tri, levels = c("1", "2", "3", "4"))),
                   by = c("Ano" = "ano",
                          "Trimestre" = "tri",
                          "atividade" = "atividade")) %>% 
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
  ~ read_excel("Dados/PRODUTIVIDADE/tabPSTRI_BR.xlsx", sheet = .x),
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
# GRUPOS: merc, merc_n_agr, merc_n_agr_fin
###################################################################

#####
# merc
#####

grupos <- list(
  "merc" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc     ~ "merc",
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

############
# merc_n_agr
###########

grupos <- list(
  "merc_n_agr" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc_n_agr <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr     ~ "merc_n_agr",
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

############
# merc_n_agr_fin
###########

grupos <- list(
  "merc_n_agr_fin" = c(2, 3, 4, 5, 6, 7, 8, 11)
)

table_grupos_merc_n_agr_fin <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr_fin     ~ "merc_n_agr_fin",
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
    ), table_grupos, table_grupos_Total, table_grupos_merc, table_grupos_merc_n_agr, table_grupos_merc_n_agr_fin)

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
#################### ANUAL #####################
###           Rio Grande do Sul (RS)       #####
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
  ~ read_excel("Dados/PRODUTIVIDADE/tabPSANOS_RS.xlsx", sheet = .x),
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
# GRUPOS: merc, merc_n_agr, merc_n_agr_fin
###################################################################

#####
# merc
#####

grupos <- list(
  "merc" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc     ~ "merc",
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

############
# merc_n_agr
###########

grupos <- list(
  "merc_n_agr" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc_n_agr <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr     ~ "merc_n_agr",
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

############
# merc_n_agr_fin
###########

grupos <- list(
  "merc_n_agr_fin" = c(2, 3, 4, 5, 6, 7, 8, 11)
)

table_grupos_merc_n_agr_fin <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr_fin     ~ "merc_n_agr_fin",
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
    ), table_grupos, table_grupos_Total, table_grupos_merc, table_grupos_merc_n_agr, table_grupos_merc_n_agr_fin)

################################################
#### Indicador
################################################


table_ANORS_PS_indicador <- table_ANORS_PS %>%
  dplyr::filter(atividade != 0) %>%
  #dplyr::filter(Ano != 2025) %>% 
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
  ~ read_excel("Dados/PRODUTIVIDADE/tabPSANOS_BR.xlsx", sheet = .x),
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
# GRUPOS: merc, merc_n_agr, merc_n_agr_fin
###################################################################

#####
# merc
#####

grupos <- list(
  "merc" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc     ~ "merc",
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

############
# merc_n_agr
###########

grupos <- list(
  "merc_n_agr" = c(2, 3, 4, 5, 6, 7, 8, 9, 11)
)

table_grupos_merc_n_agr <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr     ~ "merc_n_agr",
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

############
# merc_n_agr_fin
###########

grupos <- list(
  "merc_n_agr_fin" = c(2, 3, 4, 5, 6, 7, 8, 11)
)

table_grupos_merc_n_agr_fin <- table_atividades %>% 
  dplyr::mutate(
    categoria = dplyr::case_when(
      atividade %in% grupos$merc_n_agr_fin     ~ "merc_n_agr_fin",
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
    ), table_grupos, table_grupos_Total, table_grupos_merc, table_grupos_merc_n_agr, table_grupos_merc_n_agr_fin)


################################################
#### Indicador
################################################

table_ANOBR_PS_indicador <- table_ANOBR_PS %>%
  dplyr::mutate(Ano = base::as.character(Ano)) %>% 
  dplyr::filter(atividade != 0) %>%
  #dplyr::filter(Ano != 2025) %>%
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
                    paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/table_PS.xlsx"))


