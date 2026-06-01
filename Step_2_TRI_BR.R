##############################################
#          PNAD Contínua
# Dados: 2012 - 2026 (Brasil)
##############################################
#          TRIMESTRAL            #
##################################
#    LEITURA DE PACOTES
##################################
options(timeout = 600) 
# install.packages("PNADcIBGE")
# install.packages("survey")
library(PNADcIBGE)
library(survey)
library(foreign)
library(srvyr)
library(reactable)
library(purrr)
library(readxl)
library(purrr)
library(dplyr)

#####################################################
#        CONFIGURANDO DIRETÓRIO DE DADOS
#####################################################

library(here)
here::set_here("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD")

#####################################################

# 2012


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2012_completo.RData")



dadosPNADc2012_completo <- dadosPNADc2012_completo %>%
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2012_completoPR <- dadosPNADc2012_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2012_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2012_completoPR)
dadosPNADc2012_completoSRPR <- srvyr::as_survey(dadosPNADc2012_completoPR)


### Total de Pessoas


table_1P_2012 <- dadosPNADc2012_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2012 <- dadosPNADc2012_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2012 <- dadosPNADc2012_completoSRPR %>%
  dplyr::filter(VD4032 != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4032,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2012_completoSEC <-  dadosPNADc2012_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2012_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2012_completoSEC)
dadosPNADc2012_completoSRSEC <- srvyr::as_survey(dadosPNADc2012_completoSEC)


### Total de Pessoas


table_1S_2012 <- dadosPNADc2012_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2012 <- dadosPNADc2012_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2012 <- dadosPNADc2012_completoSRSEC %>%  
  dplyr::filter(VD4033 != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4033,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2012 <- dadosPNADc2012_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2012 <- dadosPNADc2012_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2012 <- dadosPNADc2012_completoSRPR %>%
  dplyr::filter(VD4032 != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4032,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2012 <- dadosPNADc2012_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2012 <- dadosPNADc2012_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2012 <- dadosPNADc2012_completoSRSEC %>%  
  dplyr::filter(VD4033 != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4033,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))





rm(dadosPNADc2012_completo,   
    dadosPNADc2012_completoPR, dadosPNADc2012_completoSEC,
   dadosPNADc2012_completoSRPR, dadosPNADc2012_completoSRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2012,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2012, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2012,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2012,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2012, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2012,
               
               "N PRINCIPAL"                   = table_1P_2012,
               "HABITUAL PRINCIPAL"            = table_2P_2012, 
               "EFETIVA PRINCIPAL"             = table_3P_2012,
       
               "N SECUNDÁRIO"                  = table_1S_2012,
               "HABITUAL SECUNDÁRIO"           = table_2S_2012, 
               "EFETIVA SECUNDÁRIO"            = table_3S_2012)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2012.xlsx"))




# 2013


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2013_completo.RData")



dadosPNADc2013_completo <- dadosPNADc2013_completo %>%
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2013_completoPR <- dadosPNADc2013_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2013_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2013_completoPR)
dadosPNADc2013_completoSRPR <- srvyr::as_survey(dadosPNADc2013_completoPR)


### Total de Pessoas


table_1P_2013 <- dadosPNADc2013_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2013 <- dadosPNADc2013_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2013 <- dadosPNADc2013_completoSRPR %>%
  dplyr::filter(VD4032 != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4032,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2013_completoSEC <-  dadosPNADc2013_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2013_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2013_completoSEC)
dadosPNADc2013_completoSRSEC <- srvyr::as_survey(dadosPNADc2013_completoSEC)


### Total de Pessoas


table_1S_2013 <- dadosPNADc2013_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2013 <- dadosPNADc2013_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2013 <- dadosPNADc2013_completoSRSEC %>%  
  dplyr::filter(VD4033 != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4033,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2013 <- dadosPNADc2013_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2013 <- dadosPNADc2013_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2013 <- dadosPNADc2013_completoSRPR %>%
  dplyr::filter(VD4032 != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4032,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2013 <- dadosPNADc2013_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2013 <- dadosPNADc2013_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2013 <- dadosPNADc2013_completoSRSEC %>%  
  dplyr::filter(VD4033 != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4033,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2013, dadosPNADc1VISITA_2013PR, dadosPNADc1VISITA_2013SEC,
   dadosPNADc1VISITA_2013SRPR, dadosPNADc1VISITA_2013SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2013,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2013, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2013,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2013,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2013, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2013,
               
               "N PRINCIPAL"                   = table_1P_2013,
               "HABITUAL PRINCIPAL"            = table_2P_2013, 
               "EFETIVA PRINCIPAL"             = table_3P_2013,
               
               "N SECUNDÁRIO"                  = table_1S_2013,
               "HABITUAL SECUNDÁRIO"           = table_2S_2013,
               "EFETIVA SECUNDÁRIO"            = table_3S_2013)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2013.xlsx"))


# 2014


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2014_completo.RData")



dadosPNADc2014_completo <- dadosPNADc2014_completo %>%
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%
  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2014_completoPR <- dadosPNADc2014_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2014_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2014_completoPR)
dadosPNADc2014_completoSRPR <- srvyr::as_survey(dadosPNADc2014_completoPR)


### Total de Pessoas


table_1P_2014 <- dadosPNADc2014_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2014 <- dadosPNADc2014_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2014 <- dadosPNADc2014_completoSRPR %>%
  dplyr::filter(VD4032 != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4032,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2014_completoSEC <-  dadosPNADc2014_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2014_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2014_completoSEC)
dadosPNADc2014_completoSRSEC <- srvyr::as_survey(dadosPNADc2014_completoSEC)


### Total de Pessoas


table_1S_2014 <- dadosPNADc2014_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2014 <- dadosPNADc2014_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2014 <- dadosPNADc2014_completoSRSEC %>%  
  dplyr::filter(VD4033 != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4033,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2014 <- dadosPNADc2014_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2014 <- dadosPNADc2014_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2014 <- dadosPNADc2014_completoSRPR %>%
  dplyr::filter(VD4032 != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4032,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2014 <- dadosPNADc2014_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2014 <- dadosPNADc2014_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2014 <- dadosPNADc2014_completoSRSEC %>%  
  dplyr::filter(VD4033 != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(VD4033,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




rm(dadosPNADc1VISITA_2014, dadosPNADc1VISITA_2014PR, dadosPNADc1VISITA_2014SEC,
   dadosPNADc1VISITA_2014SRPR, dadosPNADc1VISITA_2014SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2014,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2014, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2014,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2014,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2014, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2014,
               
               "N PRINCIPAL"                    = table_1P_2014,
               "HABITUAL PRINCIPAL"             = table_2P_2014, 
               "EFETIVA PRINCIPAL"              = table_3P_2014,

               "N SECUNDÁRIO"                   = table_1S_2014,
               "HABITUAL SECUNDÁRIO"            = table_2S_2014,
               "EFETIVA SECUNDÁRIO"             = table_3S_2014)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2014.xlsx"))


# 2015


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2015_completo.RData")



dadosPNADc2015_completo <- dadosPNADc2015_completo %>%
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%

     #ANO 2015 USAR:
     dplyr::mutate(V4039C = base::ifelse(is.na(V4039C),
                                        VD4032,
                                        V4039C),
                  V4056C = base::ifelse(is.na(V4056C),
                                        VD4033,
                                        V4056C)) %>% 
  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2015_completoPR <- dadosPNADc2015_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2015_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2015_completoPR)
dadosPNADc2015_completoSRPR <- srvyr::as_survey(dadosPNADc2015_completoPR)


### Total de Pessoas


table_1P_2015 <- dadosPNADc2015_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2015 <- dadosPNADc2015_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2015 <- dadosPNADc2015_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2015_completoSEC <-  dadosPNADc2015_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2015_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2015_completoSEC)
dadosPNADc2015_completoSRSEC <- srvyr::as_survey(dadosPNADc2015_completoSEC)


### Total de Pessoas


table_1S_2015 <- dadosPNADc2015_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2015 <- dadosPNADc2015_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2015 <- dadosPNADc2015_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2015 <- dadosPNADc2015_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2015 <- dadosPNADc2015_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2015 <- dadosPNADc2015_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2015 <- dadosPNADc2015_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2015 <- dadosPNADc2015_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2015 <- dadosPNADc2015_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))





rm(dadosPNADc1VISITA_2015, dadosPNADc1VISITA_2015PR, dadosPNADc1VISITA_2015SEC,
   dadosPNADc1VISITA_2015SRPR, dadosPNADc1VISITA_2015SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2015,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2015, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2015,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2015,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2015, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2015,
               
               "N PRINCIPAL"                    = table_1P_2015,
               "HABITUAL PRINCIPAL"             = table_2P_2015, 
               "EFETIVA PRINCIPAL"              = table_3P_2015,

               "N SECUNDÁRIO"                   = table_1S_2015,
               "HABITUAL SECUNDÁRIO"            = table_2S_2015,
               "EFETIVA SECUNDÁRIO"             = table_3S_2015)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2015.xlsx"))


# 2016


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2016_completo.RData")



dadosPNADc2016_completo <- dadosPNADc2016_completo %>%  

  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%   

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2016_completoPR <- dadosPNADc2016_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2016_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2016_completoPR)
dadosPNADc2016_completoSRPR <- srvyr::as_survey(dadosPNADc2016_completoPR)


### Total de Pessoas


table_1P_2016 <- dadosPNADc2016_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2016 <- dadosPNADc2016_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2016 <- dadosPNADc2016_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2016_completoSEC <-  dadosPNADc2016_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2016_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2016_completoSEC)
dadosPNADc2016_completoSRSEC <- srvyr::as_survey(dadosPNADc2016_completoSEC)


### Total de Pessoas


table_1S_2016 <- dadosPNADc2016_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2016 <- dadosPNADc2016_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2016 <- dadosPNADc2016_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2016 <- dadosPNADc2016_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2016 <- dadosPNADc2016_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2016 <- dadosPNADc2016_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2016 <- dadosPNADc2016_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2016 <- dadosPNADc2016_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2016 <- dadosPNADc2016_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))





rm(dadosPNADc1VISITA_2016, dadosPNADc1VISITA_2016PR, dadosPNADc1VISITA_2016SEC,
   dadosPNADc1VISITA_2016SRPR, dadosPNADc1VISITA_2016SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2016,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2016, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2016,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2016,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2016, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2016,
               
               "N PRINCIPAL"                    = table_1P_2016,
               "HABITUAL PRINCIPAL"             = table_2P_2016, 
               "EFETIVA PRINCIPAL"              = table_3P_2016,
                               
               "N SECUNDÁRIO"                   = table_1S_2016,
               "HABITUAL SECUNDÁRIO"            = table_2S_2016,
               "EFETIVA SECUNDÁRIO"             = table_3S_2016)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2016.xlsx"))


# 2017


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2017_completo.RData")



dadosPNADc2017_completo <- dadosPNADc2017_completo %>% 

  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2017_completoPR <- dadosPNADc2017_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2017_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2017_completoPR)
dadosPNADc2017_completoSRPR <- srvyr::as_survey(dadosPNADc2017_completoPR)


### Total de Pessoas


table_1P_2017 <- dadosPNADc2017_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2017 <- dadosPNADc2017_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2017 <- dadosPNADc2017_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2017_completoSEC <-  dadosPNADc2017_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2017_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2017_completoSEC)
dadosPNADc2017_completoSRSEC <- srvyr::as_survey(dadosPNADc2017_completoSEC)


### Total de Pessoas


table_1S_2017 <- dadosPNADc2017_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2017 <- dadosPNADc2017_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2017 <- dadosPNADc2017_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2017 <- dadosPNADc2017_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2017 <- dadosPNADc2017_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2017 <- dadosPNADc2017_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2017 <- dadosPNADc2017_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2017 <- dadosPNADc2017_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2017 <- dadosPNADc2017_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))





rm(dadosPNADc1VISITA_2017, dadosPNADc1VISITA_2017PR, dadosPNADc1VISITA_2017SEC,
   dadosPNADc1VISITA_2017SRPR, dadosPNADc1VISITA_2017SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2017,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2017, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2017,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2017,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2017, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2017,
               
               "N PRINCIPAL"                    = table_1P_2017,
               "HABITUAL PRINCIPAL"             = table_2P_2017, 
               "EFETIVA PRINCIPAL"              = table_3P_2017,
                               
               "N SECUNDÁRIO"                   = table_1S_2017,
               "HABITUAL SECUNDÁRIO"            = table_2S_2017,
               "EFETIVA SECUNDÁRIO"             = table_3S_2017)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2017.xlsx"))



# 2018

load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2018_completo.RData")



dadosPNADc2018_completo <- dadosPNADc2018_completo %>%  

  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%   

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2018_completoPR <- dadosPNADc2018_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2018_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2018_completoPR)
dadosPNADc2018_completoSRPR <- srvyr::as_survey(dadosPNADc2018_completoPR)


### Total de Pessoas


table_1P_2018 <- dadosPNADc2018_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2018 <- dadosPNADc2018_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2018 <- dadosPNADc2018_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2018_completoSEC <-  dadosPNADc2018_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2018_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2018_completoSEC)
dadosPNADc2018_completoSRSEC <- srvyr::as_survey(dadosPNADc2018_completoSEC)


### Total de Pessoas


table_1S_2018 <- dadosPNADc2018_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2018 <- dadosPNADc2018_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2018 <- dadosPNADc2018_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2018 <- dadosPNADc2018_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2018 <- dadosPNADc2018_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2018 <- dadosPNADc2018_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2018 <- dadosPNADc2018_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2018 <- dadosPNADc2018_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2018 <- dadosPNADc2018_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2018, dadosPNADc1VISITA_2018PR, dadosPNADc1VISITA_2018SEC,
   dadosPNADc1VISITA_2018SRPR, dadosPNADc1VISITA_2018SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2018,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2018, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2018,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2018,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2018, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2018,
               
               "N PRINCIPAL"                    = table_1P_2018,
               "HABITUAL PRINCIPAL"             = table_2P_2018, 
               "EFETIVA PRINCIPAL"              = table_3P_2018,
                               
               "N SECUNDÁRIO"                   = table_1S_2018,
               "HABITUAL SECUNDÁRIO"            = table_2S_2018,
               "EFETIVA SECUNDÁRIO"             = table_3S_2018)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2018.xlsx"))



rm(list = ls())


# 2019


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2019_completo.RData")



dadosPNADc2019_completo <- dadosPNADc2019_completo %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2019_completoPR <- dadosPNADc2019_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2019_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2019_completoPR)
dadosPNADc2019_completoSRPR <- srvyr::as_survey(dadosPNADc2019_completoPR)


### Total de Pessoas


table_1P_2019 <- dadosPNADc2019_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2019 <- dadosPNADc2019_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2019 <- dadosPNADc2019_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2019_completoSEC <-  dadosPNADc2019_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2019_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2019_completoSEC)
dadosPNADc2019_completoSRSEC <- srvyr::as_survey(dadosPNADc2019_completoSEC)


### Total de Pessoas


table_1S_2019 <- dadosPNADc2019_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2019 <- dadosPNADc2019_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2019 <- dadosPNADc2019_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2019 <- dadosPNADc2019_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2019 <- dadosPNADc2019_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2019 <- dadosPNADc2019_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2019 <- dadosPNADc2019_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2019 <- dadosPNADc2019_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2019 <- dadosPNADc2019_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2019, dadosPNADc1VISITA_2019PR, dadosPNADc1VISITA_2019SEC, 
   dadosPNADc1VISITA_2019SRPR, dadosPNADc1VISITA_2019SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2019,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2019, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2019,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2019,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2019, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2019,
               
               "N PRINCIPAL"                    = table_1P_2019,
               "HABITUAL PRINCIPAL"             = table_2P_2019, 
               "EFETIVA PRINCIPAL"              = table_3P_2019,
                               
               "N SECUNDÁRIO"                   = table_1S_2019,
               "HABITUAL SECUNDÁRIO"            = table_2S_2019,
               "EFETIVA SECUNDÁRIO"             = table_3S_2019)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2019.xlsx"))


# 2020



load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2020_completo.RData")



dadosPNADc2020_completo <- dadosPNADc2020_completo %>%
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2020_completoPR <- dadosPNADc2020_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2020_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2020_completoPR)
dadosPNADc2020_completoSRPR <- srvyr::as_survey(dadosPNADc2020_completoPR)


### Total de Pessoas


table_1P_2020 <- dadosPNADc2020_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2020 <- dadosPNADc2020_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2020 <- dadosPNADc2020_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2020_completoSEC <-  dadosPNADc2020_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2020_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2020_completoSEC)
dadosPNADc2020_completoSRSEC <- srvyr::as_survey(dadosPNADc2020_completoSEC)


### Total de Pessoas


table_1S_2020 <- dadosPNADc2020_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2020 <- dadosPNADc2020_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2020 <- dadosPNADc2020_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2020 <- dadosPNADc2020_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2020 <- dadosPNADc2020_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2020 <- dadosPNADc2020_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2020 <- dadosPNADc2020_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2020 <- dadosPNADc2020_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2020 <- dadosPNADc2020_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2020, dadosPNADc1VISITA_2020PR, dadosPNADc1VISITA_2020SEC,
   dadosPNADc1VISITA_2020SRPR, dadosPNADc1VISITA_2020SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2020,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2020, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2020,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2020,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2020, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2020,
               
               "N PRINCIPAL"                    = table_1P_2020,
               "HABITUAL PRINCIPAL"             = table_2P_2020, 
               "EFETIVA PRINCIPAL"              = table_3P_2020,
                               
               "N SECUNDÁRIO"                   = table_1S_2020,
               "HABITUAL SECUNDÁRIO"            = table_2S_2020,
               "EFETIVA SECUNDÁRIO"             = table_3S_2020)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2020.xlsx"))


# 2021


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2021_completo.RData")



dadosPNADc2021_completo <- dadosPNADc2021_completo %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  
  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2021_completoPR <- dadosPNADc2021_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2021_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2021_completoPR)
dadosPNADc2021_completoSRPR <- srvyr::as_survey(dadosPNADc2021_completoPR)


### Total de Pessoas


table_1P_2021 <- dadosPNADc2021_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2021 <- dadosPNADc2021_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2021 <- dadosPNADc2021_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2021_completoSEC <-  dadosPNADc2021_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2021_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2021_completoSEC)
dadosPNADc2021_completoSRSEC <- srvyr::as_survey(dadosPNADc2021_completoSEC)


### Total de Pessoas


table_1S_2021 <- dadosPNADc2021_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2021 <- dadosPNADc2021_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2021 <- dadosPNADc2021_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2021 <- dadosPNADc2021_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2021 <- dadosPNADc2021_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2021 <- dadosPNADc2021_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2021 <- dadosPNADc2021_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2021 <- dadosPNADc2021_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2021 <- dadosPNADc2021_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2021, dadosPNADc1VISITA_2021PR, dadosPNADc1VISITA_2021SEC,
   dadosPNADc1VISITA_2021SRPR, dadosPNADc1VISITA_2021SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2021,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2021, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2021,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2021,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2021, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2021,
               
               "N PRINCIPAL"                    = table_1P_2021,
               "HABITUAL PRINCIPAL"             = table_2P_2021, 
               "EFETIVA PRINCIPAL"              = table_3P_2021,
                               
               "N SECUNDÁRIO"                   = table_1S_2021,
               "HABITUAL SECUNDÁRIO"            = table_2S_2021,
               "EFETIVA SECUNDÁRIO"             = table_3S_2021)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2021.xlsx"))


# 2022


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2022_completo.RData")



dadosPNADc2022_completo <- dadosPNADc2022_completo %>%
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2022_completoPR <- dadosPNADc2022_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2022_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2022_completoPR)
dadosPNADc2022_completoSRPR <- srvyr::as_survey(dadosPNADc2022_completoPR)


### Total de Pessoas


table_1P_2022 <- dadosPNADc2022_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2022 <- dadosPNADc2022_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2022 <- dadosPNADc2022_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2022_completoSEC <-  dadosPNADc2022_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2022_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2022_completoSEC)
dadosPNADc2022_completoSRSEC <- srvyr::as_survey(dadosPNADc2022_completoSEC)


### Total de Pessoas


table_1S_2022 <- dadosPNADc2022_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2022 <- dadosPNADc2022_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2022 <- dadosPNADc2022_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2022 <- dadosPNADc2022_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2022 <- dadosPNADc2022_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2022 <- dadosPNADc2022_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2022 <- dadosPNADc2022_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2022 <- dadosPNADc2022_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2022 <- dadosPNADc2022_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2022, dadosPNADc1VISITA_2022PR, dadosPNADc1VISITA_2022SEC, 
   dadosPNADc1VISITA_2022SRPR, dadosPNADc1VISITA_2022SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2022,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2022, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2022,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2022,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2022, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2022,
               
               "N PRINCIPAL"                    = table_1P_2022,
               "HABITUAL PRINCIPAL"             = table_2P_2022, 
               "EFETIVA PRINCIPAL"              = table_3P_2022,
               
               "N SECUNDÁRIO"                   = table_1S_2022,
               "HABITUAL SECUNDÁRIO"            = table_2S_2022,
               "EFETIVA SECUNDÁRIO"             = table_3S_2022)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2022.xlsx"))


# 2023


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2023_completo.RData")



dadosPNADc2023_completo <- dadosPNADc2023_completo %>% 
  dplyr::mutate(Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  
  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2023_completoPR <- dadosPNADc2023_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2023_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2023_completoPR)
dadosPNADc2023_completoSRPR <- srvyr::as_survey(dadosPNADc2023_completoPR)


### Total de Pessoas


table_1P_2023 <- dadosPNADc2023_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2023 <- dadosPNADc2023_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2023 <- dadosPNADc2023_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2023_completoSEC <-  dadosPNADc2023_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2023_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2023_completoSEC)
dadosPNADc2023_completoSRSEC <- srvyr::as_survey(dadosPNADc2023_completoSEC)


### Total de Pessoas


table_1S_2023 <- dadosPNADc2023_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2023 <- dadosPNADc2023_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2023 <- dadosPNADc2023_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2023 <- dadosPNADc2023_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2023 <- dadosPNADc2023_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2023 <- dadosPNADc2023_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2023 <- dadosPNADc2023_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2023 <- dadosPNADc2023_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2023 <- dadosPNADc2023_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2023, dadosPNADc1VISITA_2023PR, dadosPNADc1VISITA_2023SEC,
   dadosPNADc1VISITA_2023SRPR, dadosPNADc1VISITA_2023SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2023,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2023, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2023,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2023,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2023, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2023,
               
               "N PRINCIPAL"                    = table_1P_2023,
               "HABITUAL PRINCIPAL"             = table_2P_2023, 
               "EFETIVA PRINCIPAL"              = table_3P_2023,
                               
               "N SECUNDÁRIO"                   = table_1S_2023,
               "HABITUAL SECUNDÁRIO"            = table_2S_2023,
               "EFETIVA SECUNDÁRIO"             = table_3S_2023)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2023.xlsx"))


# 2024


load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2024_completo.RData")



dadosPNADc2024_completo <- dadosPNADc2024_completo %>% 
  dplyr::mutate(Trimestre = dplyr::case_when(Trimestre == "Trimestre_1" ~ 1,
                                             Trimestre == "Trimestre_2" ~ 2,
                                             Trimestre == "Trimestre_3" ~ 3,
                                             Trimestre == "Trimestre_4" ~ 4),
    Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  

  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2024_completoPR <- dadosPNADc2024_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2024_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2024_completoPR)
dadosPNADc2024_completoSRPR <- srvyr::as_survey(dadosPNADc2024_completoPR)


### Total de Pessoas


table_1P_2024 <- dadosPNADc2024_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2024 <- dadosPNADc2024_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2024 <- dadosPNADc2024_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2024_completoSEC <-  dadosPNADc2024_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2024_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2024_completoSEC)
dadosPNADc2024_completoSRSEC <- srvyr::as_survey(dadosPNADc2024_completoSEC)


### Total de Pessoas


table_1S_2024 <- dadosPNADc2024_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2024 <- dadosPNADc2024_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2024 <- dadosPNADc2024_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2024 <- dadosPNADc2024_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2024 <- dadosPNADc2024_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2024 <- dadosPNADc2024_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2024 <- dadosPNADc2024_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2024 <- dadosPNADc2024_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2024 <- dadosPNADc2024_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))






rm(dadosPNADc1VISITA_2024, dadosPNADc1VISITA_2024PR, dadosPNADc1VISITA_2024SEC, 
   dadosPNADc1VISITA_2024SRPR, dadosPNADc1VISITA_2024SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2024,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2024, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2024,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2024,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2024, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2024,
               
               "N PRINCIPAL"                    = table_1P_2024,
               "HABITUAL PRINCIPAL"             = table_2P_2024, 
               "EFETIVA PRINCIPAL"              = table_3P_2024,
                               
               "N SECUNDÁRIO"                   = table_1S_2024,
               "HABITUAL SECUNDÁRIO"            = table_2S_2024,
               "EFETIVA SECUNDÁRIO"             = table_3S_2024)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2024.xlsx"))


# 2025



load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2025_completo.RData")



dadosPNADc2025_completo <- dadosPNADc2025_completo %>% 
  dplyr::mutate(Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  
  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2025_completoPR <- dadosPNADc2025_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2025_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2025_completoPR)
dadosPNADc2025_completoSRPR <- srvyr::as_survey(dadosPNADc2025_completoPR)


### Total de Pessoas


table_1P_2025 <- dadosPNADc2025_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2025 <- dadosPNADc2025_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2025 <- dadosPNADc2025_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2025_completoSEC <-  dadosPNADc2025_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2025_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2025_completoSEC)
dadosPNADc2025_completoSRSEC <- srvyr::as_survey(dadosPNADc2025_completoSEC)


### Total de Pessoas


table_1S_2025 <- dadosPNADc2025_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2025 <- dadosPNADc2025_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2025 <- dadosPNADc2025_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2025 <- dadosPNADc2025_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2025 <- dadosPNADc2025_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2025 <- dadosPNADc2025_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2025 <- dadosPNADc2025_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2025 <- dadosPNADc2025_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2025 <- dadosPNADc2025_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))





rm(dadosPNADc1VISITA_2025, dadosPNADc1VISITA_2025PR, dadosPNADc1VISITA_2025SEC,
   dadosPNADc1VISITA_2025SRPR, dadosPNADc1VISITA_2025SRSEC)

gc()


# 2026



load(file = "C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/Trimestre/dadosPNADc2026_completo.RData")



dadosPNADc2026_completo <- dadosPNADc2026_completo %>% 
  dplyr::mutate(Trimestre = base::factor(Trimestre, levels = c("1", "2", "3", "4"))) %>%  
  dplyr::mutate(cod_SCN_PR =  dplyr::case_when(V4013 %in% c("01101","01102","01103","01104","01105",
                                                            "01106","01107","01108","01109","01110",
                                                            "01111","01112","01113","01114","01115",
                                                            "01116","01117","01118","01119","01201",
                                                            "01202","01203","01204","01205","01206",
                                                            "01207","01208","01209","01401","01402",
                                                            "01500","01999","02000","03001","03002") ~ 1,

V4013 %in% c("05000","06000","07001","07002","08001",
             "08002","08009","09000") ~ 2,

V4013 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4013 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4013 %in% c("41000","42000","43000") ~ 5,

V4013 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,

V4013 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4013 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4013 %in% c("64000","65000","66001","66002") ~ 9,

V4013 == "68000" ~ 10,

#######################################################
V4013 %in% c("55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,

V4012 == "Trabalhador doméstico" & V4013 %in% c("85011","85012","85013",
                                                "85014","85021","85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4012 == "Empregado do setor privado" & V4013 %in% c("85011","85012","85013",
                                                     "85014","85021","85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Empregador" & V4013 %in% c("85011","85012","85013",
                                     "85014","85021","85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4012 == "Conta própria" & V4013 %in% c("85011","85012","85013",
                                        "85014","85021","85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4012 == "Trabalhador familiar não remunerado" & V4013 %in% c("85011","85012","85013",
                                                              "85014","85021","85029",
                                                              "86001","86002","86003",
                                                              "86004","86009","87000","88000") ~ 11,

########################################################

V4013 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4012 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4013 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4012 == "Empregado do setor público (inclusive empresas de economia mista)" & V4013 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4013 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 )  %>% 
 
    dplyr::mutate(cod_SCN_SEC =  dplyr::case_when(V4044 %in% c(
"01101","01102","01103","01104","01105",
"01106","01107","01108","01109","01110",
"01111","01112","01113","01114","01115",
"01116","01117","01118","01119","01201",
"01202","01203","01204","01205","01206",
"01207","01208","01209","01401","01402",
"01500","01999","02000","03001","03002") ~ 1,

V4044 %in% c("05000","06000","07001","07002","08001","08002","08009","09000") ~ 2,

V4044 %in% c("10010","10021","10022","10030","10091",
             "10092","10093","10099","11000","12000",
             "13001","13002","14001","14002","15011",
             "15012","15020","16001","16002","17001",
             "17002","18000","19010","19020","19030",
             "20010","20020","20090","21000","22010",
             "22020","23010","23091","23099","24001",
             "24002","24003","25001","25002","26010",
             "26020","26030","26041","26042","27010",
             "27090","28000","29001","29002","29003",
             "30010","30020","30030","30090","31000",
             "32001","32002","32003","32009","33001","33002") ~ 3,

V4044 %in% c("35010","35021","35022","36000","37000","38000","39000") ~ 4,

V4044 %in% c("41000","42000","43000") ~ 5,

V4044 %in% c("45010","45020","45030","45040","48010",
             "48020","48030","48041","48042","48050",
             "48060","48071","48072","48073","48074",
             "48075","48076","48077","48078","48079",
             "48080","48090","48100") ~ 6,


V4044 %in% c("49010","49030","49040","49090","50000",
             "51000","52010","52020","53001","53002") ~ 7,

V4044 %in% c("58000","59000","60001","60002","61000","62000","63000") ~ 8,

V4044 %in% c("64000","65000","66001","66002") ~ 9,

V4044 == "68000" ~ 10,


###################################################################

V4044 %in% c(
"55000","56011","56012","56020","69000",
"70000","71000","72000","73010","73020",
"74000","75000","77010","77020","78000",
"79000","80000","81011","81012","81020",
"82001","82002","82003","82009","90000",
"91000","92000","93011","93012","93020",
"94010","94020","94091","94099","95010",
"95030","96010","96020","96030","96090",
"97000") ~ 11,



V4043 == "Trabalhador doméstico" & V4044 %in% c("85011","85012","85013",
                                                "85014","85021", "85029",
                                                "86001","86002","86003",
                                                "86004","86009","87000","88000") ~ 11,
V4043 == "Empregado do setor privado" & V4044 %in% c("85011","85012","85013",
                                                     "85014","85021", "85029",
                                                     "86001","86002","86003",
                                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Empregador" & V4044 %in% c("85011","85012","85013",
                                     "85014","85021", "85029",
                                     "86001","86002","86003",
                                     "86004","86009","87000","88000") ~ 11,
V4043 == "Conta própria" & V4044 %in% c("85011","85012","85013",
                                        "85014","85021", "85029",
                                        "86001","86002","86003",
                                        "86004","86009","87000","88000") ~ 11,
V4043 == "Trabalhador não remunerado em ajuda a membro do domicílio ou parente" & V4044 %in% c("85011","85012","85013",
                                                                                               "85014","85021", "85029",
                                                                                               "86001","86002","86003",
                                                                                               "86004","86009","87000","88000") ~ 11,


####################################################################

V4044 %in% c("84011","84012","84013","84014","84015","84016","84017","84020") ~ 12,

V4043 == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" & V4044 %in% c("85011","85012","85013",
                                                                                                                               "85014","85021","85029",
                                                                                                                               "86001","86002","86003",
                                                                                                                               "86004","86009","87000",
                                                                                                                               "88000") ~ 12,
V4043 == "Empregado do setor público (inclusive empresas de economia mista)" & V4044 %in% c("85011","85012","85013",
                                                                                            "85014","85021","85029",
                                                                                            "86001","86002","86003",
                                                                                            "86004","86009","87000","88000") ~ 12,

########################################################

V4044 %in% c("99000","00000") ~ 0,

.default = NA_integer_)
 ) 


## Principal
## Pessoas ocupadas


dadosPNADc2026_completoPR <- dadosPNADc2026_completo %>% 
  dplyr::filter(!(is.na(V4013))) %>%
  dplyr::filter(VD4002 == "Pessoas ocupadas") 

dadosPNADc2026_completoPR  <- PNADcIBGE::pnadc_design(dadosPNADc2026_completoPR)
dadosPNADc2026_completoSRPR <- srvyr::as_survey(dadosPNADc2026_completoPR)


### Total de Pessoas


table_1P_2026 <- dadosPNADc2026_completoSRPR %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


### Total de horas Habituais



table_2P_2026 <- dadosPNADc2026_completoSRPR %>%  
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))




### Total de horas Efetivas



table_3P_2026 <- dadosPNADc2026_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(cod_SCN_PR, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Secundário 
## Pessoas Ocupadas



dadosPNADc2026_completoSEC <-  dadosPNADc2026_completo %>% 
  dplyr::filter(!(is.na(V4044))) %>% 
  dplyr::filter(VD4002 == "Pessoas ocupadas")  
  
dadosPNADc2026_completoSEC  <- PNADcIBGE::pnadc_design(dadosPNADc2026_completoSEC)
dadosPNADc2026_completoSRSEC <- srvyr::as_survey(dadosPNADc2026_completoSEC)


### Total de Pessoas


table_1S_2026 <- dadosPNADc2026_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



### Total de horas Habituais



table_2S_2026 <- dadosPNADc2026_completoSRSEC %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Total de horas Efetivas




table_3S_2026 <- dadosPNADc2026_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(cod_SCN_SEC, Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


## Total

### Principal

#### Pessoas Ocupadas


table_1TP_2026 <- dadosPNADc2026_completoSRPR %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))


#### Horas Habituais



table_2TP_2026 <- dadosPNADc2026_completoSRPR %>%  
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4039,
                                                   na.rm = TRUE,
                                                 vartype = c("se", "ci", "var", "cv")))


#### Horas Efetivas


table_3TP_2026 <- dadosPNADc2026_completoSRPR %>%
  dplyr::filter(V4039C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4039C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


### Secundário

#### Total de Pessoas


table_1TS_2026 <- dadosPNADc2026_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(freq = srvyr::survey_total(vartype = c("se", "ci", "var", "cv")))



#### Total de horas Habituais



table_2TS_2026 <- dadosPNADc2026_completoSRSEC %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasHabituais = srvyr::survey_total(V4056,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))


#### Total de horas Efetivas




table_3TS_2026 <- dadosPNADc2026_completoSRSEC %>%  
  dplyr::filter(V4056C != 0) %>% 
  dplyr::group_by(Ano, Trimestre) %>% 
  dplyr::summarise(Qtd_horasEfetivas = srvyr::survey_total(V4056C,
                                                   na.rm = TRUE,
                                                   vartype = c("se", "ci", "var", "cv")))





rm(dadosPNADc1VISITA_2026, dadosPNADc1VISITA_2026PR, dadosPNADc1VISITA_2026SEC, 
   dadosPNADc1VISITA_2026SRPR, dadosPNADc1VISITA_2026SRSEC)

gc()



sheets <- list("N TOTAL PRINCIPAL"             = table_1TP_2026,
               "HABITUAL TOTAL PRINCIPAL"      = table_2TP_2026, 
               "EFETIVA TOTAL PRINCIPAL"       = table_3TP_2026,
               
               "N TOTAL SECUNDÁRIO"            = table_1TS_2026,
               "HABITUAL TOTAL SECUNDÁRIO"     = table_2TS_2026, 
               "EFETIVA TOTAL SECUNDÁRIO"      = table_3TS_2026,
               
               "N PRINCIPAL"                    = table_1P_2026,
               "HABITUAL PRINCIPAL"             = table_2P_2026, 
               "EFETIVA PRINCIPAL"              = table_3P_2026,
            
               "N SECUNDÁRIO"                   = table_1S_2026,
               "HABITUAL SECUNDÁRIO"            = table_2S_2026,
               "EFETIVA SECUNDÁRIO"             = table_3S_2026)

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/BR_TRI/tabPSTRI_2026.xlsx"))


# Empilhamento



anos <- 2012:2026

tabPSTRI_NTOTALPRINCIPAL <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "N TOTAL PRINCIPAL"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_HABITUALTOTALPRINCIPAL <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "HABITUAL TOTAL PRINCIPAL"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_EFETIVATOTALPRINCIPAL <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "EFETIVA TOTAL PRINCIPAL"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_NTOTALSECUNDARIO <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "N TOTAL SECUNDÁRIO"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_HABITUALTOTALSECUNDARIO <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "HABITUAL TOTAL SECUNDÁRIO"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_EFETIVATOTALSECUNDARIO <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "EFETIVA TOTAL SECUNDÁRIO"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_NPRINCIPAL <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "N PRINCIPAL"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_HABITUALPRINCIPAL <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "HABITUAL PRINCIPAL"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_EFETIVAPRINCIPAL <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "EFETIVA PRINCIPAL"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_NSECUNDARIO <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "N SECUNDÁRIO"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_HABITUALSECUNDARIO <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "HABITUAL SECUNDÁRIO"
  ) %>%
    mutate(Ano = .x)
)

tabPSTRI_EFETIVASECUNDARIO <- map_dfr(
  anos,
  ~ read_excel(
    path  = glue::glue("Dados_completos/BR_TRI/tabPSTRI_{.x}.xlsx"),
    sheet = "EFETIVA SECUNDÁRIO"
  ) %>%
    mutate(Ano = .x)
)




# Excel


sheets <- list("N TOTAL PRINCIPAL"             = tabPSTRI_NTOTALPRINCIPAL,
               "HABITUAL TOTAL PRINCIPAL"      = tabPSTRI_HABITUALTOTALPRINCIPAL, 
               "EFETIVA TOTAL PRINCIPAL"       = tabPSTRI_EFETIVATOTALPRINCIPAL,
               
               "N TOTAL SECUNDÁRIO"            = tabPSTRI_NTOTALSECUNDARIO,
               "HABITUAL TOTAL SECUNDÁRIO"     = tabPSTRI_HABITUALTOTALSECUNDARIO, 
               "EFETIVA TOTAL SECUNDÁRIO"      = tabPSTRI_EFETIVATOTALSECUNDARIO,
               
               "N PRINCIPAL"                   = tabPSTRI_NPRINCIPAL,
               "HABITUAL PRINCIPAL"            = tabPSTRI_HABITUALPRINCIPAL, 
               "EFETIVA PRINCIPAL"             = tabPSTRI_EFETIVAPRINCIPAL,
                       
               "N SECUNDÁRIO"                  = tabPSTRI_NSECUNDARIO,
               "HABITUAL SECUNDÁRIO"           = tabPSTRI_HABITUALSECUNDARIO, 
               "EFETIVA SECUNDÁRIO"            = tabPSTRI_EFETIVASECUNDARIO)
         

writexl::write_xlsx(sheets, 
           paste0("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/PRODUTIVIDADE/tabPSTRI_BR.xlsx"))

