
# Importar librerías ------------------------------------------------------
library(tidyverse)
library(janitor)
library(pollster) #Weighted data survey tables in R
library(gtsummary)
library(knitr)

# Importar data -----------------------------------------------------------
endo_14 <- rio::import("data/bd_endo2014.sav")
endo_16 <- rio::import("data/bd_endo2016.sav")
endo_18 <- rio::import("data/bd_endo2018.sav")
endo_20 <- rio::import("data/bd_endo2020.sav")
endo_21 <- rio::import("data/bd_endo2021.sav")

# Prevalencia de estrés, ansiedad y depresión -----------------------------

#2014; p36_: Durante los últimos dos años ¿ha sufrido alguna de las siguientes enfermedades? (_3: Estrés, _4: Depresión)
#2016; P401: Durante el año 2015 (de enero a diciembre), ¿sufrió alguna de las siguientes enfermedades? (_9: Estrés, _10: Depresión)
#2018; P401: Durante el año 2017 (de enero a diciembre), ¿sufrió alguna de las siguientes enfermedades? (_9: Estrés, _10: Ansiedad, _12: Depresión)
#2020; P11:  Durante el año 2020, ¿sufrió o sufre alguna de las siguientes enfermedades y/ o padecimiento? (_E: Estrés, _F: Ansiedad, _G: Depresión)
#2021; P4:   Durante el presente año usted, ¿sufrió o sufre alguna de las siguientes enfermedades o malestares? (_E: Estrés, _F: Ansiedad, _G: Depresión)

# Estrés
topline(df = endo_14, variable = p36_3, weight = fe)
topline(df = endo_16, variable = p401_9, weight = ponderador)
topline(df = endo_18, variable = P401_9, weight = FACTOREXPANSION)
topline(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), variable = P1_11_E, weight = FACTOR_EXPANSION)
topline(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), variable = P4_4_E, weight = FACTOR_EXPANSION)

# Depresión
topline(df = endo_14, variable = p36_4, weight = fe)
topline(df = endo_16, variable = p401_10, weight = ponderador)
topline(df = endo_18, variable = P401_12, weight = FACTOREXPANSION)
topline(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), variable = P1_11_G, weight = FACTOR_EXPANSION)
topline(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), variable = P4_4_G, weight = FACTOR_EXPANSION)

# Ansiedad
topline(df = endo_18, variable = P401_10, weight = FACTOREXPANSION)
topline(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), variable = P1_11_F, weight = FACTOR_EXPANSION)
topline(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), variable = P4_4_F, weight = FACTOR_EXPANSION)


# SM + Sexo ---------------------------------------------------------------

# Estrés + sexo
pollster::crosstab(df = endo_14, y = p36_3, x = p68, weight = fe) 
pollster::crosstab(df = endo_16, y = p401_9, x = p109_c_01, weight = ponderador) 
pollster::crosstab(df = endo_18, y = P401_9, x = `P109_C$01`, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_E, x = P1_1, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_E, x = Sexo, weight = FACTOR_EXPANSION) 

# Depresion + sexo
pollster::crosstab(df = endo_14, y = p36_4, x = p68, weight = fe) 
pollster::crosstab(df = endo_16, y = p401_10, x = p109_c_01, weight = ponderador) 
pollster::crosstab(df = endo_18, y = P401_12, x = `P109_C$01`, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_G, x = P1_1, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_G, x = Sexo, weight = FACTOR_EXPANSION) 

# Ansiedad + sexo
pollster::crosstab(df = endo_18, y = P401_10, x = `P109_C$01`, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_F, x = P1_1, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_F, x = Sexo, weight = FACTOR_EXPANSION) 



# SM + Área ---------------------------------------------------------------

# Estrés + area
pollster::crosstab(df = endo_14, y = p36_3, x = area, weight = fe) 
pollster::crosstab(df = endo_16, y = p401_9, x = p04, weight = ponderador) 
pollster::crosstab(df = endo_18, y = P401_9, x = P04, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_E, x = cod_area, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_E, x = AREA_DESC, weight = FACTOR_EXPANSION) 

# Depresión + area
pollster::crosstab(df = endo_14, y = p36_4, x = area, weight = fe) 
pollster::crosstab(df = endo_16, y = p401_10, x = p04, weight = ponderador) 
pollster::crosstab(df = endo_18, y = P401_12, x = P04, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_G, x = cod_area, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_G, x = AREA_DESC, weight = FACTOR_EXPANSION) 

# Ansiedad + area
pollster::crosstab(df = endo_18, y = P401_10, x = P04, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_F, x = cod_area, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_F, x = AREA_DESC, weight = FACTOR_EXPANSION) 


# SM + Nivel educativo  ------------------------------------------------------------

# Estrés + NE
pollster::crosstab(df = endo_14, y = p36_3, x = nivel, weight = fe) 
pollster::crosstab(df = endo_16, y = p401_9, x = p09, weight = ponderador) 
pollster::crosstab(df = endo_18, y = P401_9, x = P08, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_E, x = NIVEL, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_E, x = NIVEL_DESC, weight = FACTOR_EXPANSION) 

# Depresión + NE
pollster::crosstab(df = endo_14, y = p36_4, x = nivel, weight = fe) 
pollster::crosstab(df = endo_16, y = p401_10, x = p09, weight = ponderador) 
pollster::crosstab(df = endo_18, y = P401_12, x = P08, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_G, x = NIVEL, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_G, x = NIVEL_DESC, weight = FACTOR_EXPANSION) 

# Ansiedad + NE
pollster::crosstab(df = endo_18, y = P401_10, x = P08, weight = FACTOREXPANSION) 
pollster::crosstab(df = filter(endo_20, !is.na(FACTOR_EXPANSION)), y = P1_11_F, x = NIVEL, weight = FACTOR_EXPANSION) 
pollster::crosstab(df = filter(endo_21, !is.na(FACTOR_EXPANSION)), y = P4_4_F, x = NIVEL_DESC, weight = FACTOR_EXPANSION) 

