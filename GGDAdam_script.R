
# R-script voor genereren outputtabellen GM2016

# Dit script is gebaseerd op een script van Jolien ten Velden, GGD Hart voor Brabant
# Aanpassingen door Camiel Wijffels & Johan Werkhoven (GGD Amsterdam)


## Let op!! Variabelen in "var_cross" en "var_cross_subset" mogen geen 
## '0'als waarde hebben. Dit moet in SPSS gehercodeerd worden.


# Clear workspace
rm(list=ls())

# Clear console
cat("\014")


#  Working directory definiëren
setwd("")

# Libraries laden

library(plyr)
library(haven)
library(dplyr)
library(survey)
library(glue)
library(sjPlot)


#########################
# Initialiseer een boel #
#########################

# Do not create factors from string variables
options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

# # Open databestand en sla op als Rdata
gm_2016 <- read_spss('')

# Herschrijf naar data om code niet steeds aan te hoeven passen.
data <- gm_2016

gm_2016 %>% sjPlot::view_df(,show.frq = TRUE) ## Mooi overzicht voor variabelen. Kan ook uitgelaten worden.

# Initialiseer data_sub met volledige data. Dit omdat later in de code data_sub opgeroepen wordt, die anders niet gedefinieerd is.
data_sub <- data

# Er wordt gebruik gemaakt van zowel data (voor het opstellen van het volledige survey design) als data_sub. 
# data_sub wordt verderop gebruikt voor svytable(), om population counts van de subset te krijgen
# Als je een survey design bouwt met alleen de gesubsette data dan krijg je onbetrouwbare betrouwbaarheidsintervallen.

#survey designs opstellen (met volledige data, niet data_sub!). Er zijn twee verschillende weegfactoren (verschilt per variabele welke je nodig hebt,
# dus 2 designs aanmaken)
survey_design_ewGGD <- svydesign(ids = ~1,strata=data$PrimaireEenheid, weights=data$ewGGD, data=data) # ids = ~1 => Geen clustered sampling gebruikt
survey_design_ewCBSGGD <- svydesign(ids = ~1,strata=data$PrimaireEenheid, weights=data$ewCBSGGD, data=data)
# Initialiseer survey_design_sub met volledige data. Dit omdat later in de code survey_design_sub opgeroepen wordt, die anders niet gedefinieerd is.
survey_design_ewGGD_sub <- survey_design_ewGGD
survey_design_ewCBSGGD_sub <- survey_design_ewCBSGGD
# Initialiseer survey_design voor 18-64
survey_design_ewGGD_1864 <- subset(survey_design_ewGGD, lftcat3 != 3)
survey_design_ewCBSGGD_1864 <- subset(survey_design_ewCBSGGD, lftcat3 != 3)
# Initialiseer survey_design voor 65+
survey_design_ewGGD_65 <- subset(survey_design_ewGGD, lftcat3 == 3)
survey_design_ewCBSGGD_65 <- subset(survey_design_ewCBSGGD, lftcat3 == 3)



### leeg df voor cijfers
df <- data.frame(varcode = character(0), varlabel = character(0), waarde = integer(0), label = character(0),
                 n = integer(0), CIest = integer(0), CIlower = integer(0), CIupper = integer(0), n_unweighted = integer(0))
# Dataframe voor crossings
df_cross <- data.frame(varcode = character(0), varlabel = character(0), waarde = integer(0), label = character(0), crossing = character(0), crossing_var = character(0),
                       n_afgerond = integer(0), CIest = integer(0), CIlower = integer(0), CIupper = integer(0), n_unweighted = integer(0), chisq = integer(0))

# Dataframe voor subsets met daarin crossings
df_subset <- data.frame(subset_varcode = character(0), subset_waarde = integer(0), varcode = character(0), varlabel = character(0), waarde = integer(0), label = character(0), crossing = character(0), crossing_var = character(0),
                        n_afgerond = integer(0), CIest = integer(0), CIlower = integer(0), CIupper = integer(0), n_unweighted = integer(0), chisq = integer(0))

#inladen lijst met de variabelen voor de tabellenboeken, alle leeftijden
var_df <- read.csv("varlijst_weging.csv", sep=";", header = FALSE)
#inladen lijst met de variabelen voor de tabellenboeken, 18-64
var_df1864 <- read.csv("varlijst_weging1864.csv", sep=";", header = FALSE)
#inladen lijst met de variabelen voor de tabellenboeken, 65+
var_df65 <- read.csv("varlijst_weging65.csv", sep=";", header = FALSE)
# Lijst met alle vars voor de df zonder crossings/subsets
var_df_all <- rbind(var_df, var_df1864, var_df65)
# Lijst met crossings
var_cross <- read.csv("crosslijst.csv", sep=";", header = FALSE)
# Lijst met te subsetten variabelen
var_subset <- read.csv("subsetlijst.csv", sep=";", header = FALSE)
# Lijst met crossing voor in subset
var_cross_subset <- read.csv("crosslijst_subset.csv", sep=";", header = FALSE)
# Lijst met crossing voor in subset voor 65+
var_cross_subset_65 <- read.csv("crosslijst_subset65.csv", sep=";", header = FALSE)


# Functie voor afronden

afronden_agm <- function(n) {
  if (n < 200) {
     n <- round_any(n,10)
  } else if (n >= 200 & n < 1000) {
    n <-round_any(n,50)
  } else if (n <= 1000 & n < 2000) {
    n <-round_any(n,100)
  } else if (n <= 2000 & n < 10000) {
    n <-round_any(n,500)
  } else {
    n <-round_any(n,1000)
  }
}



####################
# Zonder crossings #
####################

for (varcode in var_df_all$V1){
  
  varlabels <- attr(data[[varcode]], "labels") # value labels
  varlabel <- attr(data[[varcode]], "label") # variable label

  
  # Maak table met juiste weegfactor. # Schrijf in beide gevallen survey design over naar survey_design, zodat voor de volgende loop maar 1 keer gedefinieerd hoeft te worden.
  if (var_df_all$V2[var_df_all$V1 == varcode] == "ewGGD") {
    survey_design <- survey_design_ewGGD_sub
  } else if (var_df_all$V2[var_df_all$V1 == varcode] == "ewCBSGGD"){
    survey_design <- survey_design_ewCBSGGD_sub
  }
  
  tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design)
  
  ct <- prop.table(tb) # ct bevat estimates als percentages
  
  for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
    
    val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
    
    # loopen met survey package wil niet op normale manier. Daarom onderstaande methode om betrouwbaarheidsintervallen te krijgen.
    string <- "svyciprop(~I({varcode}=={val}), survey_design, method='xlogit', na.rm=TRUE)"
    expr <- glue(string)
    ci <- eval(parse(text = expr)) # confidence intervals
    
    # Schrijf info weg naar dataframe
    idx <- nrow(df) + 1
    df[idx, 1] <- varcode # variabelenaam
    df[idx, 2] <- varlabel # variabelenlabel
    df[idx, 3] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
    df[idx, 4] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
    df[idx, 5] <- afronden_agm(round(tb[[j]]))  # Populatie n / gewogen n 
    df[idx, 6] <- ci[[1]] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
    df[idx, 7] <- attr(ci, "ci")[1] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
    df[idx, 8] <- attr(ci, "ci")[2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
    df[idx, 9] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
    
  }
}  

View(df)

# Tel aantal geldige antwoorden per vraag op.
df <- df %>%
  group_by(varcode) %>%
  mutate(n_groep = sum(n_unweighted)) %>%
  ungroup()  

#############
# CROSSINGS #
#############

### Variabelen voor alle leeftijden

for (varcode in var_df$V1){
  
  varlabels <- attr(data[[varcode]], "labels") # value labels
  varlabel <- attr(data[[varcode]], "label") # variable label
  
  # Maak table met juiste weegfactor
  
  if (var_df$V2[var_df$V1 == varcode] == "ewGGD") {
    survey_design <- survey_design_ewGGD_sub
  }
  else if (var_df$V2[var_df$V1 == varcode] == "ewCBSGGD"){
    survey_design <- survey_design_ewCBSGGD_sub
  }
  
  tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design) # tb bevat estimates van populatieaantallen
  
  ct <- prop.table(tb) # ct bevat estimates als percentages  
  
  for (crossvar in var_cross$V1){
    
    ifelse(crossvar == "MMWSA205", (survey_design <- survey_design_ewGGD), (survey_design <- survey_design))
    ## Bovenstaande omdat crossings met MMWSA205 moeten worden uitgevoerd met de ewGGD weging, onafhankelijk van welke weging bij de variabele hoort
    
    varlabels_cross <- attr(data[[crossvar]], "labels")
    varlabel_cross <- attr(data[[crossvar]], "label")
    
    string5 <- "svychisq(~{varcode}+{crossvar}, design = survey_design, statistic='Chisq')"
    expr <- glue(string5)
    chisqtest <- eval(parse(text = expr)) 
    
    for (j in 1:length(tb)){ # voor elke waarde van de variabele 
      
      val <- names(tb)[j] 
     
      string <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr <- glue(string)
      ci_cross <- eval(parse(text = expr))
      
      string2 <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr2 <- glue(string2)
      population_count <- eval(parse(text = expr2))
      
      for (k in 1:nrow(ci_cross)){
        
        idx <- nrow(df_cross) + 1
        df_cross[idx, 1] <- varcode # variabelenaam
        df_cross[idx, 2] <- varlabel # variabelenlabel
        df_cross[idx, 3] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_cross[idx, 4] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        df_cross[idx, 5] <- names(ci_cross)[1]  # Naam van crossing variabele
        df_cross[idx, 6] <- names(varlabels_cross[k]) # Level van crossing variabele
        df_cross[idx, 7] <- afronden_agm(population_count[k,3]) # Populatie n / gewogen n (afgrond naar AGM-normen)
        df_cross[idx, 8] <- ci_cross[k,2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 9] <- ci_cross[k,3] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 10] <- ci_cross[k,4] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 11] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]) & survey_design[["variables"]][crossvar] == as.numeric(ci_cross[k,1]), na.rm = TRUE) 
        df_cross[idx, 12] <- chisqtest$p.value
      }
    }
  }
}

View(df_cross)

### Variabelen alleen voor 18-64

data <- subset(data, lftcat3 != 3)
data_sub <- subset(data_sub, lftcat3 != 3)


for (varcode in var_df1864$V1){
  
  varlabels <- attr(data[[varcode]], "labels") # value labels
  varlabel <- attr(data[[varcode]], "label") # variable label
  
  # Maak table met juiste weegfactor
  
  if (var_df1864$V2[var_df1864$V1 == varcode] == "ewGGD") {
    survey_design <- survey_design_ewGGD_1864
  }
  else if (var_df1864$V2[var_df1864$V1 == varcode] == "ewCBSGGD"){
    survey_design <- survey_design_ewCBSGGD_1864
  }
  
  tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design) # tb bevat estimates van populatieaantallen
  
  ct <- prop.table(tb) # ct bevat estimates als percentages  
  
  for (crossvar in var_cross$V1){
    
    if ((dim(table(data_sub[[crossvar]]))) <=1) { #dit omdat sommige crossvars zoals lftcat2 nu maar 1 categorie hebben en dan de chisq niet werkt.
      next
    } else {
    
    ifelse(crossvar == "MMWSA205", (survey_design <- survey_design_ewGGD_1864), (survey_design <- survey_design))
    ## Bovenstaande omdat crossings met MMWSA205 moeten worden uitgevoerd met de ewGGD weging, onafhankelijk van welke weging bij de variabele hoort
    
    varlabels_cross <- attr(data[[crossvar]], "labels")
    varlabel_cross <- attr(data[[crossvar]], "label")
    
    string5 <- "svychisq(~{varcode}+{crossvar}, design = survey_design, statistic='Chisq')"
    expr <- glue(string5)
    chisqtest <- eval(parse(text = expr)) 
    
    }
    
    for (j in 1:length(tb)){ # voor elke waarde van de variabele 
      
      
      val <- names(tb)[j] 
      
      string <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr <- glue(string)
      ci_cross <- eval(parse(text = expr))
      
      string2 <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr2 <- glue(string2)
      population_count <- eval(parse(text = expr2))
      
      for (k in 1:nrow(ci_cross)){
        
        idx <- nrow(df_cross) + 1
        df_cross[idx, 1] <- varcode # variabelenaam
        df_cross[idx, 2] <- varlabel # variabelenlabel
        df_cross[idx, 3] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_cross[idx, 4] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        df_cross[idx, 5] <- names(ci_cross)[1]  # Naam van crossing variabele
        df_cross[idx, 6] <- names(varlabels_cross[k]) # Level van crossing variabele
        df_cross[idx, 7] <- afronden_agm(population_count[k,3]) # Populatie n / gewogen n (afgrond naar AGM-normen)
        df_cross[idx, 8] <- ci_cross[k,2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 9] <- ci_cross[k,3] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 10] <- ci_cross[k,4] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 11] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]) & survey_design[["variables"]][crossvar] == as.numeric(ci_cross[k,1]), na.rm = TRUE) 
        df_cross[idx, 12] <- chisqtest$p.value
      }
    }
  }
}

View(df_cross)

### Variabelen alleen voor 65+

data <- gm_2016
data_sub <- gm_2016
data <- subset(data, lftcat3 == 3)
data_sub <- subset(data_sub, lftcat3 == 3)


for (varcode in var_df65$V1){
  
  varlabels <- attr(data[[varcode]], "labels") # value labels
  varlabel <- attr(data[[varcode]], "label") # variable label
  
  # Maak table met juiste weegfactor
  
  if (var_df65$V2[var_df65$V1 == varcode] == "ewGGD") {
    survey_design <- survey_design_ewGGD_65
  }
  else if (var_df65$V2[var_df65$V1 == varcode] == "ewCBSGGD"){
    survey_design <- survey_design_ewCBSGGD_65
  }
  
  tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design) # tb bevat estimates van populatieaantallen
  
  ct <- prop.table(tb) # ct bevat estimates als percentages  
  
  for (crossvar in var_cross$V1){
    
    if ((dim(table(data_sub[[crossvar]]))) <=1) { #dit omdat sommige crossvars zoals lftcat2 nu maar 1 categorie hebben en dan de chisq niet werkt.
      next
    } else {
      
      varlabels_cross <- attr(data[[crossvar]], "labels")
      varlabel_cross <- attr(data[[crossvar]], "label")
      
      string5 <- "svychisq(~{varcode}+{crossvar}, design = survey_design, statistic='Chisq')"
      expr <- glue(string5)
      chisqtest <- eval(parse(text = expr)) 
      
    }
    
    for (j in 1:length(tb)){ # voor elke waarde van de variabele 
      
      
      val <- names(tb)[j] 
      
      string <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr <- glue(string)
      ci_cross <- eval(parse(text = expr))
      
      string2 <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr2 <- glue(string2)
      population_count <- eval(parse(text = expr2))
      
      for (k in 1:nrow(ci_cross)){
        
        idx <- nrow(df_cross) + 1
        df_cross[idx, 1] <- varcode # variabelenaam
        df_cross[idx, 2] <- varlabel # variabelenlabel
        df_cross[idx, 3] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_cross[idx, 4] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        df_cross[idx, 5] <- names(ci_cross)[1]  # Naam van crossing variabele
        df_cross[idx, 6] <- names(varlabels_cross[as.numeric(ci_cross[k,1])]) # Level van crossing variabele 
        df_cross[idx, 7] <- afronden_agm(population_count[k,3]) # Populatie n / gewogen n (afgrond naar AGM-normen)
        df_cross[idx, 8] <- ci_cross[k,2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 9] <- ci_cross[k,3] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 10] <- ci_cross[k,4] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 11] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]) & survey_design[["variables"]][crossvar] == as.numeric(ci_cross[k,1]), na.rm = TRUE) 
        df_cross[idx, 12] <- chisqtest$p.value
      }
    }
  }
}

View(df_cross)


# Tel aantal geldige antwoorden per crossing op.
df_cross <- df_cross %>%
  group_by(varcode, crossing, crossing_var) %>%
  mutate(n_groep = sum(n_unweighted)) %>%
  ungroup()  




###########
# SUBSETS #
########### 

### Variabelen voor alle leeftijden

data <- gm_2016
data_sub <- gm_2016

var_df_subset <- rbind(var_df, var_df1864)

# Variabalen alle leeftijden en 18-64

for (subsetvar in var_subset$V1){
  
  
  subset_value_nr <- (length(attr(data[[subsetvar]], "labels"))-1) # -1 omdat 9 ook een antwoordoptie is maar niet in de tabel moet verschijnen
  
  for (l in 1:subset_value_nr){
    
    stringdatasub <- "data_sub <- subset(data, {subsetvar} == l)" # subset de data, moet omslachtig omdat loopen van subsetvar anders niet gaat
    expr <- glue(stringdatasub)
    eval(parse(text = expr))
    
    stringweight1 <- "survey_design_ewGGD_sub <- subset(survey_design_ewGGD, {subsetvar} == l)" # subset allebei de weegfactors, moet omslachtig omdat loopen van subsetvar anders niet gaat
    expr2 <- glue(stringweight1)                
    eval(parse(text = expr2))
    
    stringweight2 <- "survey_design_ewCBSGGD_sub <- subset(survey_design_ewCBSGGD, {subsetvar} == l)" # subset allebei de weegfactors, moet omslachtig omdat loopen van subsetvar anders niet gaat
    expr3 <- glue(stringweight2)
    eval(parse(text = expr3))    
  
  
    for (varcode in var_df_subset$V1){
    
      varlabels <- attr(data[[varcode]], "labels") # value labels
      varlabel <- attr(data[[varcode]], "label") # variable label
    
    # Maak table met juiste weegfactor
      if (var_df_subset$V2[var_df_subset$V1 == varcode] == "ewGGD") {
      survey_design <- survey_design_ewGGD_sub
      }
      else if (var_df_subset$V2[var_df_subset$V1 == varcode] == "ewCBSGGD"){
      survey_design <- survey_design_ewCBSGGD_sub
      }
    
      tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design) # tb bevat estimates van populatieaantallen
    
      ct <- prop.table(tb) # ct bevat estimates als percentages
      
      for (crossvar in var_cross_subset$V1){
        
        if (crossvar == subsetvar) {
          next
        } else {
        varlabels_cross <- attr(data[[crossvar]], "labels")
        varlabel_cross <- attr(data[[crossvar]], "label")
        
        string5 <- "svychisq(~{varcode}+{crossvar}, design = survey_design, statistic='Chisq')"
        expr <- glue(string5)
        chisqtest <- eval(parse(text = expr))
        }
        
        for (j in 1:length(tb)){ # voor elke waarde van de variabele 
          
          val <- names(tb)[j] 
          
          string <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
          expr <- glue(string)
          ci_cross <- eval(parse(text = expr))
          
          string2 <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
          expr2 <- glue(string2)
          population_count <- eval(parse(text = expr2))
        
          for (k in 1:nrow(ci_cross)){
          
            idx <- nrow(df_subset) + 1
            df_subset[idx, 1] <- subsetvar # subsetvariabelenaam
            df_subset[idx, 2] <- l # waarde van subset-variabele
            df_subset[idx, 3] <- varcode # variabelenaam
            df_subset[idx, 4] <- varlabel # variabelenlabel
            df_subset[idx, 5] <- as.integer(names(tb)[j]) # numerieke waarde van huidige antwoordoptie
            df_subset[idx, 6] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
            df_subset[idx, 7] <- names(ci_cross)[1]  # Naam van crossing variabele
            df_subset[idx, 8] <- names(varlabels_cross[as.numeric(ci_cross[k,1])]) # Level van crossing variabele
            df_subset[idx, 9] <- afronden_agm(population_count[k,3]) # Populatie n / gewogen n 
            df_subset[idx, 10] <- ci_cross[k,2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
            df_subset[idx, 11] <- ci_cross[k,3] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
            df_subset[idx, 12] <- ci_cross[k,4] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
            df_subset[idx, 13] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]) & survey_design[["variables"]][crossvar] == as.numeric(ci_cross[k,1]), na.rm = TRUE) 
            df_subset[idx, 14] <- chisqtest$p.value
          }  
        }
      }
    }
  }
}


### Variabelen alleen voor 65+

data <- gm_2016
data_sub <- gm_2016
data <- subset(data, lftcat3 == 3)
data_sub <- subset(data_sub, lftcat3 == 3)


for (subsetvar in var_subset$V1){
  
  
  subset_value_nr <- (length(attr(data[[subsetvar]], "labels"))-1) # -1 omdat 9 ook een antwoordoptie is
  
  for (l in 1:subset_value_nr){

    stringdatasub <- "data_sub <- subset(data, {subsetvar} == l)" # subset de data, moet omslachtig omdat loopen van subsetvar anders niet gaat
    expr <- glue(stringdatasub)
    eval(parse(text = expr))
    
    stringweight1 <- "survey_design_ewGGD_sub <- subset(survey_design_ewGGD_65, {subsetvar} == l)" # subset allebei de weegfactors, moet omslachtig omdat loopen van subsetvar anders niet gaat
    expr2 <- glue(stringweight1)                
    eval(parse(text = expr2))
    
    stringweight2 <- "survey_design_ewCBSGGD_sub <- subset(survey_design_ewCBSGGD_65, {subsetvar} == l)" # subset allebei de weegfactors, moet omslachtig omdat loopen van subsetvar anders niet gaat
    expr3 <- glue(stringweight2)
    eval(parse(text = expr3))    
    
    for (varcode in var_df65$V1){
      
      varlabels <- attr(data[[varcode]], "labels") # value labels
      varlabel <- attr(data[[varcode]], "label") # variable label

      # Maak table met juiste weegfactor
      if (var_df65$V2[var_df65$V1 == varcode] == "ewGGD") {
        survey_design <- survey_design_ewGGD_sub
      }
      else if (var_df65$V2[var_df65$V1 == varcode] == "ewCBSGGD"){
        survey_design <- survey_design_ewCBSGGD_sub
      }
      
      tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design) # tb bevat estimates van populatieaantallen
      
      ct <- prop.table(tb) # ct bevat estimates als percentages
      
      for (crossvar in var_cross_subset_65$V1){
        
        if (crossvar == subsetvar) {
          next
        } else {
          varlabels_cross <- attr(data[[crossvar]], "labels")
          varlabel_cross <- attr(data[[crossvar]], "label")
          
          string5 <- "svychisq(~{varcode}+{crossvar}, design = survey_design, statistic='Chisq')"
          expr <- glue(string5)
          chisqtest <- eval(parse(text = expr))
          
        }
        
        for (j in 1:length(tb)){ # voor elke waarde van de variabele 
          
          val <- names(tb)[j] 
          
          string <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
          expr <- glue(string)
          ci_cross <- eval(parse(text = expr))
          
          string2 <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
          expr2 <- glue(string2)
          population_count <- eval(parse(text = expr2))
          
          for (k in 1:nrow(ci_cross)){
            
            idx <- nrow(df_subset) + 1
            df_subset[idx, 1] <- subsetvar # subsetvariabelenaam
            df_subset[idx, 2] <- l # waarde van subset-variabele
            df_subset[idx, 3] <- varcode # variabelenaam
            df_subset[idx, 4] <- varlabel # variabelenlabel
            df_subset[idx, 5] <- as.integer(names(tb)[j]) # numerieke waarde van huidige antwoordoptie
            df_subset[idx, 6] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
            df_subset[idx, 7] <- names(ci_cross)[1]  # Naam van crossing variabele
            df_subset[idx, 8] <- names(varlabels_cross[as.numeric(ci_cross[k,1])]) # Level van crossing variabele
            df_subset[idx, 9] <- afronden_agm(population_count[k,3]) # Populatie n / gewogen n 
            df_subset[idx, 10] <- ci_cross[k,2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
            df_subset[idx, 11] <- ci_cross[k,3] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
            df_subset[idx, 12] <- ci_cross[k,4] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
            df_subset[idx, 13] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]) & survey_design[["variables"]][crossvar] == as.numeric(ci_cross[k,1]), na.rm = TRUE) 
            df_subset[idx, 14] <- chisqtest$p.value
          }  
        }
      }
    }
  }
}



# Tel aantal geldige antwoorden per crossing per subset op.

df_subset <- df_subset %>%
  group_by(subset_varcode, varcode, crossing, crossing_var) %>%
  mutate(n_groep = sum(n_unweighted)) %>%
  ungroup()  




write.csv2(df, file = "df.csv", row.names = FALSE)
write.csv2(df_cross, file = "df_cross.csv", row.names = FALSE)
write.csv2(df_subset, file = "df_subset.csv", row.names = FALSE)



