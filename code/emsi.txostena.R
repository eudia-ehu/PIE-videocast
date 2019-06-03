# EMSIko datuak sartu eta antolatu
library(likert)
# Aurrekoa garbitu
rm(list = ls())

# Sartu
df.emsi <- read.csv('data/raw/Aintzanek_PIE/datuak/gordinik-EMSI-zurituta.csv')

#Egokitu
# Irakurgarrira lantzeko modukora
ordurako <- function(x)strptime(x, format = '%Y/%m/%e %k:%M:%S')
df.emsi$Marca.temporal <- ordurako(df.emsi$Marca.temporal)

# Aztrtu
str(df.emsi)

# Datuen egitura, emsi
names(df.emsi)
str(df.emsi[2:6])

# Titulazioen araberaku
knitr::kable(sort(table(df.emsi$Titulazioa)))
barplot(sort(table(df.emsi$Titulazioa), 
             decreasing = T), 
        main = "Informatzaileak",
        # sub = "Karreraren arabera (zenbaki absolutuak)",
        col = 1,
        las = 2, 
        cex.axis=1, 
        cex.names=0.7)



# Azterketa -------------------------------------------
itemak.emsi <- df.emsi[, 7:22]


# Izenen arazoa konpondu ==============================
# FUNTZIOA EGIN ####

zuzentzeko <- names(itemak.emsi)
# zuzentzeko
zuzentzeko <- gsub('X...', '', zuzentzeko)
zuzentzeko <- gsub('\\.\\.', ', ', zuzentzeko)
zuzentzeko <- gsub('\\.', ' ', zuzentzeko)
zuzentzeko <- trimws(zuzentzeko, which = 'left')
# zuzentzeko
names(itemak.emsi) <- zuzentzeko

knitr::kable(itemak.emsi[1:6, 1:3])

#'
#' # Erantzunen azterketa(rako)
#' 
#' Lehenengo begirada orokorra
#' 

# Likert obejektua sortu
lik.emsi <- likert::likert(itemak.emsi)

summary(lik.emsi)
plot(lik.emsi, type = "bar", centered = TRUE)
plot(lik.emsi, type = 'heat')

# Taldekatzea ======================================


#'
#' ## Taldekatzea
#' 
#' Dauden aukeretatik lehenengo hurrerapen orokor bat banan banan
#' 


#' 
#' ### Karreraren arabera
#' 
# knitr::kable(sort(table(df.emsi$Titulazioa)))
barplot(table(df.emsi$Titulazioa), col = heat.colors(length(table(df.emsi$Titulazioa))), las = 2)
lik.tit.emsi <- likert(df.emsi[ , 7:10], grouping = df.emsi$Titulazioa)
plot(lik.tit.emsi, type = "bar", centered = TRUE)
# summary(lik.tit.emsi)

#' Emoten dau iragazi behar dirala kateogria batzuk. Akaso, kendu be bai (aztertu barplota)

#'
#'  ### Kurtsoaren arabera
#' 
#' 

knitr::kable(sort(table(df.emsi$Kurtsoa)))
barplot(table(df.emsi$Kurtsoa), col = 1)
lik.kurtsu.emsi <- likert(df.emsi[ , 7:10], grouping = df.emsi$Kurtsoa)
plot(lik.kurtsu.emsi, type = "bar", centered = TRUE)

#' 
#' ### Generoaren arabera
#' 
knitr::kable(sort(table(df.emsi$Género)))
barplot(table(df.emsi$Género), col = 2)
lik.sex.emsi <- likert(df.emsi[ , 7:10], grouping = df.emsi$Género)
plot(lik.sex.emsi, type = "bar", centered = TRUE)

#' 
#' ### H1 arabera
#' 
knitr::kable(sort(table(df.emsi$Lengua.materna)))
barplot(table(df.emsi$Lengua.materna), col = 'darkred')
lik.hiz.emsi <- likert(df.emsi[ , 7:10], grouping = df.emsi$Lengua.materna)
plot(lik.hiz.emsi, type = "bar", centered = TRUE)

#' 
#' ### Adinaren arabera
#' 
barplot(table(df.emsi$Adina), col = 'white')
# Taldekatzeak egin behar dira adinakin
# 20 baino gazteagoak eta 21tik gora



# x <- marrazkitxuk.emsi[ , 6, drop = F]
# names(x) <- "Participo por interés"
# obejektua.item.bakarra <- likert::likert(x)

# Fakultadeka

# IZENAK ARTIKULUAN BEGITTU
# Hezkuntza = Lehen Hezkuntza, Haur Hezkuntza
# Komunikazioa = Kazetaritza, Ikusentzunezko Komunikazioa, Harreman publikoak
# Zientzia eta Teknologia = Matematika, Fisika, Ingeniería Electrónica

