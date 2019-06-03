# EMSI galdetegiko datuak sartu, antolatu eta analizatu
rm(list = ls())
# Hurrengu bihar da barplot-ak egiteko
library(grid) 

df.emsi <- read.csv('data/raw/EMSI/gordinik-EMSI-zurituta.csv')

str(df.emsi)

mintrinseca <- c(1, 5, 9, 13)
mregulacionidentificada <- c(2, 6, 10, 14)
mregulacionexterna <- c(3, 7, 11, 15)
mamotivacion <- c(4, 8, 12, 16)

# Aldagaien izenak egokitu (read.csv-k izen arraroak eragiten ditualako)
df.itemak.emsi <- df.emsi[, 7:22]
emsi.itemak <- read.csv('data/raw/EMSI/gordinik-EMSI-zurituta.csv', header = F, stringsAsFactors = F)[1,7:22]
emsi.itemak <- as.matrix(emsi.itemak)
emsi.itemak <-  as.character(emsi.itemak[1,])
names(df.itemak.emsi) <- emsi.itemak

# Likert objektuak sortu

# Gutztiak
lik.emsi <- likert::likert(df.itemak.emsi)
lik.emsi.faku <- likert::likert(df.itemak.emsi, grouping = df.emsi$Faku)

# Motivación intrinseca ===================
lik.emsi.mintrinseca <- likert::likert(df.itemak.emsi[, mintrinseca])
summary(lik.emsi.mintrinseca)
str(lik.emsi.mintrinseca)
plot(lik.emsi.mintrinseca)
# Fakultadeka
lik.emsi.mintrinseca.faku <- likert::likert(df.itemak.emsi[, mintrinseca], grouping = df.emsi$Faku)
summary(lik.emsi.mintrinseca.faku)
str(lik.emsi.mintrinseca.faku)
plot(lik.emsi.mintrinseca.faku)



# Regulacion identificada ==================
lik.emsi.mregulacionidentificada <- likert::likert(df.itemak.emsi[, mregulacionidentificada])
summary(lik.emsi.mregulacionidentificada)
str(lik.emsi.mregulacionidentificada)
plot(lik.emsi.mregulacionidentificada)
# Fakultadeka
lik.emsi.mregulacionidentificada.faku <- likert::likert(df.itemak.emsi[, mregulacionidentificada], grouping = df.emsi$Faku)
summary(lik.emsi.mregulacionidentificada.faku)
str(lik.emsi.mregulacionidentificada.faku)
plot(lik.emsi.mregulacionidentificada.faku)

# Regulación externa =======================
lik.emsi.mregulacionexterna <- likert::likert(df.itemak.emsi[, mregulacionexterna])
summary(lik.emsi.mregulacionexterna)
str(lik.emsi.mregulacionexterna)
plot(lik.emsi.mregulacionexterna)
# Fakultadeka
lik.emsi.mregulacionexterna.faku <- likert::likert(df.itemak.emsi[, mregulacionexterna], grouping = df.emsi$Faku)
summary(lik.emsi.mregulacionexterna.faku)
str(lik.emsi.mregulacionexterna.faku)
plot(lik.emsi.mregulacionexterna.faku)

# Amotibazioa ===============================
lik.emsi.mamotivacion <- likert::likert(df.itemak.emsi[, mamotivacion])
summary(lik.emsi.mamotivacion)
str(lik.emsi.mamotivacion)
plot(lik.emsi.mamotivacion)
plot(lik.emsi.mamotivacion, type = "heat")
plot(lik.emsi.mamotivacion.faku, type = "bar", include.histogram = TRUE)
# Fakultadeka
lik.emsi.mamotivacion.faku <- likert::likert(df.itemak.emsi[, mamotivacion], grouping = df.emsi$Faku)
summary(lik.emsi.mamotivacion.faku)
str(lik.emsi.mamotivacion.faku)
plot(lik.emsi.mamotivacion.faku)
plot(lik.emsi.mamotivacion.faku, type = "density")


## U Mann-Wtihney-Wilcoxon
# Convertimos las variables en numéricas
dfcomp <- lapply( df.emsi[, 7:22 ], as.numeric )
compU <- function( varnun, vfactor){
  wt <- wilcox.test( varnun ~ vfactor, alternative = "greater" )
  res <- data.frame( wt$statistic, wt$p.value, sig = "Ez" )
  names( res ) <- c("U Mann-Whitney-Wilcoxon", "p-balioa", "Adi.")
  # Significacion
  if( res$p.value < 0.05) res$Sig.<- 'Bai'
  return( res )
}

tab <- lapply( dfcomp, compU, vfactor = df.emsi$Generoa )
# str( tab )

tabla <- ldply( tab )
colnames( tabla )[1] <- "Pregunta"

kable( tabla , digits = 2, caption ="Comparaciones de los ítems por Sexo"
)





