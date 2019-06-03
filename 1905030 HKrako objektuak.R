## ---------------------------
##
## Script name: 1905030 HKrako objektuak
##
## Purpose of script: Hezkuntza Kongresurako PIEko datuakin sortutako elementuak
##
## Author: Juan
##
## Date Created: 2019-06-02
##
## Email: juan.abasolo@ehu.eus
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

## ---------------------------

## load up the packages we will need:  (uncomment as required)

suppressMessages(require(fmsb))
require(likert)
# source('')       # loads up all the packages we need

## ---------------------------
## Lagina

df.chaea <-  read.csv('data/raw/CHAEA/gordinik-CHAEA-zuritutaV2.csv')

# 'ordurako' funtzinoa, denpora-datuak denpora informazinotzat hartzeko
ordurako <- function(x)strptime(x, format = '%Y/%m/%e %k:%M:%S')
# funtzino hori erabili
df.chaea$Marca.temporal <- ordurako(df.chaea$Marca.temporal)
estiloak <- c('ekintzailea', 'hausnarkorra', 'teorikoa', 'pragmatikoa')

## Irudiak

# p.lagina.adina <- barplot(table(df.chaea$Edad), col = 1,
#         main = 'Banaketa adinaren arabera', las = 2)
# p.lagina.faku <- barplot(table(df.chaea$Faku), col = 1,
#         main = 'Banaketa fakultatearen arabera')
# p.lagina.genero <- barplot(table(df.chaea$Género), col = 1,
#         main = 'Banaketa generoaren arabera')
# p.lagina.h1 <- barplot(table(df.chaea$Lengua.materna), col = 1,
#         main = 'Banaketa ama hizkuntzaren arabera')

## CHAEA ---------------------------
##   

# Erantzunak baloratzeko datuak sartu -----------------------
neurriak.hartzekoa <- t(read.csv('data/raw/CHAEA/ITEMak_antolau.csv'))
# neurriak.hartzekoa

# Zenbatu denak --------------------------
for(i in 1:length(row.names(df.chaea))){
  # print(i)
  df.chaea$ACTIVO[i] <- sum(df.chaea[i,7:86]+neurriak.hartzekoa[2,] == 2)
  df.chaea$REFLEXIVO[i] <- sum(df.chaea[i,7:86]+neurriak.hartzekoa[3,] == 2)
  df.chaea$TEORICO[i] <- sum(df.chaea[i,7:86]+neurriak.hartzekoa[4,] == 2)
  df.chaea$PRAGMATICO[i] <- sum(df.chaea[i,7:86]+neurriak.hartzekoa[5,] == 2)
}

# Identifikau nagusia
# df.chaea$nagusi <-
for(i in 1:length(row.names(df.chaea))){
  # print(i)
  m <- max(df.chaea[i,89:92])
  df.chaea$nagusi[i] <- ifelse(m==df.chaea[i,89], 'ACTIVO', 
                               ifelse(m==df.chaea[1,90], 'REFLEXIVO',
                                      ifelse(m==df.chaea[1,91], 'TEORICO', 'PRAGMATICO')))
}

t.chaea.faku.nagusi <- table(df.chaea$Faku,df.chaea$nagusi)

# chisq.test(t.chaea.faku.nagusi)
# vcd::mosaic(vcd::structable(t.chaea.faku.nagusi), shade=TRUE, legend=TRUE, split_vertical = T)

## Datu interpretatuak ---------------------------
##   


## Activo
activo.q <- vector()
activo.q[df.chaea$ACTIVO < 7] <- 1
activo.q[df.chaea$ACTIVO >= 7 & df.chaea$ACTIVO < 9] <- 2
activo.q[df.chaea$ACTIVO >= 9 & df.chaea$ACTIVO < 13] <- 3
activo.q[df.chaea$ACTIVO >= 13 & df.chaea$ACTIVO < 15] <- 4
activo.q[df.chaea$ACTIVO >= 15] <- 5
df.chaea$activo.q <- as.factor(activo.q)

levels(df.chaea$activo.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")

## Reflexivo
reflexivo.q <- vector()
reflexivo.q[df.chaea$REFLEXIVO < 11] <- 1
reflexivo.q[df.chaea$REFLEXIVO >= 11 & df.chaea$REFLEXIVO < 14] <- 2
reflexivo.q[df.chaea$REFLEXIVO >= 14 & df.chaea$REFLEXIVO < 18] <- 3
reflexivo.q[df.chaea$REFLEXIVO >= 18 & df.chaea$REFLEXIVO < 20] <- 4
reflexivo.q[df.chaea$REFLEXIVO >= 20] <- 5
df.chaea$reflexivo.q <- as.factor(reflexivo.q)

levels(df.chaea$reflexivo.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")

## Téorico
teorico.q <- vector()
teorico.q[df.chaea$TEORICO < 7] <- 1
teorico.q[df.chaea$TEORICO >= 7 & df.chaea$TEORICO < 10] <- 2
teorico.q[df.chaea$TEORICO >= 10 & df.chaea$TEORICO < 14] <- 3
teorico.q[df.chaea$TEORICO >= 14 & df.chaea$TEORICO < 16] <- 4
teorico.q[df.chaea$TEORICO >= 16] <- 5
df.chaea$teorico.q <- as.factor(teorico.q)

levels(df.chaea$teorico.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")

## Pragmático
pragmatico.q <- vector()
pragmatico.q[df.chaea$PRAGMATICO < 9] <- 1
pragmatico.q[df.chaea$PRAGMATICO >= 9 & df.chaea$PRAGMATICO < 11] <- 2
pragmatico.q[df.chaea$PRAGMATICO >= 11 & df.chaea$PRAGMATICO < 14] <- 3
pragmatico.q[df.chaea$PRAGMATICO >= 14 & df.chaea$PRAGMATICO < 16] <- 4
pragmatico.q[df.chaea$PRAGMATICO >= 16] <- 5
df.chaea$pragmatico.q <- as.factor(pragmatico.q)

levels(df.chaea$pragmatico.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")

## Kuatridimentisonala ---------------------------
##   
### Ahalegina bost neurritan (ta ez 20 item)

df.chaea$activo.qn <- as.numeric(df.chaea$activo.q)
df.chaea$reflexivo.qn <- as.numeric(df.chaea$reflexivo.q)
df.chaea$teorico.qn <- as.numeric(df.chaea$teorico.q)
df.chaea$pragmatico.qn <- as.numeric(df.chaea$pragmatico.q)

starrerako <- aggregate(x = df.chaea[, c("activo.qn","reflexivo.qn", "teorico.qn", "pragmatico.qn")], 
                        by = list(df.chaea$Faku), 
                        FUN = mean)
rownames(starrerako) <- starrerako[,1]

starrerako <- rbind(rep(5,4) , rep(0,4) , starrerako[, 2:length(names(starrerako))])

# fmsb::radarchart(starrerako)

colors_border <- c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in <- c( rgb(0.2,0.5,0.5,0.3), rgb(0.8,0.2,0.5,0.3) , rgb(0.7,0.5,0.1,0.3) )
fmsb::radarchart( starrerako, axistype=1 ,
                  #custom polygon
                  pcol=colors_border ,
                  pfcol=colors_in ,
                  plwd=4 ,
                  plty=1,
                  #custom the grid
                  cglcol="grey", cglty=1,
                  axislabcol="gray40",
                  caxislabels=levels(df.chaea$activo.q),
                  calcex = 0.7,
                  cglwd=0.8,
                  #custom labels
                  vlcex=0.9,
                  vlabels = c("Activo","Reflexivo","Teórico", "Pragmático"),
                  # Izenburua
                  title = 'Estilos de aprendizaje y facultades'
)

legend(x = 1, y = 1,
       legend = paste(rownames(starrerako[-c(1,2),]), table(df.chaea$Faku), sep = ' n=' ),
       bty = "n",
       pch=20 ,
       col=colors_border,
       text.col = "black",
       cex=1.1,
       pt.cex=3)

## Unidimentsionala ---------------------------
##   
ji.chaea.faku.nagusi.mono <- suppressWarnings(chisq.test(table(df.chaea$Faku, 
                                                               df.chaea$nagusi)))
# vcd::mosaic(vcd::structable(df.chaea$Faku~df.chaea$nagusi), shade=TRUE, legend=TRUE)

## Monte Carlo 100000 erabilita
# chisq.test(table(df.chaea$Faku, 
#                  df.chaea$nagusi), simulate.p.value = T, B=100000)
## Azalpena
taula.p.chaea <- prop.table(table(df.chaea$Faku, df.chaea$nagusi))
# barplot(apply(taula.p.chaea, 2, function(x){x*100/sum(x,na.rm=T)}), 
#         beside = F, 
#         col = c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9)),
#         legend.text = levels(df.chaea$Faku), 
#         names.arg = c("Activo","Reflexivo","Teórico", "Pragmático"))


## EMSI ---------------------------
##   

df.emsi <- read.csv('data/raw/EMSI/gordinik-EMSI-zurituta.csv')

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

## Likert objektuak sortu

## Gutztiak
lik.emsi <- likert::likert(df.itemak.emsi)
lik.emsi.faku <- likert::likert(df.itemak.emsi, grouping = df.emsi$Faku)

## homozedestizitatea
e.l <- c()
levene.pbalioak <- c()
for (i in 1:length(names(df.itemak.emsi))){
  e.l <- car::leveneTest(as.numeric(df.itemak.emsi[,i])~df.emsi$Faku, center = 'median')
  levene.pbalioak[i] <- round(e.l$`Pr(>F)`[1], 3)
}

knitr::kable(data.frame(itemak = emsi.itemak, 
                        p.balioak = levene.pbalioak, 
                        ebidentziarik = ifelse(levene.pbalioak < 0.05, 'bai', 'ez')), row.names = F)
## Chi taulak ---------------------------
##   
e.k.ch <- c()
e.k.pb <- c()
for (i in 1:length(names(df.itemak.emsi))){
  e.k <- kruskal.test(df.itemak.emsi[,i]~df.emsi$Faku)
  e.k.ch[i] <- e.k$statistic
  e.k.pb[i] <- e.k$p.value
}

df.k <- data.frame(itema = emsi.itemak,
                   chikarratu = round(e.k.ch, 3),
                   'p-balioa' = round(e.k.pb, 3),
                   ad.garritasuna = ifelse(e.k.pb > 0.05, '',
                                           ifelse(e.k.pb > 0.01, '*',
                                                  ifelse(e.k.pb > 0.001, '**', '***'
                                                  ))))

knitr::kable(df.k)

## Chi karratu adierazgarri emonikoekin kontrasteak
e.p.w.t1 <- pairwise.wilcox.test(x = as.numeric(df.itemak.emsi[, 5]), 
                                g = df.emsi$Faku, p.adjust.method = "holm")
knitr::kable(round(e.p.w.t1$p.value, 3), caption = paste(emsi.itemak[5], 'itemean alde adierazgarria'))

e.p.w.t2 <- pairwise.wilcox.test(x = as.numeric(df.itemak.emsi[, 9]), 
                                g = df.emsi$Faku, p.adjust.method = "holm")
knitr::kable(round(e.p.w.t2$p.value, 3), caption = paste(emsi.itemak[9], 'itemean alde adierazgarria'))

e.p.w.t3 <- pairwise.wilcox.test(x = as.numeric(df.itemak.emsi[, 14]), 
                                g = df.emsi$Faku, p.adjust.method = "holm")
knitr::kable(round(e.p.w.t3$p.value, 3), caption = paste(emsi.itemak[14], 'itemean alde adierazgarria'))

## Likert irudiak ---------------------------
##   
# Motivación intrinseca ===================
lik.emsi.mintrinseca <- likert::likert(df.itemak.emsi[, mintrinseca])
# summary(lik.emsi.mintrinseca)
# str(lik.emsi.mintrinseca)
plot(lik.emsi.mintrinseca)


# Fakultadeka
lik.emsi.mintrinseca.faku <- likert::likert(df.itemak.emsi[, mintrinseca], grouping = df.emsi$Faku)
# summary(lik.emsi.mintrinseca.faku)
# str(lik.emsi.mintrinseca.faku)
plot(lik.emsi.mintrinseca.faku)

plot(lik.emsi.mintrinseca.faku, type = 'density')

# Regulacion identificada ==================
lik.emsi.mregulacionidentificada <- likert::likert(df.itemak.emsi[, mregulacionidentificada])
# summary(lik.emsi.mregulacionidentificada)
# str(lik.emsi.mregulacionidentificada)
plot(lik.emsi.mregulacionidentificada)

# Fakultadeka
lik.emsi.mregulacionidentificada.faku <- likert::likert(df.itemak.emsi[, mregulacionidentificada], grouping = df.emsi$Faku)
# summary(lik.emsi.mregulacionidentificada.faku)
# str(lik.emsi.mregulacionidentificada.faku)
plot(lik.emsi.mregulacionidentificada.faku)

plot(lik.emsi.mregulacionidentificada.faku, type = 'density')

# Regulación externa =======================
lik.emsi.mregulacionexterna <- likert::likert(df.itemak.emsi[, mregulacionexterna])
# summary(lik.emsi.mregulacionexterna)
# str(lik.emsi.mregulacionexterna)
plot(lik.emsi.mregulacionexterna)

# Fakultadeka
lik.emsi.mregulacionexterna.faku <- likert::likert(df.itemak.emsi[, mregulacionexterna], grouping = df.emsi$Faku)
# summary(lik.emsi.mregulacionexterna.faku)
# str(lik.emsi.mregulacionexterna.faku)
plot(lik.emsi.mregulacionexterna.faku)

plot(lik.emsi.mregulacionexterna.faku, type = 'density')

# Amotibazioa ===============================
lik.emsi.mamotivacion <- likert::likert(df.itemak.emsi[, mamotivacion])
# summary(lik.emsi.mamotivacion)
# str(lik.emsi.mamotivacion)
plot(lik.emsi.mamotivacion)

# Fakultadeka
lik.emsi.mamotivacion.faku <- likert::likert(df.itemak.emsi[, mamotivacion], grouping = df.emsi$Faku)
# summary(lik.emsi.mamotivacion.faku)
# str(lik.emsi.mamotivacion.faku)
plot(lik.emsi.mamotivacion.faku)

plot(lik.emsi.mamotivacion.faku, type = "density")

