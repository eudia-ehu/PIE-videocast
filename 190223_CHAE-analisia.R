# Garbitu
rm(list = ls())

# Erantzunak sartu ---------------------------------------
# Data frame batera sartu
df.chaea <-  read.csv('data/raw/CHAEA/gordinik-CHAEA-zuritutaV2.csv')

# 'ordurako' funtzinoa, denpora-datuak denpora informazinotzat hartzeko
ordurako <- function(x)strptime(x, format = '%Y/%m/%e %k:%M:%S')
# funtzino hori erabili
df.chaea$Marca.temporal <- ordurako(df.chaea$Marca.temporal)

# Aldagaien izenak zerrendatu
aldagaien.izenak <- names(df.chaea)
str(aldagaien.izenak)

#' 
#' Lehenengo 6ak eta azkana informatzaileak sailkatzeko dira
#' 
#' `c(1:6, length(aldagaien.izenak)`
#' 
#' Eta beste guztiak dira CHAEAko 80 itemak dira
#' 
#' 'c(7:86)'
#' 

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


# AZTERKETA ITSUKA ======
df.chaea[1:5,7:10]

print(
  cat(' Estilo ACTIVOn \t',
        sum(df.chaea[1,7:86]+neurriak.hartzekoa[2,] == 2),
        'puntu hartu ditu \n',
      'Estilo REFLEXIVOn \t',
      sum(df.chaea[1,7:86]+neurriak.hartzekoa[3,] == 2),
      'puntu hartu ditu \n',
      'Estilo TEORICOn \t',
      sum(df.chaea[1,7:86]+neurriak.hartzekoa[4,] == 2),
      'puntu hartu ditu \n',
      'Estilo PRAGMATICOn \t',
      sum(df.chaea[1,7:86]+neurriak.hartzekoa[5,] == 2),
      'puntu hartu ditu \n'
      
)
)

t.1 <- table(df.chaea$Faku,df.chaea$nagusi)
t.1
chisq.test(t.1)
barplot(t.1, beside = T)
# mosaicplot(t.1, shade = 1)
vcd::mosaic(vcd::structable(df.chaea$Faku~df.chaea$nagusi), shade=TRUE, legend=TRUE, split_vertical = T)


t.2 <- table(df.chaea$Lengua.materna,df.chaea$nagusi)
t.2
chisq.test(t.2)
# mosaicplot(t.2, shade = 1)
barplot(t.2, beside = T)
vcd::mosaic(vcd::structable(df.chaea$Lengua.materna~df.chaea$nagusi), shade=TRUE, legend=TRUE)


t.3 <- table(df.chaea$Curso,df.chaea$nagusi)
t.3
chisq.test(t.3)
# mosaicplot(t.3, shade = 1)
barplot(t.3, beside = T)
vcd::mosaic(vcd::structable(df.chaea$Curso~df.chaea$nagusi), shade=TRUE, legend=TRUE)

# Kontuan euki beharrekoak (aldagaiak kontrolateko)
vcd::mosaic(vcd::structable(df.chaea$Faku ~ df.chaea$Curso), shade=TRUE, legend=TRUE)
vcd::mosaic(vcd::structable(df.chaea$Faku ~ df.chaea$Lengua.materna), shade=TRUE, legend=TRUE)

#'
#' # Emaitzaren lehenengo hurrerapena
#' 
#' Hemen aztertzen da emaitzen banaketa, ea normaltasunaren barruan ulertu behar denentz 
#' 
#' 

plotadentsi <- function(aldagaia, zer){
  par(mfrow = c(1,2))
  x <- aldagaia
  y <- shapiro.test(aldagaia)
  h <- hist(x, breaks=10, 
            col="azure2", 
            # prob = T,
            main=paste('Distribucion de estilo', zer),
            sub = paste('W = ', round(y$statistic,3), 'p-balioa', round(y$p.value, 3)),
            ylab = 'Maiztasuna',
            xlab=paste(y$method)
  )
  # lines(density(aldagaia), # density plot
  #       lwd = 1, # thickness of line
  #       col = 1)
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col = "darkred", lwd = 2) 
  qqnorm(aldagaia, pch = 19, col = "gray50")
  qqline(aldagaia, col = 2, lwd = 2)
  # nortest::lillie.test(aldagaia)
}

plotadentsi(df.chaea$ACTIVO, 'activo')
plotadentsi(df.chaea$REFLEXIVO, 'reflexivo')
plotadentsi(df.chaea$TEORICO, 'teorico')
plotadentsi(df.chaea$PRAGMATICO, 'pragmatico')

# Datuak eskalatuta
aztertu <- scale(df.chaea$TEORICO)
shapiro.test(aztertu)
ks.test(aztertu, pnorm, mean(aztertu), sd(aztertu))

# Pareak desegin
aztertu <- df.chaea$TEORICO
aztertu <- rank(aztertu, ties.method = 'random')
shapiro.test(aztertu) # Honek dino ez dala normala
ks.test(aztertu, pnorm, mean(aztertu), sd(aztertu)) # Honek ez dino normala ez danik
plot(density(aztertu))
plot(density(df.chaea$TEORICO))

#' 
#' # Datuak interpretatzeko baremo orokorra
#' 
#' 
#' |            | 10%         | 20%   | 40%      | 20%   | 10%        |
#' | ---------- | ----------- | ----- | -------- | ----- | ---------- |
#' |            | Preferencia |       |          |       |            |
#' |            | Muy   Baja  | Baja  | Moderada | Alta  | Muy   alta |
#' | Activo     | 0-6         | 7-8   | 9-12     | 13-14 | 15-20      |
#' | Reflexivo  | 0-10        | 11-13 | 14-17    | 18-19 | 20         |
#' | Teórico    | 0-6         | 7-9   | 10-13    | 14-15 | 16-20      |
#' | Pragmático | 0-8         | 9-10  | 11-13    | 14-15 | 16-20      |
#' 
#' p. 114
#' 
#'
#


# Activo
activo.q <- vector()
activo.q[df.chaea$ACTIVO < 7] <- 1
activo.q[df.chaea$ACTIVO >= 7 & df.chaea$ACTIVO < 9] <- 2
activo.q[df.chaea$ACTIVO >= 9 & df.chaea$ACTIVO < 13] <- 3
activo.q[df.chaea$ACTIVO >= 13 & df.chaea$ACTIVO < 15] <- 4
activo.q[df.chaea$ACTIVO >= 15] <- 5
df.chaea$activo.q <- as.factor(activo.q)

levels(df.chaea$activo.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")
table(df.chaea$activo.q)


# Reflexivo
reflexivo.q <- vector()
reflexivo.q[df.chaea$REFLEXIVO < 11] <- 1
reflexivo.q[df.chaea$REFLEXIVO >= 11 & df.chaea$REFLEXIVO < 14] <- 2
reflexivo.q[df.chaea$REFLEXIVO >= 14 & df.chaea$REFLEXIVO < 18] <- 3
reflexivo.q[df.chaea$REFLEXIVO >= 18 & df.chaea$REFLEXIVO < 20] <- 4
reflexivo.q[df.chaea$REFLEXIVO >= 20] <- 5
df.chaea$reflexivo.q <- as.factor(reflexivo.q)

levels(df.chaea$reflexivo.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")
table(df.chaea$reflexivo.q)

# Téorico
teorico.q <- vector()
teorico.q[df.chaea$TEORICO < 7] <- 1
teorico.q[df.chaea$TEORICO >= 7 & df.chaea$TEORICO < 10] <- 2
teorico.q[df.chaea$TEORICO >= 10 & df.chaea$TEORICO < 14] <- 3
teorico.q[df.chaea$TEORICO >= 14 & df.chaea$TEORICO < 16] <- 4
teorico.q[df.chaea$TEORICO >= 16] <- 5
df.chaea$teorico.q <- as.factor(teorico.q)

levels(df.chaea$teorico.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")
table(df.chaea$teorico.q)


# Pragmático
pragmatico.q <- vector()
pragmatico.q[df.chaea$PRAGMATICO < 9] <- 1
pragmatico.q[df.chaea$PRAGMATICO >= 9 & df.chaea$PRAGMATICO < 11] <- 2
pragmatico.q[df.chaea$PRAGMATICO >= 11 & df.chaea$PRAGMATICO < 14] <- 3
pragmatico.q[df.chaea$PRAGMATICO >= 14 & df.chaea$PRAGMATICO < 16] <- 4
pragmatico.q[df.chaea$PRAGMATICO >= 16] <- 5
df.chaea$pragmatico.q <- as.factor(pragmatico.q)

levels(df.chaea$pragmatico.q) <- c("Muy baja", "Baja", "Moderada", "Alta", "Muy alta")
table(df.chaea$pragmatico.q)


mosaicplot(df.chaea$activo.q~df.chaea$reflexivo.q)

#' Aurreko horrek dio bagoela kasuren bat edo bat oso altu ematen duena activo eta reflexivo mailetan
#' 

mosaicplot(df.chaea$teorico.q~df.chaea$pragmatico.q)
#'
#' Eta goiko horrek erakusten du baudela bat edo batzuk pragmatikoan eta teorikoan oso altu puntuatzen dutenak
#' 
vcd::mosaic(vcd::structable(df.chaea$activo.q~df.chaea$reflexivo.q), shade=TRUE, legend=TRUE)
vcd::mosaic(vcd::structable(df.chaea$teorico.q~df.chaea$reflexivo.q), shade=TRUE, legend=TRUE)
vcd::mosaic(vcd::structable(df.chaea$activo.q~df.chaea$pragmatico.q), shade=TRUE, legend=TRUE)
vcd::mosaic(vcd::structable(df.chaea$teorico.q~df.chaea$pragmatico.q), shade=TRUE, legend=TRUE)

par(mfrow=c(2,2))
mosaicplot(df.chaea$activo.q~df.chaea$reflexivo.q)
mosaicplot(df.chaea$teorico.q~df.chaea$reflexivo.q)
mosaicplot(df.chaea$activo.q~df.chaea$pragmatico.q)
mosaicplot(df.chaea$teorico.q~df.chaea$pragmatico.q)
par(mfrow=c(1,1))

#' Emoten dau beste era bateko irudia sortu behar dala, garapena dimentsino guztietan erakusten daben era bat
#psych::spider(y=c("ACTIVO", "REFLEXIVO","TEORICO","PRAGMATICO"), x=5, df.chaea, overlay = T)
#str(df.chaea$ACTIVO)

# fmsb::radarchart(df.chaea[1:5,c("ACTIVO", "REFLEXIVO", "PRAGMATICO", "TEORICO")], axistype=1, 
#                  
#                  #custom polygon
#                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=1 , 
#                  
#                  #custom the grid
#                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#                  
#                  #custom labels
#                  vlcex=0.8 )

# Beste ikuskera batzuk
x <- df.chaea$PRAGMATICO

hist(x, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE#, # show densities instead of frequencies
     # xlab = "temp",
     # main = "Beaver #1"
     )
lines(density(x), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


plot(density(x), # density plot
     lwd = 2, # thickness of line
     col = 1)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col = 2, lwd = 1) 

# Radarchart
starrerako <- df.chaea[, c("ACTIVO","REFLEXIVO","TEORICO", "PRAGMATICO")]
starrerako <- rbind(rep(20,4) , rep(0,4) , starrerako)

fmsb::radarchart(starrerako[1:5,])
legend(x = 0.7, y = 1, 
       legend = rownames(starrerako[1:5,][-c(1,2),]), 
       bty = "n", 
       pch=20 , 
       col=1:(5-2), 
       text.col = "black",
       cex=1.2, 
       pt.cex=3)



# Tuneauta
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
fmsb::radarchart( starrerako[1:5,]  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(starrerako[1:5,][-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# Esperimentazinoian
starrerako <- aggregate(x = df.chaea[, c("ACTIVO","REFLEXIVO","TEORICO", "PRAGMATICO")], 
                        by = list(df.chaea$Nombre.de.la.titulación), 
                        FUN = mean)
rownames(starrerako) <- starrerako[,1]

starrerako <- rbind(rep(20,4) , rep(0,4) , starrerako[, 2:length(names(starrerako))])
fmsb::radarchart(starrerako)

legend(x = 0.7, y = 1, 
       legend = rownames(starrerako[-c(1,2),]), 
       bty = "n", 
       pch=20 , 
       col=1:(length(row.names(starrerako))-2), 
       text.col = "black",
       cex=1.2, 
       pt.cex=3)

# Fakultadeka

starrerako <- aggregate(x = df.chaea[, c("ACTIVO","REFLEXIVO","TEORICO", "PRAGMATICO")], 
                        by = list(df.chaea$Faku), 
                        FUN = mean)
rownames(starrerako) <- starrerako[,1]

starrerako <- rbind(rep(20,4) , rep(0,4) , starrerako[, 2:length(names(starrerako))])
fmsb::radarchart(starrerako)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
fmsb::radarchart( starrerako[1:5,]  , axistype=1 , 
                  #custom polygon
                  pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                  #custom labels
                  vlcex=0.8 
)

legend(x = 0.7, y = 1, 
       legend = rownames(starrerako[-c(1,2),]), 
       bty = "n", 
       pch=20 , 
       col=colors_in, 
       text.col = "black",
       cex=1.2, 
       pt.cex=3)


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

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
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
                  vlabels = c("Ekintzailea","Hausnarkorra","Teorikoa", "Pragmatikoa"),
                  # Izenburua
                  title = 'Ikas-estiloak fakultadeen arabera'
)

# legend(x = 1, y = 1, 
#        legend = rownames(starrerako[-c(1,2),]), 
#        bty = "n", 
#        pch=20 , 
#        col=colors_in, 
#        text.col = "black",
#        cex=1.2, 
#        pt.cex=3)
# 
# table(df.chaea$Faku)


legend(x = 1, y = 1, 
       legend = paste(rownames(starrerako[-c(1,2),]), table(df.chaea$Faku), sep = ' n=' ), 
       bty = "n", 
       pch=20 , 
       col=colors_in, 
       text.col = "black",
       cex=1.2, 
       pt.cex=3)


barplot(table(df.chaea$Faku, df.chaea$nagusi), 
        beside = T, 
        col = c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9)),
        legend.text = levels(df.chaea$Faku), 
        names.arg = c("Ekintzailea","Hausnarkorra","Teorikoa", "Pragmatikoa"))

chisq.test(table(df.chaea$Faku, df.chaea$nagusi))

vcd::mosaic(vcd::structable(df.chaea$Faku~df.chaea$nagusi), shade=TRUE, legend=TRUE)

# Exprimir:
esprimiduta <- df.chaea[-which(df.chaea$Faku=='ZeT'),]
chisq.test(table(as.character(esprimiduta$Faku), esprimiduta$nagusi))
vcd::mosaic(vcd::structable(as.character(esprimiduta$Faku)~esprimiduta$nagusi), shade=TRUE, legend=TRUE)

# Esprimiduta II (HH+LH soilik)
#' Honek balio dau HH eta LHko ikasleak alderatzeko.
#' 
esprimiduta <- df.chaea[which(df.chaea$Faku=='BHF'),]
chisq.test(table(as.character(esprimiduta$Nombre.de.la.titulación), esprimiduta$nagusi))
vcd::mosaic(vcd::structable(as.character(esprimiduta$Nombre.de.la.titulación)~esprimiduta$nagusi), shade=TRUE, legend=TRUE)

# Azalpena
data <- prop.table(table(df.chaea$Faku, df.chaea$nagusi))
barplot(apply(data, 2, function(x){x*100/sum(x,na.rm=T)}), 
        beside = F, 
        col = c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9)),
        legend.text = levels(df.chaea$Faku), 
        names.arg = c("Ekintzailea","Hausnarkorra","Teorikoa", "Pragmatikoa"))

