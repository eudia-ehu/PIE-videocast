# Informatzaileak eta haien banaketa:

rm(list = ls())
df.chaea <-  read.csv('data/raw/CHAEA/gordinik-CHAEA-zuritutaV2.csv')

# 'ordurako' funtzinoa, denpora-datuak denpora informazinotzat hartzeko
ordurako <- function(x)strptime(x, format = '%Y/%m/%e %k:%M:%S')

#' ZENBAT FAKULTATEAREN ARABERA, 
#' % ZENBAT ADINAREN ARABERA, 
#' % ZENBAT EMAKUMEZKOAK ETA GIZONEZKOAK, 
#' % ZENBATEK AMA-HIZKUNTZA ESPAÑOL ETA EUSKARA
#' 
#' Erantzunak zenbaki absolutu eta erlatiboetan

# Titulazioak
knitr::kable(table(df.chaea$Nombre.de.la.titulación))
knitr::kable(prop.table(table(df.chaea$Nombre.de.la.titulación)))

# Sexuak
knitr::kable(table(df.chaea$Género))
knitr::kable(prop.table(table(df.chaea$Género)))

# H1
knitr::kable(table(df.chaea$Lengua.materna))
knitr::kable(prop.table(table(df.chaea$Lengua.materna)))

#' ---
#' 
#' # Ikas-estilo guztien garapena erakusten dauan irudiaren ganekoa.
#' 
#' Galdetegiak informatzaile bakoitzari erakusten dio ikas-estilo bakoitzean zein garapen maila duen. Grafiakak erakuste du, fakultateen arabera banatuta, informatzaile guztien garapenaren batez bestekoa.
#' 
#' Espero izatekoa den moduan, unibertsitate ikasketetan ari diren informatzaileek ikas-estilo guztiak nahiko garatuta dituzte. Hala eta guztiz ere, fakultateen arabera bada alderik.
#' 
#' * BHFko ikasleriak estilo teorikoan eman ditu emaitzik gorenak, beste fakultatetakoen aldean.
#' * GKZko ikasleek estilo ekintzailean eta pragmatikoan eman dute erantzun positibo gehien.
#' * ZeTko ikasleen erantzunetan aurkitu dira, berriz, estilo hausnarkorreko erantzun gehien.
#' 
#' ---
#' 
#'  # Likert eskalaren grafikoak
#'  
#'  Aintzanek dino:
#'  > Datu hauek azaldu behar dira, nik ez dot ulertzen zeri dagozkion %ak
#'  
#'  **Erantzuna**: 
#'  
#'  * Ezkerreko alderdiko portzentaiak erantzun negatiboen portzentaiaren barri emoten dabe.
#'  * Erdiko zenbakiak, grisaren gainean idatzitakoek, ez positibo ez negatibo erantzun dabenen barri emoten dabe.
#'  * Eskumako zenbakiak berdexkaren alderdikoak, adierazten dau zenbatek erantzun daben positibo edo oso positibo 
#'  (*moderadamente* edo *muy de acuerdo*, alegia)
#'  
#'  *Oharra*: Nik ez dot uste zentzun handiegirik daukenik datu orokor guztiak beste guztiakin batera emoteak, 
#'  ez bada zeozer zehatz erakutsi nahi. Beste kontu bat litzateke artikuluan, horren interesa bageunke. 
#'  Kontuan euki behar da hau: datuak fakultateka ez badoguz banatzen BHFekoek hartzen dabe pisurik handiena eta 
#'  euren emoitzetara daroiez erantzun guztiak
#'  

x <- table(df.chaea$Faku)
knitr::kable(x)

knitr::kable(structure(c(x[1], x[2]+x[3]), col.names = c('BHF', 'Beste biak')))

#'  
#'  
#' # Ondorioak
#'  
#'  1. Ondorioetan emon nahi dozun informazinoia ez dakit zelan emon. Ez dakit emon deikegun; 
#'  EMSIko analisirako erizpideak behar geunkez.
#'  
#'  2. Esan deikegu 
#'    * Barneko motibazioari dagokionez, GKZko informatzaileek puntuazio altuagoak eman dituzte lautik hirutan.
#'    * *Regulacion identifikada* irizpidearen arabera, ZeTeko ikasleek kanporago kokatzen dute ekintza egitearen ardura,
#'    lautik lautan.
#'    * Azken hori errapikatzen da lautik bitan *regulación externa* neurtzeko itemetan.
#'    * Amotibazioa neurtzeko itemetan, berriz, nabarmenak dira amotibazioaren ezezko balioak. 
#'    Amotibaziorik baxuena erakutsi dute GKZ fakultatekoek, lautik hirutan; puntuazio negatibo guztiak %50etik gorakoak.
#'    Aurreko datuekin koherenteki, amotibaziorik altuenak ZeT fakultateko informatzaileek erakutsi dute neurketa guztietan, 
#'    beti ere, herenetik behera eta balio negatiboak baino baxuagoak direla.
#'     
#'  