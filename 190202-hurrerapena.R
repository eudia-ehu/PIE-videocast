# Lehenengo hurrerapena
df.cooped <- read.csv('Aintzanek_PIE/datuak/gordinik-COOPEDSUP.csv')
df.chaea <- read.csv('Aintzanek_PIE/datuak/gordinik-CHAEA.csv')
df.emsi <- read.csv('Aintzanek_PIE/datuak/gordinik-EMSI.csv')

ordurako <- function(x)strptime(x, format = '%Y/%m/%e %k:%M:%S')

df.cooped$Marca.temporal <- ordurako(df.cooped$Marca.temporal)
df.chaea$Marca.temporal <- ordurako(df.chaea$Marca.temporal)
df.emsi$Marca.temporal <- ordurako(df.emsi$Marca.temporal)

df.cooped$Marca.temporal

# Datuen egitura, emsi
str(df.emsi)

# Hiru galdetegiak batera eta alkarren segidan pasauta dagozela emoten dau:
df.emsi$Marca.temporal[1]
df.cooped$Marca.temporal[1]
df.chaea$Marca.temporal[1]

# 1.a
aztertu.emsi <- colSums(table(df.emsi$Kurtsoa, 
                              paste(df.emsi$Adina, 
                                    df.emsi$Titulazioa, 
                                    df.emsi$Kurtsoa,
                                    df.emsi$Género, 
                                    df.emsi$Lengua.materna)))
# 2.a
aztertu.cooped <- colSums(table(df.cooped$Curso, 
                                paste(df.cooped$Edad, 
                                      df.cooped$Nombre.de.la.titulación, 
                                      df.cooped$Curso,
                                      df.cooped$Género, 
                                      df.cooped$Lengua.materna)))

# 3.a
aztertu.chaea <- colSums(table(df.chaea$Curso, 
                               paste(df.chaea$Edad, 
                                    df.chaea$Nombre.de.la.titulación, 
                                    df.chaea$Curso,
                                    df.chaea$Género, 
                                    df.chaea$Lengua.materna)))

# Arazua
df.chaea[141, 'Nombre.de.la.titulación']
# Arazodunaren ezaugarriak:
df.chaea[140:142, c('Edad', 'Género', "Lengua.materna", "Curso", "Marca.temporal")]

# Identifikau hori besteren baten
df.emsi[which(df.emsi$Kurtsoa==4 & 
                df.emsi$Lengua.materna=='Euskara' &
                df.emsi$Adina==26), 1:6]
df.cooped[which(df.cooped$Curso==4 & 
                  df.cooped$Lengua.materna=='Euskara' &
                  df.cooped$Edad==26), 1:6]



# Zenbat maila kategoria taldekatze desbardin dagozan
summary(factor(aztertu.emsi))
summary(factor(aztertu.cooped))
summary(factor(aztertu.chaea))

aztertu.emsi[aztertu.emsi==4]
aztertu.cooped[aztertu.cooped==5]
aztertu.chaea[aztertu.chaea==6]

df.emsi[which(df.emsi$Adina==21 & 
                df.emsi$Kurtsoa==4 &
                df.emsi$Lengua.materna=='Español' &
                df.emsi$Género=='Emakumea' &
                df.emsi$Titulazioa=='Haur Hezkuntza'),  c(1,3:6)]

df.chaea[which(df.chaea$Edad==21 & 
                 df.chaea$Curso==4 &
                 df.chaea$Lengua.materna=='Español'),  c(1,3:6)]

df.emsi[which(df.emsi$Adina==21 & 
                df.emsi$Kurtsoa==4 &
                df.emsi$Lengua.materna=='Español'),  c(1:6)]

# strptime(df.cooped$Marca.temporal,
#          format = '%Y/%m/%e %k:%M:%S')


