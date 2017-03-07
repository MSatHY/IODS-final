# Markus Selin; markus.selin@helsinki.fi; 7.3.2017; Data-wrangling for Open Data Science course final exercise

# Note: Course requirements state that the code should be clearly commented.

library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(ggplot2)

# There have been several fusions of Finnish muncipalities from 2000 until 2015.
# We will fuse the information from the expired muncipalities to match the fused muncipalities in 2016.

setwd("C:\\Users\\Markus\\Documents\\OpenDataScience\\IODS-final\\data")

# ---------------------------------------------
# First: population data will be read
# ---------------------------------------------

# Information on populations of Finnish muncipalities may be found from
# www.kunnat.net/fi/tietopankit/tilastot/vaestotietoja/Documents/Kuntajaot%20ja%20asukasluvut%202000-2016.xls

# Let us experiment with the year 2000
#setwd("C:\\Users\\Markus\\Documents\\OpenDataScience\\Final\\kunnatnet")
popul_info <- read_excel("Kuntajaot ja asukasluvut 2000-2016.xls", sheet = "2000")[,2:3] # initial year 
popul_info = data.frame(popul_info) 
head(popul_info, n=20) # 13 excess rows in the beginning (always same)
popul_info <- popul_info[14:nrow(popul_info),] # remove excess valuees at the beginning
tail(popul_info) # 1 excess value at the end
#popul_info <- popul_info[1:(nrow(popul_info) -sum(is.na(popul_info[,1])) ),] # remove excess values at the end...
popul_info[,1] <- gsub(" mlk", "_mlk", popul_info[,1])  # change " mlk" into "_mlk"
popul_info[,1] <- gsub(" ", "", popul_info[,1])  # remove extra spaces in the names variable
#rownames(popul_info) <- popul_info[,1]
colnames(popul_info) <- c("area_name_fi","popul_2000_start")
#popul_info <- select(popul_info, one_of("popul_2000_start"))

# Now let us copy all the data into the data.frame
vuodet <- 2001:2016 %>% as.character()
for (year in 1:16){
  #year<-year+1
  add_year <- read_excel("Kuntajaot ja asukasluvut 2000-2016.xls", sheet = vuodet[year])[,2:3] # other years 
  add_year = data.frame(add_year) 
  add_year <- add_year[14:nrow(add_year),] # remove 13 excess rows in the beginning (always same number)...
  #add_year <- add_year[1:(nrow(add_year) -sum(is.na(popul_info[,1])) ),] # remove excess values at the end...
  add_year[,1] <- gsub(" mlk", "_mlk", add_year[,1]) # change " mlk" into "_mlk"
  add_year[,1] <- gsub(" ", "", add_year[,1]) # remove extra spaces in the names variable
  #rownames(add_year) <- add_year[,1]
  variablename <- paste("popul_", vuodet[year], collapse="", sep="") %>% paste("_start", collapse="", sep="") 
  colnames(add_year) <- c("area_name_fi", variablename)
  #add_year <- select(add_year, one_of(variablename))
  popul_info <- full_join(popul_info, add_year, by= c("area_name_fi") )
}
rm(add_year, year, vuodet, variablename)
#Remove excess values
popul_info <- filter(popul_info, complete.cases(popul_info[,1])==TRUE ) # now it works (apparently the excel file has problems...)

# Checking the results
tail(popul_info, n=20) # row number 453 ("Pieksänmaa") is the first muncipality not existing already during 2000

# The xlsx file contains information about the fusions, e.g.  
# "Pieksämäki" (2000)
# "Pieksämäen mlk" (2000)
# "Pieksämäen mlk" => "Pieksänmaa" (2004) 
# "Pieksänmaa" => "Pieksämäki" (2007)

# Novel rows aftetr 2000...
# new_mun <- popul_info[453:nrow(popul_info),]






# --------------------------------------------------
# Second: Let us gather the information on fusion of muncipalties
# --------------------------------------------------

# Let us examine the first year of fusions (2001)
fusion_year <- read_excel("Kuntajaot ja asukasluvut 2000-2016.xls", sheet = "2001")[,4] # read the first year with fusions 
fusion_year <- fusion_year[grep('=>', t(fusion_year[,1])),] # Selects only the relevant rows based on pattern "=>"
pituus <- length(grep('=>', t(fusion_year[,1]))) # amount of fusions during 2001 
fusion_year[,2] <- rep("2001", each=pituus) # store the year of these fusions
fusion_info <- fusion_year # store information in a new object

# Let us gather the remaining fusion information
vuodet <- 2001:2016 %>% as.character()
for (year in 2:16){ # let us examine all the remaining years
  sivu <- read_excel("Kuntajaot ja asukasluvut 2000-2016.xls", sheet = vuodet[year]) # read a sheet into object "sivu"
  if(length(sivu) > 3){ # if there is a column containing extra information about fusions (the 4th column) 
    sivu <- sivu[,length(sivu)] # keep only the relevant column (the last) of the sheet
    fusion_year <- sivu[grep('=>', t(sivu[,1])),] # Selects only the relevant rows based on pattern "=>"
    pituus <- length(grep('=>', t(fusion_year[,1]))) # amount of fusions during the year 
    if(pituus > 0){ # there are also columns without the pattern "=>" reporting yet additional info...
      fusion_year[,2] <- rep(vuodet[year], each=pituus) # store the year of these fusions
      fusion_info[(nrow(fusion_info)+1):(nrow(fusion_info)+nrow(fusion_year)),] <- fusion_year[,]
    }
  }
}
rm(fusion_year, sivu, year, vuodet, pituus)
colnames(fusion_info) <- c("fusion","fusion_year")
for(i in 1:nrow(fusion_info)) {
  fusion_info[i,1] <- gsub(" mlk", "_mlk", fusion_info[i,1])  # change " mlk" into "_mlk"
}
#rownames(fusion_info) <- 1:nrow(fusion_info) # there was something weird in the naming of some rows (e.g. 19th row was "10.1")

# separation of fusing_from and fusing_into parts of the fusion expressions
alut = vector(mode="character", length = nrow(fusion_info))
for(i in 1:nrow(fusion_info)){
  pos = regexpr('=>', fusion_info[i,1])
  alut[i] <- substr(fusion_info[i,1], 1, (pos-2))
}
fusion_info[,3] <- alut
loput = vector(mode="character", length = nrow(fusion_info))
for(i in 1:nrow(fusion_info)){
  pos = regexpr('=>', fusion_info[i,1])
  loput[i] <- substr(fusion_info[i,1], (pos+3), nchar(fusion_info[i,1]))
}
fusion_info[,4] <- loput

# Now there are multiple fusing muncipalities on same lines
# separated by " - ", " ja ", and ", "

#Let us separate each individual fusion to it's own row...
for(i in 1:nrow(fusion_info)){
  pos = regexpr(', ', fusion_info[i,3]) # e.g. note row 18: i<-18
  while(pos>0){
    loppu <- substr(fusion_info[i,3], (pos+2), nchar(fusion_info[i,3]))
    alku <- substr(fusion_info[i,3], 1, (pos-1))
    fusion_info[(nrow(fusion_info)+1),3]<-alku
    fusion_info[(nrow(fusion_info)),c(1:2,4)]<-fusion_info[i,c(1:2,4)]
    fusion_info[i,3]<-loppu
    pos = regexpr(', ', loppu)
  }
  pos = regexpr(' - ', fusion_info[i,3]) # e.g. note row 68: i<-68
  while(pos>0){
    loppu <- substr(fusion_info[i,3], (pos+3), nchar(fusion_info[i,3]))
    alku <- substr(fusion_info[i,3], 1, (pos-1))
    fusion_info[(nrow(fusion_info)+1),3]<-alku
    fusion_info[(nrow(fusion_info)),c(1:2,4)]<-fusion_info[i,c(1:2,4)]
    fusion_info[i,3]<-loppu
    pos = regexpr(' - ', loppu)
  }  
  pos = regexpr(' ja ', fusion_info[i,3], ignore.case=T) # e.g. note row 26: i<-26
  while(pos>0){
    loppu <- substr(fusion_info[i,3], (pos+4), nchar(fusion_info[i,3]))
    alku <- substr(fusion_info[i,3], 1, (pos-1))
    fusion_info[(nrow(fusion_info)+1),3]<-alku
    fusion_info[(nrow(fusion_info)),c(1:2,4)]<-fusion_info[i,c(1:2,4)]
    fusion_info[i,3]<-loppu
    pos = regexpr(' ja ', loppu, ignore.case=T)
  }  
}

# Let us still fix the names of the resulting fused muncipalities

# The information states that "Tyrnävä" is the major destination for old "Temmes" in the fusion, note
fusion_info[4,1] # Temmes => Liitettiin suurimmaksi osaksi Tyrnävään mutta osia myös Liminkaan ja Rantsilaan
# Let us proceed with "Tyrnävä" as the destination muncipality...
fusion_info[4,4] <- "Tyrnävä"  

# "Länsi-Turunmaa" changed name to "Parainen" in 2012
fusion_info[(nrow(fusion_info)+1),2:4] <- c("2012","Länsi-Turunmaa","Parainen")

# Let us remove notes of newly named muncipalities...
for(i in 1:nrow(fusion_info)){ # e.g. note row 38: i<-38
  pos = regexpr(", uusi nimi", fusion_info[i,4]) # find note
  if(pos>0){
    alku <- substr(fusion_info[i,4], 1, (pos-1)) #if found, remove note
    fusion_info[i,4]<-alku # store clear version of the destination muncipality
  }
}

for(i in 1:nrow(fusion_info)) {
  fusion_info[i,3] <- gsub(" ", "", fusion_info[i,3])  # remove extra spaces in the names variable
  fusion_info[i,4] <- gsub(" ", "", fusion_info[i,4])  # remove extra spaces in the names variable
}

rm(alku, alut, i, loppu, loput, pos)

# Cosmetics for the "fusion_info"...
colnames(fusion_info)[3:4] <- c("fusion_from", "fusion_into") # column names
fusion_info <- fusion_info[,2:4] # remove the initial "variable"
fusion_info <- arrange(fusion_info, fusion_year, fusion_from, fusion_into) # organize the data primarily based on year...

# "Längelmäki" was split between two muncipalities in 2007 
# Quote "Jämsän kaupunkiin liittyvällä alueella asuu 64 prosenttia ja Oriveteen liittyvällä alueella 36" from
# http://w3.verkkouutiset.fi/arkisto/politiikka/73820.html tells how
# the division of the population went.
# Quote "Kuntakeskus ja sen mukana pääosa kunnan henkilöstöstä siirtyy Jämsään" tells
# that the majority of the work-force hired by the muncipality went to Jämsä.
fusion_info <- fusion_info[c(1:27,30:nrow(fusion_info)),]

# Spelling errors:

# The "Pieksänmaa" is written wrong ("Pieksanmaa")
fusion_info[7,3] <- "Pieksänmaa"

# "Kemiönsaari" is wrongly written as "Keminönsaari" in fusion information
fusion_info[44,3] <- "Kemiönsaari"
fusion_info[57,3] <- "Kemiönsaari"

# >"Muohijärvi" should be "Mouhijärvi"
fusion_info[76,2] <- "Mouhijärvi"

# "Pylkönäki" should be "Pylkönmäki"
fusion_info[88,2] <- "Pylkönmäki"

# "Kemiönsaari" is wrongly written as "Keminönsaari" in fusion information
fusion_info[104,3] <- "Kemiönsaari"

# last 4 observations (year 2016) still have some problems to solve... will be handled separately.
tail(fusion_info) 
fusion_info <- fusion_info[1:(nrow(fusion_info)-4),]






# --------------------------------------------------
# Now, let us reformulate population information "popul_info" with the fusion information...
# --------------------------------------------------

# Convert NAs into zeros, and round numbers to zero digits
popul_info[is.na(popul_info)] <- 0
str(popul_info) # all variables are still type = chr (character)

# Convert population numbers to numeric variables 
# stored <- popul_info
for(i in 2:ncol(popul_info)){
  popul_info[,i] <- popul_info[,i] %>% as.numeric() %>% round(digits = 0)
}
# popul_info <- stored
# colSums(is.na(popul_info[,2:ncol(popul_info)]))

# now add up the populations of muncipalities that have fused (for using the current muncipalities in analysis)
# stored <- popul_info
for(i in 1:nrow(fusion_info)){
  from_row <- 0 # initiation
  into_row <- 0 # initiation
  for(ii in 1:nrow(popul_info)){
    if(from_row == 0){if(fusion_info[i,2] == popul_info[ii,1]){from_row <- ii} } # Check, unless correct row was already found
    if(into_row == 0){if(fusion_info[i,3] == popul_info[ii,1]){into_row <- ii} } # Check, unless correct row was already found
  }
  year <- fusion_info[i,1] %>% as.numeric() # copy the fusion year as numeric
  year <- year-2000+1 # now "year" can be used to point directly to the column of the "popul_info"...
  # Let us sum together the values of 
  popul_info[into_row, 2:year] <- as.vector(popul_info[into_row, 2:year]) + as.vector(popul_info[from_row, 2:year])
  # mark the used values as zeroes
  popul_info[from_row, 2:year] <- rep(0, each=length(2:year))
}
# popul_info <- stored

# Manual fix of the last rows in "popul_info":
# Maarianhamina-Mariehamn: row 462
# Maarianhamina: row 218
from_row <- 462 # initiation
into_row <- 218 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# Hollola(+Hämeenkoski): row 463
# Hollola: row 49
# Hämeenkoski: row 56
from_row <- 463 # initiation
from_row2 <- 56 # initiation
into_row <- 49 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)]) + as.vector(popul_info[from_row2, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row2, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# Kurikka(+Jalasjärvi): row 464
# Kurikka: row 164
# Jalasjärvi: row 72
from_row <- 464 # initiation
from_row2 <- 72 # initiation
into_row <- 164 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)]) + as.vector(popul_info[from_row2, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row2, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# Lahti(+Nastola): row 465
# Lahti: row 177
# Nastola: row 244
from_row <- 465 # initiation
from_row2 <- 244 # initiation
into_row <- 177 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)]) + as.vector(popul_info[from_row2, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row2, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# Säkylä(+Köyliö): row 466
# Säkylä: row 371
# Köyliö: row 176
from_row <- 466 # initiation
from_row2 <- 176 # initiation
into_row <- 371 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)]) + as.vector(popul_info[from_row2, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row2, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# Also, the fusion information in "Vöyri-Maksamaa + Oravainen = Vöyri, uusi nimi" was missing the arrow ("=>")
# Vöyri: row 439
# Vöyri-Maksamaa: row 455
# Oravainen: 256
from_row <- 455 # initiation
from_row2 <- 256 # initiation
into_row <- 439 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)]) + as.vector(popul_info[from_row2, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row2, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# The same in "Tammisaari, Karjaa ja Pohja = Raasepori, uusi nimi" (missing the "=>")
# Tammisaari: row 377
# Karjaa: row 105
# Pohja: row 289
# Raasepori: row 459
from_row <- 377 # initiation
from_row2 <- 105 # initiation
from_row3 <- 289 # initiation
into_row <- 459 # initiation
popul_info[into_row, 2:length(popul_info)] <- as.vector(popul_info[into_row, 2:length(popul_info)]) + as.vector(popul_info[from_row, 2:length(popul_info)]) + as.vector(popul_info[from_row2, 2:length(popul_info)]) + as.vector(popul_info[from_row3, 2:length(popul_info)])
popul_info[from_row, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row2, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))
popul_info[from_row3, 2:length(popul_info)] <- rep(0, each=length(2:length(popul_info)))

# Now, let us remove the non-existing muncipalities (as in 2016) from the data:
popul_info <- arrange(popul_info, area_name_fi) # organize the data by name...
kunnat2016 <- popul_info[(popul_info$popul_2016_start > 0),1] #store the names of existing muncipalities 
popul_info <- popul_info[,2:ncol(popul_info)] #drop the names variable
popul_info <- dplyr::filter(popul_info, popul_2016_start > 0)
rownames(popul_info) <- kunnat2016

# "Akaa" has a missing value (17091) in 2012 since the starting row for the excel-list was changed
popul_info[1,13] <- 17091

# Now, the populations of muncipalities in 2016 are expressed as sums of populations in past muncipalities
# The exceptions are: 
#  fusion of "Temmes" since the parts joined to "Liminka" and "Rantsila" (in 2001) were not considered (all population was placed in "Tyrnävä")
#  fusion of "Längelmäki" since it was split between two muncipalities in 2007 (population of "Längelmäki" was totally omitted) 

# Visual check-up of the data:
colnames(popul_info) <- 2000:2016

# 31 plots of 10 muncipalities (plotting in for loop does not seem to work) + remaining 3 muncipalities in a plot
# result: the partial fusion of Sipoo to Helsinki in 2009 can be seen from the resulting plots
# Analysis idea: limit the number of muncipalities to 20 by selecting 10 muncipalities growing the most (%-vise)
# and diminishing the most (again, in %). Try to find open background information on the selected muncipalities
# and try to find explaining factors for the increases and decreases. Later: maybe include random 10 to 20 muncipalities
# randomly selected from the remaining muncipalities and check how well you can predict their population changes, or 
# alternatively how well you can group them (growing/diminishing) based on the background variables. 
write.table(popul_info, file="population.txt", append = F, quote = T, sep = "\t", row.names = T)
#test <- read.table("population.txt", sep="\t", header=T)

rm(i, ii, year, into_row, from_row, from_row2, from_row3, kunnat2016)
rm(fusion_info)

# plot 1
start_row <- 1
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 2
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 3
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 4
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 5
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 6
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 7
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 8
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 9
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 10
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 11
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()    

# plot 12
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 13
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 14
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 15
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 16
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 17
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 18
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 19
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 20
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 21
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()  

# plot 22
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 23
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 24
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 25
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 26
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()          

# plot 27
start_row <- start_row+10
end_row <- start_row+9
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 28
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 29
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 30
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# plot 31
start_row <- start_row+10
end_row <- start_row+9
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()

# and the remaining muncipalities

# plot 32
start_row <- 311
end_row <- 313
test4 <- popul_info[start_row:end_row,] 
test4[,(ncol(test4)+1)] <- rownames(test4)
colnames(test4)[ncol(test4)] <- "Muncipality"
test5 <- test4 %>% gather("Year", "Population", 1:(ncol(test4)-1) )
ggplot(test5, aes(x = Year, y = Population, col = Muncipality)) + geom_point()




# Changes (%) in population (between from 2000 to 2016)
changes_popul <- select(popul_info, one_of("2000", "2016"))
changes_popul <- mutate(changes_popul, change = 100*(changes_popul$`2016`-changes_popul$`2000`)/changes_popul$`2000`)
changes_popul <- select(changes_popul, change)
changes_popul[,2] <- rownames(popul_info)
colnames(changes_popul)[2] <- "Muncipality"
changes_popul <- arrange(changes_popul, change, Muncipality) # organize the data primarily based on change...
# changes_popul <- mutate(changes_popul, has_grown = (change > 0) )
selected_muncipal <- changes_popul[1:20,]
selected_muncipal[21:40,] <- changes_popul[(nrow(changes_popul)-10):nrow(changes_popul),]
popul_info[,(ncol(popul_info)+1)] <- rownames(popul_info)
colnames(popul_info)[ncol(popul_info)] <- "Muncipality"
selected_muncipal <- left_join(selected_muncipal, popul_info, by = "Muncipality")
rownames(selected_muncipal) <- selected_muncipal$Muncipality

# Areal information:
# www.kunnat.net/fi/tietopankit/tilastot/aluejaot/kuntien-pinta-alat-ja-asukastiheydet/Documents/Kuntien%20pinta-alat%20ja%20asukastiheydet%202016.xlsx
# setwd("C:\\Users\\Markus\\Documents\\OpenDataScience\\Final\\kunnatnet")
area_info <- read_excel("Kopio Kuntien pinta-alat ja asukastiheydet 2016.xlsx", sheet = "Kuntien pinta-alat 2016")
area_info <- area_info[9:nrow(area_info),1:(ncol(area_info)-2)]
columns <- c("area_code", "Muncipality", "area_name_sw", "land_km2", "sweet_h2o_km2", "salt_h2o_km2", "total_km2", "population_2015", "popul_dens_tot", "popul_dens_land")
colnames(area_info) <- columns
rm(columns)
area_info <- filter(area_info, complete.cases(area_info[,1])==TRUE )
keep <-c("Muncipality", "land_km2", "sweet_h2o_km2", "salt_h2o_km2")
area_info <- select(area_info, one_of(keep)) 
area_info$land_km2 <- area_info$land_km2 %>% as.numeric()
area_info$sweet_h2o_km2 <- area_info$sweet_h2o_km2 %>% as.numeric()
area_info$salt_h2o_km2 <- area_info$salt_h2o_km2 %>% as.numeric()
selected_muncipal <- left_join(selected_muncipal, area_info, by = "Muncipality")
selected_muncipal <- mutate(selected_muncipal, land_density_2016 = (selected_muncipal$`2016`/selected_muncipal$land_km2) )
# it is clear from the result, that the population densities calculated using the land areas show
# clear difference between diminishing (0.16 - 3.2 persons/km2) and growing (18 - 232 persons/km2) muncipalities
# however, this is also partly result of the moving

