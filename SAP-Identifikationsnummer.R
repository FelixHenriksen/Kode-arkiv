### CPR eller CVR ###
df$cprcvr <- if_else(length(df$Identifikationsnummer)<=8,"CVR","CPR", missing = "Mangler id")

#Frekvenstabel over variabel
ftable(df$fptype)

### Erstatnings CPR ###
df$falskcpr <- factor(df$Identifikationsnummer>3112999999, levels = c(TRUE, FALSE), labels = c("Erstatningcpr", "Almindeligcpr"))

#Frekvenstabel over variabel
ftable(df$falskcpr)


### Køn kolonne ###
df$køn <- factor(as.numeric(df$Identifikationsnummer)%% 2!=0, levels = c(TRUE, FALSE), labels = c("Mand", "Kvinde"))

#Frekvenstabel over variabel
ftable(df$køn)


### Alders funktion på baggrund af CPR med højde for århundrede. Tak til J88X og stack Overflow ###
A <- c(rep("19",4),rep("20",6))
B <- c(rep("19",5),rep("20",4),"19")
C <- c(rep("19",5),rep("18",4),"19")

bday <- function(code){
  day <- substr(code,1,2)
  month <- substr(code,3,4)
  year <- substr(code,5,6)
  snum <- 1+as.numeric(substr(code,7,7))
  prefix <- ifelse(as.numeric(year) <= 36,A[snum],ifelse(as.numeric(year)<=57,B[snum],C[snum]))
  year <- paste0(prefix,year)
  paste(year,month,day,sep = "-")
}

df$fødselsdag <- ifelse(df$cprcvr == "CPR", bday(df$ID), NA)
df$fødselsdag <- as.Date(df$fødselsdag)
Alder <- floor(eeptools::age_calc(na.omit(df$fødselsdag), enddate = Sys.Date(), units = "years"))
df$Alder[!is.na(df$fødselsdag)] <- Alder
freq(df$Alder)
