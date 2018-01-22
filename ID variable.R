#CPR eller CVR
df$fptype <- if_else(length(df$Identifikationsnummer)<=8,"CVR","CPR", missing = "Mangler id")
ftable(df$fptype)

#Erstatnings CPR
df$falskcpr <- factor(df$Identifikationsnummer>3112999999, levels = c(TRUE, FALSE), labels = c("Erstatningcpr", "Almindeligcpr"))
ftable(df$falskcpr)


#Køn kolonne
df$køn <- factor(as.numeric(df$Identifikationsnummer)%% 2!=0, levels = c(TRUE, FALSE), labels = c("Mand", "Kvinde"))
ftable(df$køn)


