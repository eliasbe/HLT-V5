
# Þú þarft væntanlega að breyta þessu
setwd("~/Desktop/H21/Hagnýtt línuleg tölfræðilíkön/Heimadæmi/HLT-V5")

library(tidyverse)
library(GGally)
set.seed(11)


data <- data.frame(read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ","))

summary(data)
str(data)

data[ ,"kdagur"] <- as.Date(data[ ,"kdagur"]) # Kaupdagur sem dagsetning
data[ ,"teg_eign"] <- as.factor(data[ ,"teg_eign"]) # Tegund eignar sem flokkur
data[ ,"svfn"] <- as.factor(data[ ,"svfn"]) # Svæði sem flokkur

# Ath að í verkefni segir að "lyfta" sé binary en í gögnum virðist hún vera fjöldi lyfta.
data[ ,"lyfta"] <- data[,"lyfta"]>0 # Kannski halda, kannski sleppa

# Kannski hafa stig10 logical 10 eða ekki 10.
data[ ,"matssvaedi"] <- as.factor(data[ ,"matssvaedi"]) # Staðsetning sem flokkur
data[ ,"undirmatssvaedi"] <- as.factor(data[ ,"undirmatssvaedi"]) # Undirstaðsetning sem flokkur
data[ ,"ibteg"] <- as.factor(data[ ,"ibteg"]) # Tegund íbúðar sem flokkur (ath, bara tveir flokkar og stendur numerical í verkefnalýsingu)

# (i) Miðbær frá Bræðraborgarstíg að Tjörn; (ii) Melar að sjó; (iii) Háleiti/Skeifa;
# (iv) Hólar, Berg; (v) Réttarholt
sublocations = c(25, 70, 91, 160, 281)

sample_data = data[data$matssvaedi == sublocations, ]


summary(sample_data)
str(data)
typeof(data) # afhverju list en ekki data frame?

group_by(data, ibteg, teg_eign) %>% count()
# Sleppa ibteg
data <- subset(data, select = ibteg)

# Spurning um að einfalda teg_eign í Ibudareign vs. rest (Einbyli, par- og raðhús)
# Gera betur
# data[data["teg_eign"]  == "Einbylishus" || data["teg_eign"] == "Parhus" || data["teg_eign"] == "Radhus"] = "Sérbýli"


# ggpairs(data, cardinality_threshold = 168)
# ggpairs(data[ , -which(names(data) %in% c("svfn", "matssvaedi", "undirmatssvaedi"))])

# PLOT RESIDUAL Á MÓTI FITTED

