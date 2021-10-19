
# Þú þarft væntanlega að breyta þessu
setwd("~/Documents/CS HI/2021-22/HLT/Verkefni 5")

library(tidyverse)
library(GGally)
set.seed(11)


data_raw <- data.frame(read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ","))

# (i) Miðbær frá Bræðraborgarstíg að Tjörn; (ii) Melar að sjó; (iii) Háleiti/Skeifa;
# (iv) Hólar, Berg; (v) Réttarholt
sublocations = c(25, 70, 91, 160, 281)
data = data_raw[data_raw$matssvaedi == sublocations, ]

summary(data)
str(data)

# Ný breyta sem einfaldar teg_eign í tvo flokka miðað við stærð flokkanna
# ATH, VISTA Í BREYTU DATA
data <- data %>% mutate(teg_eign2=ifelse(teg_eign=="Ibudareign", teg_eign, "Sereign"))

# ATH yfirskrifa eða ekki? Geyma þar til maður er sáttur með breyturnar?
data[ ,"kdagur"] <- as.Date(data[ ,"kdagur"]) # Kaupdagur sem dagsetning
data[ ,"teg_eign"] <- as.factor(data[ ,"teg_eign"]) # Tegund eignar sem flokkur
data[ ,"teg_eign2"] <- as.factor(data[ ,"teg_eign2"]) # Tegund eignar sem flokkur
data[ ,"svfn"] <- as.factor(data[ ,"svfn"]) # Svæði sem flokkur

# Ath að í verkefni segir að "lyfta" sé binary en í gögnum virðist hún vera fjöldi lyfta.
data[ ,"lyfta_logical"] <- data[,"lyfta"]>0 # Kannski halda, kannski sleppa

# Kannski hafa stig10 logical 10 eða ekki 10.
data[ ,"matssvaedi"] <- as.factor(data[ ,"matssvaedi"]) # Staðsetning sem flokkur
data[ ,"undirmatssvaedi"] <- as.factor(data[ ,"undirmatssvaedi"]) # Undirstaðsetning sem flokkur
data[ ,"ibteg"] <- as.factor(data[ ,"ibteg"]) # Tegund íbúðar sem flokkur (ath, bara tveir flokkar og stendur numerical í verkefnalýsingu)

summary(data)
str(data)
typeof(data) # afhverju list en ekki data frame?

# Athugum tengsl ibteg og teg_eign
group_by(data, ibteg, teg_eign) %>% count()
# Þetta bendir til að gott væri að sleppa ibteg
data <- subset(data, select = ibteg)


factorNames <- colnames(select(data, where(is.factor)))
#for (col in factorNames){
#  group_by(data, col) %>% count()
#}

data[ , factorNames] %>%
  gather(var, val) %>%
  group_by(var, val) %>% count() %>% view()

# Droppa svfn, allir punktar eins
# droppa rfastnum (halda sem ID?)
# stig10? fer frá 9.7 f 10 og nánast allir í 10
# Sameina undirmatssvæði eða droppa?



# TRAIN TEST SET

training_size = floor(0.75 * nrow(data))
trainingSampleRowId <- sample(1:nrow(data), size = training_size, replace = F)
data_train <- data[trainingSampleRowId, ]
data_test <- data[-trainingSampleRowId, ]


# ggpairs(data)
# ggpairs(data[ , -which(names(data) %in% c("svfn", "matssvaedi", "undirmatssvaedi"))])


# PLOT RESIDUAL Á MÓTI FITTED

