
# Þú þarft væntanlega að breyta þessu
setwd("~/Desktop/H21/Hagnýtt línuleg tölfræðilíkön/Heimadæmi/HLT-V5")

library(tidyverse)
library(GGally)
set.seed(11)

sizeTraining = floor(0.75 * nrow(savings))
trainingSampleRowId <- sample(1:nrow(savings), size = sizeTraining, replace = F)
trainingSavings <- savings[trainingSampleRowId, ]
testSavings <- savings[-trainingSampleRowId, ]

data_big <- data.frame(read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ","))

summary(data_big)
str(data_big)

# (i) Miðbær frá Bræðraborgarstíg að Tjörn; (ii) Melar að sjó; (iii) Háleiti/Skeifa;
# (iv) Hólar, Berg; (v) Réttarholt
sublocations = c(25, 70, 91, 160, 281)

# Við getum klippt út 'svfn' breytuna hér, hún er eins hjá öllum.
data = data_big[data_big$matssvaedi == sublocations, ]
# ATH, tökum út SVFN því öll gögn taka sama gildi þar, 0
data <- subset( data, select = -svfn )

data[ ,"kdagur"] <- as.Date(data[ ,"kdagur"]) # Kaupdagur sem dagsetning
data[ ,"teg_eign"] <- as.factor(data[ ,"teg_eign"]) # Tegund eignar sem flokkur

# Ath að í verkefni segir að "lyfta" sé binary en í gögnum virðist hún vera fjöldi lyfta.
data[ ,"lyfta"] <- data[,"lyfta"]>0 # Kannski halda, kannski sleppa

# Kannski hafa stig10 logical 10 eða ekki 10.
data[ ,"matssvaedi"] <- as.factor(data[ ,"matssvaedi"]) # Staðsetning sem flokkur
data[ ,"undirmatssvaedi"] <- as.factor(data[ ,"undirmatssvaedi"]) # Undirstaðsetning sem flokkur
data[ ,"ibteg"] <- as.factor(data[ ,"ibteg"]) # Tegund íbúðar sem flokkur (ath, bara tveir flokkar og stendur numerical í verkefnalýsingu)

summary(data)
str(data)


sizeTraining = floor(0.75 * nrow(data))
trainingSampleRowId <- sample(1:nrow(data), size = sizeTraining, replace = F)
train_data <- data[trainingSampleRowId, ]
test_data <- data[-trainingSampleRowId, ]

group_by(data, ibteg, teg_eign) %>% count()
#data <- subset(data, select = -ibteg)

lm.first = lm(nuvirdi ~ ., data = train_data)
summary(lm.first)
summary(residuals(lm.first)^2)

nums <- unlist(lapply(data, is.numeric))  
ggpairs(data[,nums])

pm <- ggpairs(
  data[,nums],
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
  lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1), 
               combo = wrap("dot", alpha = 0.4,            size=0.2) ),
  title = "Diamonds"
)
pm
