
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
  data[,nums],lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))
pm

# Grunur um collinearity, skoðum fylki eigingilda
X <- model.matrix(lm( nuvirdi ~ ., data[,nums]))
eigenX <- eigen(t(X) %*% X)
condNumber <- max(eigenX$values)/min(eigenX$values)
condNumber
# Risastór ástandstala fyrir fylkið, skoðum eiginvigra með lág eigingildi
eigenX$values
# Sjáum að eigingildi 15 er pínkulítið
eigenX$vectors[, 14]
eigenX$vectors[, 15]
# Getum byrjað að skoða vigur 15, þurfum að finna línulega háðar tölur innan þess
# NP COMPLETE vandamál hér en við getum staðfest grun með því að leggja saman á eftir.
heatmap(cor(data[,nums]))
library(gplots)
heatmap.2(cor(data[,nums])) 
# Út frá hitakortinu sjáum við að summa þessara dálka er mjög nærri 0.
# Þeir eru því nokkurn veginn línuleg samantekt hver af öðrum...
colnames(data[,nums])[c(5,6,9,10,12,13)]
sum(eigenX$vectors[c(5,6,9,10,12,13), 15])
sum(eigenX$vectors[,15])

library(reshape2)
cor(data[,nums]) %>%
  as_tibble(rownames = 'var') %>%
  melt() %>% # from reshape2
  ggplot(aes(x = var, y = variable, fill = value)) +
  geom_tile() + theme(axis.text.x = element_text(angle = -45, vjust = -0.5, hjust=0.5))

cor(data[,nums])
# Skoða umbreytingar á y til að fitta gögnin
library(MASS)
bcTest <- boxcox(lm( nuvirdi ~ ., data))

indexOfLLPeak <- which.max(bcTest$y)
lambda <- bcTest$x[indexOfLLPeak]
lambda
# Skoða þetta betur þegar við höfum hreinsað gagnasettið dálítið...

# Þá má líka nota BoxCox á sérhverja skýribreytu.
# 

