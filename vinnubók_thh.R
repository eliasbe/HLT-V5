
# Þú þarft væntanlega að breyta þessu
setwd("~/Desktop/H21/Hagnýtt línuleg tölfræðilíkön/Heimadæmi/HLT-V5")

library(tidyverse)
library(GGally)
set.seed(11)


data_big <- data.frame(read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ","))

summary(data_big)
str(data_big)

# (i) Miðbær frá Bræðraborgarstíg að Tjörn; (ii) Melar að sjó; (iii) Háleiti/Skeifa;
# (iv) Hólar, Berg; (v) Réttarholt
sublocations = c(25, 70, 91, 160, 281)

# Við getum klippt út 'svfn' breytuna hér, hún er eins hjá öllum.
data = data_big[data_big$matssvaedi == sublocations, ]
# ATH, tökum út SVFN því öll gögn taka sama gildi þar, 0
data <- subset( data, select = -c(svfn,rfastnum) )

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
sqrt(mean(residuals(lm.first)^2))
summary(lm.first)

test_resid = (predict(lm.first, test_data) - test_data$nuvirdi)
sqrt(mean(test_resid^2))

nums <- unlist(lapply(data, is.numeric))  

# Grunur um collinearity, skoðum fylki eigingilda
X <- model.matrix(lm( nuvirdi ~ ., data[,nums]))
eigenX <- eigen(t(X) %*% X)
condNumber <- max(eigenX$values)/min(eigenX$values)
condNumber
# Risastór ástandstala fyrir fylkið, skoðum eiginvigra með lág eigingildi
eigenX$values
# Sjáum að eigingildi 14 er pínkulítið
eigenX$vectors[, 14]
# Getum byrjað að skoða vigur 15, þurfum að finna línulega háðar tölur innan þess
# NP COMPLETE vandamál hér en við getum staðfest grun með því að leggja saman á eftir.
heatmap(cor(data[,nums]))
library(gplots)
heatmap.2(cor(data[,nums])) 
# Út frá hitakortinu sjáum við að summa þessara dálka er mjög nærri 0.
# Þeir eru því nokkurn veginn línuleg samantekt hver af öðrum...
colnames(data[,nums])[c(4, 5, 9, 11, 12)]
sum(eigenX$vectors[c(4, 5, 9, 11, 12), 14])
sum(eigenX$vectors[,14])
plot(eigenX$vectors[c(4, 5, 9, 11, 12),14])
bad_actors <- eigenX$vectors[c(4, 5, 9, 11, 12), 14]
sum(bad_actors[c(1,5)]) - sum(bad_actors[c(2,3,4)])
# Þessi eigingildi eru svo gott sem línulega háð innbyrðis.
# Það er réttlætanlegt að velja annað hvort
#   "ibm2" "fjstof" inni og "fjherb" "fjhaed" "fjklos" úti

library(reshape2)
cor(data[,nums]) %>%
  as_tibble(rownames = 'var') %>%
  melt() %>% # from reshape2
  ggplot(aes(x = var, y = variable, fill = value)) +
  geom_tile() + theme(axis.text.x = element_text(angle = -45, vjust = -0.5, hjust=0.5))

cor(data[,nums])

# Einfaldasta módelið, verð sem fall af fermetrum.
# Gæti verið baseline
lm.simple <- lm(nuvirdi ~ ibm2, data = train)
summary(lm.simple)

# MIKILVÆGAST: PLOT RESIDUAL Á MÓTI FITTED
fortData <- fortify(lm.first)
fortData %>%
  ggplot(aes(x = .fitted, y = .resid, color = matssvaedi)) +
  geom_jitter(width = 0.25)

fortData %>%
  ggplot(aes(x = .fitted, y = .resid, color = teg_eign)) +
  geom_jitter(width = 0.25) 

# Athugum stepWise. 
library(MASS)
stepWiseLM <- stepAIC(object = lm.first, direction = 'both', trace = T)
summary(stepWiseLM)

# Skoða umbreytingar á y til að fitta gögnin
bcTest <- boxcox(lm( nuvirdi ~ ., data))

indexOfLLPeak <- which.max(bcTest$y)
lambda <- bcTest$x[indexOfLLPeak]
lambda
# Skoða þetta betur þegar við höfum hreinsað gagnasettið dálítið...

# Þá má líka nota BoxCox á sérhverja skýribreytu.
# 

