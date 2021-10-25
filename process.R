## STIG 0: FORVINNSLA

# Hlöðum inn gagnasettinu og veljum undirsvæðin:
setwd("~/Desktop/H21/Hagnýtt línuleg tölfræðilíkön/Heimadæmi/HLT-V5")

library(MASS)
library(tidyverse)
library(GGally)
set.seed(11)

data_big <- data.frame(read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ","))
sublocations = c(25, 70, 91, 160, 281)
data = data_big[data_big$matssvaedi == sublocations, ]

# Fjarlægjum breytur sem augljóslega skipta ekki máli:
data <- subset( data, select = -c(svfn,rfastnum) )

# Skilgreinum tegundir breyta:
data[ ,"kdagur"] <- as.Date(data[ ,"kdagur"]) # Kaupdagur sem dagsetning
data[ ,"teg_eign"] <- as.factor(data[ ,"teg_eign"]) # Tegund eignar sem flokkur

# Ath að í verkefni segir að "lyfta" sé binary en í gögnum virðist hún vera fjöldi lyfta.
data[ ,"lyfta"] <- data[,"lyfta"]>0 # Kannski halda, kannski sleppa

# Kannski hafa stig10 logical 10 eða ekki 10.
data[ ,"matssvaedi"] <- as.factor(data[ ,"matssvaedi"]) # Staðsetning sem flokkur
data[ ,"undirmatssvaedi"] <- as.factor(data[ ,"undirmatssvaedi"]) # Undirstaðsetning sem flokkur
data[ ,"ibteg"] <- as.factor(data[ ,"ibteg"]) # Tegund íbúðar sem flokkur (ath, bara tveir flokkar og stendur numerical í verkefnalýsingu)

# Splittum datasetti í þjálfun og prófun:
sizeTraining = floor(0.75 * nrow(data))
trainingSampleRowId <- sample(1:nrow(data), size = sizeTraining, replace = F)
train_data <- data[trainingSampleRowId, ]
test_data <- data[-trainingSampleRowId, ]

## STIG 1: FYRSTA LÍKAN OG TÖLFRÆÐI HEILDARGAGNASETTS

# Skoðum mean, sd, min, max, kvantíla (og annað?) fyrir gagnasettið okkar:
# Óklárað

# Fittum fyrsta líkan, án nokkurrar vinnslu:
lm.first = lm(nuvirdi ~ ., data = train_data)

# Skráum RMSE, R^2 adjusted og PRESS fyrir líkanið:
sqrt(mean(residuals(lm.first)^2))
summary(lm.first)

# Skoðum spágildi líkansins út frá prófunargagnasetti:
test_resid = (predict(lm.first, test_data) - test_data$nuvirdi)
sqrt(mean(test_resid^2))

# Skoðum einnig annars konar grunnlíkan, sem tekur bara mið af fermetrum:
# Gæti verið baseline
lm.simple <- lm(nuvirdi ~ ibm2, data = train)
summary(lm.simple)

# Skoðum loks STEPWISE framleitt líkan, sem við notum bara til samanburðar:
stepWiseLM <- stepAIC(object = lm.first, direction = 'both', trace = T)
summary(stepWiseLM)

## STIG 2: Gagnaúrvinnsla, augljósir flokkar fjarlægðir

# Skoðum breytur sem eru of líkar, multiple collinearity:
nums <- unlist(lapply(data, is.numeric))  
library(gplots)
heatmap.2(cor(data[,nums])) 
# Sjáum cluster af hópum sem eru mjög líkir. Skoðum eigingildi:
X <- model.matrix(lm( nuvirdi ~ ., data[,nums]))
eigenX <- eigen(t(X) %*% X)
condNumber <- max(eigenX$values)/min(eigenX$values)
condNumber
# Risastór ástandstala fyrir fylkið, skoðum eiginvigra með lág eigingildi
eigenX$values
# Sjáum að eigingildi 14 er pínkulítið
eigenX$vectors[, 14]
colnames(data[,nums])[c(4, 5, 9, 11, 12)]
sum(eigenX$vectors[c(4, 5, 9, 11, 12), 14])
sum(eigenX$vectors[,14])
plot(eigenX$vectors[c(4, 5, 9, 11, 12),14])
bad_actors <- eigenX$vectors[c(4, 5, 9, 11, 12), 14]
sum(bad_actors[c(1,5)]) - sum(bad_actors[c(2,3,4)])
# Sjáum að ibm2 og fjstof tjá nokkurn veginn sömu upplýsingar og hinar
# -> niðurstaða, taka út fjherb, fjlkos, fjhaed

# Tökum þessa þætti út og skoðum aftur módelið:
td2 = subset( train_data, select = -c(fjherb, fjklos, fjhaed) )
lm.second = lm(nuvirdi ~ ., data = td2)

# Fáum stærra RMSE en örlítið lægra R^2 adjusted...
summary(lm.first)
summary(lm.second)
sqrt(mean(residuals(lm.first)^2))
sqrt(mean(residuals(lm.second)^2))
# Gætum gert F prófs samanburð hérna, skoða glósur...

# Sjáum samt að fjbilast og fjeld eru nokkurn veginn eins fyrir alla punkta:
hist(data$fjeld)
hist(data$fjbilast)
# Þau mælast líka með mjög hátt p-gildi, svo við metum það svo að þetta megi fjúka
td3 = subset( td2, select = -c(fjeld, fjbilast) )
lm.third = lm(nuvirdi ~ ., data = td3)
sqrt(mean(residuals(lm.third)^2))
summary(lm.third)
# Lítil sem engin breyting í RMSE en R^2 hækkar, svo þetta er gott...

## STIG 3: GAGNAÚRVINNSLA, MINNA AUGLJÓSIR FLOKKAR FJARLÆGÐIR
group_by(data, undirmatssvaedi) %>% count()
# Einu hóparnir sem fá lágt p-gildi eru 3 og 6 (Ægissíða, Vesturbær NA við Hringbraut)
# en aðeins 7 og 4 stök falla þar undir.
# Fjölmennustu hóparnir, 21 og 28 (Vesturberg í Breiðholti og Blokkir við 
# Kringlumýrabraut), virðast skipta litlu máli.
# Mér finnst þetta ástæða til að sleppa undirmatssvæðum í heild sinni.
# -> Prófum það!

td4 = subset( td3, select = -c(undirmatssvaedi))
lm.fourth = lm(nuvirdi ~ ., data = td4)
sqrt(mean(residuals(lm.fourth)^2))
summary(lm.fourth)
# Hér hækkar RMSE en R^2 lækkar. Þyrftum að skoða þetta betur...
# Ég ætla að prófa að vinna áfram með td3 en þetta er samt örugglega gott skref seinna

# Af teg_eign er Parhus að mælast með besta svörun (fyrir utan mögulega einbylishus)
# en það eru bara 6 gagnapunktar. Skoðum þess vegna að fella teg_eign inn í ibteg.
# Það nær utan um nokkuð mikið, eins og sést:
group_by(data, ibteg, teg_eign) %>% count()

td4 = subset( td3, select = -c(ibteg))
lm.fourth = lm(nuvirdi ~ ., data = td4)
sqrt(mean(residuals(lm.fourth)^2))
summary(lm.fourth)

td4 = subset( td3, select = -c(teg_eign))
lm.fourth = lm(nuvirdi ~ ., data = td4)
sqrt(mean(residuals(lm.fourth)^2))
summary(lm.fourth)

# Sjáum að -ibteg stendur sig betur en -teg_eign, svo við veljum út ibteg
# Núna stækkar R^2 á meðan RMSE minnkar, svo þetta er gott...
# Upp á grínið, prófum að taka hvort tveggja út og sjá hvernig það hegðar sér:

td4 = subset( td3, select = -c(ibteg, undirmatssvaedi))
lm.fourth = lm(nuvirdi ~ ., data = td4)
sqrt(mean(residuals(lm.fourth)^2))
summary(lm.fourth)

# Ég meina, þetta er alveg sæmilegt... Miðað við hversu margt er fjarlægt,
# þá er þetta mögulega bara leiðin fram á við...

# Hægt að skoða muninn á plottunum að neðan (resid v. fitted) fyrir
# third og fourth, enginn svakalegur munur.

## STIG 4A: HELDUR LÍNULEIKI? GRUNNSKOÐUN

# MIKILVÆGAST: PLOT RESIDUAL Á MÓTI FITTED
fortData <- fortify(lm.fourth)
fortData %>%
  ggplot(aes(x = .fitted, y = .resid, color = matssvaedi)) +
  geom_jitter(width = 0.25)

fortData %>%
  ggplot(aes(x = .fitted, y = .resid, color = teg_eign)) +
  geom_jitter(width = 0.25) 

# Ættum að geta séð þetta vel líka með QQ-plotti af leifðinni:
# Ófullkomið

# Sjáum skýr merki um heteroskedasticity! Viljum þá væntanlega vinna með y-breytuna okkar.
# Áður en við gerum það, er réttast að skoða útlaga og áhrifamikla punkta,
# þar sem BoxCox og aðrar aðferðir eru sérstaklega næmar fyrir slíku.

## STIG 5: ÚTLAGAR OG MIKILVÆGIR PUNKTAR

# Byrjum á því að greina y-punkta með jackknife og Cook's distance:
# Ófullkomið

# Greinum nú x-punkta með Mahalanobis/H_ii gildum:
# Ófullkomið







