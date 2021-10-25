## STIG 0: FORVINNSLA

# Hlöðum inn gagnasettinu og veljum undirsvæðin:
setwd("~/Desktop/H21/Hagnýtt línuleg tölfræðilíkön/Heimadæmi/HLT-V5")

library(MASS)
library(tidyverse)
library(GGally)
library(knitr)
library(kableExtra)
set.seed(11)

data_big <- data.frame(read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ","))
sublocations = c(25, 70, 91, 160, 281)
data_big$id <- 1:nrow(data_big)
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
lm.simple <- lm(nuvirdi ~ ibm2, data = train_data)
summary(lm.simple)
sqrt(mean(residuals(lm.simple)^2))

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

# Fáum stærra RMSE en örlítið hærra R^2 adjusted...
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
  ggplot(aes(x = .fitted, y = .resid, color = matssvaedi, shape = teg_eign)) +
  geom_jitter(width = 0.25)

# Ættum að geta séð þetta vel líka með QQ-plotti af leifðinni:
tibble(Normal = fortData$.stdresid) %>%
  gather(type, val) %>%
  ggplot(aes(sample = val)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'blue')
# Einn punktur alveg út úr kú-kú en svo er þetta að falla nokkuð vel að y=x...
# Heldur samt ekki í við þetta í endann og við sjáum að tilhneigingin er samhverf,
# svo þetta er eitthvað aðeins annað en línulegt.

# Sjáum skýr merki um heteroskedasticity! Viljum þá væntanlega vinna með y-breytuna okkar.
# Áður en við gerum það, er réttast að skoða útlaga og áhrifamikla punkta,
# þar sem BoxCox og aðrar aðferðir eru sérstaklega næmar fyrir slíku.

## STIG 5: ÚTLAGAR OG MIKILVÆGIR PUNKTAR


# Greinum nú x-punkta með Mahalanobis/H_ii gildum:
# Skoðum matið okkar á y (y.hat) 
fortData$rn <- row.names(fortData)
fortData$index <- 1:nrow(fortData)
fortData$.jackknife <- rstudent(lm.fourth)
n <- nrow(fortData)
p <- nrow(summary(lm.fourth)$coefficients)

fortData %>%
  ggplot(aes(x = index, y = .hat)) +
  geom_point() +
  geom_hline(yintercept = 2*p/n, lty = 2, col = 'red') +
  geom_text(aes(label = ifelse(.hat > 2*p/n, rn, '')), hjust = 0.5, vjust = -0.5)

# Þetta er bara rugl að skoða, við þurfum að finna betri leið... Notum töflu:

fortData %>%
  filter(.hat > 2*p/n) %>%
  arrange(desc(.hat)) %>%
  dplyr::select(rn, .hat) %>%
  kbl(align = 'c') %>%
  kable_styling()

# Með því að skoða
fortData[fortData$rn == 10944,]
# sést að þessum punkti er spáð nákvæmlega rétt gildi og því er hann veigamikill.
# Hann er sem sagt veigamikill, því hann er að spá þessu svo vel, ætti ekki að skoða
# frekar. Aðrir gagnapunktar eru innan marka.

# Byrjum á því að greina y-punkta með jackknife og Cook's distance:

fortData %>%
  dplyr::select(.jackknife, rn, index) %>%
  gather(residual, jackknife, -index, -rn) %>%
  mutate(residual = factor(residual, 
                           levels = c('.jackknife'))) %>%
  ggplot(aes(x = index, y = jackknife, , label=rn)) +
  geom_point(aes(color = ifelse(abs(jackknife)>2, "darkred", "black"))) +
  geom_text(aes(label=ifelse(abs(jackknife)>2,rn,'')),hjust=0.5,vjust=-0.5) +
  geom_hline(yintercept = 0, lty = 2, col = 'red') +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = 1, lty = 2, col = 'blue') +
  geom_hline(yintercept = -1, lty = 2, col = 'blue') +
  geom_hline(yintercept = 2, lty = 2, col = 'red') +
  geom_hline(yintercept = -2, lty = 2, col = 'red') +
  geom_hline(yintercept = 3, lty = 2, col = 'green') +
  geom_hline(yintercept = -3, lty = 2, col = 'green') +
  geom_hline(yintercept = 0, lty = 2) + expand_limits(x = c(0, 282))

alpha <- 0.05
tCrit <- qt(p = 1 - alpha/(2 * n), n - p - 1)
outliers = fortData$rn[c(which(abs(fortData$.jackknife) > tCrit))]

fortData %>%
  ggplot(aes(x = index, y = .cooksd, label=rn)) +
  geom_point(aes(color = ifelse(.cooksd>0.1, "darkred", "black"))) +
  geom_text(aes(label=ifelse(.cooksd>0.1,rn,'')),hjust=-0.1,vjust=0.2) +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

theoreticalT <- qt(p = 1 - 0.05/(2 * n), df = n - p - 1)
suspicious <- c(25036, 23907, 21791, 18612, 34066, 11182, 8494)
fortData %>%
  filter(rn %in% suspicious) %>%
  mutate(highLeverage = .hat > 2*p/n,
         outlier = abs(.jackknife) > theoreticalT,
         influential = .cooksd > 0.15) %>%
  dplyr::select(rn, highLeverage, outlier, influential) %>%
  mutate(totalMarks = highLeverage + outlier + influential) %>%
  kbl(align = 'c') %>%
  kable_styling()

possible_rejects = c(23907, 25036, 18612)

fortData[fortData$rn %in% possible_rejects,] %>%
  kbl(align = 'c') %>%
  kable_styling()

# Ég er búinn að reyna að fletta upp þessari eign eftir fasteignanúmerinu 10106610 en
# ekkert gengur. Frá og með 8. apríl 2018 var fasteignanúmerum breytt og því er ekki
# lengur hægt að fletta því upp hvaða eignir þetta eru.
# Þetta er því sérstakt tilfelli, þar sem við getum ekkert vitað um þessa gagnapunkta.
# Við vitum hins vegar að þetta er langverðmætasta eignin í safninu okkar!
summary(data$nuvirdi)
# Þess vegna er sanngjarnt að taka gagnapunktinn út, þetta er örugglega ráðherrabústaðurinn
# við Tjarnargötu eða eitthvað. Það er allavega alveg rökfæranlegt að klippa hann út.

# Hinir tveir eru metnir á miklu hærra verð en þeir ættu að vera. Báðir liggja við
# Ægissíðu og eru metnir á verð sem kemur heim og saman við aðrar eignir þar:
summary(data[data$matssvaedi == 70,]$nuvirdi)
# Aðeins yfir meðallagi í stærð ibm2 en virðast annars vera nokkuð eðlilegar eignir.
# Hvers vegna er módelið að spá þeim svona hátt? Ég held að það sé ekki réttlætanlegt
# að klippa þá út úr módelinu, því við ættum ekki að vera spá svona hátt.
# Við viljum lagfæra þessa spá, ekki sópa henni undir teppið!

td5 = td4[td4$id != 25036,]

## STIG 4B: HELDUR LÍNULEIKI? UMBREYTINGAR

# Umbreytum y út frá BoxCox og skoðum lambda gildi
bcTest <- boxcox(lm( nuvirdi ~ ., td5))

indexOfLLPeak <- which.max(bcTest$y)
lambda <- bcTest$x[indexOfLLPeak]
# Lambda langt fyrir neðan 5, sem er góðs viti (sjá 8_transformations)
lambda
# Líklegast nákvæmlega 26/99

fortData$logy <- (fortData$nuvirdi^lambda - 1)/lambda

# Sjáum að logy hegðar sér betur (meira línulega) en venjulega núvirðið:
# (sameina á eitt plott með facet wrap eða eitthvað)
fortData %>% ggplot(aes(x = logy)) +
  geom_density()

fortData %>% ggplot(aes(x = nuvirdi)) +
  geom_density()

# Andhverfan er (26*y/99 + 1)^(99/26)

td5$logy <- (td5$nuvirdi^lambda - 1)/lambda
lm.fifth = lm(logy ~ . -nuvirdi, data = td5)
summary(lm.fifth)
adjusted_residuals = residuals(lm.fifth)^2
adjusted_residuals = (26*adjusted_residuals/99 + 1)^(99/26)
sqrt(mean(adjusted_residuals))

# Þetta er allt of gott til að vera satt, þurfum að skoða þetta betur....

# Skoðum aftur x-punkta og greinum frávik
# Skoðum partial residual / partial regression á x-gildum, ef þau eru nógu fá
# Ófullkomið

X <- model.matrix(lm.fifth)
partialRegPlots <- list()
for(i in 2:p) {
  delIndependentX <- X[, i]
  delX <- as.data.frame(X[, -c(1, i)])
  lmDelta <- lm(fortifyp1$PRICE ~ ., data = delX)
  lmGamma <- lm(delIndependentX ~ ., data = delX)
  resDelta <- residuals(lmDelta)
  resGamma <- residuals(lmGamma)
  betaPartialReg <- summary(lm(resDelta ~ resGamma))$coefficients[2, 1]
  tibble(x = resGamma,
         y = resDelta) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_abline(slope = betaPartialReg, intercept = 0, lty = 2, col = 'blue') +
    labs(x = TeX('$\\hat{\\gamma}$'), y = TeX('$\\hat{\\delta}$')) +
    ggtitle(colnames(p1)[i]) -> partialRegPlots[[(i - 1)]]
}
cowplot::plot_grid(partialRegPlots[[1]], partialRegPlots[[2]], partialRegPlots[[3]])

## STIG 6: Fitta nýtt líkan og skoða fleiri breytur til að taka út

# Ófullkomið


## STIG 7: Fara í ANCOVA greiningu á þeim breytum sem eru eftir inni

# Nota TukeyHSD
# Ófullkomið

## STIG 8: Fitta lokalíkanið og skrifa allt um það (RMSE, mean etc., R^2, blablabla)







