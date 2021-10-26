## STIG 0: FORVINNSLA

# Hlöðum inn gagnasettinu og veljum undirsvæðin:
setwd("~/Desktop/H21/Hagnýtt línuleg tölfræðilíkön/Heimadæmi/HLT-V5")

library(MASS)
library(tidyverse)
library(GGally)
library(knitr)
library(kableExtra)
library(FitAR)
library(broom)
library(forcats)
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
td3 = subset( td2, select = -c(fjeld, fjbilast, stig10) )
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

# Sjáum að -ibteg stendur sig betur en -teg_eign, svo við veljum út ibteg
# Núna stækkar R^2 á meðan RMSE minnkar, svo þetta er gott...

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


# Byrjum á því að greina y-punkta með jackknife og Cook's distance:

fortData %>%
  dplyr::select(.jackknife, rn, index) %>%
  gather(residual, jackknife, -index, -rn) %>%
  mutate(residual = factor(residual, 
                           levels = c('.jackknife'))) %>%
  ggplot(aes(x = index, y = jackknife, label=rn)) +
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
suspicious <- c(25036, 23907, 21791, 18612, 34066, 11182, 8494, 4216)
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
hist(data$nuvirdi)
# Þess vegna er sanngjarnt að taka gagnapunktinn út, þetta er örugglega ráðherrabústaðurinn
# við Tjarnargötu eða eitthvað. Það er allavega alveg rökfæranlegt að klippa hann út.

# Hinir tveir eru metnir á miklu hærra verð en þeir ættu að vera. Báðir liggja við
# Ægissíðu og eru metnir á verð sem kemur heim og saman við aðrar eignir þar:
summary(data[data$matssvaedi == 70,]$nuvirdi)
hist(data[data$matssvaedi == 70,]$nuvirdi)
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

td5$logy <- bxcx(td5$nuvirdi, lambda, InverseQ = FALSE, type = "BoxCox")
lm.fifth = lm(logy ~ . -nuvirdi, data = td5)
summary(lm.fifth)

fortLogy = fortify(lm.fifth)
# Sjáum að logy hegðar sér betur (meira línulega) en venjulega núvirðið:
# (sameina á eitt plott með facet wrap eða eitthvað)
fortLogy%>% ggplot(aes(x = logy)) +
  geom_density()

fortLogy %>% ggplot(aes(x = nuvirdi)) +
  geom_density()

asd <- bxcx(residuals(lm.fifth), lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(asd^2))

# Þetta er allt of gott til að vera satt, þurfum að skoða þetta betur....

# Skoðum aftur x-punkta og greinum frávik í næsta kafla

fortLogy$rn <- row.names(fortLogy)
fortLogy$index <- 1:nrow(fortLogy)
fortLogy$.jackknife <- rstudent(lm.fifth)
n5 <- nrow(fortLogy)
p5 <- nrow(summary(lm.fifth)$coefficients)

betas <- c("(Intercept", "kdagur", "teg_eignIbudareign", "teg_eignParhus", "teg_eignRadhus", 
           "byggar", "haednr", "lyftaTRUE", "ibm2", "fjbkar", "fjsturt", "fjstof", "fjgeym", 
           "stig10", "matssvaedi70", "matssvaedi91", "matssvaedi160", "matssvaedi281", 
           "undirmatssvaedi3", "undirmatssvaedi6", "undirmatssvaedi21", "undirmatssvaedi28", 
           "undirmatssvaedi40", "undirmatssvaedi48", "undirmatssvaedi54", "id")

beta1 <- summary(lm.fifth)$coefficients[2, 1]
beta2 <- summary(lm.fifth)$coefficients[3, 1]
beta3 <- summary(lm.fifth)$coefficients[4, 1]
beta4 <- summary(lm.fifth)$coefficients[5, 1]
beta5 <- summary(lm.fifth)$coefficients[6, 1]
beta6 <- summary(lm.fifth)$coefficients[7, 1]
beta7 <- summary(lm.fifth)$coefficients[8, 1]
beta8 <- summary(lm.fifth)$coefficients[9, 1]
beta9 <- summary(lm.fifth)$coefficients[10, 1]
beta10 <- summary(lm.fifth)$coefficients[11, 1]
beta11 <- summary(lm.fifth)$coefficients[12, 1]
beta12 <- summary(lm.fifth)$coefficients[13, 1]
beta13 <- summary(lm.fifth)$coefficients[14, 1]
beta14 <- summary(lm.fifth)$coefficients[15, 1]
beta15 <- summary(lm.fifth)$coefficients[16, 1]
beta16 <- summary(lm.fifth)$coefficients[17, 1]
beta <- c(beta1, beta2, beta3, beta4,beta5, beta6, beta7, beta8,
          beta9, beta10, beta11, beta12,beta13, beta14, beta15, beta16)
plots <- list()
for(i in 1:length(beta)) {
  regressor <-  model.matrix(lm.fifth)[, (i + 1)]
  partialRes <- fortLogy$.resid + regressor * beta[i]
  tibble(x = regressor,
         y = partialRes) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    stat_smooth(method = 'lm', se = F) +
    labs(x = betas[i + 1],
         y = 'Partial residual') -> plots[[i]]
}
cowplot::plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                   plots[[5]], plots[[6]], plots[[7]], plots[[8]],
                   plots[[9]], plots[[10]], plots[[11]], plots[[12]],
                   plots[[13]], plots[[14]], plots[[15]], plots[[16]],
                   nrow = 4, ncol = 4)
cowplot::plot_grid(plots[[1]], plots[[5]], plots[[6]], plots[[8]],
                   nrow = 2, ncol = 2)

# Sjáum smá dæmi um ólínuleika í plotti 8, sem ætti að svara til ibm2.
# Plottin að ofan svara til kaupdags, byggingarár, hæðarnúmer og fermetrar.

# Við viljum eflaust vinna meira með fermetrabreytuna...

lmOrthoPolyIBM2 <- lm(td5$logy ~ poly(td5$ibm2, 3))
summary(lmOrthoPolyIBM2)

summary(lm.fifth)

td6 <- subset(td5, select = -c(lyfta, fjsturt))
td6$ibm22 <- td6$ibm2^2
# MJÖG significant, prófum samt að velja bara 2. veldi fyrst

lmOrthoPolyBYGGAR <- lm(td5$logy ~ poly(td5$byggar, 3))
summary(lmOrthoPolyBYGGAR)
# Ekki nógu significant, eða hvað?

lm.sixth <- lm(logy ~ . -nuvirdi -id, data = td6)
summary(lm.sixth)

rev_resid_6 <- bxcx(residuals(lm.sixth), lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(rev_resid_6^2))
test_data$logy <- bxcx(test_data$nuvirdi, lambda, InverseQ = FALSE, type = "BoxCox")
test_data$ibm22 <- test_data$ibm2^2

test_resid = (predict(lm.sixth, test_data) - test_data$logy)

rev_resid_test <- bxcx(test_resid, lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(rev_resid_test^2))

## Fara yfir það hvernig andhverfa y er fengin, gera það rétt
## Fara yfir það hvort betra sé að tiltaka poly(td5$ibm2, 3) inní lm eða 
# bæta við breytum
## Finna leið til þess að bera RMSE almennilega saman. Þetta er rugl gott núna - overfitting?

# STIG 6: AFTUR Í GREININGU:

# Greinum nú x-punkta með Mahalanobis/H_ii gildum:
# Skoðum matið okkar á y (y.hat) 

fortNG = fortify(lm.sixth)

fortNG$rn <- row.names(fortNG)
fortNG$index <- 1:nrow(fortNG)
fortNG$.jackknife <- rstudent(lm.sixth)
n6 <- nrow(fortNG)
p6 <- nrow(summary(lm.sixth)$coefficients)

fortNG %>%
  ggplot(aes(x = index, y = .hat)) +
  geom_point() +
  geom_hline(yintercept = 2*p6/n6, lty = 2, col = 'red') +
  geom_text(aes(label = ifelse(.hat > 2*p6/n6, rn, '')), hjust = 0.5, vjust = -0.5)

# Þetta er bara rugl að skoða, við þurfum að finna betri leið... Notum töflu:

fortNG %>%
  filter(.hat > 2*p/n) %>%
  arrange(desc(.hat)) %>%
  dplyr::select(rn, .hat) %>%
  kbl(align = 'c') %>%
  kable_styling()

# Byrjum á því að greina y-punkta með jackknife og Cook's distance:

fortNG %>%
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
tCrit <- qt(p = 1 - alpha/(2 * n6), n6 - p6 - 1)
outliers = fortNG$rn[c(which(abs(fortNG$.jackknife) > tCrit))]

fortNG %>%
  ggplot(aes(x = index, y = .cooksd, label=rn)) +
  geom_point(aes(color = ifelse(.cooksd>0.1, "darkred", "black"))) +
  geom_text(aes(label=ifelse(.cooksd>0.1,rn,'')),hjust=-0.1,vjust=0.2) +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

theoreticalT <- qt(p = 1 - 0.05/(2 * n6), df = n6 - p6 - 1)
suspicious <- c(34066, 23907, 6099, 4216, 11182, 25074)
fortNG %>%
  filter(rn %in% suspicious) %>%
  mutate(highLeverage = .hat > 2*p/n,
         outlier = abs(.jackknife) > theoreticalT,
         influential = .cooksd > 0.15) %>%
  dplyr::select(rn, highLeverage, outlier, influential) %>%
  mutate(totalMarks = highLeverage + outlier + influential) %>%
  kbl(align = 'c') %>%
  kable_styling()

possible_rejects = c(34066, 23907, 11182, 25074)

fortNG[fortNG$rn %in% possible_rejects,] %>%
  kbl(align = 'c') %>%
  kable_styling()


hist(td6$logy)
hist(td6$nuvirdi)
# Getum skoðað þessa gagnapunkta eitthvað frekar en ég sé ekkert í fljótu bragði að þeim...

summary(data[data$matssvaedi == 70,]$nuvirdi)
hist(data[data$matssvaedi == 70,]$nuvirdi)

betas2 <- c("(Intercept", "kdagur", "teg_eignIbudareign", "teg_eignParhus", "teg_eignRadhus", 
           "byggar", "haednr", "ibm2", "fjbkar", "fjstof", "fjgeym", "matssvaedi70", 
           "matssvaedi91", "matssvaedi160", "matssvaedi281", 
           "undirmatssvaedi3", "undirmatssvaedi6", "undirmatssvaedi21", "undirmatssvaedi28", 
           "undirmatssvaedi40", "undirmatssvaedi48", "undirmatssvaedi54", "ibm22")

beta1 <- summary(lm.sixth)$coefficients[2, 1]
beta2 <- summary(lm.sixth)$coefficients[3, 1]
beta3 <- summary(lm.sixth)$coefficients[4, 1]
beta4 <- summary(lm.sixth)$coefficients[5, 1]
beta5 <- summary(lm.sixth)$coefficients[6, 1]
beta6 <- summary(lm.sixth)$coefficients[7, 1]
beta7 <- summary(lm.sixth)$coefficients[8, 1]
beta8 <- summary(lm.sixth)$coefficients[9, 1]
beta9 <- summary(lm.sixth)$coefficients[10, 1]
beta10 <- summary(lm.sixth)$coefficients[11, 1]
beta11 <- summary(lm.sixth)$coefficients[12, 1]
beta12 <- summary(lm.sixth)$coefficients[13, 1]
beta13 <- summary(lm.sixth)$coefficients[14, 1]
beta14 <- summary(lm.sixth)$coefficients[15, 1]
beta15 <- summary(lm.sixth)$coefficients[16, 1]
beta16 <- summary(lm.sixth)$coefficients[17, 1]
beta17 <- summary(lm.sixth)$coefficients[18, 1]
beta18 <- summary(lm.sixth)$coefficients[19, 1]
beta19 <- summary(lm.sixth)$coefficients[20, 1]
beta20 <- summary(lm.sixth)$coefficients[21, 1]
beta21 <- summary(lm.sixth)$coefficients[22, 1]
beta22 <- summary(lm.sixth)$coefficients[23, 1]
beta <- c(beta1, beta2, beta3, beta4,beta5, beta6, beta7, beta8,
          beta9, beta10, beta11, beta12,beta13, beta14, beta15, beta16,
          beta17, beta18, beta19, beta20, beta21, beta22)
plots <- list()
for(i in 1:length(beta)) {
  regressor <-  model.matrix(lm.sixth)[, (i + 1)]
  partialRes <- fortNG$.resid + regressor * beta[i]
  tibble(x = regressor,
         y = partialRes) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    stat_smooth(method = 'lm', se = F) +
    labs(x = betas2[i + 1],
         y = 'Partial residual') -> plots[[i]]
}
cowplot::plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                   plots[[5]], plots[[6]], plots[[7]], plots[[8]],
                   plots[[9]], plots[[10]], plots[[11]], plots[[12]],
                   plots[[13]], plots[[14]], plots[[15]], plots[[16]], 
                   plots[[17]], plots[[18]], plots[[19]], plots[[20]], 
                   plots[[21]], plots[[22]],
                   nrow = 6, ncol = 4)
cowplot::plot_grid(plots[[1]], plots[[5]], plots[[7]], plots[[22]],
                   nrow = 2, ncol = 2)


cowplot::plot_grid(plots[[6]], plots[[9]], plots[[10]], plots[[8]],
                   nrow = 2, ncol = 2)

lmOrthoPolyHAEDIR <- lm(td6$logy ~ poly(td6$haednr, 3))
summary(lmOrthoPolyHAEDIR)

td7 <- subset(td6, select = -c(fjbkar))
td7$haednr2 <- td7$haednr^2
td7$haednr3 <- td7$haednr^3

lm.seventh <- lm(logy ~ . -nuvirdi -id, data = td7)
summary(lm.seventh)

rev_resid_7 <- bxcx(residuals(lm.seventh), lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(rev_resid_7^2))
test_data$logy <- bxcx(test_data$nuvirdi, lambda, InverseQ = FALSE, type = "BoxCox")
test_data$ibm22 <- test_data$ibm2^2
test_data$haednr2 <- test_data$haednr^2
test_data$haednr3 <- test_data$haednr^3

test_resid = (predict(lm.seventh, test_data) - test_data$logy)

rev_resid_test <- bxcx(test_resid, lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(rev_resid_test^2))

## STIG 7: Fara í ANOVA greiningu á þeim breytum sem eru eftir inni

## MATSSVÆÐI:

## Bakfæra fyrir logy umbreytingu, allt saman:

ggplot(td7, aes(x=matssvaedi, y=logy)) + geom_boxplot()

group_by(td7, matssvaedi) %>%
  summarise(
    count = n(),
    mean = mean(logy, na.rm = TRUE),
    sd = sd(logy, na.rm = TRUE)
  ) %>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

matssv.lm <- lm(logy ~ matssvaedi, data = td7)
tidy(summary(matssv.lm))%>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

fit.matssv <- aov(logy ~ matssvaedi, data = td7)
tidy(fit.matssv) %>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

tukeymatssv <- TukeyHSD(fit.matssv)

data.frame(tukeymatssv[1:1]) %>% kbl(align ='c') %>%
  kable_styling(latex_options = "HOLD_position")

# Möguleiki á að sameina 281-70-25 og halda 160 og 91 aðskildum

## TEGUND EIGNAR:

ggplot(td7, aes(x=teg_eign, y=logy)) + geom_boxplot()

group_by(td7, teg_eign) %>%
  summarise(
    count = n(),
    mean = bxcx(mean(logy, na.rm = TRUE), lambda, InverseQ = TRUE, type = "BoxCox"),
    sd = bxcx(sd(logy, na.rm = TRUE), lambda, InverseQ = TRUE, type = "BoxCox")
  ) %>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

teg_eign_check.lm <- lm(logy ~ teg_eign, data = td7)
tidy(summary(teg_eign_check.lm))%>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

fit.teg_eign_check <- aov(logy ~ teg_eign, data = td7)
tidy(fit.teg_eign_check) %>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

tukeyteg_eign_check <- TukeyHSD(fit.teg_eign_check)

data.frame(tukeyteg_eign_check[1:1]) %>% kbl(align ='c') %>%
  kable_styling(latex_options = "HOLD_position")

# Gætum klárlega sameinað parhús og einbýlishús, út frá þessum tölum.

## UNDIRMATSSVÆÐI:

ggplot(td7, aes(x=undirmatssvaedi, y=logy)) + geom_boxplot()

group_by(td7, undirmatssvaedi) %>%
  summarise(
    count = n(),
    mean = mean(logy, na.rm = TRUE),
    sd = sd(logy, na.rm = TRUE)
  ) %>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

undirmatssvaedi_check.lm <- lm(logy ~ undirmatssvaedi, data = td7)
tidy(summary(undirmatssvaedi_check.lm))%>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

fit.undirmatssvaedi_check <- aov(logy ~ undirmatssvaedi, data = td7)
tidy(fit.undirmatssvaedi_check) %>% kbl(align = 'c') %>%
  kable_styling(latex_options = "HOLD_position")

tukeyundirmatssvaedi_check <- TukeyHSD(fit.undirmatssvaedi_check)

data.frame(tukeyundirmatssvaedi_check[1:1]) %>% kbl(align ='c') %>%
  kable_styling(latex_options = "HOLD_position")


# 54-0 998
# 21-3 999
# 28-3 992
# 40-6 9999
# 54-6 9999
# 28-21 999
# 48-49 9999
# 54-40 991
# 54-48 999

# Sameina 6-40-48-54?
# Sameina 3-21-28?
# 0 er stakt

td8 <- td7
         
td8$undirmatssvaedi <- fct_collapse(td8$undirmatssvaedi, "3/21/28" = c('3','21','28'), "6/40/48/54" = c('6','40','48','54'))

td8$matssvaedi <- fct_collapse(td8$matssvaedi, "25/70/281" = c('25','70','281'))

td8$teg_eign <- fct_collapse(td8$teg_eign, "Parhus/Einbyli" = c('Parhus','Einbylishus'))

lm.eigth <- lm(logy ~ . -nuvirdi -id, data = td8)
summary(lm.eigth)

rev_resid_8 <- bxcx(residuals(lm.eigth), lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(rev_resid_8^2))
test_data$logy <- bxcx(test_data$nuvirdi, lambda, InverseQ = FALSE, type = "BoxCox")
test_data$ibm22 <- test_data$ibm2^2
test_data$haednr2 <- test_data$haednr^2
test_data$haednr3 <- test_data$haednr^3

test_data$matssvaedi <- fct_collapse(test_data$matssvaedi, "25/70/281" = c('25','70','281'))

test_data$teg_eign <- fct_collapse(test_data$teg_eign, "Parhus/Einbyli" = c('Parhus','Einbylishus'))

test_resid = (predict(lm.eigth, test_data) - test_data$logy)

rev_resid_test <- bxcx(test_resid, lambda, InverseQ = TRUE, type = "BoxCox")
sqrt(mean(rev_resid_test^2))

group_by(td7, teg_eign) %>% count()


## STIG 8: Fitta lokalíkanið og skrifa allt um það (RMSE, mean etc., R^2, blablabla)







