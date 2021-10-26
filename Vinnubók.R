setwd("~/Documents/CS HI/2021-22/HLT/Verkefni 5")
library(reshape2)
library(MASS)
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
data <- data %>% mutate(teg_eign2=ifelse(teg_eign=="Ibudareign", teg_eign, "Sereign"))

# BREYTUM Í FLOKKUNARBREYTUR
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

# Athugum tengsl ibteg og teg_eign
group_by(data, ibteg, teg_eign) %>% count()
# Þetta bendir til að gott væri að sleppa ibteg
# data <- subset(data, -select = ibteg)

# Skoðum fjölda staka í mismunandi flokkum
factorNames <- colnames(select(data, where(is.factor)))
data[ , factorNames] %>%
  gather(var, val) %>%
  group_by(var, val) %>% count() %>% view()

# Ástæða til að
# Droppa svfn, allir punktar eins
# droppa rfastnum, þar sem að við vitum að það er bara ID og inniheldur engar upplýsingar um fasteign

# TRAIN TEST SET
training_size = floor(0.75 * nrow(data))
trainingSampleRowId <- sample(1:nrow(data), size = training_size, replace = F)
data_train <- data[trainingSampleRowId, ]
data_test <- data[-trainingSampleRowId, ]

# droppa óþarfa dálkum
data_test <- subset(data_test, select = -c(svfn, rfastnum))
data_train <- subset(data_train, select = -c(svfn, rfastnum))

lm.first = lm(nuvirdi ~ ., data = data_train)
summary(lm.first)
summary(residuals(lm.first)^2)

# Plottum tölulegarbreytur og tengsl þeirra
numericNames <- colnames(select(data, where(is.numeric)))
pm <- ggpairs(
  data, columns = numericNames, lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))
pm
# Ath, er enn þörf á að gera þetta læsilegra eða erum við farnir annað?

# Einfaldasta módelið, verð sem fall af fermetrum.
# Gæti verið baseline
lm.simple <- lm(nuvirdi ~ ibm2, data = data_train)
summary(lm.simple)

# Skoðum fylgni breyta
heatmap(cor(data[numericNames])) 


cor(data[numericNames]) %>%
  as_tibble(rownames = 'var') %>%
  melt() %>% # from reshape2
  ggplot(aes(x = var, y = variable, fill = value)) +
  geom_tile() + theme(axis.text.x = element_text(angle = -45, vjust = -0.5, hjust=0.5))

group_by(data, undirmatssvaedi) %>% count()
# Einu hóparnir sem fá lágt p-gildi eru 3 og 6 (Ægissíða, Vesturbær NA við Hringbraut) en aðeins 7 og 4 stök falla þar undir
# Fjölmennustu hóparnir, 21 og 28 (Vesturberg í Breiðholti og Blokkir við Kringlumýrabraut), virðast skipta litlu máli
# Mér finnst þetta ástæða til að sleppa undirmatssvæðum í heild sinni.

# Einnig ástæða til að droppa fjeld og fjbilast þar sem þær virðast engin áhrif hafa
# Ath ibteg og teg_eign2

data_mod_test <- subset(data_test, select = -c(undirmatssvaedi, fjeld, fjbilast, ibteg, teg_eign2))
data_mod_train <- subset(data_train, select = -c(undirmatssvaedi, fjeld, fjbilast, ibteg, teg_eign2))

lm.dev <- lm(nuvirdi ~ ., data = data_mod_train)
summary(lm.dev)

# Athugum stepWise. 

stepWiseLM <- stepAIC(object = lm.first, direction = 'both', trace = T)
summary(stepWiseLM)
# Sleppur undirmatssvæði 

# MIKILVÆGAST: PLOT RESIDUAL Á MÓTI FITTED
fortData <- fortify(lm.first)
fortData %>%
  ggplot(aes(x = .fitted, y = .resid, color = matssvaedi)) +
  geom_jitter(width = 0.25)

fortData %>%
  ggplot(aes(x = .fitted, y = .resid, color = teg_eign)) +
  geom_jitter(width = 0.25) 

#fortStep <- fortify(stepWiseLM)
#fortStep %>%
#  ggplot(aes(x = .fitted, y = .resid, color = matssvaedi)) +
#  geom_jitter(width = 0.25)

library(kableExtra)
tibble("Fastanúmer íbúðar", "Kaupdagur", "Tegund eignar", "Svæðisnúmer", "Byggingarár", "Fjöldi Hæða", "Fjöldi lyfta", "Fermetrafjöldi", "Fjöldi hæða", "Fjöldi bílastæða", "Fjöldi Baðkara", "Fjöldi sturta", "Fjöldi klósetta", "Fjöldi eldhúsa", "Fjöldi herbergja", "Fjöldi stofa", "fjöldi geymsla", "Stig framkvæmdar", "Matssvæði", "Undirmatssvæði", "Tegund íbúðar") %>%
  kbl(align = 'c') %>%
  kable_styling()
