
# Þú þarft væntanlega að breyta þessu
setwd("~/Documents/CS HI/2021-22/HLT/Verkefni 5")

library(tidyverse)
library(GGally)
set.seed(11)


data <- read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ",") #, col.names = dalkar

data[ ,"kdagur"] <- as.Date(data[ ,"kdagur"])
data[ ,"teg_eign"] <- as.factor(data[ ,"teg_eign"])
data[ ,"svfn"] <- as.factor(data[ ,"svfn"])
# Ath að í verkefni segir að "lyfta" sé binary en í gögnum virðist hún vera fjöldi lyfta.
data[ ,"lyfta"] <- data[,"lyfta"]>0 # Kannski halda, kannski sleppa

summary(data)

str(data)

data[data["nuvirdi"]==203781]

# Spurning um að hafa svfn (póstnúmer? svæðisnúmer?) flokkunarbreytu
# lyfta logical - nei virðist vera fjöldi lyfta
# matssvæði og undirmatssvæði categorical
# ibteg, numerical?

