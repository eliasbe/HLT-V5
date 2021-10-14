
# Þú þarft væntanlega að breyta þessu
setwd("~/Documents/CS HI/2021-22/HLT/Verkefni 5")

library(tidyverse)
library(GGally)

data <- read.table("gagnasafn_endurmat2017_litid.csv", header = T, sep = ",") #, col.names = dalkar

set.seed(11)

summary(data)

str(data)

# Spurning um að hafa svfn (póstnúmer? svæðisnúmer?) flokkunarbreytu
# lyfta logical
# matssvæði og undirmatssvæði categorical
# ibteg, numerical?

