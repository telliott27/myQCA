library(QCA)
library(myQCA)

data("CVF")

truth<-truthTable(CVF, "PROTEST", incl.cut=0.8, sort.by="incl")
pretty(truth)

sol<-minimize(truth, details = TRUE)
