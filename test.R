library(QCA)
library(myQCA)

data("CVF")

truth<-truthTable(CVF, "PROTEST", incl.cut=0.8, sort.by="incl")
pretty(truth)
pretty(truth,TRUE)

sol<-minimize(truth, details = TRUE, include="?", dir.exp=c(1,1,1,0,1))
pretty(sol)