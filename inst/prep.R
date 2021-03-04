beazley <- read.csv("../data/beazley_archive.csv")
beazley <- beazley[which(beazley$Fabric == "ATHENIAN"), ]
beazley <- beazley[-which(beazley$Date == ""), ]
beazley$Date <- as.character(beazley$Date)


data("Beazley")


beazley$Date_new <- strsplit(beazley$Date, " ")
beazley$DAT_min <- NA
beazley$DAT_max <- NA

for (i in 1:nrow(beazley)) {
  if (beazley$Date_new[i][[1]][1] == "to") {
    print("Nope.")
  } else {
    print("Yip.")
    beazley$DAT_min[i] <- beazley$Date_new[i][[1]][1]
    beazley$DAT_max[i] <- beazley$Date_new[i][[1]][3]
  }
}
beazley <- beazley[-which(is.na(beazley$DAT_min)), ]


beazley$Attributed.To <- as.character(beazley$Attributed.To)
beazley$Attributed.To  <- strsplit(beazley$Attributed.To , " by ")
beazley$Attributed.To <- gsub(beazley$Attributed.To, pattern = " by .*.",
                              replacement = "")


beazley <- beazley[,-c(11:20)]
beazley <- beazley[,-9]
beazley <- beazley[,-10]

str(beazley)

testset <- beazley[,c(2,4,6,10,11)]

testset$Shape <- as.character(testset$Shape.Name)
testset$Shape <- gsub(testset$Shape, pattern = ",", replacement = "")
testset$Shape <- gsub(testset$Shape, pattern = " (?)", replacement = "")
testset$Shape  <- strsplit(testset$Shape , " ")

for (i in 1:nrow(testset)) {
#    print(testset$Shape[i][[1]][1])
    testset$Shape.Name[i] <- testset$Shape[i][[1]][1]
}

testset <- testset[-which(testset$Shape.Name == "FRAGMENT"), ]
testset <- droplevels(testset)
unique(testset$Shape.Name)
testset <- testset[,1:5]
testset <- na.omit(testset)

write.table(testset, file = "inst/data/testset_beazley_shape.csv", sep = ";")


sample <- sample(1:nrow(df), 1000, replace = FALSE)
df <- df[sample, ]
str(df)
head(df)
write.table(df, file = "inst/data/testset_beazley_1000.csv", sep = ";")

