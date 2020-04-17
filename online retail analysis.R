retailonline.uci<-Online_Retail
retailonline.uci$Amount <- retailonline.uci$Quantity* retailonline.uci$UnitPrice
head(retailonline.uci[,-c(2,3)])
firstpart <- unique(retailonline.uci[,c(1,7,5)])
head(firstpart)
secondPart <- aggregate(list(Amount = retailonline.uci$Amount),by = list(InvoiceNo = retailonline.uci$InvoiceNo), FUN = sum)
head(secondPart)
dataRFM <- merge(firstpart,secondPart, by = "InvoiceNo")
head(dataRFM)
dataRFM$InvoiceDate <-as.Date(dataRFM$InvoiceDate, format = "%m/%d/%Y")
head(dataRFM)
dataRFM <-na.omit(dataRFM)
library(didrooRFM)
resultsRFM <-findRFM(dataRFM, recencyWeight = 4, frequencyWeight = 4, monetoryWeight = 4)
head(resultsRFM[,c(1:4)])
head(resultsRFM[,c(1,8:10,16)])
tblClass <-table(resultsRFM$FinalCustomerClass)
tblClass
barplot(tblClass)
retail.nondup <-retailonline.uci[!duplicated(retailonline.uci$CustomerID),c(7,8)]
head(retail.nondup)
RFMCountry <- merge(resultsRFM[,c(1,8:10,16)],retail.nondup, by = "CustomerID")
colnames(RFMCountry) <- c("ID","Money","Frequency","Recency","Class","Country")
head(RFMCountry)
table(RFMCountry$Country,RFMCountry$Class)
totwss <- NULL
for (i in 2:15) {
  totwss <- append(totwss,kmeans(resultsRFM[,8:10],centers = i)$tot.withinss)
}
plot(x = 2:15, y = totwss, type = "b", xlab = "Clusters", ylab = "Total Within SS")
rounded.clusters <-as.data.frame.matrix(round(crs$kmeans$centers))
  