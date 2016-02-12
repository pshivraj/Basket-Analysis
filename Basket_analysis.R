#reading data and formatting to sparse matrix
basket <- read.csv('basket.csv',sep='\t')
mydata <- basket[-grep("undefined",basket$external_data),]
mydata <- mydata[-grep("---",mydata$external_data),]
lookup <- read.csv('map.csv')
mydata <- merge(x = mydata, y = lookup, by = "external_data", all.y = TRUE)
library(dplyr)
xx <- as.data.frame(mydata %>% 
                      group_by(user_id_64) %>%
                      summarise(pages=paste(Category1, collapse=',')))
write.csv(xx[,2], "basket_1.csv")
library(arules)
library(datasets)
library(arulesViz)
products <- read.transactions("basket_1.csv",sep=",",format= "basket",rm.duplicates=T)
itemFrequencyPlot(products,topN=20,type="absolute")
rules <- apriori(products, parameter = list(supp = 0.001, conf = 0.5))

inspect(rules[1:5])
summary(rules)
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])
rules<-apriori(data=products, parameter=list(supp = 0.001, conf = 0.5),
               appearance = list(default="lhs",rhs="Schlafen"),control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="lift")
inspect(rules[1:5])
