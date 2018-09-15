library(glmnet)  # for generalized linear regression
service <- read.csv("Datathon Materials/311_service_requests.csv", stringsAsFactors = F)
service$cdate <- strptime(service$created_date, "%m/%d/%Y %l:%M:%S %p")

service <- service[order(service$cdat), ]
write.csv(service, file="Datathon Materials/311_service_requests_sort.csv")

service0 <- read.csv("Datathon Materials/311_features_byd.csv", stringsAsFactors = F)
service_cor <- cor(service0[,-1], method = "spearman")

service0 <- read.csv("Datathon Materials/311_features.csv", stringsAsFactors = F)

season <- rep(1:3, each = 4)
service0$month <- c(rep(season, 8), 1,1,1) 
service1 <- service0 
# regression against cycle
for(k in 2:14)
{
  res.glm <- glm(service0[,k] ~ as.factor(service0$month), family = poisson(link = "identity"))
  
  service1[,k] <- res.glm$residuals
  
}

write.csv(service1, file="311_features_rm_season.csv")


service_cor <- cor(service1[,c(-1, -15)], method = "spearman")
library(pheatmap)
pdf("zhu/311_calls_cor_heatmap_rmseason.pdf", onefile = T)
pheatmap(service_cor)
dev.off()

# get PC
sng = service1[,c(-1, -15)]
sng[sng > 0] = 1
sng[sng < 0] = -1

###pcs <- svd(scale(sng *log(abs(service1[,c(-1, -15)]))))
pcs <- svd(scale(service1[,c(-1, -15)]))
rownames(pcs$v) <- colnames(service1)[c(-1, -15)]
pcs$v[order(-abs(pcs$v[,2])),2]

plot(pcs$u[,1])
plot(pcs$u[,2])

write.csv(pcs$u, file="311_feature_pc1.csv")


library(tidyr)
library(dplyr)
service_per <- cbind("X_date" = service0[,1], service0[,-1]/rowSums(service0[,-1]))
service2 =  service1[,-15] 
df <- service2 %>% #select(X_date, Food.Poisoning, Sanitation.Condition) 
  gather(key = "variable", value = "value", -X_date)
head(df, 3)

pdf("zhu/bills_rm_season_addchange.pdf", width = 10)
ggplot(df1, aes(x = as.Date(X_date), y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + geom_vline(data = cps2, aes(xintercept = as.Date(V2)),col='grey25',linetype=3) +geom_vline(data = cps2, aes(xintercept = as.Date(V3)),col='grey25',linetype=3) +geom_vline(data = cps2, aes(xintercept = as.Date(V4)),col='grey25',linetype=3) + facet_wrap(~variable, scale="free") +
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +theme_minimal()
dev.off()

# add change point
cps <- read.table("JZ/complaint_type_freq_bkps_filename.txt", sep="\t")
colnames(cps)[1] = "variable"
cps2 <- cps
for(k in 2:5)
{
  cps2[,k] = service1[cps2[,k],1]
}




####
service_sub <- service[1:5000, ]
plot(service_sub$latitude, service_sub$longitude, col=as.factor(service_sub$complaint_type), pch=16)
plot(service_sub$cdate, as.factor(service_sub$complaint_type))

gplots::venn(list(unique(service$city), unique(food_q$city)))

health <- read.csv("Datathon Materials/community_health.csv") # 2008-2015, yearly, county_name
health1 <- health[health$unit_measurement == "Rate",]
pdf("zhu/boxplot_health1_year_topic.pdf")
ggplot(health1, aes(x=year, y=precent_or_rate, fill=year)) + 
  geom_boxplot() +
  facet_wrap(~health_topic, scale="free")
dev.off()

# lat_long health




food_q<-read.csv("Datathon Mateials/food_establishment_inspections.csv", stringsAsFactors = F)

food_q_sub <- food_q[1:10000,]
plot(food_q_sub$latitude, food_q_sub$longitude, col=as.factor(food_q_sub$violation_item),xlim=c(40,50), ylim=c(-80,-70))

food <- read.csv("Datathon Materials/food_venues.csv")
food_ny <- food[food$state=="NY", ]


## 

bills <- read.csv("Datathon Materials/bills_bin.csv")
bills_per <- cbind("X_date" = bills[,1], (bills[,-1]+1)/rowSums(bills[,-1]+1))

combine <- merge(bills, service2, by= "X_date")
pheatmap::pheatmap(cor(combine[,-1], use = "pairwise.complete.obs"))


df1 <- bills%>% #select(X_date, Food.Establishment) %>%
  gather(key = "variable", value = "value", -X_date)
head(df1, 3)

df <- service2 %>% select(X_date, Food.Establishment) %>%
  gather(key = "variable", value = "value", -X_date)
head(df, 3)

ggplot(df, aes(x = as.Date(X_date), y = value)) + 
  geom_line(aes(color = 1), size = 1) + 
  geom_line(data = df1, aes(x = as.Date(X_date), y = log(value), color = 2), size = 1) + 
  theme_minimal() 
  

ggplot(df1, aes(x = as.Date(X_date), y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + facet_wrap(~variable, scale="free") +
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +theme_minimal()

## group bills

## bills


bills_smoke <- bills$Indoor.Air.Quality+  bills$Smoking + bills$Dirty.Conditions

bills_rodent <- bills$Food.Establishment+  bills$Rodent 

bills1 <- data.frame("smoke" = bills_smoke, "food" = bills_rodent, "sum" = rowSums(bills[,-1]) )



bills1_per <- data.frame("X_date" = bills[,1], bills1 /rowSums(bills[,-1]+1))
combine <- merge(bills1_per, service2, by= "X_date", all=T)
summary(glm(combine$Rodent[-1]~combine$food))

bills1$year = c(rep(2011:2014, each = 12), rep(2015, 9))


bills1[,1:2] <- bills1[,1:2]/bills1[,3]
ggplot(data=bills1, aes(x=year, y=food)) +
  geom_bar(stat="identity")


bills$year = c(rep(2011:2014, each = 12), rep(2015, 9))
sums <- sapply(2011:2015, function(x) colSums(bills[bills$year == x,2:14]))
sums = t(sums)
sums = data.frame(sums)
sums$sum = rowSums(sums)
##sums[,-15] <- sums[,1:2]/sums[,3]
sums$year = 2012:2016

cq <- matrix(0, 5, 13)
for(y in 1:5)
{
  for(j in 1:13)
{
  cq[y, j] <- chisq.test(matrix(c(sums[y,j], sum(sums[-y,j]), sums[y, 14] , sum(sums[-y, 14])), ncol=2))$p.value
  }
}

colnames(cq) <- colnames(sums)[1:13]


sum_per = sums[,1:13]/sums[,14]
sum_per$year=sums$year

df <- sum_per %>% 
    gather(key = "variable", value = "value", -year)

library(gridExtra)

pdf("zhu/bill_barplot.pdf")
ggplot(data=df, aes(x=year, y=value)) +
  geom_bar(stat="identity") +  facet_wrap(~variable, scale="free")
dev.off()



ggplot(df, aes(x = as.Date(X_date), y = value)) + 
  geom_line(aes(color = variable), size = 1) + 
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  ) + geom_vline(data = cps2, aes(xintercept = as.Date(V2)),col='grey25',linetype=3) +geom_vline(data = cps2, aes(xintercept = as.Date(V3)),col='grey25',linetype=3) +geom_vline(data = cps2, aes(xintercept = as.Date(V4)),col='grey25',linetype=3) + facet_wrap(~variable, scale="free") +
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +theme_minimal()

for(k in 1:13)
{
    ey = c(2012:2016)[which(cq[,k]<0.01)]
    
}
change<-matrix(0,13,5)
change[1,c(1,2,3)] <- 1
change[2,c(1,2,5)] <- 1
change[3,c(1,5)] <- 1