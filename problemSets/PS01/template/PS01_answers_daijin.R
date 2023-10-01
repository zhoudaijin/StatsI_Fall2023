#excercise 1 
#excercise 1-1
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#sample mean and standard deviation
mean<-mean(y)
sd<-sd(y)
n<-length(y)
#standard error
se<-sd/sqrt(n)
#degrees of freedom 
df<-n-1
#calculate t_value
t_value<-qt(0.95,df)
#margin of error
me<-t_value*se
#calculate confidence interval
a<-mean-me
b<-mean+me
#result
cat("The 90% confidence interval is (",a,"",",",b,")")

#excercise 1-2
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
all_schools_average<-100
#one-sample t_test
t_test<-t.test(y,mu=all_schools_average,alternative="greater")
#p-value
p_value<-t_test$p.value
α<-0.05
#result
cat("p-value:", p_value)
if (p_value < α) {
  cat("reject H0")
} else {
  cat("accept Ha")
}

#excercise 2
#excercise 2-1
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
par(mfrow=c(2,3)) 
#X1_Y
X1_Y<-plot(expenditure$X1,expenditure$Y,main="X1&Y",xlab="X1",ylab="Y")
X1_Y
#X2_Y
X2_Y<-plot(expenditure$X2,expenditure$Y,main="X2&Y",xlab="X2",ylab="Y")
X2_Y
#X3_Y
X3_Y<-plot(expenditure$X3,expenditure$Y,main="X3&Y",xlab="X3",ylab="Y")
X3_Y
#X1_X2
X1_X2<-plot(expenditure$X1,expenditure$X2,main="X1&X2",xlab="X1",ylab="X2")
X1_X2
#X1_X3
X1_X3<-plot(expenditure$X1,expenditure$X3,main="X1&X3",xlab="X1",ylab="X3")
X1_X3
#X2_X3
X2_X3<-plot(expenditure$X2,expenditure$X3,main="X2&X3",xlab="X2",ylab="X3")
X2_X3
par(mfrow=c(1,1))
#check correlations
all_correlation_coes<-c(cor(expenditure$X1,expenditure$Y),cor(expenditure$X2,expenditure$Y),cor(expenditure$X3,expenditure$Y),cor(expenditure$X1,expenditure$X2),cor(expenditure$X1,expenditure$X3),cor(expenditure$X2,expenditure$X3))
all_correlation_coes
for(j in all_correlation_coes){
  if(j>=0){
    if(j>0){
      print("positive correlation")
    }else{
      print("irrelevant")
    }
  }else{
    print("negative correlation")
  } 
}

#excercise 2-2
Region_1<-mean(expenditure[expenditure$Region == 1,]
     $Y)
Region_2<-mean(expenditure[expenditure$Region == 2,]
               $Y)
Region_3<-mean(expenditure[expenditure$Region == 3,]
               $Y)
Region_4<-mean(expenditure[expenditure$Region == 4,]
               $Y)
region_cat = c("1", "2", "3", "4")
region_mean = c(79, 84, 69, 88)
Region_Y_bar<-barplot(region_mean, names.arg = region_cat, main = "Region_Y_bar", 
        xlab = "region_cat", ylab = "region_mean")
result<-cat("the region of NO.4 has the highest per capita expenditure on housing assistance")

#excercise 2-3
region_car_var<-as.character(expenditure$Region)
region_car_var
colors <- c("red", "blue", "green", "yellow")
for (i in 1:length(region_car_var)) {
  points(expenditure[[i]]$X1, expenditure[[i]]$Y, col = colors[i], pch = 19)
}
legend("topright", legend = unique(expenditure$Region), col = colors, pch = 19)








