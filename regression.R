# x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
# y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
y <- c(1,2,3,20,1,2,3,20,1,2,3,20,1,2,3,20,1,2,3)
relation <- lm(y~x)

print(summary(relation))
print(predict(relation, data.frame(x=c(20,21,22,23,24))))
plot(x,y,col = "blue",main = "Height & Weight Regression",
     abline(lm(y~x)),cex = 1,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

# -----------------multiple regression---------------------
# input <- mtcars[,c("mpg","disp","hp","wt")]
# input = read.csv('~/Documents/R/test.csv')
# print(input)
# model <- lm(d1~d2+d3, data = input)
# model <- lm(mpg~disp+hp+wt, data = input)

# print(model)
# a <- coef(model)[1]
# X1 <- coef(model)[2]
# X2 <- coef(model)[3]
# X3 <- coef(model)[4]
# X4 <- coef(model)[5]

# x1=15
# x2=7
# x3=1
# x4=28
# ifNA <- function(x){
#   if(is.na(x)){return(0)}
#   else{return(x)}
# }
# Y = a + ifNA(X1)*x1 + ifNA(X2)*x2# + ifNA(X3)*x3 + ifNA(X4)*x4
# print(Y)
# -----------------multiple regression---------------------

# -----------------logistic regression---------------------
# input <- mtcars[,c("am","cyl","hp","wt")]
# print(head(input))

# am.data = glm(formula = d0 ~ d1, data = input, family = binomial)
# newdata = data.frame(d1 = 1)
# print(summary(am.data))
# print(predict(am.data, newdata,type='response'))
# -----------------logistic regression---------------------