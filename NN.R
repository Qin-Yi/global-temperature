setwd("/Users/Queensatoria/Downloads/时间序列分析R软件及实现/RData")
library(TSA)
load("tsa3.rda")
plot(gtemp, type="o", ylab="Global Temperature Deviations")

data <- gtemp
require( quantmod )
x1<-Lag(data, k = 1)
x2<-Lag(data, k = 2)
x3<-Lag(data, k = 3)
x4<-Lag(data, k = 4)

x<-cbind ( x1 , x2 , x3 , x4 , data )
x <- x [ (5:126) , ]

x<-data.matrix ( x )
range_data<-function ( x ) {(x-min( x ) ) /(max( x )-min( x ) ) }
min_data<-min( x )
max_data<-max( x )
x <-range_data ( x )

x1<-as.matrix ( x [ , 1 ] )
x2<-as.matrix ( x [ , 2 ] )
x3<-as.matrix ( x [ , 3 ] )
x4<-as.matrix ( x [ , 4 ] )
y<-as.matrix ( x [ , 5 ] )

n_train <- 100
y_train<-as.matrix ( y [ 1 : n_train ] )
x1_train<-as.matrix ( t ( x1 [ 1 : n_train , ] ) )
x2_train<-as.matrix ( t ( x2 [ 1 : n_train , ] ) )
x3_train<-as.matrix ( t ( x3 [ 1 : n_train , ] ) )
x4_train<-as.matrix ( t ( x4 [ 1 : n_train , ] ) )

x_train <- array ( c(x1_train ,x2_train ,x3_train ,x4_train) ,dim=c(dim( x1_train ) , 4 ) )

require( rnn )
set.seed (2018)
model1 <- trainr (Y = t(y_train) , X = x_train,
                       learningrate = 0.05 ,
                       hidden_dim = c(3,2) ,
                       numepochs = 500 ,
                       network_type = " rnn " ,
                       sigmoid = "logistic" )

train <- sample (1:122, n_train , FALSE )
inputs <- x [ ,1:4]
outputs <- x [ ,5]
fit <- jordan ( inputs [ train ,],
                outputs [ train ],
                size =2,
                learnFuncParams =c (0.01) ,
                maxit =1000)
plotIterativeError ( fit )
pred <- predict(fit , inputs[-train,])
round( cor ( outputs [ train ],fit$fitted.values )^2 ,3)

fit <- elman ( inputs [ train ] ,
                 outputs [ train ] ,
                 size=c ( 2 , 2 ) ,
                 maxit=1000)
plotRegressionError(outputs [ train] ,fit$fitted.values )
pred <- predict(fit , inputs[-train])
round( cor ( outputs [ train ],fit$fitted.values )^2 ,3)

unscale_data <- function (x,max_x , min_x ){x*( max_x - min_x )+ min_x }
actual <- unscale_data ( outputs [- train],max_data , min_data )
actual <-as.matrix ( actual )
rownames ( actual ) <- 1: length (actual )
pred <- unscale_data (pred ,max_data ,min_data )
result <- cbind (as.ts( actual ), as.ts( pred ))
plot ( result )