Power = function(){
  print(2^2)
}

Power2 = function(x,a){
  print(x^a)
}

Power3 = function(x,a){
  return(x^a)
}

PlotPower = function(x,a) {
  y = x^a
  plot(x, y, col = "red", main = "plot x^a  vs x",
       type = "l", xlab = "x", ylab = "x^a")
}

Power()
Power2(3,8)
Power2(8,17)
Power2(131,3)
x=seq(1,10,0.01)
plot(x, Power3(x,2), type = "l" , xlab = "x", 
     ylab = "x²", main = "Plot x² vs x", log = "y")

PlotPower(-10:10, 3)

