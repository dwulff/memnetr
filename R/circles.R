##### circles #######

inPi = function(x) (x/360)*(pi*2)

x_circ  = function(n,deg,rad,orig,start){
  degs = seq(0,deg,length = n + 1)
  cbind(degs, cos(inPi(degs))*rad + orig[1])}

y_circP = function(x,rad,orig) orig[2] + sqrt(abs(rad^2-(x-orig[1])^2))
y_circM = function(x,rad,orig) orig[2] - sqrt(abs(rad^2-(x-orig[1])^2))

circle_raw<-function(n,deg,rad,orig){
  xs  = x_circ(n,-deg,rad,orig)
  ys1 = y_circP(xs[,2],rad,orig)
  ys2 = y_circM(xs[,2],rad,orig)
  sel = xs[,1]>180 & xs[,1]<360 | xs[,1]>-180 & xs[,1]<0 | xs[,1]>640 & xs[,1]<720
  ys = ys1
  ys[sel] = ys2[sel]
  cbind(xs[,2],ys)
  }


circle = function(rad, orig, ..., n = 100){

  polygon(circle_raw(n,360,rad,orig),...)

}



