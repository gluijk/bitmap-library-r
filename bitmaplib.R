# Dibujando gráficos de mapa de bits con R
# www.datosimagensonido.com


# LIBRERÍA GRÁFICA BITMAP

NewBitmap = function(dimx, dimy, val=0) {
  # Crea bitmap de dimensiones dimx y dimy
  return(array(val,c(dimx,dimy)))
}

ClearBitmap = function(img, inc=F, val=0) {
  # Limpia bitmap
  # Por defecto método destructivo y con valor=0
  if (inc) img=img+val
  else img[]=val
  
  return(img)
}

DrawPoint = function(img, x0, y0, inc=T, val=1) {
  # Dibuja punto en (x0,y0)
  # Por defecto método no destructivo y con valor=1
  img=DrawLine(img, x0, y0, x0, y0, inc, val)
  
  return(img)
}

DrawLine = function(img, x0, y0, x1, y1, inc=T, val=1) {
  # Dibuja recta desde (x0,y0)-(x1,y1)
  # Por defecto método no destructivo y con valor=1
  indices=indices.drawline(x0, y0, x1, y1)
  if (inc) img[indices]=img[indices]+val
  else img[indices]=val
  
  return(img)
}

# Por Carlos Gil Bellosta
indices.drawline = function(x0, y0, x1, y1) {
  x0=round(x0)
  x1=round(x1)
  y0=round(y0)
  y1=round(y1)
  
  if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
  if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
    m = (y1 - y0) / (x1 - x0)
    cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
  } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
  # Llamada traspuesta recursiva y traspuesta
}

DrawRect = function(img, x0, y0, x1, y1, inc=T, val=1, fill=F) {
  # Dibuja rectángulo (x0,y0)-(x1,y1)
  # Por defecto método no destructivo, con valor=1 y sin relleno
  x0=round(x0)
  x1=round(x1)
  y0=round(y0)
  y1=round(y1)
  
  if (fill) {
    if (inc) img[x0:x1,y0:y1]=img[x0:x1,y0:y1]+val
    else img[x0:x1,y0:y1]=val
    
    return(img)
  } else {
    indices=which( ( (row(img)==x0         | row(img)==x1        ) &
                     (col(img)>=min(y0,y1) & col(img)<=max(y0,y1)) ) |
                   ( (col(img)==y0         | col(img)==y1        ) &
                     (row(img)>=min(x0,x1) & row(img)<=max(x0,x1)) ) )
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
  }
}

DrawCircle = function(img, x0, y0, r, inc=T, val=1, fill=F, thick=1) {
  # Dibuja círculo de centro (x0,y0) y radio r
  # Por defecto método no destructivo, con valor=1 y sin relleno
  # Puede elegirse el grosor si no se rellena
  img=DrawEllip(img, x0, y0, r, r, inc, val, fill, thick)
  
  return(img)
}

DrawEllip = function(img, x0, y0, a, b, inc=T, val=1, fill=F, thick=1) {
  # Dibuja elipse de centro (x0,y0) y radios a y b
  # Por defecto método no destructivo, con valor=1 y sin relleno
  # Puede elegirse el grosor si no se rellena
  # Aquí no redondeamos para tener más precisión en la división
  if (fill) {
    indices=which( ((row(img)-x0)/a)^2 + ((col(img)-y0)/b)^2 < 1 )
  } else {
    indices=which( ((row(img)-x0)/(a+thick/2))^2 + ((col(img)-y0)/(b+thick/2))^2 <  1 &
                   ((row(img)-x0)/(a-thick/2))^2 + ((col(img)-y0)/(b-thick/2))^2 >= 1 )
  }
  if (inc) img[indices]=img[indices]+val
  else img[indices]=val
  
  return(img)
}

CopyRect = function(img, x0, y0, x1, y1, xdst, ydst, inc=T, val=1) {
  # Copia rectángulo (x0,y0)-(x1,y1) en (xdst,ydst)
  # Por defecto método no destructivo y con escalado=1
  x0=round(x0)
  x1=round(x1)
  y0=round(y0)
  y1=round(y1)  
  xdst=round(xdst)
  ydst=round(ydst)
  
  if (inc) img[xdst:(xdst+x1-x0), ydst:(ydst+y1-y0)] =
    img[xdst:(xdst+x1-x0), ydst:(ydst+y1-y0)]+img[x0:x1, y0:y1]*val
  else img[xdst:(xdst+x1-x0), ydst:(ydst+y1-y0)] =
    img[x0:x1, y0:y1]  
  
  return(img)
}

ShowBitmap = function(img, trunc=T, gamma=1, chan=2, interpolate=F) {
  # Muestra bitmap en pantalla
  # Solo si trunc=F y la imagen excede de 1 se reescala a 1
  # Si no es monocromo se muestra el canal chan (por defecto G)
  if (length(dim(img))>2) img=img[,,chan]
  img[img<0]=0
  if (trunc) img[img>1]=1
  plot(as.raster(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), max=1),
    interpolate=interpolate)
}

SaveBitmap = function(img, name, trunc=T, gamma=1) {
  # Guarda bitmap en formato PNG
  # Solo si trunc=F y la imagen excede de 1 se reescala a 1
  library(png)
  img[img<0]=0
  if (trunc) img[img>1]=1
  if (tolower(substr(name, nchar(name)-3, nchar(name))) != ".png") name=paste0(name,".png")
  writePNG(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), name)
}

LoadBitmap = function(name, chan=2) {
  # Lee bitmap en formato PNG
  # Si no es monocromo se carga el canal chan (por defecto G)
  library(png)
  img=readPNG(name)
  if (length(dim(img))>2) img=img[,,chan]
  
  return(t(img[nrow(img):1,]))
}

iif = function(condicion, val1, val2) {
  if (condicion) return(val1)
  return(val2)
}


# EJEMPLO

img=NewBitmap(512, 385, 0.1)
img=DrawRect(img, 40, 300, 360, 350, val=0.5)
for (n in 1:5) img=DrawCircle(img,
    250+n*10, 200+n*7, n^0.9*30, val=0.1, fill=T)
img=DrawRect(img, 75, 30, 125, 360, val=0.5, fill=T)
for (n in 1:80) img=DrawLine(img,
    n*5+40, 90+70*sin(2*pi/20*n), n*5+40, 90)

# Para borrar la elipse: val=-0.5
img=DrawEllip(img, 320, 150, 150, 90, val=0.5, thick=25)

ShowBitmap(img/1.6)
SaveBitmap(img/1.6, "ejemplo")


# SERIE DE FOURIER DE DIENTE DE SIERRA

f=1  # f=1: animación de 512px, f=2: animación de 1024px
ALTO=300*f
ANCHO=(250-18)*f
CENTRO=ANCHO*0.6
AMPLITUD=90*f  # Píxeles máx. del diente de sierra
NPERIODO=140*f  # Píxeles del período
CICLOS=2  # Núm. períodos completos
NTERMINOS=5  # Núm. términos desarrollo Fourier

frm=NewBitmap(ANCHO+NPERIODO*CICLOS, ALTO)

radio=2*AMPLITUD/pi*(-1^(1:NTERMINOS))/(1:NTERMINOS)
xprev=ANCHO+2
yprev=ALTO/2

for (t in 0:(NPERIODO*CICLOS-1)) {
  x=array(0, NTERMINOS+1)  # Un valor más largo que NTERMINOS: x[1]=0, y[1]=0
  y=array(0, NTERMINOS+1)
  
  # Para cada t se tiene set de circunferencias y valor de salida -> frm.png
  for (n in 1:NTERMINOS) {
    x[n+1] = x[n] + radio[n]*cos(2*pi/NPERIODO*n*t)
    y[n+1] = y[n] + radio[n]*sin(2*pi/NPERIODO*n*t)
    frm=DrawCircle(frm, CENTRO+x[n], ALTO/2+y[n], abs(radio[n]), val=0.18, fill=T)  # Círculo
    frm=DrawLine(frm, CENTRO+x[n], ALTO/2+y[n], CENTRO+x[n+1], ALTO/2+y[n+1], inc=F)  # Radio
  }
  
  frm=DrawLine(frm, CENTRO+x[n+1], ALTO/2+y[n+1], ANCHO, ALTO/2+y[n+1], val=0.2)  # Línea hasta gráfica
  frm=DrawPoint(frm, ANCHO+1, ALTO/2+y[n+1], inc=F)  # Punto de la gráfica
  if (t>0) frm=DrawLine(frm, xprev, yprev, ANCHO+1, ALTO/2+y[n+1], val=0.4)  # Unión puntos gráfica
  yprev=ALTO/2+y[n+1]
  
  SaveBitmap(frm[,ncol(frm):1], paste0("frm", iif(t<10, "00", iif(t<100, "0", "")), t))
  
  frm=CopyRect(frm, ANCHO+1, 1, nrow(frm)-1, ncol(frm), ANCHO+2, 1, inc=F)
  frm=DrawRect(frm, 1, 1, ANCHO+1, ALTO, inc=F, val=0, fill=T)
}
