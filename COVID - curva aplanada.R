# install.packages(c("EpiModel", "extrafont", "animation"), dependencies = TRUE)

library(EpiModel)
library(extrafont)
library(animation)

param <- param.dcm(inf.prob = 0.2, act.rate = 1, rec.rate = 1/20,
                   a.rate = 0, ds.rate = 0, di.rate = 1/100, dr.rate = 0)
init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
mod <- dcm(param, init, control)
mod.df <- as.data.frame(mod)

plot(mod.df$time, mod.df$i.num, type="l")

init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 1000, dt = 0.5)

par(family="sans", cex.axis=.9, las=1)
plot(NA, type="n", xlim=c(0,1000), ylim=c(0, 300),
     bty="n", axes=FALSE, 
     xlab="Dias desde el primer caso detectado", ylab="Numero de infectados",  
     main="54cuatro | APLANAMIENTO DE CURVA")
axis(1, seq(0,1000,250), lwd=0, lwd.ticks = .5, pos = -5)
axis(2, at=seq(0, 250, 50), lwd=0, lwd.ticks=.5, pos=-2)

act.rates <- seq(.8, 0, by=-.05)
for (rt in act.rates) {
    
    param <- param.dcm(inf.prob = 0.2, act.rate = rt, rec.rate = 1/20,
                       a.rate = 0, ds.rate = 0, di.rate = 1/100, dr.rate = 0)
    mod <- dcm(param, init, control)
    mod.df <- as.data.frame(mod)
    lines(mod.df$time, mod.df$i.num, 
          col=rgb(85/255,158/255,161/255,min(1-rt+.1, 1)),
          lwd=1+(1-rt))    
}

saveGIF({
    
    param <- param.dcm(inf.prob = 0.2, act.rate = .8, rec.rate = 1/20,
                       a.rate = 0, ds.rate = 0, di.rate = 1/100, dr.rate = 0)
    mod.orig <- dcm(param, init, control)
    mod.orig.df <- as.data.frame(mod.orig)
    
    act.rates <- seq(.8, .1, by=-.02)
    for (rt in act.rates) {
        
        # Blank plot.
        par(family="Consolas", cex.axis=1.1, las=1, cex.main=2)
        plot(NA, type="n", xlim=c(0,1000), ylim=c(0, 300),
             bty="n", axes=FALSE, 
             xlab="Dias desde el primer caso detectado", ylab="Numero de Infectados",  
             main="54cuatro | APLANAMIENTO DE CURVA")
        mtext("con distanciamiento social", side=3)
        axis(1, seq(0,1000,250), lwd=0, lwd.ticks = .5, pos = -5)
        axis(2, at=seq(0, 250, 50), lwd=0, lwd.ticks=.5, pos=-2)
        
        # Marca de la capacidad maxima del sistema de salud
        lines(x=c(0,1000), y=c(100,100), lty=2)
        text(x=500, y=100, "Capacidad instalada del Sistema de Salud", pos=3)
        
        # Lenea base
        lines(mod.orig.df$time, mod.orig.df$i.num,
              col=rgb(85/255,158/255,161/255,.5),
              lwd=1)
        
        # Simulacion.
        param <- param.dcm(inf.prob = 0.2, act.rate = rt, rec.rate = 1/20,
                           a.rate = 0, ds.rate = 0, di.rate = 1/100, dr.rate = 0)
        mod <- dcm(param, init, control)
        mod.df <- as.data.frame(mod)
        if (max(mod.df$i.num) <= 100) {
            linecol <- "#815C93"
            lwd <- 6
        } else {
            linecol <- rgb(85/255,158/255,161/255,1)
            lwd=3
        }
        
        lines(mod.df$time, mod.df$i.num, 
              col=linecol,
              lwd=lwd)    
    }
}, movie.name="curva aplanada.gif", interval=.30, ani.width=800, ani.height=600)

