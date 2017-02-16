####5###10###15###20###25###30###35###40###45###50###55###60###65###70###75###80
#
#~~ DESCRIPTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Figures for the paper:
#   *Population dynamics of mutualism and intraspecific density dependence*
#   by Christopher M. Moore, Sam A. Catella, and Karen C. Abbott
#
#~~ DETAILS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#~~ VALUE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#
#~~ CONTENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   0. Figure 1: etas and theta values
#   1. Figure 2: birth and death matrix
#   2. Figure 3: density dependence by strength of mutualism
#   3. Figure 4: phase planes
#   4. Figure 5: linear mutaulism benefit
#   5. Figure 6: eta versus theta versus beta
#   6. Figure 7: saturating mutualism benefit
#
####5###10###15###20###25###30###35###40###45###50###55###60###65###70###75###80
fig.wd <- NA

# 0. Figure 1: etas and theta values
	if (!is.na(fig.wd) == T) {pdf(paste0(fig.wd, "/eta&theta.pdf"), width = 5.5, height = 2.75)}
		x <- seq(0,1,0.01)
		etas <- 10^seq(from = -2, to =2, length.out = 55)
		thetas <- 10^seq(from = -2, to = 2,length.out = 55)

		par(mfrow = c(1,2), mar = c(1.3, 1.3, 1.3, 1.3), oma = c(0,0,0,0), xpd = T)
		plot(x, type = "n", ylim = c(0, 10), xlim = c(0, 1), ann = F, axes = F)
		box()
		for (i in 1:length(etas)){
			fx <- 10 - 10*x^etas[i]
			lines(x, fx, col = "grey85", lwd = 0.75)
		}
		lines(x, (10 - 10*x^10^-1), lwd = 1.25, lty = "33")
		lines(x, (10 - 10*x^10^0), lwd = 1.25, lty = "93")
		lines(x, (10 - 10*x^10^1), lwd = 1.25, lty = "F3")
		plot(x, type = "n", ylim = c(0, 10), xlim = c(0, 1), ann = F, axes = F)
		box()
		for (i in 1:length(thetas)){
			fx <- 10*x^etas[i]
			lines(x, fx, col = "grey85", lwd = 0.75)
		}
		lines(x, (10*x^10^-1), lwd = 1.25, lty = "33")
		lines(x, (10*x^10^0), lwd = 1.25, lty = "93")
		lines(x, (10*x^10^1), lwd = 1.25, lty = "F3")
	if (!is.na(fig.wd) == T) {dev.off()}

# 1. Figure 2: birth and death matrix
	# brith functions
		cons.b <- function(b, a, x) {b - a*x^0}
			cons.b.p <- list(b = 10, a = 1)
		conc.b <- function(a, b, x) {b - a*x^0.5}
			conc.b.p <- list(a = 3, b = 10)
		lin.b <- function(a, b, x) {b - a*x^1}
			lin.b.p <- list(a = 1, b = 10)
		conv.b <- function(b, a, m, x) {b - a*(x^2)}
			conv.b.p <- list(b = 10, a = 0.1, m = .5)
	# death functions
		cons.d <- function(d, x) {d + a*x^0}
			cons.d.p <- list(d = 1, a = 1)
		conv.d <- function(a, d, x) {d + a*x^0.5}
			conv.d.p <- list(a = 3, d = 1)
		lin.d <- function(a, d, x) {d + a*x^1}
			lin.d.p <- list(a = 1, d = 1)
		conc.d <- function(d, a, x) {d + a*x^2}
			conc.d.p <- list(a = 0.1, d = 1)
	# plotting function
		plot.diff <- function(birth, birth.parms, death, death.parms, x.lim, n.points, ylim = 'NULL', ...){
			x.seq <- seq(min(x.lim), max(x.lim), length.out = n.points)
			formals(birth) <- c(birth.parms, x = list(x.seq))
			formals(death) <- c(death.parms, x = list(x.seq))
			birth.out <- birth()
			death.out <- death()
			diff.out <- birth.out - death.out
	
			if(missing(ylim)) {ylim <- c(min(birth.out, death.out, diff.out), max(birth.out, death.out, diff.out))}
	
			plot(0 , type = "n", xlim = c(x.seq[1], x.seq[length(x.seq)]), ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
	
			#shade the colors between births and deaths
			x.pos <- which(birth.out >= death.out)
			x.neg <- which(birth.out < death.out)
			birth.pos <- birth.out[x.pos]
			death.pos <- death.out[x.pos]
			birth.neg <- birth.out[x.neg]
			death.neg <- death.out[x.neg]
			# polygon(x = c(x.seq[x.pos], rev(x.seq[x.pos])), y = c(birth.pos, rev(death.pos)), col = rgb(0.1,0,1,0.2,,1))
			# polygon(x = c(x.seq[x.neg], rev(x.seq[x.neg])), y = c(birth.neg, rev(death.neg)), col = rgb(1,0,0.1,0.2,,1))
	
			# draw the lines
			lines(x.seq, birth.out, col = rgb(0,0,1,0.9,,1), lwd = 1.5, lty = "F2")
			lines(x.seq, death.out, col = rgb(1,0,0,0.9,,1), lwd = 1.5, lty = "62")
			lines(x.seq, diff.out, col = "black", lwd = 2.25)
		}

		birth.funcs <- list(cons.b, conc.b, lin.b, conv.b)
		birth.params <- list(cons.b.p, conc.b.p, lin.b.p, conv.b.p)
		death.funcs <- list(cons.d, conv.d, lin.d, conc.d)
		death.params <- list(cons.d.p, conv.d.p, lin.d.p, conc.d.p)
	
	if (!is.na(fig.wd) == T) { pdf(paste(fig.wd, "/b&d.pdf", sep = ""), width = 6, height = 6) }
		par(mfrow = c(4,4), mar = rep(0.25,4), oma = rep(1,4))
		y.lim <- c(-1, 10)
		for(i in 1:4){
			for(j in 1:4){
			plot(0, type = "n", xlim = c(0, 10), ylim = y.lim, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
			abline(h = 0, v = 0, lty = 1, lwd = 1, col = "grey50")
			par(new = T)
			plot.diff(birth.funcs[[i]], birth.params[[i]], death.funcs[[j]], death.params[[j]], x.lim = c(0,10), n.points = 100, ylim = y.lim)
			if(i == 4){axis(side = 1, labels = 0, line = -.95, at = 0, las = 1, tick = F, cex.axis = 1)}
			if(j == 1){axis(side = 2, labels = 0, line = -0.75, at = 0, las = 1, tick = F, cex.axis = 1)}
			}
		}
	if (!is.na(fig.wd) == T) { dev.off() }