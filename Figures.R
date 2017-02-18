####5###10###15###20###25###30###35###40###45###50###55###60###65###70###75###80
#
#~~ DESCRIPTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Figures for the paper:
#   *Population dynamics of mutualism and intraspecific density dependence*
#   by Christopher M. Moore, Sam A. Catella, and Karen C. Abbott
#
#~~ DETAILS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   This file reproduces all of the figures from the above paper.  There are a
#   few things to note:
#      -fig.wd sets where to print the files (.pdf and one .png) if left to NA
#       then figure is not printed and just displayed in the graphics device
#      -set data.wd to where the data are for Figures 3, 5, 6, 7
#         -Figure 6 reads .Rdata files, so place data in a folder called Fig6
#          within the data.wd directory.  The reason is that I ran a numerical
#          simulation for each of the betas in the paper, 0.1, 0.01, and 0.001.
#
#~~ VALUE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   .pdfs and one .png (Figure 3) if fig.wd != NA  Otherwise, figures produced
#   in graphics device
#
#~~ CONTENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   0. Load libraries and set directories
#   1. Figure 1: etas and theta values
#   2. Figure 2: birth and death matrix
#   3. Figure 3: eta = theta by beta
#   4. Figure 4: phase planes
#   5. Figure 5: linear mutaulism benefit
#   6. Figure 6: eta versus theta versus beta
#   7. Figure 7: saturating mutualism benefit
#
####5###10###15###20###25###30###35###40###45###50###55###60###65###70###75###80
# 0. Load libraries and set directories
	data.wd <- "~/Dropbox/MutPopDyn/Paper_BirthDeathMutualism/Data"
		data.wd.fig6 <- paste0(data.wd, "/Fig7")
	fig.wd <- NA
	library(package = "viridis") # R package version 0.3.4
	library(package = "plot3D") # R package version 1.1
	library(package = "rgl") # R package version 0.97.0

# 1. Figure 1: etas and theta values
	if (!is.na(fig.wd) == T) { pdf(paste0(fig.wd, "/eta&theta.pdf"), width = 5.5, height = 2.75) }
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
	if (!is.na(fig.wd) == T) { dev.off() }

# 2. Figure 2: birth and death matrix
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
			# if red and blue shading is wanted, uncomment the two lines below
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
		par(mfrow = c(4, 4), mar = rep(0.25, 4), oma = rep(1, 4))
		y.lim <- c(-1, 10)
		for (i in 1:4) {
			for (j in 1:4) {
			plot(0, type = "n", xlim = c(0, 10), ylim = y.lim, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
			abline(h = 0, v = 0, lty = 1, lwd = 1, col = "grey50")
			par(new = T)
			plot.diff(birth.funcs[[i]], birth.params[[i]], death.funcs[[j]], death.params[[j]], x.lim = c(0,10), n.points = 100, ylim = y.lim)
			if (i == 4) { axis(side = 1, labels = 0, line = -.95, at = 0, las = 1, tick = F, cex.axis = 1) }
			if (j == 1) { axis(side = 2, labels = 0, line = -0.75, at = 0, las = 1, tick = F, cex.axis = 1) }
			}
		}
	if (!is.na(fig.wd) == T) { dev.off() }

# 3. Figure 3: eta = theta by beta
	# 3.1. Load data and package(s)
		load(paste0(data.wd, "/EtaByThetaByBeta.Rdata"))
	# 3.2. Generate equilibria, equilibria classifications, and the eta.by.eq (matrix with eta, equilibria, and beta columns) data
		l.res <- length(sim.results)
		equilibria <- vector(mode = "numeric", length = l.res)
			for(i in 1:l.res){
				equilibria[i] <- sim.results[[i]]$n.equilibria
			}
		eta.by.eq <- matrix(data = NA, nrow = l.res, ncol = 3)
		colnames(eta.by.eq) <- c("eta", "n.equilibria", "beta")
			for(i in 1:l.res){
				eta.by.eq[i, ] <- c(sim.results[[i]]$parameters["eta"], sim.results[[i]]$n.equilibria, sim.results[[i]]$parameters["beta"])
			}
	# 3.3. Plot eta == theta by beta
	if (!is.na(fig.wd) == T) { png(paste0(fig.wd, "/NumEquilibria_OnePlot.png"), width = 4, height = 4, units = "in", res = 300, pointsize = 10) }
			u.etas <- unique(eta.by.eq[,1])
			u.betas <- unique(eta.by.eq[,3])
			etXbet <- matrix(data = NA, nrow = length(u.etas), ncol = length(u.betas))
			rownames(etXbet) <- round(u.etas, digits = 2)
			colnames(etXbet) <- round(u.betas, digits = 2)
			etXbet[1:5, 1:5]
			for (i in 1:length(u.etas)) {
				for (j in 1:length(u.betas)) {
					etXbet[i,j] <- eta.by.eq[which(eta.by.eq[,1] == u.etas[i] & eta.by.eq[,3] == u.betas[j]),2]
				}
			}
			x.adj <- 10^-1.725
			par(mar = c(2, 2, 0, 0), oma = rep(1, 4))
			plot(NA, type = "n", xlim = c(0.01, 100), ylim = c(0.01, 100), log = "xy", axes = F, xaxs = "i", yaxs = "i")
			polygon(x = c(0.01, 0.01, 2, 2), y = c(0.01, 100, 100, 0.01), col = "grey80", border = F)
			polygon(x = c(1+x.adj, 1+x.adj, 100, 100), y = c(0.01, 100, 100, 0.01), col = "grey50", border = F)
			image(x = u.etas, y = u.betas, etXbet, col = c("grey80", "grey50", "grey20"), add = T)
			eta.at <- (max(pretty(log10(u.etas)))-min(pretty(log10(u.etas))))/(length(pretty(log10(u.etas)))-1)*(1:length(pretty(log10(u.etas)))-1)/(max(pretty(log10(u.etas)))-min(pretty(log10(u.etas))))
			beta.at <- (max(pretty(log10(u.betas)))-min(pretty(log10(u.betas))))/(length(pretty(log10(u.betas)))-1)*(1:length(pretty(log10(u.betas)))-1)/(max(pretty(log10(u.betas)))-min(pretty(log10(u.betas))))
			box()
			axis(1, at = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100))
			axis(2, at = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100), las = 1)
		if (!is.na(fig.wd) == T) { dev.off() }

# 4. Figure 4: phase planes
	# 1. Functions
		# 1.1. Nullclines for no mutaulism
			nomut.nc <- function(model, mut.parm, x.lim, y.lim, parameters = NULL, colour = c("#CC000077", "#0000CC77"), lwd = 2, lty = 1, points = 100)
				{	
				parameters[[mut.parm]] <- 0
				null.clines <- nc(model = model , x.lim = x.lim , y.lim = y.lim , parameters = parameters , points = points , lwd = lwd, lty = lty, add = T , col = colour )
				}
		# 1.2. Nullclines with mutaulism
			nc <- function (model, x.lim, y.lim, parameters = NULL, points = 101, colour = c("#CC0000", "#0000CC"), add = TRUE, lwd = 2, lty = "F6", verbose = F) 
			{
				if (parameters[["eta"]]%%1 != 0 | parameters[["theta"]]%%1 != 0) {
					x.lim[1] <- 0
					y.lim[1] <- 0
					abline(h = 0, v = 0, col = colour, lwd = lwd, lty = lty)
				}
			    x <- seq(from = x.lim[1], to = x.lim[2], length = points)
			    y <- seq(from = y.lim[1], to = y.lim[2], length = points)
			    dx <- matrix(0, ncol = points, nrow = points)
			    dy <- matrix(0, ncol = points, nrow = points)
			        for (i in 1:length(x)) {
			            for (j in 1:length(y)) {
			                df <- model(t = 0, y = c(x = x[i], y = y[j]), parameters = parameters)
			                dx[i, j] <- df[[1]][1]
			                dy[i, j] <- df[[1]][2]
			            }
			        }
			        contour(x, y, dx, levels = 0, add = add, col = colour[1], 
			            drawlabels = FALSE, lwd = lwd, lty = lty)
			        contour(x, y, dy, levels = 0, add = TRUE, col = colour[2], 
			            drawlabels = FALSE, lwd = lwd, lty = lty)
				output <- list()
			    output$colour <- colour
			    output$deriv <- deriv
				output$dx <- dx
			    output$dy <- dy
			    output$parameters <- parameters
			    output$points <- points
			    output$system <- system
			    output$x.lim <- x.lim
			    output$y.lim <- y.lim
			    output$x <- x
			    output$y <- y
			    if (verbose == T) {
			    	return(output)
			    	}
			}
	
	# 2. Plot preparation
		bd <- function(t, y, parameters) {
			with(as.list(c(y, parameters)),{
				  dx = (b - d - mu*(x^(eta)) - nu*(x^(eta)) + beta*y)*x
				  dy = (b - d - mu*(y^(eta)) - nu*(y^(eta)) + beta*x)*y
			list(c(dx, dy))})}
	
			if (!is.na(fig.wd) == T) { pdf(paste0(fig.wd, "/PhasePlanes.pdf"), width = 6, height = 5) }
			par(mfrow = c(2, 3), mar = c(4, 0.2, 0.2, 0.2), oma = c(0, 3, 3, 2))
	
	# 3. Plots
		# 3.1. Trivial solution
			plot.pars <- list(b = 5, d = 1, mu = 1, nu = 1, eta = 0, theta = 0, beta = 0)
			lims <- c(0, 10)
			plot(x = 0, type = "n", xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", ann  = F)
			abline(v = 0, h = 0, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			abline(v = 0, h = 0, col = rev(c("#CC0000", "#0000CC")), lty = "F3", lwd = 1.25)
			arrows(3, 3, 6, 3, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(3, 3, 3, 6, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(3, 3, 5.5, 5.5, col = "#000000", lwd = 2, length = 0.05)
			axis(2, at = 0, labels = 0, las = 1, tick = F, line = -0.6)
			axis(2, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = 0, las = 1, tick = F, line = -0.7)
			points(x = 0, y = 0, pch = 21, col = "black", bg = "white", cex = 1.5)
	
		# 3.2. eta = theta < 1, 3-eq solution
			plot.pars <- list(b = 5, d = 1, mu = 1, nu = 1, eta = 0.25, theta = 0.25, beta = .045)
			lims <- c(0, 100)
			plot(x = 0, type = "n", xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", ann  = F)
			abline(v = 0, h = 0, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			abline(v = 2^4, h = 2^4, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			nc(model = bd, x.lim = lims, y.lim = lims, parameters = plot.pars, verbose = FALSE, points = 100, lty = "F3", lwd = 1.25)
	
			arrows(50, 50, 70, 50, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(50, 50, 50, 70, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(50, 50, 65, 65, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(10, 80, 10, 60, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(10, 80, 30, 80, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(10, 80, 25, 65, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(90, 10, 90, 30, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(90, 10, 70, 10, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(90, 10, 75, 25, col = "#000000", lwd = 2, length = 0.05)
	
			abline(v = 0, h = 0, col = rev(c("#CC0000", "#0000CC")), lty = "F3", lwd = 1.25)
	
			points(x = 0, y = 0, pch = 21, col = "black", bg = "white", cex = 1.5)
			points(x = 16, y = 0, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x = 0, y = 16, pch = 21, col = "black", bg = "grey50", cex = 1.5)
	
			axis(1, at = 0, labels = 0, las = 1, tick = F, line = -0.7)
			axis(2, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = F, tck = -0.02)
	
		# 3.3. eta = theta < 1, 2 internal eq
			plot.pars <- list(b = 5, d = 1, mu = 1, nu = 1, eta = 0.25, theta = 0.25, beta = .025)
			lims <- c(0, 120)
			plot(x = 0, type = "n", xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", ann  = F)
			abline(v = 0, h = 0, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			abline(v = 2^4, h = 2^4, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			nc(model = bd, x.lim = lims, y.lim = lims, parameters = plot.pars, verbose = FALSE, points = 200, lty = "F3", lwd = 1.25)
	
			arrows(5, 5, 17, 5, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(5, 5, 5, 17, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(5, 5, 18, 18, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(105, 105, 117, 105, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(105, 105, 105, 117, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(105, 105, 118, 118, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(10, 80, 10, 60, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(10, 80, 30, 80, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(10, 80, 25, 65, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(90, 10, 90, 30, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(90, 10, 70, 10, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(90, 10, 75, 25, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(65, 65, 65, 45, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(65, 65, 45, 65, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(65, 65, 48, 48, col = "#000000", lwd = 2, length = 0.05)
	
			abline(v = 0, h = 0, col = rev(c("#CC0000", "#0000CC")), lty = "F3", lwd = 1.25)
	
			points(x = 0, y = 0, pch = 21, col = "black", bg = "white", cex = 1.5)
			points(x = 16, y = 0, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x = 0, y = 16, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x = 35, y = 35, pch = 21, col = "black", bg = "black", cex = 1.5)
			points(x =  77, y = 77, pch = 21, col = "black", bg = "grey50", cex = 1.5)
	
			axis(1, at = 0, labels = 0, las = 1, tick = F, line = -0.7)
			axis(2, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = F, tck = -0.02)
	
		# 3.4. eta = theta = 1, unstable
			plot.pars <- list(b = 5, d = 1, mu = 1, nu = 1, eta = 1, theta = 1, beta = 3)
			lims <- c(0, 10)
			plot(x = 0, type = "n", xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", ann  = F)
			nc(model = bd, x.lim = lims, y.lim = lims, parameters = plot.pars, verbose = FALSE, points = 100, lty = "F3", lwd = 1.25)
			abline(v = 0, h = 0, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			abline(v = 2, h = 2, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
	
			arrows(4, 4, 5.5, 4, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(4, 4, 4, 5.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(4, 4, 5.5, 5.5, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(1, 8, 2.5, 8, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(1, 8, 1, 6.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(1, 8, 2.4, 6.6, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(8, 1, 6.5, 1, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(8, 1, 8, 2.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(8, 1, 6.6, 2.4, col = "#000000", lwd = 2, length = 0.05)
	
			abline(v = 0, h = 0, col = rev(c("#CC0000", "#0000CC")), lty = "F3", lwd = 1.25)
	
			points(x = 0, y = 0, pch = 21, col = "black", bg = "white", cex = 1.5)
			points(x = 2, y = 0, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x = 0, y = 2, pch = 21, col = "black", bg = "grey50", cex = 1.5)
	
			axis(2, at = 0, labels = 0, las = 1, tick = F, line = -0.6)
			axis(1, at = 0, labels = 0, las = 1, tick = F, line = -0.7)
			axis(2, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = F, tck = -0.02)
	
		# 3.5. eta = theta = 1, stable
			plot.pars <- list(b = 5, d = 1, mu = 1, nu = 1, eta = 1, theta = 1, beta = 1.15)
			lims <- c(0, 10)
			plot(x = 0, type = "n", xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", ann  = F)
			nc(model = bd, x.lim = lims, y.lim = lims, parameters = plot.pars, verbose = FALSE, points = 100, lty = "F3", lwd = 1.25)
	
			abline(v = 0, h = 0, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			abline(v = 2, h = 2, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
	
			arrows(.6, .6, 2.1, 0.6, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(.6, .6, .6, 2.1, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(.6, .6, 2.2, 2.2, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(1, 7, 2.5, 7, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(1, 7, 1, 5.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(1, 7, 2.4, 5.6, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(7, 1, 5.5, 1, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(7, 1, 7, 2.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(7, 1, 5.6, 2.4, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(10, 10, 8.5, 10, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(10, 10, 10, 8.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(10, 10, 8.5, 8.5, col = "#000000", lwd = 2, length = 0.05)
	
			abline(v = 0, h = 0, col = rev(c("#CC0000", "#0000CC")), lty = "F3", lwd = 1.25)
	
			points(x = 0, y = 0, pch = 21, col = "black", bg = "white", cex = 1.5)
			points(x = 2, y = 0, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x = 0, y = 2, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x =  4.705882, y = 4.705882, pch = 21, col = "black", bg = "black", cex = 1.5)
	
			axis(1, at = 0, labels = 0, las = 1, tick = F, line = -0.7)
			axis(2, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = F, tck = -0.02)
	
		# 3.6. eta = theta > 1, stable
			plot.pars <- list(b = 10, d = 1, mu = 1, nu = 1, eta = 4, theta = 4, beta = 100)
			lims <- c(0, 10)
			plot(x = 0, type = "n", xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", ann  = F)
			nc(model = bd, x.lim = lims, y.lim = lims, parameters = plot.pars, verbose = FALSE, points = 100, lty = "F3", lwd = 1.25)
			abline(v = 0, h = 0, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
			abline(v = 4.5^0.25, h = 4.5^0.25, col = rev(c("#CC000077", "#0000CC77")), lwd = 1)
	
			arrows(.6, .6, 2.1, 0.6, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(.6, .6, .6, 2.1, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(.6, .6, 2.2, 2.2, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(1, 7, 2.5, 7, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(1, 7, 1, 5.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(1, 7, 2.4, 5.6, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(7, 1, 5.5, 1, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(7, 1, 7, 2.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(7, 1, 5.6, 2.4, col = "#000000", lwd = 2, length = 0.05)
	
			arrows(10, 10, 8.5, 10, col = "#CC0000", lwd = 2, length = 0.05)
			arrows(10, 10, 10, 8.5, col = "#0000CC", lwd = 2, length = 0.05)
			arrows(10, 10, 8.5, 8.5, col = "#000000", lwd = 2, length = 0.05)
	
			abline(v = 0, h = 0, col = rev(c("#CC0000", "#0000CC")), lty = "F3", lwd = 1.25)
	
			points(x = 0, y = 0, pch = 21, col = "black", bg = "white", cex = 1.5)
			points(x = 4.5^0.25, y = 0, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x = 0, y = 4.5^0.25, pch = 21, col = "black", bg = "grey50", cex = 1.5)
			points(x =  3.75, y = 3.75, pch = 21, col = "black", bg = "black", cex = 1.5)
	
			axis(1, at = 0, labels = 0, las = 1, tick = F, line = -0.7)
			axis(2, at = 0, labels = F, tck = -0.02)
			axis(1, at = 0, labels = F, tck = -0.02)
	
		if (!is.na(fig.wd) == T) { dev.off() }

# 5. Figure 5: linear mutaulism benefit
	# 5.1. Load data and package(s)
		load(paste0(data.wd, "/MutBen.Rdata"))
	# 5.2. Generate equilibria, equilibria classifications, and the eta.by.eq (matrix with eta, equilibria, and beta columns) data
		l.res <- length(sim.results)
		equilibria <- vector(mode = "numeric", length = l.res)
			for(i in 1:l.res){
				equilibria[i] <- sim.results[[i]]$n.equilibria
			}	
		eta.by.eq <- matrix(data = NA, nrow = l.res, ncol = 3)
		colnames(eta.by.eq) <- c("eta", "n.equilibria", "beta")
		for(i in 1:l.res){
			eta.by.eq[i, ] <- c(sim.results[[i]]$parameters["eta"], sim.results[[i]]$n.equilibria, sim.results[[i]]$parameters["beta"])
		}	
	# 5.3. clean data 
		eta.by.eq.5 <- if(any(eta.by.eq[,2] > 5)) {eta.by.eq[-which(eta.by.eq[,2] > 5), ]} else {eta.by.eq.5 <- eta.by.eq}
	# 5.4. plot location of the stable equilibria series
		# 5.4.0. Extract data
			mut.int.eq <- matrix(data = NA, nrow = l.res, ncol = 4)
				for(i in 1:l.res) {
					mut.int.eq[i,c(1,2)] <- sim.results[[i]]$analysis[matrix(data = c(3,1,2,2), ncol = 2, byrow = T)]
					if (sim.results[[i]]$n.equilibria > 3) {
						mut.int.eq[i,c(3,4)] <- sim.results[[i]]$analysis[4,c(1,2)]
					} else {
						mut.int.eq[i,c(3,4)] <- NA
					}
			}
		# 5.4.1. Generate data
			eqs <- cbind(eta.by.eq, mut.int.eq)
			colnames(eqs) <- c(colnames(eqs[,1:3]), "no.mut.x", "no.mut.y", "mut.x", "mut.y")
			eqs <- eqs[which(eqs[,2] < 6),]
			eqs <- eqs[which(is.na(eqs[,6]) == FALSE),]
			unique.etas <- unique(eqs[,1])
			l.unique.etas <- length(unique.etas)
			no.mut.dist <- (eqs[,4]^2 + eqs[,5]^2)^0.5
			mut.dist <- (eqs[,6]^2 + eqs[,7]^2)^0.5
			eqs <- cbind(eqs[,1:7], no.mut.dist, mut.dist)
			mut.ben <- eqs[,9] - eqs[,8]
			eqs <- cbind(eqs, mut.ben)
		# 5.4.2. Generate data
		unique.etas <- unique(eqs[,1])
		unique.betas <- unique(eqs[,3])
		l.eqs <- nrow(eqs)
		no.mut.dist.mat <- matrix(data = NA, nrow = l.eqs)
	
		eta.seq <- 10^seq(-2, 2, length.out = 55)
		beta.seq <- 10^seq(-2, 2, length.out = 55)
		no.mut.dist.mat <- matrix(data = NA, nrow = 55, ncol = 55, dimnames = list(eta.seq, beta.seq))
		for (i in 1:55){
			for (j in 1:55) {
				if (any(eta.seq[i] == eqs[,1]) == T) {
					if (any(beta.seq[j] == eqs[,3]) == T) {
						if (any(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3])) {
						no.mut.dist.mat[i, j] <- eqs[which(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3]), 8]
						}
					}
				}
			}
		}
	
		mut.dist.mat <- matrix(data = NA, nrow = 55, ncol = 55, dimnames = list(eta.seq, beta.seq))
		for (i in 1:55){
			for (j in 1:55) {
				if (any(eta.seq[i] == eqs[,1]) == T) {
					if (any(beta.seq[j] == eqs[,3]) == T) {
						if (any(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3])) {
						mut.dist.mat[i, j] <- eqs[which(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3]), 9]
						}
					}
				}
			}
		}

		mut.ben.dist.mat <- matrix(data = NA, nrow = 55, ncol = 55, dimnames = list(eta.seq, beta.seq))
		for (i in 1:55){
			for (j in 1:55) {
				if (any(eta.seq[i] == eqs[,1]) == T) {
					if (any(beta.seq[j] == eqs[,3]) == T) {
						if (any(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3])) {
						mut.ben.dist.mat[i, j] <- eqs[which(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3]), 10]
						}
					}
				}
			}
		}

	if (!is.na(fig.wd) == T) { pdf(paste0(fig.wd, "/MutBen3D.pdf"), width = 8, height = 4, pointsize = 12) }
		par(mfrow = c(1, 3), mar = c(1.5, 1.5, 0, 0), oma = c(0, 0, 0,0))
		x.axis <- seq(from = -2, to = 2, by = 1)
		min.x <- min(x.axis); max.x <- max(x.axis)
		y.axis <- seq(from = -2, to = 2, by = 1)
		min.y <- min(y.axis); max.y <- max(y.axis)
		z.axis <- pretty(seq(from = -5, to = max(log10(mut.dist.mat), na.rm = T),))
		min.z <- min(z.axis); max.z <- max(z.axis)
		pmat1 <- persp3D(x = log10(eta.seq), y =  log10(beta.seq), z = log10(no.mut.dist.mat), phi = 7.5, theta = 40, contour = list(lwd = 0.5), zlim = c(min.z, max.z), col = jet2.col(100, 0.35),  colkey = F, xlab = "", ylab = "", zlab = "", border = "#00000018", ticktype = "detailed", bty = "b", box = TRUE)

		x.axis <- seq(from = -2, to = 2, by = 1)
		min.x <- min(x.axis); max.x <- max(x.axis)
		y.axis <- seq(from = -2, to = 2, by = 1)
		min.y <- min(y.axis); max.y <- max(y.axis)
		z.axis <- pretty(seq(from = -5, to = max(log10(mut.dist.mat), na.rm = T),))
		min.z <- min(z.axis); max.z <- max(z.axis)
		pmat2 <- persp3D(x = log10(eta.seq), y =  log10(beta.seq), z = log10(mut.dist.mat), phi = 7.5, theta = 40, contour = list(lwd = 0.75), zlim = c(min.z, max.z), col = jet2.col(100, 0.35), colkey = F, xlab = "", ylab = "", zlab = "", border = "#00000018", ticktype = "detailed")

		x.axis <- seq(from = -2, to = 2, by = 1)
		min.x <- min(x.axis); max.x <- max(x.axis)
		y.axis <- seq(from = -2, to = 2, by = 1)
		min.y <- min(y.axis); max.y <- max(y.axis)
		z.axis <- pretty(seq(from = -5, to = max(log10(mut.dist.mat), na.rm = T),))
		min.z <- min(z.axis); max.z <- max(z.axis)
		pmat3 <- persp3D(x = log10(eta.seq), y =  log10(beta.seq), z = log10(mut.ben.dist.mat), phi = 7.5, theta = 40, contour = list(lwd = 0.75), zlim = c(min.z, max.z), col = jet2.col(100, 0.35), colkey = F, xlab = "", ylab = "", zlab = "", border = "#00000018", ticktype = "detailed")
	if (!is.na(fig.wd) == T) { dev.off() }

# 6. Figure 6: eta versus theta versus beta
	# define working directory
		files <- list.files(path = paste0(data.wd.fig6), pattern = ".Rdata")
	# import all .Rdata in the directory and combine as a single list
		res <- vector(mode = "list")
		for (i in 1:length(files)) {
			load(paste0(paste0(data.wd, "/ETBData/RealData"), paste0("/",files[i])))
			res <- append(res, sim.results)
		}

	# remove list elements that are NULL
		if(any(lapply(res, is.null) == T) == T) {
			res <- res[-which(lapply(res, is.null) == T)]
		}

	# extract parameter values
		l.res <- length(res)
		eta.theta.beta.neq <- matrix(data = NA, nrow = l.res, ncol = 4)
		colnames(eta.theta.beta.neq) <- c("eta", "theta", "beta", "n.equilibria")
		eta.theta.beta.neq[,1] <- unlist(lapply(names(res), function(x) res[[x]][["parameters"]][["eta"]]))
		eta.theta.beta.neq[,2] <- unlist(lapply(names(res), function(x) res[[x]][["parameters"]][["theta"]]))
		eta.theta.beta.neq[,3] <- unlist(lapply(names(res), function(x) res[[x]][["parameters"]][["beta"]]))
		eta.theta.beta.neq[,4] <- unlist(lapply(names(res), function(x) res[[x]][["n.equilibria"]]))

	# create list of unique parameter values and an array to store n.equilibra
		u.etas <- sort(unique(eta.theta.beta.neq[,1]))
		u.thetas <- sort(unique(eta.theta.beta.neq[,2]))
		u.betas <- sort(unique(eta.theta.beta.neq[,3]))
		etXbet <- array(data = NA, dim = c(length(u.etas), length(u.thetas), length(u.betas)))
		rownames(etXbet) <- round(u.etas, digits = 2)
		colnames(etXbet) <- round(u.thetas, digits = 2)

	# insert equilibria into array, based on dimensions
		dims <- cbind(match(eta.theta.beta.neq[,1], u.etas), match(eta.theta.beta.neq[,2], u.thetas), match(eta.theta.beta.neq[,3], u.betas))
		for (i in 1:l.res){
			etXbet[dims[i, 1], dims[i, 2], dims[i, 3]] <- eta.theta.beta.neq[,4][i]
		}
	cls <- list(length = length(u.betas))
	for (i in 1:length(u.betas)) {
		cl <- contourLines(x = u.etas, y = u.thetas, z = etXbet[,,i], nlevels = 1)
		cls[i] <- cl
	}
	# print pdf
	if (!is.na(fig.wd) == T) { pdf(paste0(fig.wd, "/AssymEtaTheta.pdf"), width = 5, height = 5) }
		par(mfrow = c(1, 1), mar = c(2.1,2.1,1,1), oma = c(1,1,0,0))
		plot(NA, type = "n", log = "xy", xlim = c(0.01, 100), ylim = c(0.01, 100), xaxs = "i", yaxs = "i", las = 1, ann = F, xaxt = "n", yaxt = "n")
		polygon(x = c(0.01, 0.01, 100, 100, 1, 1), y = c(1, 100, 100, 0.01, 0.01, 1), col = "black", border = F)		
		for (i in 1:(length(cls))) { # eliminates the 10^-0.5
			polygon(x = c(cls[[i]]$x, 1, 1, 0.01), y = c(cls[[i]]$y, 0.01, 1, 1), col = rgb(0, 0, 0, 0.15,, 1), border = F)
			lines(cls[[i]]$x, cls[[i]]$y, lty = c("33", "43","73", "C3")[i], lwd = 1)
		}
		box()
		axis(1, at = c(0.01, 10^-1, 10^0, 10^1, 100), labels = c(0.01, 10^-1, 10^0, 10^1, 100))
		axis(2, las = 1, at = c(0.01, 10^-1, 10^0, 10^1, 100), labels = c(0.01, 10^-1, 10^0, 10^1, 100))
	if (!is.na(fig.wd) == T) { dev.off() }

# 7. Figure 7: saturating mutualism benefit
	# 7.1. Load data and package(s)
		load(paste0(data.wd, "/MutBenSat.Rdata"))
	# 7.2. Generate equilibria, equilibria classifications, and the eta.by.eq (matrix with eta, equilibria, and beta columns) data
		l.res <- length(sim.results)
		equilibria <- vector(mode = "numeric", length = l.res)
			for(i in 1:l.res){
				equilibria[i] <- sim.results[[i]]$n.equilibria
			}
		eta.by.eq <- matrix(data = NA, nrow = l.res, ncol = 3)
		colnames(eta.by.eq) <- c("eta", "n.equilibria", "beta")
			for(i in 1:l.res){
				eta.by.eq[i, ] <- c(sim.results[[i]]$parameters["eta"], sim.results[[i]]$n.equilibria, sim.results[[i]]$parameters["beta"])
			}	
	# 7.3. Clean data 
		eta.by.eq.5 <- if(any(eta.by.eq[,2] > 5)) {eta.by.eq[-which(eta.by.eq[,2] > 5), ]} else {eta.by.eq.5 <- eta.by.eq}
	# 7.4. plot location of the stable equilibria series
		# 7.4.1. Extract data
			mut.int.eq <- matrix(data = NA, nrow = l.res, ncol = 4)
				for(i in 1:l.res) {
					mut.int.eq[i,c(1,2)] <- sim.results[[i]]$analysis[matrix(data = c(3,1,2,2), ncol = 2, byrow = T)]
					if (sim.results[[i]]$n.equilibria > 3) {
						mut.int.eq[i,c(3,4)] <- sim.results[[i]]$analysis[4,c(1,2)]
					} else {
						mut.int.eq[i,c(3,4)] <- NA
					}
				}
		# 7.4.2. Generate data
			eqs <- cbind(eta.by.eq, mut.int.eq)
			colnames(eqs) <- c(colnames(eqs[,1:3]), "no.mut.x", "no.mut.y", "mut.x", "mut.y")
			eqs <- eqs[which(eqs[,2] < 6),]
			eqs <- eqs[which(is.na(eqs[,6]) == FALSE),]
			unique.etas <- unique(eqs[,1])
			l.unique.etas <- length(unique.etas)
			no.mut.dist <- (eqs[,4]^2 + eqs[,5]^2)^0.5
			mut.dist <- (eqs[,6]^2 + eqs[,7]^2)^0.5
			eqs <- cbind(eqs[,1:7], no.mut.dist, mut.dist)
			mut.ben <- eqs[,9] - eqs[,8]
			eqs <- cbind(eqs, mut.ben)
		# 7.4.3. Generate data
			unique.etas <- unique(eqs[,1])
			unique.betas <- unique(eqs[,3])
			l.unique.betas <- length(unique.betas)
			l.eqs <- nrow(eqs)
			no.mut.dist.mat <- matrix(data = NA, nrow = l.eqs)
			eta.seq <- 10^seq(-2, 2, length.out = l.unique.etas)
			beta.seq <- 10^seq(-2, 2, length.out = l.unique.betas)
			no.mut.dist.mat <- matrix(data = NA, nrow = 55, ncol = 55, dimnames = list(eta.seq, beta.seq))
			for (i in 1:55){
				for (j in 1:55) {
					if (any(eta.seq[i] == eqs[,1]) == T) {
						if (any(beta.seq[j] == eqs[,3]) == T) {
							if (any(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3])) {
							no.mut.dist.mat[i, j] <- eqs[which(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3]), 8]
							}
						}
					}
				}
			}
			mut.dist.mat <- matrix(data = NA, nrow = 55, ncol = 55, dimnames = list(eta.seq, beta.seq))
			for (i in 1:55){
				for (j in 1:55) {
					if (any(eta.seq[i] == eqs[,1]) == T) {
						if (any(beta.seq[j] == eqs[,3]) == T) {
							if (any(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3])) {
							mut.dist.mat[i, j] <- eqs[which(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3]), 9]
							}
						}
					}
				}
			}
			mut.ben.dist.mat <- matrix(data = NA, nrow = 55, ncol = 55, dimnames = list(eta.seq, beta.seq))
			for (i in 1:55){
				for (j in 1:55) {
					if (any(eta.seq[i] == eqs[,1]) == T) {
						if (any(beta.seq[j] == eqs[,3]) == T) {
							if (any(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3])) {
							mut.ben.dist.mat[i, j] <- eqs[which(eta.seq[i] == eqs[,1] & beta.seq[j] == eqs[,3]), 10]
							}
						}
					}
				}
			}	
			zeros <- which(mut.ben.dist.mat ==0, arr.ind = T)
			mut.ben.dist.mat[zeros[,1], zeros[,2]] <- NA
		if (!is.na(fig.wd) == T) { pdf(paste0(fig.wd, "/MutBen3D_Sat.pdf"), width = 8, height = 4, pointsize = 12) }
			par(mfrow = c(1, 3), mar = c(1.5, 1.5, 0, 0), oma = c(0, 0, 0,0))
			x.axis <- seq(from = -2, to = 2, by = 1)
			min.x <- min(x.axis); max.x <- max(x.axis)
			y.axis <- seq(from = -2, to = 2, by = 1)
			min.y <- min(y.axis); max.y <- max(y.axis)
			z.axis <- pretty(seq(from = -5, to = max(log10(mut.dist.mat), na.rm = T),))
			min.z <- min(z.axis); max.z <- max(z.axis)
			pmat1 <- persp3D(x = log10(eta.seq), y =  log10(beta.seq), z = log10(no.mut.dist.mat), phi = 7.5, theta = 40, contour = list(lwd = 0.5), zlim = c(min.z, max.z), col = jet2.col(100, 0.35),  colkey = F, xlab = "", ylab = "", zlab = "", border = "#00000018", ticktype = "detailed", bty = "b", box = TRUE)

			x.axis <- seq(from = -2, to = 2, by = 1)
			min.x <- min(x.axis); max.x <- max(x.axis)
			y.axis <- seq(from = -2, to = 2, by = 1)
			min.y <- min(y.axis); max.y <- max(y.axis)
			z.axis <- pretty(seq(from = -5, to = max(log10(mut.dist.mat), na.rm = T),))
			min.z <- min(z.axis); max.z <- max(z.axis)
			pmat2 <- persp3D(x = log10(eta.seq), y =  log10(beta.seq), z = log10(mut.dist.mat), phi = 7.5, theta = 40, contour = list(lwd = 0.75), zlim = c(min.z, max.z), col = jet2.col(100, 0.35), colkey = F, xlab = "", ylab = "", zlab = "", border = "#00000018", ticktype = "detailed")

			x.axis <- seq(from = -2, to = 2, by = 1)
			min.x <- min(x.axis); max.x <- max(x.axis)
			y.axis <- seq(from = -2, to = 2, by = 1)
			min.y <- min(y.axis); max.y <- max(y.axis)
			z.axis <- pretty(seq(from = -5, to = max(log10(mut.ben.dist.mat), na.rm = T),))
			min.z <- min(z.axis); max.z <- max(z.axis)
			pmat3 <- persp3D(x = log10(eta.seq), y =  log10(beta.seq), z = log10(mut.ben.dist.mat), phi = 7.5, theta = 40, contour = list(lwd = 0.75), zlim = c(min.z, max.z), col = jet2.col(100, 0.35), colkey = F, xlab = "", ylab = "", zlab = "", border = "#00000018", ticktype = "detailed")
		if (!is.na(fig.wd) == T) { dev.off() }