####5###10###15###20###25###30###35###40###45###50###55###60###65###70###75###80
#
#~~ DESCRIPTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Numerical condiction of local stabilty analysis, v. 0.2, for the paper:
#   *Population dynamics of mutualism and intraspecific density dependence*
#   by Christopher M. Moore, Sam A. Catella, and Karen C. Abbott
#
#~~ DETAILS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   This .R file contains the code used to numerically determine local stability
#   for our paper.  We create our model, set parameters, and loop over the
#   analysis.  In the paper, we focus on the exponential parameters, eta and
#   theta, the strength of mutualism, beta_i, and show results from the analysis
#   from 10^-2 to 10^2, with fixedvalues of the density-indepdendent parameters,
#   b_i and d_i, and density-dependent coefficients, mu_i and nu_i.  In the
#   paper we use 55 values of each parameter, but only 5 (so, 5^3 = 125
#   simulations) here.
#   An important thing to notice about the code is that different techniques are
#   used depending on the equilibria for which we are searching.  We knew the
#   number and types of equilibria, so it was just a matter of numerically
#   finding thier values.
#   Another important note is that because the solvers approximate solutions
#   from vastly differnt places (searching paramter space from 0 to 100^100)
#   slightly different solutions could be generated.  That is, some solutions
#   were different by < 10^-15.  In these cases we did our best to eliminate
#   redudundancies and estimate as precisely as we could.
#
#~~ VALUE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Results from the simluation are entered into a list called `sim.results`
#   Each element corresponds to one parameter combination, and includes:
#      -a name of the unique run identificaiton (the parameter values)
#      -a list of parameter values
#      -a numberic value of number of equilibria from the analysis
#      -a matrix with rows for each equilibirum and columns:
#         -x,y cooridnates
#         -Jacobian Matrix values (11, 12, 21, 22)
#         -eigenvalues
#         -determinant
#         -discriminant
#         -trace
#      -a character vector with a classification of each equilibrium
#
#~~ CONTENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   0. Load libraries
#   1. Model
#   2. Parameter space
#   3. Preallocate space in list
#   4. Simulation
#     4.1. Loop over paramter space
#     4.2. Set parameter values
#     4.3. Search for equilibria
#       4.3.1. Set trivial and find boundary equilibria
#       4.3.2. Find interior equilibria
#     4.4. Assess stability of equilibria
#     4.5. Record results
#
####5###10###15###20###25###30###35###40###45###50###55###60###65###70###75###80

# 0. Load libraries
	library(package = "deSolve") # R package version 1.14
	library(package = "rootSolve") # R-package version 1.6

# 1. Model
	bd <- function(t, y, parameters) {
		with(as.list(c(y, parameters)),{
			  dx = (b - d - mu*(x^(eta)) - nu*(x^(theta)) + beta*y)*x
			  dy = (b - d - mu*(y^(eta)) - nu*(y^(theta)) + beta*x)*y
		list(c(dx, dy))})}

# 2. Parameter space
	parspace <- list(b = 5, d = 1, mu = 1, nu = 1,
	eta = 10^seq(from = -2, to = 2, length.out = 5),
	theta = 10^seq(from = -2, to = 2, length.out = 5),
	beta = 10^seq(from = -2, to = 2, length.out = 5) )

# 3. Preallocate space in list
	# Note: because we are strictly dealing with facultative mutualism, we only simulate for values of b > d, reflected in the calculation below
	sims <- sum(vapply(parspace[["d"]], function(x) x < parspace[["b"]], logical(length(parspace[["b"]]))))*length(parspace[["mu"]])*length(parspace[["nu"]])*length(parspace[["eta"]])*length(parspace[["theta"]])*length(parspace[["beta"]])
	sim.results <- vector(mode = "list", length = sims)

# 4. Simulation
		counter <- 1
	# 4.1 Loop over paramter space
		for (A in 1:length(parspace[["b"]])){
		for (B in 1:length(parspace[["d"]])){
			if (parspace[["d"]][B] < parspace[["b"]][A]){
		for (C in 1:length(parspace[["mu"]])){
		for (D in 1:length(parspace[["nu"]])){
		for (E in 1:length(parspace[["eta"]])){
		for (G in 1:length(parspace[["theta"]])){
		for (H in 1:length(parspace[["beta"]])){

	# 4.2. Set parameter values
		pars <- c(b = parspace[["b"]][A], d = parspace[["d"]][B], mu = parspace[["mu"]][C], nu = parspace[["nu"]][D], eta = parspace[["eta"]][E], theta = parspace[["theta"]][G], beta = parspace[["beta"]][H])

	# 4.3. Search for equilibria
		# 4.3.1. Set trivial and find boundary equilibria 
			no.mut.samp.space <- matrix(c(0, 0, 0.01, 0, 0.01, 0), ncol = 2)
			no.mut.samp.space.pts <- nrow(no.mut.samp.space)
			no.mut.space.mat <- matrix(data = NA, nrow = no.mut.samp.space.pts, ncol = 2)
			no.mut.ODE.max.time <- 1000
			no.mut.ODE.steps <- 0.01
			no.mut.pars <- pars
			no.mut.pars[["beta"]] <- 0

			no.mut.space.mat[1,] <- c(0, 0)
			for (i in 2: no.mut.samp.space.pts){
				init <- c(x = no.mut.samp.space[i, 1], y = no.mut.samp.space[i, 2])
				ODE <- ode(y = init, func = bd, times = seq(0, no.mut.ODE.max.time, no.mut.ODE.steps), parms = no.mut.pars)[((no.mut.ODE.max.time/no.mut.ODE.steps) + 1), c(2, 3)]
				no.mut.space.mat[i,] <- ODE
			}

		# 4.3.2. Find interior equilibria 
			no.mut.int.eq <- c(x = no.mut.space.mat[2, 2], y = no.mut.space.mat[3, 1])
			ode.out <- ode(y = no.mut.int.eq, func = bd, times = seq(0, no.mut.ODE.max.time, no.mut.ODE.steps), parms = pars)

			hyp.l.out <- (no.mut.ODE.max.time/no.mut.ODE.steps) + 1
			obs.l.out <- nrow(ode.out)

			if (obs.l.out != hyp.l.out) { # if there are only three equilibria, then assign space.mat
				space.mat <- no.mut.space.mat
			} else { # beginning if there are > three equilibria, then assign space.mat
				if(any(is.na(ode.out[hyp.l.out, c(2, 3)]) == TRUE)) { # make sure last value of out.out is not NA
					space.mat <- no.mut.space.mat
				} else { # beginning of last value of out.out not NA
					int.eq <- ode.out[hyp.l.out, c(2, 3)]
	
					if (pars[["eta"]] <= 1 & pars[["theta"]] <= 1) { # beginning of eta and theta being <= 1
						x.space <- int.eq[1]*(10^seq(from = 0, to = 100, length.out = 100))/int.eq[1]
						y.space <- int.eq[2]*(10^seq(from = 0, to = 100, length.out = 100))/int.eq[2]
						saddle.space <- matrix(data = c(x.space, y.space), ncol = 2, byrow = FALSE)
						saddle.samp.points <- nrow(saddle.space)
						saddle.mat <- matrix(data = NA, nrow = saddle.samp.points, ncol = 2)

						for (j in 1:saddle.samp.points) { # beginning of j for loop over saddle sampling space
							init <- c(x = saddle.space[j, 1], y = saddle.space[j, 2])
							ST <- steady(y = init, func = bd, parms = pars, method = "stode", positive = T, verbose = F, rtol = 1e-100)
							run.precis <- unlist(attributes(ST)["precis"])
							l.run.precis <- length(run.precis)

							if( (run.precis[l.run.precis] < 0.01) == T) { # if solution is precise, keep
								saddle.mat[j,] <- ST$y	
							} else { # otherwise, give zeros
								saddle.mat[j,] <- c(0, 0)
							}
						} # end of j for loop over saddle sampling space

						stab.r <- ((int.eq[1]^2) + (int.eq[2]^2))^0.5
						sadd.r <- ((saddle.mat[,1]^2) + (saddle.mat[,2]^2))^0.5
						if (any(sadd.r < stab.r) ) { # removes values < the stable node
							small.sadds <- which(sadd.r < stab.r)
							saddle.mat <- saddle.mat[-small.sadds,]
						}

						sadd.r2 <- ((saddle.mat[,1]^2) + (saddle.mat[,2]^2))^0.5
						diff.r <- sadd.r2 - stab.r
						prop.r <- diff.r/stab.r
						if (any(prop.r < 0.01)) {
							close.r <- which(prop.r < 0.01)
							saddle.mat <- saddle.mat[-close.r,]
						}

						if (length(saddle.mat) > 2){
							bound.eq1 <- which( (((no.mut.space.mat[2,1]-saddle.mat[,1])^2) + ((no.mut.space.mat[2,2]-saddle.mat[,2])^2))^0.5 < 0.01) # removes boundry eq with x = 0
							if (length(bound.eq1) > 0) {saddle.mat <- saddle.mat[-bound.eq1, ]}
						}

						if(length(saddle.mat) > 2){	
						bound.eq2 <- which( (((no.mut.space.mat[3,1]-saddle.mat[,1])^2) + ((no.mut.space.mat[3,2]-saddle.mat[,2])^2))^0.5 < 0.01) # removes boundry eq with y = 0
							if (length(bound.eq2) > 0) {saddle.mat <- saddle.mat[-bound.eq2, ]}
						}

						u.sad.mat <- unique(saddle.mat)
						l.u.sad.mat <- length(u.sad.mat)
						if (l.u.sad.mat > 2){
							ones <- rep(1, nrow(saddle.mat))
							agg <- aggregate(ones, by = list(saddle.mat[,1], saddle.mat[,2]), FUN = sum)
							max.count <- max(agg[,3], na.rm = T)
							u.sad.mat <- as.matrix(agg[sample(which(agg[,3]== max.count), 1),c(1,2)])
						}

						space.mat <- rbind(no.mut.space.mat, int.eq, u.sad.mat)

					} else { # end of eta and theta being <= 1
						space.mat <- rbind(no.mut.space.mat, int.eq)
				} # end of eta and theta being < 1
			} # end of last value of out.out not NA
		} # end if there are > three equilibria, then assign space.mat

		if (any(is.na(space.mat) == TRUE)) {
			space.mat.no.na <- space.mat[-which(is.na(space.mat) == TRUE, arr.ind = TRUE)[,1],]
			u <- space.mat
		} else {
			u <- space.mat
		}

	# 4.4. Assess stability of equilibria
		n.equilibria <- nrow(u)

		if (n.equilibria < 10 & n.equilibria > 0) {
			results.data <- c("eqx", "eqy", "JacA", "JacB", "JacC", "JacD", "eig1", "eig2", "delta", "disc", "trace")
			eq.mat <- matrix(data = NA, nrow = n.equilibria, ncol = length(results.data))
			colnames(eq.mat) <- results.data
			eq.mat[,c(1,2)] <- u
			classification <- vector(mode = "character", length = n.equilibria)
			for (i in 1:n.equilibria){
				eq.eval <- c(x = u[i,1], y = u[i,2])
				eq.dist <- (eq.eval[1]^2 + eq.eval[2]^2)^0.5
				if ( eq.dist < 1e-8) { # if equilibrium distance < 1e-8 from 0
					if(eq.dist == 0) {
						if (n.equilibria == 3) {
								stab.dist <- ((eq.mat[2,1]^2) + (eq.mat[2,2]^2))^0.5
							} else {
								stab.dist <- ((int.eq[1]^2) + (int.eq[2]^2))^0.5	
							}
						if (stab.dist < 1e-8) { # the stable point < 1e-8 away
							pert.size <- stab.dist/10
						} else { # beginning is the stable point > 1e-8 away
							pert.size <- 1e-8
						}
					} else { # equilibrium > 0
						pert.size <- eq.dist/10
					}
				} else { # if equilibrium distance >= 1e-8 from 0
					pert.size <- 1e-8
				} # end if equilibrium distance >= 1e-8
				jac <- jacobian.full(eq.eval, func = bd, parms = pars, pert = pert.size)
				eq.mat[i, c(3:6)] <- jac
				e.vals <- eigen(jac, only.values = TRUE)$values
				eq.mat[i, c(7, 8)] <- e.vals
				tr <- jac[1,1] + jac[2, 2]
				eq.mat[i, 11] <- tr
				mod.det <- det(jac)
				eq.mat[i, 9] <- mod.det
				disc <- (tr^2) - (4*mod.det)
				eq.mat[i, 10] <- disc
			if (is.complex(e.vals) == F) {
				if(e.vals[1] < 0 & e.vals[2] < 0) {mod.class <- "Stable node"}
				if(any(e.vals < 0) == T & any(e.vals > 0) == T) {mod.class <- "Saddle node"}
				if(e.vals[1] > 0 & e.vals[2] > 0) {mod.class <- "Unstable node"}
			} else {
				if(Re(e.vals[1]) < 0) {mod.class <- "Stable spirial"}
				if(Re(e.vals[1]) > 0) {mod.class <- "Unstable spiral"}
				if(Re(e.vals[1]) == 0) {mod.class <- "Neutral center"}
			}
			classification[i] <- mod.class
				} # stability for each equilibrium
		} else { # end if < 10 & > 0
			eq.mat <- matrix(data = u, nrow = n.equilibria, ncol = 2, byrow = TRUE)
			}

	# 4.5. Record results
		runID <- paste0(
			"b:", pars[["b"]],
			",d:", pars[["d"]],
			",mu:", pars[["mu"]],
			",nu:", pars[["nu"]],
			",eta:", pars[["eta"]],
			",theta:", pars[["theta"]],
			",beta:", pars[["beta"]]
			)
		print(runID)
		names(sim.results)[counter] <- runID
		sim.results[[runID]] <- list(
			"parameters" = pars,
			"n.equilibria" = n.equilibria,
			"analysis" = eq.mat,
			"classification" = classification
			)

		counter <- counter + 1
		} # beta, H
		} # theta, G
		} # eta, E
		} # nu, D
		} # mu, C
			} # if B < A
		} # d, B
		} # b, A