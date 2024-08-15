# Compare  stepped wedge vs parallel  Hussey Hughes B.R

# chb 2024 07 15
# compare statistical power for these 2 trial designs
#   use Hussey Hughes model, time is a factor, 1 common random effect per site
#
#
#  Compare power for 2 runs:
#     Par - parallel design where half the units are condition 0, half are condition 1
#     Sw - stepped wedge where crossover occurs individually for each site
#
# parameters N - number of units also number of time points - 1 observed
#            beta - common change in mean due to outcome
#            fix variance at each time to be 1

# parallel group design
#      condition matrix is Tx
#    generate 1 random intercept for all units:
#       for comparability w sw use a_i + b_i as unit specific
#       total variance is Var(a) + Var(b) + Var(e) = 1
#     #       analyze w 1 random effect   ( 1 | row )

# stepped wedge group design
#   condition matrix is Cross
#   generate 1 random effect for units
#        a_i  common random effect on all time points
#           b0_i random effect specific to CrossoverTime = 0
#           b1_i random effect specific to CrossoverTime = 1
#        combine a + b0 * ( 1 - Cross ) + b1 * (Cross)  + e


## Settings

TheModel <- "Hussey Hughes Time(Factor),Random Intercept"


####################################
# Parameters
####################################

alpha <- 0.05  # Type I error
beta  <- 0.25  # effect size
N <- 20 # must be even
Proportion <- 1/10  # for monitoring progress


Time <- 1 : ( N + 1)  # treat as a fixed factor later


Va <- 0.005
Vb <- 0.05
Ve <- 1 - Va - Vb # error variance


#  get power near .8 to +/- .02 as CI
Nreps <-   10 # ( (.8 * .2 ) / .01^2  ) # 1600

Stats <- matrix ( NA , nrow = Nreps , ncol = 8 )
colnames ( Stats ) <- c( "Tval.par.null", "Pval.par.null",
                         "Tval.par.alt" , "Pval.par.alt",
                         "Tval.sw.null", "Pval.sw.null",
                         "Tval.sw.alt" , "Pval.sw.alt" )


# this function is
FixedEffects <- function ( N  , Time  ) {
  # generate long vectors of Treatment for parallel design and stepped wedge, rows, columns,
  # input
  #     N - number of steps , N+1 is length of obsns
  #     Time 1 : N+1
  #
  #  output:
  #     Tx - treatment indicator for parallel
  #     Cross - treatment indicator for stepped wedge
  #     the.rows -- index of site
  #     TimeValue -- index of time

  ones <- rep ( 1 , N + 1 )

  the.rows <- as.vector (  ( 1 : N ) %*% t ( ones ))

  # for par
  Tx <- as.vector ( rbind ( matrix ( 0 , nrow = N / 2 , ncol = N + 1 ) ,
                            matrix ( 1 , nrow = N / 2 , ncol = N + 1 )))
  # TimeValue is categorical value of time
  TimeValue <- factor ( as.vector ( rep( 1 , N ) %*% t(Time) ))


  # indicator of times since crossover occurs - use as main covariate in sw analses
  Crossover <- matrix ( 0 , nrow = N , ncol = (N + 1 ) )
  for ( u in 1 : N   ) {
    Crossover [ u , c ( (u + 1) : (N + 1 ) ) ] <- 1
  }

  Cross <- as.vector ( Crossover )

  fixed <- cbind ( the.rows , Tx ,  Cross , TimeValue )
  colnames ( fixed ) <- c ( "the.rows" , "Tx" , "Cross" , "TimeValue")
  return ( fixed )
}


################
#
################

Fixed <- FixedEffects (N , N+1 )
the.rows <- Fixed [ , "the.rows"]
Tx <- Fixed [ , "Tx"]
Cross <- Fixed [ , "Cross"]
TimeValue <- Fixed [ , "TimeValue"]



Outcomes <- function ( Va , Vb , N , beta ) {
  # generate a single set of outcomes
  #  Y.null - parallel design under null
  #  Y      - parallel design under alt
  #  Z.null - stepped wedge design under null
  #  Z      - stepped wedge design under alt

 Ve <- 1 - Va - Vb

  # Random effects -  include 2 randoom effects for parallel, 3 for sw
  a <- as.vector ( (rnorm ( N ) * sqrt ( Va)) %*% t ( ones) )
  b0 <- rnorm ( N ) * sqrt ( Vb )
  b <- as.vector ( ( b0  ) %*% t ( ones ))
  e <-  matrix ( rnorm ( N * ( N + 1 ) ) * sqrt ( Ve ) , ncol = N + 1 )

  e <- as.vector ( e )

  Y.null <- a + b + e

  #######################
  #  Stepped Wedge
  #######################

  # generate a new random effect just for after crossover
  b1 <- rnorm ( N ) * sqrt ( Vb )

  # different random intercept before and after crossover
  b.combined <- b0 * ( 1 - Crossover ) + b1 * Crossover
  b.combined <- as.vector ( b.combined )

  beta.crossover <- as.vector ( beta * Crossover )

  Z.null <- a + b.combined + e

  Y <- beta * Tx + a + b + e

  Z <- beta.crossover + a + b.combined + e


  the.Outcomes <- cbind ( Y.null , Y , Z.null  , Z  )
  names ( the.Outcomes ) <- c( "Y.null" , "Y" , "Z.null" , "Z")
  return ( the.Outcomes)
}


GrabTP <- function ( model , Covar , rslts = c( "t value" , "Pr(>|t|)" )) {
  # pick out results from a model for Covar
  return ( summary( model )$coeff [ Covar , rslts ]  )

  }





StartTime <- Sys.time()

for ( r in 1 : Nreps ) {

   theOutcomes <- Outcomes  ( Va , Vb , N , beta )

   Y.null <- theOutcomes [ , "Y.null"]
   Y <- theOutcomes [ , "Y"]
   Z.null <- theOutcomes [ , "Z.null"]
   Z <- theOutcomes [ , "Z"]

  #######################
  #  Parallel design
  #######################


  # Analysis as per Hussey Hughes, time as fixed factor, 1 random intercept
  m0.par.null <- suppressMessages ( lmer ( Y.null ~ -1 + TimeValue +  Tx + ( 1 | the.row ) ))
  Stats [ r , 1:2 ] <- GrabTP ( m0.par.null , "Tx")


  m1.par.alt <- suppressMessages ( lmer ( Y ~ -1 + TimeValue + Tx + ( 1 | the.row ) ))
  Stats [ r , 3:4 ] <- GrabTP ( m1.par.alt , "Tx" )

  ##  Stepped Wedge design

  m0.sw.null <- suppressMessages ( lmer ( Z.null ~ -1 + TimeValue + Cross + ( 1 | the.row ) ))
  Stats [ r , 5:6 ] <- GrabTP ( m0.sw.null , "Cross")

  m1.sw.alt <- suppressMessages ( lmer ( Z ~ -1 + TimeValue + Cross + ( 1 | the.row ) ) )
  Stats [ r, 7:8 ] <- GrabTP ( m1.sw.alt , "Cross" )

  if ( round ( r / Nreps / Proportion ) == r / Nreps / Proportion ) {

    cat ("\n" , r , "\n")
  } else {
      if ( r %% 50 == 0 ) cat ( "\n")
      cat ( ".")
  }
}

EndTime <- Sys.time()
# it looks machine dependent whether this is in miliseconds or tenths of a seconds
Seconds <- ( as.numeric ( EndTime ) - as.numeric ( StartTime ) )

ElapsedTime <- paste ( floor ( Seconds / 60 ) , " Minutes , " , round ( Seconds %% 60) , " Seconds" )
ElapsedTime




TheParameters <- data.frame ( beta , alpha , Nreps , Va , Vb , ElapsedTime , TheModel )
names( TheParameters ) <- c( "Effect Size" , "alpha" , "Nreps" , "Var a" , "Var b" , "Elapsed Time" , "Analysis Model")

TheParameters

Answr <- NULL
Answr [ "Actual.alpha.par" ] <- mean ( Stats [ , "Pval.par.null"] <= alpha )
Answr [ "crit.par"] <- quantile ( abs ( Stats [ , "Tval.par.null" ] ) , 1 - alpha )
Answr [ "Pval.par.alt" ] <- mean ( Stats [ , "Pval.par.alt" ] <= alpha )
Answr [ "Prob.Tval.par.alt" ] <- mean( abs ( Stats [ , "Tval.par.alt"] ) > Answr [ "crit.par"]  )


Answr [ "Actual alpha.sw" ] <- mean ( Stats [ , "Pval.sw.null"] <= alpha )
Answr [ "crit.sw"] <- quantile ( abs ( Stats [ , "Tval.sw.null" ] ) , 1 - alpha )
Answr [ "Pval.sw.alt" ] <- mean ( Stats [ , "Pval.sw.alt" ] <= alpha )
Answr [ "Prob.Tval.sw.alt" ] <- mean( abs ( Stats [ , "Tval.sw.alt"] ) > Answr [ "crit.sw"]  )


TheParameters
Answr





