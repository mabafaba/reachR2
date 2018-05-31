



#  design effect
design.effect<- function(ci,ci_rd){
  ci/ci_rd
}

random.sampling.standard.error<-function(n,p=.5){sqrt(p*(1-p))/sqrt(n)}




# stratified
stratified.standard.error<-function(sample, population, p=0.5){
  # "Survey Sampling", Kirsh et al. (1965), formula 3.3.4, p.82 (pdf p. 96)
  # reference formula variables:
  N_h<-population
  N  <- sum(population)
  n_h<- sample
  p_h<- p

  W_h=N_h/N
  f=n_h/N
  # reference formula:

  variance<-sum(
    W_h^2    *
      (1-f)   *
      p_h    *
      (1-p_h) /
      (n_h - 1)
  )

  se<- sqrt(variance)
  se
}


stratified.standard.error2<-function(sample, population, p=0.5){
  N_i<-population
  N <- sum(population)
  n_i<- sample
  p_i<- p

  SE = sqrt(sum(

    (N_i^2)*((N_i-n_i)/N_i)*(p_i*(1-p_i)/(n_i-1))

  ))/N
  SE
}

stratified.margin.of.error<-function(sample,population,cl=0.95){
  z.score(cl) * stratified.standard.error(sample,population)
}


stratified_confidence_interval<-function(sample,population,p=0.5,cl=0.95){
  SE <- stratified.standard.error2(sample,population,p)
  confidence.interval(p,SE,cl)
}

# margin of error: standard error * Z
margin.of.error<-function(standard.error,cl=0.95){
  z.score(cl)*standard_error
}

# calculating Z
z.score <- function(cl){qnorm(1-(1-cl)/2)}

binominal.variance<-function(n,p){
  n*p*(1-p)
}

binominal.standard.deviation<-function(n,p){
  sqrt(binominal.variance(n,p))
}


confidence.interval<-function(p,SE,cl=0.95){
  z = z.score(cl)
  c( p - (z * SE),
     p + (z * SE))
}









