### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="ML4",
  modelDescr ="Panel ML model",
  indivID    ="ID",
  mixing    = TRUE
)


#### LOAD DATA
database = read.csv("Data.csv",header=TRUE,sep=";")

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_opt_out=0,
              b_Price=0, b_Origin=0, b_Fiber=0, b_Wash=0, b_Dry=0, b_Env=0, b_Labor=0, b_Buy=0,
              sigma_Buy=1, sigma_Env=1, sigma_Labor=1,
              b_JeansFreq=0, 
              b_GenderPrice=0, 
              b_ResidencePrice=0, b_ConcernSwtLabor=0,
              b_SkLabor=0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_opt_out")

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 250,
  interUnifDraws = c(),
  interNormDraws = c("draws"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["RND_Env"]]   =  b_Env + sigma_Env * draws
  randcoeff[["RND_Labor"]] =  b_Labor + sigma_Labor * draws
  randcoeff[["RND_Buy"]]   =  b_Buy + sigma_Buy * draws 
  
  return(randcoeff)
}


#### GROUP AND VALIDATE INPUTS 
apollo_inputs = apollo_validateInputs()


#### DEFINE MODEL AND LIKELIHOOD FUNCTION  
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']]  = b_Price*Price1 + b_Origin*Origin1 + b_Fiber*Fiber1 +
    b_Wash*Wash1 + b_Dry*Dry1 + RND_Env*log(Env1) + RND_Labor*log(Labor1) +
    b_JeansFreq*JeansFreq +
    Price1*(Gender*b_GenderPrice + Residence*b_ResidencePrice) +
    
    log(Labor1)*(ConcernSwt*b_ConcernSwtLabor + Skepticism*b_SkLabor) + RND_Buy
  
  V[['alt2']]  = b_Price*Price2 + b_Origin*Origin2 + b_Fiber*Fiber2 +
    b_Wash*Wash2 + b_Dry*Dry2 + RND_Env*log(Env2) + RND_Labor*log(Labor2) +
    b_JeansFreq*JeansFreq +
    Price2*(Gender*b_GenderPrice + Residence*b_ResidencePrice) +
    
    log(Labor2)*(ConcernSwt*b_ConcernSwtLabor + Skepticism*b_SkLabor) + RND_Buy
  
  V[['opt_out']] = asc_opt_out
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, opt_out=3), 
    avail         = 1,
    choiceVar     = Choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#### MODEL ESTIMATION                          
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))


#### MODEL OUTPUTS 
apollo_modelOutput(model,modelOutput_settings=list(printPVal=TRUE))

apollo_saveOutput(model)

df = read.csv("ML4_estimates.csv",header=TRUE,sep=",")
library(writexl)
write_xlsx(df,"ML4_estimates.xlsx")
