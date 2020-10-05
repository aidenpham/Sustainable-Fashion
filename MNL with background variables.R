### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list (
  modelName = "MNL Combined4",
  modelDescr = "MNL model",
  indivID = "ID", ## ID of participants
  mixing = FALSE ## mixed logit or random distribution parameters
)

apollo_control$panelData = FALSE ## define if there are panel data (TRUE if panel data)

#### LOAD DATA 
database = read.csv("Data.csv",header=TRUE,sep=";")


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(c=0, asc_opt_out=0,
              b_Price=0, b_Origin=0, b_Fiber=0, b_Wash=0, b_Dry=0, b_Env=0, b_Labor=0, b_Gender=0,
              b_Residence=0, b_Clothes=0, b_JeansFreq=0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_opt_out")


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
    V[['alt1']]  = c + b_Price*Price1 + b_Origin*Origin1 + b_Fiber*Fiber1 +
                    b_Wash*Wash1 + b_Dry*Dry1 + b_Env*log(Env1) + b_Labor*log(Labor1) + 
                    b_Gender*Gender + b_Residence*Residence + b_Clothes*(Clothes*20-10) + b_JeansFreq*JeansFreq 
                    
    V[['alt2']]  = c + b_Price*Price2 + b_Origin*Origin2 + b_Fiber*Fiber2 +
                    b_Wash*Wash2 + b_Dry*Dry2 + b_Env*log(Env2) + b_Labor*log(Labor2) + 
                    b_Gender*Gender + b_Residence*Residence + b_Clothes*(Clothes*20-10) + b_JeansFreq*JeansFreq 
                    
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
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#### MODEL ESTIMATION 
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


#### MODEL OUTPUTS
apollo_modelOutput(model,modelOutput_settings=list(printPVal=TRUE))

apollo_saveOutput(model)

df = read.csv("MNL Combined4_estimates.csv",header=TRUE,sep=",")
library(writexl)
write_xlsx(df,"MNL Combined4_estimates.xlsx")
