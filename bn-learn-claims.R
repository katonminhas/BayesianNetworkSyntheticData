# Katon Minhas - Reasoning Under Uncertainty Research Paper
library(DAAG)
library(tidyverse)
library(visNetwork)
library(bnlearn)
library(LaplacesDemon)


path<-"C:/Users/Katon/Documents/JHU/ReasoningUnderUncertainty/ResearchPaper/data/NON_PHI_sample_claims.csv"
claims <- read.table(path, sep=",", header=TRUE)

log_claims <- data.frame(claims)

log_claims$AAR_FINAL_STATUS_CD <- as.factor(log_claims$AAR_FINAL_STATUS_CD)
log_claims$LINE_UNIT_CNT <- as.factor(log_claims$LINE_UNIT_CNT)


# Log all columns to ensure normal distribution
for (name in names(log_claims)[3:10]){
  log_claims[,name] = log(log_claims[,name] + 0.001)
}


structure <- hc(log_claims, score = "bic-cg")

plot.network <- function(structure, ht = "500px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE 
                      )
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "120%"))
}
plot.network(structure)

bn_mod <- bn.fit(structure, data =log_claims, method = "mle-cg")

claims_sim <- rbn(bn_mod, 500000)

# Convert to real value and Z-score standardize
for (name in names(claims_sim)[3:10]){
  # Convert to Z-scores
  claims_sim[,name] = (claims_sim[,name] - mean(claims_sim[,name])) / sd(claims_sim[,name])
  
  # fit to scale of original data
  claims_sim[,name] = (claims_sim[,name] * sd(log_claims[,name])) + mean(log_claims[,name])
  
  # Convert to real value
  claims_sim[,name] = round(exp(claims_sim[,name]) -0.001, digits=2)
}

# Rematch data types
claims_sim$AAR_FINAL_STATUS_CD <- as.numeric(claims_sim$AAR_FINAL_STATUS_CD == "Y")
claims_sim$LINE_UNIT_CNT <- as.numeric(claims_sim$LINE_UNIT_CNT)

claims$AAR_FINAL_STATUS_CD <- as.numeric(claims$AAR_FINAL_STATUS_CD == "Y")
claims$LINE_UNIT_CNT <- as.numeric(claims$LINE_UNIT_CNT)

# Evaluation

## KL-Divergence (continuous)

for (name in names(claims)){
  real <- dnorm(claims[,name])
  synth <- dnorm(claims_sim[,name])
  div <- KLD(real, synth)
  print(div['mean.sum.KLD'])
}


# Pairwise correlation difference
pcd <- abs(cor(claims) - cor(claims_sim))
pcd <- data.frame(pcd)

for (name in names(pcd)){
  print(sum(pcd[,name])/9)
}





