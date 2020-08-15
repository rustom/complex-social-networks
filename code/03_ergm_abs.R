##################################################
## Project: MDT Analysis
## Script purpose: Survey Analysis
## Date: 2019-01-25
## Author: Rustom Ichhaporia - rustomi2@illinois.edu
## Author: Diego Zara - dgomezara@u.northwestern.edu
##################################################

folder <- getwd()
setwd(folder)
set.seed(42)

# Loading data ------------------------------------------------------------

# Libraries
library(statnet)
detach("package:igraph")
rm(list = ls())

# Load datasets v2 --------------------------------------------------------
invitations.v2 <- read.csv("results/01_survey/invitations_v2.csv", sep = ",", stringsAsFactors = F)
relationships.v2 <- read.csv("results/01_survey/relationships_v2.csv", sep = ",", stringsAsFactors = F)
user.info.v2 <- read.csv("results/01_survey/users_profiles_v2.csv", sep = ",", stringsAsFactors = F)
complementary.v2 <- read.csv("results/01_survey/complement_v2.csv", sep = ",", stringsAsFactors = F)

# Load early datasets ----------------------------------------------------
invitations.v1 <- read.csv("data/v1/invitations.v1.csv", sep = ",", stringsAsFactors = F)
relationships.v1 <- read.csv("results/01_survey/relationships_v1.csv", sep = ",", stringsAsFactors = F)
user.info.v1 <- read.csv("results/01_survey/users_profiles_v1.csv", sep = ",", stringsAsFactors = F)
complementary.v1 <- read.csv("results/01_survey/complement_v1.csv", sep = ",", stringsAsFactors = F)

# Concatenate with secondary datasets ------------------------------------

# Invitations
invitations.v1$version <- NULL
names(invitations.v1)[names(invitations.v1) == "type"] <- "status"

names(invitations.v2)[names(invitations.v2) == "sender_id"] <- "sender"
names(invitations.v2)[names(invitations.v2) == "recipient_id"] <- "recipient"

keep_columns <- names(invitations.v1)
invitations.v2 <- invitations.v2[keep_columns]

invitations <- rbind(invitations.v1, invitations.v2)
invitations$origin <- NULL

rm(invitations.v1, invitations.v2)

# Profiles
shared_columns <- intersect(colnames(user.info.v1), colnames(user.info.v2))
user.info.v1 <- user.info.v1[shared_columns]
user.info.v2 <- user.info.v2[shared_columns]
user.profiles <- rbind(user.info.v1, user.info.v2)

rm(user.info.v1, user.info.v2)

# Relationships
relationships <- rbind(relationships.v1, relationships.v2)
rm(relationships.v1, relationships.v2)

# Complementary skills
complementary <- rbind(complementary.v1, complementary.v2)
rm(complementary.v1, complementary.v2)

# Variables ---------------------------------------------------------------
invitations <- invitations[!duplicated(invitations), ]
user.profiles <- user.profiles[!duplicated(user.profiles), ]
relationships <- relationships[!duplicated(relationships),]

invitations <- invitations[order(invitations$project_id),]
user.profiles <- user.profiles[order(user.profiles$project_id),]
relationships <- relationships[order(relationships$type, relationships$project_id),]

rownames(invitations) <- NULL
rownames(user.profiles) <- NULL
rownames(relationships) <- NULL

# Remove users without info
invitations <- invitations[(invitations$sender %in% user.profiles$user_id),]
invitations <- invitations[(invitations$recipient %in% user.profiles$user_id),]
relationships <- relationships[(relationships$source %in% user.profiles$user_id),]
relationships <- relationships[(relationships$target %in% user.profiles$user_id),]

# If subset = 1, keep only Leslie and Purdue data
# If subset = 2, remove Argentina, Leslie 2-4, and Purdue 2-4
# If subset = 3, remove Argentina, Leslie, Purdue
# If subset = 4, v1, and remove Argentina, Leslie, Purdue
subset_data <- 4
if(subset_data == 1){
  project_ids_keep <- c(120, 124, 130, 132, 136, 137, 138, 139)
  invitations <- invitations[invitations$project_id %in% project_ids_keep, ]
  relationships <- relationships[relationships$project_id %in% project_ids_keep, ]
} else if (subset_data == 2){
  project_ids_remove <- c(105, 106, 124, 130, 132, 137, 138, 139) #remove argentina as well 
  invitations <- invitations[!invitations$project_id %in% project_ids_remove, ]
  relationships <- relationships[!relationships$project_id %in% project_ids_remove, ]
} else if (subset_data == 3){
  project_ids_remove <- c(78, 105, 106, 124, 130, 132, 131, 133, 136, 137, 138, 139)
  invitations <- invitations[!invitations$project_id %in% project_ids_remove, ]
  relationships <- relationships[!relationships$project_id %in% project_ids_remove, ]
} else if (subset_data == 4){
  project_ids_keep <- c(11, 91, 92, 101, 120, 131, 133, 136, 221,
                        6, 10, 15, 18, 19, 25) # Secondary dataset
  invitations <- invitations[invitations$project_id %in% project_ids_keep, ]
  relationships <- relationships[relationships$project_id %in% project_ids_keep, ]
  user.profiles <- user.profiles[user.profiles$project_id %in% project_ids_keep, ]
}

# Declare international NA as 0
user.profiles$international[(is.na(user.profiles$international))] <- 0

# Log-scale
user.profiles$know.degree <- log(user.profiles$know.degree+1)
user.profiles$work.degree <- log(user.profiles$work.degree+1)
user.profiles$enjoy.degree <- log(user.profiles$enjoy.degree+1)

# Fix social-metrics
user.profiles$work.constraint[(is.na(user.profiles$work.constraint))] <- max(user.profiles$work.constraint, na.rm = T)
user.profiles$know.constraint[(is.na(user.profiles$know.constraint))] <- max(user.profiles$know.constraint, na.rm = T)
user.profiles$enjoy.constraint[(is.na(user.profiles$enjoy.constraint))] <- max(user.profiles$enjoy.constraint, na.rm = T)

# Check correlations
cor(user.profiles[,c("know.degree","know.betweenness","know.constraint","know.closeness")])
cor(user.profiles[,c("work.degree","work.betweenness","work.constraint","work.closeness")])
cor(user.profiles[,c("enjoy.degree","enjoy.betweenness","enjoy.constraint","enjoy.closeness")])
cor(user.profiles[,c("know.constraint","work.constraint","enjoy.constraint")])

# Network creation --------------------------------------------------------

# Create network (accepted (AC), rejected (RJ), invited (IN), rejected & invited (NEG), or all)
g <- network(invitations[,c("sender","recipient")], directed=T, matrix.type = "edgelist")

users.invitations <- data.frame(id=get.vertex.attribute(g, "vertex.names"), stringsAsFactors = F)
users.invitations <- merge(users.invitations, user.profiles, by.x = c("id"), by.y=c("user_id"), all.x = T)

# Fix the age
users.invitations$age <- as.numeric(users.invitations$age)
users.invitations$age[(is.na(users.invitations$age) | users.invitations$age < 18)] <- 18

# Factor attributes
users.invitations$gender <- as.numeric(as.factor(users.invitations$gender))     # 1: male, 2: female
users.invitations$international <- as.numeric(users.invitations$international)  # 0: US, 1: international 

# Create the loop interactions
for(p in unique(relationships$project_id)){
  for(t in unique(relationships$type)){
    unused_ids <- setdiff(user.profiles$user_id[user.profiles$project_id == p], 
                          union(relationships$source[relationships$project_id == p & relationships$type == t], 
                                relationships$target[relationships$project_id == p & relationships$type == t]))
    if(length(unused_ids) > 0){
      relationships <- rbind(relationships, data.frame(project_id = p, type = t, source = unused_ids, target = unused_ids))
    }
  }
}

# Set attributes
for (i in 2:ncol(users.invitations)) {
  set.vertex.attribute(g, colnames(users.invitations)[i], users.invitations[, i])
}

# Set relationship networks
for(i in unique(relationships$type)){
  assign(paste0("network.",i), network(relationships[(relationships$type == i), c("source", "target")], loops = F, directed=F, matrix.type = "edgelist"))
}

# Set complementary network
complementary.network <- network(complementary[(complementary$complement == 1), c("user1", "user2")], loops = F, directed=F, matrix.type = "edgelist")

# Plot --------------------------------------------------------------------
gplot(g, vertex.col = as.numeric(as.factor(get.vertex.attribute(g, "project_id")))+1)

# ERGM --------------------------------------------------------------------
decay <- 0.1

ergm.1 <- ergm(g ~ edges + 
                 gwidegree(decay, fixed=T) +
                 gwodegree(decay, fixed=T) +
                 dgwdsp(decay, fixed=T) + 
                 edgecov(network.1) +
                 edgecov(network.4) +
                 edgecov(complementary.network) +
                 nodematch("international") + 
                 nodematch("gender") + 
                 nodeifactor("gender") +
                 nodeicov("age") +
                 nodeicov("project.skill.score") +
                 nodeicov("leadership.score") +
                 nodeicov("collective.score") +
                 nodeicov("social.skills.score") +
                 nodeicov("creativity.score") +
                 nodeicov("personality.agreeableness.score") +
                 nodeicov("personality.conscientiousness.score") +
                 nodeicov("personality.extraversion.score") +
                 nodeicov("personality.neuroticism.score") +
                 nodeicov("personality.openness.score") +
                 nodeicov("work.constraint") +
                 nodeicov("enjoy.constraint") +
                 nodeofactor("gender") +
                 nodeocov("age") +
                 nodeocov("project.skill.score") +
                 nodeocov("leadership.score") +
                 nodeocov("collective.score") +
                 nodeocov("social.skills.score") +
                 nodeocov("creativity.score") +
                 nodeocov("personality.agreeableness.score") +
                 nodeocov("personality.conscientiousness.score") +
                 nodeocov("personality.extraversion.score") +
                 nodeocov("personality.neuroticism.score") +
                 nodeocov("personality.openness.score") +
                 nodeocov("work.constraint") +
                 nodeocov("enjoy.constraint") +
                 absdiff("age") +
                 absdiff("project.skill.score") +
                 absdiff("leadership.score") +
                 absdiff("collective.score") +
                 absdiff("social.skills.score") +
                 absdiff("creativity.score") +
                 absdiff("personality.agreeableness.score") +
                 absdiff("personality.conscientiousness.score") +
                 absdiff("personality.extraversion.score") +
                 absdiff("personality.neuroticism.score") +
                 absdiff("personality.openness.score") +
                 absdiff("work.constraint") +
                 absdiff("enjoy.constraint"),
               constraints=~blockdiag("project_id"),
               control = control.ergm(MCMLE.maxit = 800,
                                      #parallel = 3,
                                      seed=20),
               eval.loglik = F)
summary(ergm.1)
ergm.1<-logLik(ergm.1, add=TRUE)

exp(summary(ergm.1)$coefs[[1]]) #Odds
exp(summary(ergm.1)$coefs[[2]]) #Odds


# Sub-networks ------------------------------------------------------------

# Creation of filtered networks
g.AC <- g
g.RJ <- g
g.IG <- g

for(i in 1:nrow(invitations)){
  inv <- invitations[i,]
  if(inv$status == "AC"){
    g.RJ[inv$sender,inv$recipient] <- 0
    g.IG[inv$sender,inv$recipient] <- 0
  }else if(inv$status == "RJ"){
    g.AC[inv$sender,inv$recipient] <- 0
    g.IG[inv$sender,inv$recipient] <- 0
  }else{
    g.AC[inv$sender,inv$recipient] <- 0
    g.RJ[inv$sender,inv$recipient] <- 0
  }
}

# Sub-networks
for(i in c("g.AC","g.RJ","g.IG")){
  g.tmp <- get(i)
  ergm.tmp <- ergm(g.tmp ~ edges + 
                     gwidegree(decay, fixed=T) +
                     gwodegree(decay, fixed=T) +
                     dgwdsp(decay, fixed=T) + 
                     edgecov(network.1) +
                     edgecov(network.4) +
                     nodematch("international") + 
                     nodematch("gender") + 
                     nodeifactor("gender") +
                     nodeicov("age") +
                     nodeicov("project.skill.score") +
                     nodeicov("leadership.score") +
                     nodeicov("collective.score") +
                     nodeicov("social.skills.score") +
                     nodeicov("creativity.score") +
                     nodeicov("personality.agreeableness.score") +
                     nodeicov("personality.conscientiousness.score") +
                     nodeicov("personality.extraversion.score") +
                     nodeicov("personality.neuroticism.score") +
                     nodeicov("personality.openness.score") +
                     nodeicov("work.constraint") +
                     nodeofactor("gender") +
                     nodeocov("age") +
                     nodeocov("project.skill.score") +
                     nodeocov("leadership.score") +
                     nodeocov("collective.score") +
                     nodeocov("social.skills.score") +
                     nodeocov("creativity.score") +
                     nodeocov("personality.agreeableness.score") +
                     nodeocov("personality.conscientiousness.score") +
                     nodeocov("personality.extraversion.score") +
                     nodeocov("personality.neuroticism.score") +
                     nodeocov("personality.openness.score") +
                     nodeocov("work.constraint") +
                     absdiff("age") +
                     absdiff("project.skill.score") +
                     absdiff("leadership.score") +
                     absdiff("collective.score") +
                     absdiff("social.skills.score") +
                     absdiff("creativity.score") +
                     absdiff("personality.agreeableness.score") +
                     absdiff("personality.conscientiousness.score") +
                     absdiff("personality.extraversion.score") +
                     absdiff("personality.neuroticism.score") +
                     absdiff("personality.openness.score") +
                     absdiff("work.constraint"),
                   constraints=~fixallbut(g),
                   control = control.ergm(MCMLE.maxit = 800,
                                          #parallel = 3,
                                          seed=20),
                   eval.loglik = F)
  summary(ergm.tmp)
  
  ergm.tmp <- logLik(ergm.tmp, add=TRUE)
  assign(paste0("ergm.",i), ergm.tmp)
}


# T-ratios
terms <- c('edges', 'gwidegree(0.1, fixed=T)', 'gwodegree(0.1, fixed=T)', 'dgwdsp(0.1, fixed=T, type="OTP")',
           'nodeicov("age")', 'nodeifactor("gender")', 'nodeicov("project.skill.score")', 'nodeicov("project.skill.gini")', 
           'nodeicov("leadership.score")', 'nodeicov("collective.score")', 'nodeicov("social.skills.score")', 'nodeicov("creativity.score")', 
           'nodeicov("results_count")', 'nodeicov("personality.agreeableness.score")', 'nodeicov("personality.conscientiousness.score")', 
           'nodeicov("personality.extraversion.score")', 'nodeicov("personality.neuroticism.score")', 'nodeicov("personality.openness.score")', 
           'nodeocov("age")', 'nodeifactor("gender")', 'nodeocov("project.skill.score")', 'nodeocov("project.skill.gini")',
           'nodeocov("leadership.score")', 'nodeocov("collective.score")', 'nodeocov("social.skills.score")', 'nodeocov("creativity.score")', 
           'nodeocov("results_count")', 'nodeocov("personality.agreeableness.score")', 'nodeocov("personality.conscientiousness.score")', 
           'nodeocov("personality.extraversion.score")', 'nodeocov("personality.neuroticism.score")', 'nodeocov("personality.openness.score")')
t.table <- c()

# Run simulations
sim <- simulate(ergm.1, burnin=100000, interval=100000, nsim=100, verbose=T)  # Uses the ergm model to simulate a null model

# Calculate the table
for(t in terms){
  model.term <- sapply(1:100, function(x) summary(as.formula(paste0("sim[[x]] ~ ", t)))) 
  observed.term <- summary(as.formula(paste0("g ~ ", t)))                                  
  t.table <- rbind(t.table, data.frame(model = p,
                                       obs=observed.term,
                                       mean=mean(model.term),
                                       sd=sd(model.term),
                                       tstat=abs(mean(model.term)-observed.term)/sd(model.term)))
  rm(observed.term, model.term)
}

# Goodness of fitness
gof <- gof(ergm.1 ~ idegree + odegree + espartners + model,
           verbose = T, burnin=1e+5, interval=1e+5,
           control = control.gof.ergm(parallel = 3))
par(mfrow = c(2, 2))
plot(gof)

# Save --------------------------------------------------------------------
save.image("data_R/03_ergm_abs.RData")

print("Finished!")
