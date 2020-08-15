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

# Load data
rm(list = ls())
load("data_R/02_data_analysis.RData")

# Load early datasets ----------------------------------------------------
invitations.v1 <- read.csv("data/v1/invitations.v1.csv", sep = ",", stringsAsFactors = F)
relationships.v1 <- read.csv("data/v1/relationships.v1.csv", sep = ",", stringsAsFactors = F)
user.info.v1 <- read.csv("data/v1/user.info.v1.csv", sep = ",", stringsAsFactors = F)

# Concatenate with secondary datasets ------------------------------------
# Invitations
invitations.v1$version <- NULL
names(invitations.v1)[names(invitations.v1) == "type"] <- "status"

names(invitations)[names(invitations) == "sender_id"] <- "sender"
names(invitations)[names(invitations) == "recipient_id"] <- "recipient"

keep_columns <- names(invitations.v1)
invitations <- invitations[keep_columns]


invitations <- rbind(invitations, invitations.v1)
invitations$origin <- NULL

# Profiles
names(user.info.v1)[names(user.info.v1) == "breadth.skill.score"] <- "project.skill.score"
shared_columns <- intersect(colnames(user.info.v1), colnames(user.profiles))
user.info.v1 <- user.info.v1[shared_columns]
user.profiles <- user.profiles[shared_columns]
user.profiles <- rbind(user.profiles, user.info.v1)

# Relationships
relationships <- rbind(relationships, relationships.v1)

#### Load user.info.v1


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

# Fix the ids
user.profiles$user_id <- paste0(user.profiles$project_id, ".", user.profiles$user_id)
relationships$source <- paste0(relationships$project_id, ".", relationships$source)
relationships$target <- paste0(relationships$project_id, ".", relationships$target)
invitations$sender <- paste0(invitations$project_id, ".", invitations$sender)
invitations$recipient <- paste0(invitations$project_id, ".", invitations$recipient)

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
  project_ids_keep <- c(11, 91, 92, 101, 120, 131, 133, 136, 
                        6, 10, 15, 18, 19, 25) # Secondary dataset
  invitations <- invitations[invitations$project_id %in% project_ids_keep, ]
  relationships <- relationships[relationships$project_id %in% project_ids_keep, ]
  user.profiles <- user.profiles[user.profiles$project_id %in% project_ids_keep, ]
}



# Create network (accepted (AC), rejected (RJ), invited (IN), rejected & invited (NEG), or all)

g <- network(invitations[,c("sender","recipient")], directed=T, matrix.type = "edgelist")

users.invitations <- data.frame(id=get.vertex.attribute(g, "vertex.names"), stringsAsFactors = F)
users.invitations <- merge(users.invitations, user.profiles, by.x = c("id"), by.y=c("user_id"), all.x = T)

# Fix the age
users.invitations$age <- as.numeric(users.invitations$age)

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


for (i in 2:ncol(users.invitations)) {
  set.vertex.attribute(g, colnames(users.invitations)[i], users.invitations[, i])
}

for(i in unique(relationships$type)){
  assign(paste0("network.",i), network(relationships[(relationships$type == i), c("source", "target")], loops = F, directed=F, matrix.type = "edgelist"))
}

# Fixallbut
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


# Plot --------------------------------------------------------------------

gplot(g, vertex.col = as.numeric(as.factor(get.vertex.attribute(g, "project_id")))+1)

# ERGM --------------------------------------------------------------------

# qap.12 <- qaptest(list(network.1, network.2), gcor, g1=1, g2=2, reps=1000)
# summary(qap.12)
# plot(qap.12)
# 
# qap.14 <- qaptest(list(network.1, network.4), gcor, g1=1, g2=2, reps=1000)
# summary(qap.14)
# plot(qap.14)
# 
# qap.24 <- qaptest(list(network.2, network.4), gcor, g1=1, g2=2, reps=1000)
# summary(qap.24)
# plot(qap.24)

decay <- 0.1


ergm.1 <- ergm(g ~ edges + 
               gwidegree(decay, fixed=T) +
               gwodegree(decay, fixed=T) +
               dgwdsp(decay, fixed=T) + 
               #dgwesp(decay, fixed=T) +
               edgecov(network.1) +
               nodeicov("age") +
               nodeifactor("gender") +
               nodeicov("project.skill.score") +
               nodeicov("expert.skills.number") +
               nodeicov("leadership.score") +
               nodeicov("collective.score") +
               nodeicov("social.skills.score") +
               nodeicov("creativity.score") +
               nodeicov("results_count") +
               nodeicov("personality.agreeableness.score") +
               nodeicov("personality.conscientiousness.score") +
               nodeicov("personality.extraversion.score") +
               nodeicov("personality.neuroticism.score") +
               nodeicov("personality.openness.score") +
               nodeocov("age") +
               nodeofactor("gender") +
               nodeocov("project.skill.score") +
               nodeocov("expert.skills.number") +
               nodeocov("leadership.score") +
               nodeocov("collective.score") +
               nodeocov("social.skills.score") +
               nodeocov("creativity.score") +
               nodeocov("results_count") +
               nodeocov("personality.agreeableness.score") +
               nodeocov("personality.conscientiousness.score") +
               nodeocov("personality.extraversion.score") +
               nodeocov("personality.neuroticism.score") +
               nodeocov("personality.openness.score"),
             #constraints=~fixallbut(g), 
             constraints=~blockdiag("project_id"),
             control = control.ergm(MCMLE.maxit = 800,
                                    #parallel = 3,
                                    seed=20),
             eval.loglik = F)
summary(ergm.1)

ergm.1<-logLik(ergm.1, add=TRUE)
exp(summary(ergm.1)$coefs[[1]]) #Odds
exp(summary(ergm.1)$coefs[[2]]) #Odds

# T-ratios
terms <- c('edges', 'gwidegree(0.1, fixed=T)', 'gwodegree(0.1, fixed=T)', 'dgwdsp(0.1, fixed=T, type="OTP")',
           'nodeicov("age")', 'nodeifactor("gender")', 'nodeicov("project.skill.score")', 'nodeicov("expert.skills.number")', 
           'nodeicov("leadership.score")', 'nodeicov("collective.score")', 'nodeicov("social.skills.score")', 'nodeicov("creativity.score")', 
           'nodeicov("results_count")', 'nodeicov("personality.agreeableness.score")', 'nodeicov("personality.conscientiousness.score")', 
           'nodeicov("personality.extraversion.score")', 'nodeicov("personality.neuroticism.score")', 'nodeicov("personality.openness.score")', 
           'nodeocov("age")', 'nodeifactor("gender")', 'nodeocov("project.skill.score")', 'nodeocov("expert.skills.number")',
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
save.image("data_R/03_ergm.RData")
print("Finished!")
