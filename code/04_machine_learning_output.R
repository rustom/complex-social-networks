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

# Table for Machine Learning Analysis -------------------------------------
colnames(relationships) <- c("project_id", "type", "sender", "recipient")
colnames(complementary) <- c("sender", "recipient", "complement")

invitations.table <- invitations
invitations.table$progress <- 0
invitations.table$invited <- 1

# Convert the date
for(p in unique(invitations.table$project_id)){
  max.time <- as.numeric(as.POSIXct(max(invitations.table$sent_at[(invitations.table$project_id == p)])))
  min.time <- as.numeric(as.POSIXct(min(invitations.table$sent_at[(invitations.table$project_id == p)])))
  invitations.table$progress[(invitations.table$project_id == p)] <- (as.numeric(as.POSIXct(invitations.table$sent_at[(invitations.table$project_id == p)]))-min.time)/(max.time-min.time)
}

# Create the non-invited pairs
for(p in unique(invitations.table$project_id)){
  users.invitations <- invitations.table[(invitations.table$project_id == p),c("sender","recipient")]
  union.users <- union(users.invitations[,1],users.invitations[,2])
  all.permutations <- expand.grid(union.users,union.users, stringsAsFactors = F)
  colnames(all.permutations) <- c("sender","recipient")
  all.permutations <- rbind(all.permutations, users.invitations)
  
  # Remove duplicates
  non.invited <- all.permutations[!duplicated(all.permutations), ]
  
  # Add invited
  invitations.table <- rbind(invitations.table, data.frame(sent_at = NA,
                                                           project_id = p,
                                                           status = "NI",
                                                           sender = non.invited$sender,
                                                           recipient = non.invited$recipient,
                                                           progress = 1,
                                                           invited = 0,
                                                           stringsAsFactors = F))
  # Remove temporal variables
  rm(users.invitations, union.users, all.permutations, non.invited)
}

# Merge datasets
invitations.table <- merge(invitations.table, user.profiles, by.x = c("project_id", "sender"), by.y = c("project_id", "user_id"), all.x = T)
invitations.table <- merge(invitations.table, user.profiles, by.x = c("project_id", "recipient"), by.y = c("project_id", "user_id"), all.x = T)
invitations.table$work <- 0
invitations.table$friends <- 0
invitations.table$complement <- 0

work.together <- relationships[(relationships$type == 1),]
complementary.true <- complementary[(complementary$complement == 1),]

# Checking previous relationships
for(i in 1:nrow(invitations.table)){
  # Check if they have worked together
  have.worked.together <- work.together[(work.together$project_id == invitations.table$project_id[i] & 
                                           ((work.together$sender == invitations.table$sender[i] & work.together$recipient == invitations.table$recipient[i]) |
                                              (work.together$sender == invitations.table$recipient[i] & work.together$recipient == invitations.table$sender[i]))), ]
  if(nrow(have.worked.together) > 0){
    invitations.table$work[i] <- 1
  }
  
  # Check if they complement to each other
  they.complement <- complementary.true[(complementary.true$sender == invitations.table$sender[i] & complementary.true$recipient == invitations.table$recipient[i]) |
                                          (complementary.true$sender == invitations.table$recipient[i] & complementary.true$recipient == invitations.table$sender[i]), ]
  
  if(nrow(they.complement)){
    invitations.table$complement[i] <- 1
  }
  
  rm(have.worked.together, they.complement)
  print(i)
}

# Save --------------------------------------------------------------------
save.image("data_R/04_machine_learning.RData")
invitations.table$sent_at <- NULL
invitations.table[,c("goodat.x","six.x","message.x","fav.x","summary.x","goodat.y","six.y","message.y","fav.y","summary.y")] <- NULL
write.csv(invitations.table, file = paste0("results/03_ergm/invitations.table.csv"), row.names=FALSE, na="")

print("Finished!")
