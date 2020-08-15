##################################################
## Project: MDT Analysis
## Script purpose: Survey Analysis
## Date: 2019-01-25
## Author: Rustom Ichhaporia - rustomi2@illinois.edu
## Author: Diego Zara - dgomezara@u.northwestern.edu
##################################################

rm(list = ls())
folder <- getwd()
setwd(folder)
set.seed(42)

# Libraries ---------------------------------------------------------------
library(readr)
library(statnet)
library(igraph)
library(intergraph)
library(anonymizer)
library(eeptools)
library(reshape2)
library(psych)
library(plyr)
library(ineq)
library(Hmisc)
library(lubridate)
library(pracma)

# Data preprocessing ------------------------------------------------------

# Load the data
load("data_R/01_data_extraction.RData")

# Keep only the users who finished the survey
user.profiles <- user.profiles[(user.profiles$user_id %in% unique(survey.data$user_id)),]
rownames(user.profiles) <- NULL

length(unique(survey.questions$question_id))
length(unique(survey.data$question_id))

# Remove Spanish questions and any others needing to be omitted
to_be_omitted_uba <- read_csv("data/01_survey/questions.uba.csv")
to_be_omitted_extra <- read_csv("data/01_survey/omit.these.questions.csv")
for(i in to_be_omitted_uba$question_id){
  survey.data <- subset(survey.data, question_id != i)
  survey.questions <- subset(survey.questions, question_id != i)
}
for(i in to_be_omitted_extra$question_id){
  survey.data <- subset(survey.data, question_id != i)
  survey.questions <- subset(survey.questions, question_id != i)
}

length(unique(survey.questions$question_id))
length(unique(survey.data$question_id))

# Fill in birthdays for those whose birthday is in survey.data 
na_birthdays <- which(is.na(user.profiles$date_of_birth))
print(class(Sys.Date() - years(30)))
for(i in na_birthdays){
  matching_row <- survey.data[(survey.data$project_id == user.profiles[i, "project_id"] & 
                                 survey.data$user_id == user.profiles[i, "user_id"]) & 
                                 survey.data$question_text == "Your age",]
  if(is.null(matching_row$answer))
  {
    user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(18))
  }
  else if(strcmp(matching_row$answer,"15-25")){
    user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(20))}
  else if (strcmp(matching_row$answer, "25-35")){
    user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(30))}
  else if (strcmp(matching_row$answer,"35-45")){
    user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(40))}
  else if (strcmp(matching_row$answer,"45-55")){
    user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(50))}
  else if (strcmp(matching_row$answer, "55 and above")){
    user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(60))}
  else user.profiles$date_of_birth[i] = as.character(Sys.Date() - years(18))
}

# Remove domain questions
skill_and_domain_questions <- read.csv("results/02_search/unique_skills_questions.csv")
domain_questions <- skill_and_domain_questions[skill_and_domain_questions$domain == "domain",]
for(i in domain_questions$question_text){
  survey.data <- subset(survey.data, !(category == "Project_Skills" & question_text == i))
  survey.questions <- subset(survey.questions, !(category == "Project_Skills" & question_text == i))
}

# Set participants' ages
user.profiles$age <- 0
for(i in 1:nrow(user.profiles)){
  if(as.Date(user.profiles$date_of_birth[i], "%Y-%m-%d") < Sys.Date())
  
    user.profiles$age[i] <- floor(age_calc(as.Date(user.profiles$date_of_birth[i], "%Y-%m-%d"), units = "years"))
}

## the age smaller than 18 is NA
user.profiles$age[(user.profiles$age < 18)] <- 18

# Set participants' gender
user.profiles$gender[(user.profiles$gender == "O" & !is.na(user.profiles$gender))] <- NA

# Check if the participant is international
user.profiles$international <- ifelse(user.profiles$nationality == "US", 0, 1)

# Assignate the value to the radio answers
for(i in 1:nrow(survey.questions)){
  question <- survey.questions[i,c("question_id", "type")]
  if(question$type == "radio"){
    values <- unlist(strsplit(survey.questions[i,"choices"], ","))
    for(j in 1:length(values)){
      survey.data[(survey.data$question_id == question$question_id & survey.data$answer == values[j]), "answer_numeric"] <- j
    }
    rm(values)
  }
  
  rm(question)
}

# Question count
table(survey.questions$project_id)
table(survey.questions$project_id[survey.questions$type == "radio"])

# Export questions
write.csv(unique(survey.questions[,c("question_id", "category", "question_text", "choices")]), file = paste0("results/01_survey/survey_questions.csv"), row.names=FALSE, na="", fileEncoding = "UTF-8")

# Set the positive value
questions.metadata <- read.csv("data/01_survey/questions.csv", stringsAsFactors=FALSE) 

# Set the scales
survey.questions$positive <- NA
survey.questions$scale_min <- NA
survey.questions$scale_max <- NA

for(i in survey.questions$question_id){
  survey.questions$positive[(survey.questions$question_id == i)] <- questions.metadata$positive[(questions.metadata$question_id == i)]
  survey.questions$scale_min[(survey.questions$question_id == i)] <- questions.metadata$min[(questions.metadata$question_id == i)]
  survey.questions$scale_max[(survey.questions$question_id == i)] <- questions.metadata$max[(questions.metadata$question_id == i)]
}

# Extract the personality questions
big.five <- read_csv("data/01_survey/big_five.csv")

# Replace personality's questions
for(i in 1:nrow(big.five)){
  survey.data[(survey.data$question_id == big.five$question_id[i]), "category"] <- big.five$scale[i]
  survey.questions[(survey.questions$question_id == big.five$question_id[i]), "category"] <- big.five$scale[i]
}

# Social Network Questions -----------------------------------------------------------------------
user.profiles$know.degree <- 0
user.profiles$know.betweenness <- 0
user.profiles$know.closeness <- 0
user.profiles$know.constraint <- 0
user.profiles$know.clustering <- 0

user.profiles$work.degree <- 0
user.profiles$work.betweenness <- 0
user.profiles$work.closeness <- 0
user.profiles$work.constraint <- 0
user.profiles$work.clustering <- 0

user.profiles$enjoy.degree <- 0
user.profiles$enjoy.betweenness <- 0
user.profiles$enjoy.closeness <- 0
user.profiles$enjoy.constraint <- 0
user.profiles$enjoy.clustering <- 0

# Edgelist
edgelist <- c()
relationships <- c()

# Parse participants' responses 
for (p in projects.ids) {
  users.project <- user.profiles[(user.profiles$project_id == p),]
  adj.matrix.complete <- matrix(0, length(users.project$user_id), length(users.project$user_id))
  colnames(adj.matrix.complete) <- rownames(adj.matrix.complete) <- users.project$user_id
  table.networks <- c()
  
  for(i in survey.questions$question_id[(survey.questions$category == "Social_Connections" & survey.questions$project_id == p)]){
    table.network <- adj.matrix.complete
    answers <- survey.data[(survey.data$question_id == i & survey.data$project_id == p), c("user_id", "answer")]
    for(j in 1:nrow(answers)){
      source.id <- as.character(answers$user_id[j])
      if(source.id %in% users.project$user_id){
        # Parsing users from the text input field  
        users <- gsub('\\[|\\]|\\"', "", answers$answer[j])
        users <- strsplit(users,",")[[1]]
        if(length(users) > 0){
          for(k in users){
            target.id <- as.character(k) 
            if(target.id %in% users.project$user_id){
              table.network[source.id, target.id] <- 1
              #table.network[target.id, source.id] <- 1 # Union idea
              
              relationships <- rbind(relationships, data.frame(project_id = p, type = i, source = source.id, target = target.id))
              #relationships <- rbind(relationships, data.frame(project_id = p, type = i, source = target.id, target = source.id))
              
              edgelist <- rbind(edgelist, data.frame(source = source.id, target = target.id, type= i, project.id = p, stringsAsFactors = F))
            }
          }
        }
      }
    }
    
    network <- network(table.network, matrix.type="adjacency", directed=T) 
    
    # User metrics
    if(i == 1){
      work.metrics <- data.frame(user_id = as.numeric(network::get.vertex.attribute(network, "vertex.names")),
                                 degree = sna::degree(network),
                                 betweenness = sna::betweenness(network),
                                 closeness = sna::closeness(network),
                                 constraint = constraint(asIgraph(network)),
                                 clustering = transitivity(asIgraph(network), type="local"))
      for(n in work.metrics$user_id){
        user.profiles$work.degree[(user.profiles$user_id == n)] <- work.metrics$degree[(work.metrics$user_id == n)]
        user.profiles$work.betweenness[(user.profiles$user_id == n)] <- work.metrics$betweenness[(work.metrics$user_id == n)]
        user.profiles$work.closeness[(user.profiles$user_id == n)] <- work.metrics$closeness[(work.metrics$user_id == n)]
        user.profiles$work.constraint[(user.profiles$user_id == n)] <- work.metrics$constraint[(work.metrics$user_id == n)]
        user.profiles$work.clustering[(user.profiles$user_id == n)] <- work.metrics$clustering[(work.metrics$user_id == n)]
      }
      rm(work.metrics)
    } else if(i == 2){
      know.metrics <- data.frame(user_id = as.numeric(network::get.vertex.attribute(network, "vertex.names")),
                                 degree = sna::degree(network),
                                 betweenness = sna::betweenness(network),
                                 closeness = sna::closeness(network),
                                 constraint = constraint(asIgraph(network)),
                                 clustering = transitivity(asIgraph(network), type="local"))
      for(n in know.metrics$user_id){
        user.profiles$know.degree[(user.profiles$user_id == n)] <- know.metrics$degree[(know.metrics$user_id == n)]
        user.profiles$know.betweenness[(user.profiles$user_id == n)] <- know.metrics$betweenness[(know.metrics$user_id == n)]
        user.profiles$know.closeness[(user.profiles$user_id == n)] <- know.metrics$closeness[(know.metrics$user_id == n)]
        user.profiles$know.constraint[(user.profiles$user_id == n)] <- know.metrics$constraint[(know.metrics$user_id == n)]
        user.profiles$know.clustering[(user.profiles$user_id == n)] <- know.metrics$clustering[(know.metrics$user_id == n)]
      }
      rm(know.metrics)
    } else if(i == 4){
      enjoy.metrics <- data.frame(user_id = as.numeric(network::get.vertex.attribute(network, "vertex.names")),
                                  degree = sna::degree(network),
                                  betweenness = sna::betweenness(network),
                                  closeness = sna::closeness(network),
                                  constraint = constraint(asIgraph(network)),
                                  clustering = transitivity(asIgraph(network), type="local"))
      for(n in enjoy.metrics$user_id){
        user.profiles$enjoy.degree[(user.profiles$user_id == n)] <- enjoy.metrics$degree[(enjoy.metrics$user_id == n)]
        user.profiles$enjoy.betweenness[(user.profiles$user_id == n)] <- enjoy.metrics$betweenness[(enjoy.metrics$user_id == n)]
        user.profiles$enjoy.closeness[(user.profiles$user_id == n)] <- enjoy.metrics$closeness[(enjoy.metrics$user_id == n)]
        user.profiles$enjoy.constraint[(user.profiles$user_id == n)] <- enjoy.metrics$constraint[(enjoy.metrics$user_id == n)]
        user.profiles$enjoy.clustering[(user.profiles$user_id == n)] <- enjoy.metrics$clustering[(enjoy.metrics$user_id == n)]
      }
      rm(enjoy.metrics)
    }
    
    # Descriptive analysis and centrality measures
    row <- c(survey.questions$question_text[(survey.questions$question_id == i
                                             & !is.na(survey.questions$question_id)
                                             & survey.questions$project_id == p)],
             network.size(network),
             network.edgecount(network),
             round(gden(network), 4),
             sna::components(network, connected="weak"),
             max(component.dist(network, connected="weak")$csize),
             length(isolates(network)),
             round(mean(sna::degree(network)), 4),
             max(sna::degree(network)),
             round(mean(sna::betweenness(network)), 4),
             max(sna::betweenness(network)),
             round(mean(sna::closeness(network)), 4),
             max(sna::closeness(network)),
             round(transitivity(asIgraph(network)), 4),
             max(transitivity(asIgraph(network), type="local")),
             round(mean(constraint(asIgraph(network))), 4),
             max(constraint(asIgraph(network))),
             round(sna::centralization(network, sna::degree, cmode="indegree"), 4))
    table.networks <- cbind(table.networks, row)
    
    # Save the table
    #assign(paste0("df.network.",p,".",i), data.frame(names = network::get.vertex.attribute(network, "vertex.names"),
    #                                                 degree = sna::degree(network),
    #                                                 stringsAsFactors = FALSE))
    
    #assign(paste0("network.",p,".",i), network)
    rm(network, table.network, answers, users, source.id, target.id)
  }
  
  colnames(table.networks) <- NULL
  rownames(table.networks) <- c("question", "vertices", "edges", "density", 
                                "components", "max component", "isolates", 
                                "degree (mean)", "degree (max)",
                                "betweenness (mean)", "betweenness (max)", 
                                "closeness (mean)", "closeness (max)", 
                                "clustering (mean)", "clustering (max)", 
                                "Buts' constrain (mean)", "Buts' constrain (max)",
                                "centralization (indegree)")
  
  #assign(paste0("table.network.",p), table.networks)
  write.csv(table.networks, file = paste0("results/01_survey/social_networks_descriptive_",p,".csv"), na="")
}

# Fix clustering NA
user.profiles$know.clustering[(is.na(user.profiles$know.clustering))] <- 0
user.profiles$work.clustering[(is.na(user.profiles$work.clustering))] <- 0
user.profiles$enjoy.clustering[(is.na(user.profiles$enjoy.clustering))] <- 0

# Skills -----------------------------------------------------------------

# Replace and set the kind of questions
project.skills.questions <- read_csv("data/01_survey/project_skills.csv")

survey.data$is_skill <- NA
survey.data$skill_n <- NA

# Numerize project skills
skills.by.project <- unique(survey.data[(survey.data$category == "Project_Skills"),c("project_id","question_text","question_id")])
row.names(skills.by.project) <- NULL
skills.by.project$skill_n <- 0
for(i in unique(skills.by.project$project_id)){
  skills.by.project$skill_n[(skills.by.project$project_id == i)] <- c(1:nrow(skills.by.project[(skills.by.project$project_id == i),]))
}

for(i in 1:nrow(skills.by.project)){
  survey.data[(survey.data$question_id == skills.by.project$question_id[i] & 
                 survey.data$project_id == skills.by.project$project_id[i]), "skill_n"] <- skills.by.project$skill_n[i]
}

# Populate metadata
for(i in 1:nrow(project.skills.questions)){
  if(!is.na(project.skills.questions$question_id_original[i])){
    survey.data[(survey.data$question_id == project.skills.questions$question_id[i]), "question_id"] <- project.skills.questions$question_id_original[i]
  }
  if(project.skills.questions$skill_domain[i] == "skill"){
    survey.data[(survey.data$question_id == project.skills.questions$question_id[i]), "is_skill"] <- 1
  } else {
    survey.data[(survey.data$question_id == project.skills.questions$question_id[i]), "is_skill"] <- 0
  }
}

# Cronbach alpha
quantitative.answers <- survey.data[(survey.data$type == "radio"),c("project_id", "user_id", "question_id", "answer_numeric")]
quantitative.answers$user_id <- paste0(quantitative.answers$project_id,"_",quantitative.answers$user_id)
quantitative.answers$project_id <- NULL
quantitative.answers <- dcast(quantitative.answers[,c("user_id", "question_id", "answer_numeric")], user_id ~ question_id)

questions.ids = list()
questions.ids[[1]] <- unique(survey.questions$question_id[(survey.questions$category == "Agreeableness")])
questions.ids[[2]] <- unique(survey.questions$question_id[(survey.questions$category == "Conscientiousness")])
questions.ids[[3]] <- unique(survey.questions$question_id[(survey.questions$category == "Extraversion")])
questions.ids[[4]] <- unique(survey.questions$question_id[(survey.questions$category == "Neuroticism")])
questions.ids[[5]] <- unique(survey.questions$question_id[(survey.questions$category == "Openness")])
questions.ids[[6]] <- unique(survey.questions$question_id[(survey.questions$category == "Social_Skills")])
questions.ids[[7]] <- unique(survey.questions$question_id[(survey.questions$category == "Psychological_Collectivism")])
questions.ids[[8]] <- unique(survey.questions$question_id[(survey.questions$category == "Creativity")])
#questions.ids[[9]] <- unique(survey.questions$question_id[(survey.questions$category  == "Project_Skills")]) # Project skills
questions.ids[[9]] <- unique(survey.questions$question_id[(survey.questions$category == "Leadership")])

# Calculating Cronbach's alphas 
for(i in questions.ids){
  a <- psych::alpha(quantitative.answers[,as.character(i)], check.keys=TRUE, na.rm = T)
  print(a$total$raw_alpha)
}

# Users info consolidation ------------------------------------------------
# Invert the scales
for(p in projects.ids){
  survey.questions.project <- survey.questions[(survey.questions$project_id == p),]
  for(i in unique(survey.questions.project$question_id)){
    question <- survey.questions.project[(survey.questions.project$question_id == i),]
    if(!is.na(question$positive) & question$positive == 0){
      answers <- survey.data[(survey.data$project_id == p & survey.data$question_id == i),]
      for(j in 1:nrow(answers)){
        if(!is.na(answers$answer_numeric[j])){
          answers$answer_numeric[j] <- question$scale_max - answers$answer_numeric[j] + 1
        }
      }
    }
  }
 rm(survey.questions.project, question)
}

# Clear integer(0)
clearInteger0 <- function(x) {
  if (length(x) == 0)
    return(NA)
  return(x)
}

# Get the users scores
users.info <- c()
for(p in projects.ids){
  users.project <- user.profiles[(user.profiles$project_id == p), ]
  survey.data.project <- survey.data[(survey.data$project_id == p), ]
  number.skills <- max(skills.by.project$skill_n[skills.by.project$project_id == p])
  
  for(i in users.project$user_id){

    users.info <- rbind(users.info,
                        data.frame(user_id = i,
                                   project_id = p,
                                   creativity.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Creativity")]),
                                   leadership.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Leadership")]),
                                   social.skills.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Social_Skills")]),
                                   personality.agreeableness.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Agreeableness")]),
                                   personality.conscientiousness.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Conscientiousness")]),
                                   personality.extraversion.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Extraversion")]),
                                   personality.neuroticism.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Neuroticism")]),
                                   personality.openness.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Openness")]),
                                   collective.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Psychological_Collectivism")]),
                                   expert.skills.number = max(0, length(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$answer_numeric > 3)])/number.skills, na.rm = T),
                                   expert.skills.mean = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$is_skill == 1 & survey.data.project$answer_numeric > 3)], na.rm = T),
                                   project.skill.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills")], na.rm = T),
                                   project.skill.gini = Gini(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills")], na.rm = T),
                                   # domain.score = mean(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$is_skill == 0)], na.rm = T),
                                   project.skill.1 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 1)]),
                                   project.skill.2 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 2)]),
                                   project.skill.3 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 3)]),
                                   project.skill.4 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 4)]),
                                   project.skill.5 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 5)]),
                                   project.skill.6 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 6)]),
                                   project.skill.7 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 7)]),
                                   project.skill.8 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 8)]),
                                   project.skill.9 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 9)]),
                                   project.skill.10 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 10)]),
                                   project.skill.11 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 11)]),
                                   project.skill.12 = clearInteger0(survey.data.project$answer_numeric[(survey.data.project$user_id == i & survey.data.project$category == "Project_Skills" & survey.data.project$skill_n == 12)]),
                                   stringsAsFactors = F))
  }
  rm(users.project, survey.data.project, number.skills)
}

# Deal with NA, replace by the mean
for(i in 1:(ncol(users.info)-12)){
  if(is.numeric(users.info[,i])){
    users.info[(is.na(users.info[,i])),i] <- mean(users.info[,i], na.rm = T)
  }
}

# Merge the network metrics
user.profiles <- merge(user.profiles, users.info, by = c("project_id", "user_id"), all.x = T)
rm(users.info)

# Anonymize
user.profiles$user_id <- sapply(user.profiles$user_id, anonymize, .algo = "crc32")
relationships$source <- sapply(relationships$source, anonymize, .algo = "crc32")
relationships$target <- sapply(relationships$target, anonymize, .algo = "crc32")
invitations$sender_id <- sapply(invitations$sender_id, anonymize, .algo = "crc32")
invitations$recipient_id <- sapply(invitations$recipient_id, anonymize, .algo = "crc32")

user.profiles$last_name <- NULL
user.profiles$first_name <- NULL
user.profiles$email <- NULL

# Fix the ids
user.profiles$user_id <- paste0(user.profiles$project_id, ".", user.profiles$user_id)
relationships$source <- paste0(relationships$project_id, ".", relationships$source)
relationships$target <- paste0(relationships$project_id, ".", relationships$target)
invitations$sender_id <- paste0(invitations$project_id, ".", invitations$sender_id)
invitations$recipient_id <- paste0(invitations$project_id, ".", invitations$recipient_id)

# Fix gender
user.profiles$gender[!(user.profiles$gender %in% c("F","M"))] <- "F"

# Complementary network ---------------------------------------------------
complementary.list <- c()

for(p in projects.ids){
  users.per.project <- user.profiles[(user.profiles$project_id == p),]
  combinations <- t(combn(users.per.project$user_id,2))
  number.skills <- max(skills.by.project$skill_n[skills.by.project$project_id == p])
  
  for(i in 1:nrow(combinations)){
    user1.skills <- users.per.project[(users.per.project$user_id == combinations[i,1]),c(42:(42+number.skills-1))]
    user2.skills <- users.per.project[(users.per.project$user_id == combinations[i,2]),c(42:(42+number.skills-1))]
    gini.both <- Gini(user1.skills + user2.skills)
    
    if(is.nan(gini.both)){
      row <- data.frame(user1 = combinations[i,1], user2 = combinations[i,2], complement = 0)
      next
    }
    
    # If the combination has a gini score lower than their previous ginis, they complement. 
    if(users.per.project$project.skill.gini[(users.per.project$user_id == combinations[i,1])] >= gini.both &
       users.per.project$project.skill.gini[(users.per.project$user_id == combinations[i,2])] >= gini.both) {
      row <- data.frame(user1 = combinations[i,1], user2 = combinations[i,2], complement = 1)
    } else {
      row <- data.frame(user1 = combinations[i,1], user2 = combinations[i,2], complement = 0)
    }
    complementary.list <- rbind(complementary.list, row)
    rm(user1.skills, user2.skills, gini.both, row)
  }
  
  rm(number.skills, combinations, users.per.project)
}

# Save --------------------------------------------------------------------
save.image("data_R/02_data_analysis_mdt_v2.RData")

# Export the data
write.csv(user.profiles, file = paste0("results/01_survey/users_profiles_v2.csv"), row.names=FALSE, na="")
write.csv(relationships, file = paste0("results/01_survey/relationships_v2.csv"), row.names=FALSE, na="")
write.csv(invitations, file = paste0("results/01_survey/invitations_v2.csv"), row.names=FALSE, na="")
write.csv(complementary.list, file = paste0("results/01_survey/complement_v2.csv"), row.names=FALSE, na="")

print("Finished!")