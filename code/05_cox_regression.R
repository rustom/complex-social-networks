##################################################
## Project: MDT Analysis
## Script purpose: Survey Analysis
## Date: 2019-01-25
## Author: Rustom Ichaporia - rustomi2@illinois.edu
## Author: Diego Zara - dgomezara@u.northwestern.edu
##################################################

folder <- getwd()
setwd(folder)
set.seed(42)
rm(list = ls())

# Libraries ---------------------------------------------------------------
library(survival)

# Load datasets -----------------------------------------------------------
invitations <- read.csv("results/03_ergm/invitations.table.invitations.only.csv", sep = ",", stringsAsFactors = F)

# Remove columns
invitations$recipient <- NULL
invitations$sender <- NULL
invitations$team_id.x <- NULL
invitations$team_id.y <- NULL
invitations$date_of_birth.x <- NULL
invitations$date_of_birth.y <- NULL
invitations$status <- NULL
invitations$nationality.x <- NULL
invitations$nationality.y <- NULL
invitations$project.skill.1.x <- NULL
invitations$project.skill.2.x <- NULL
invitations$project.skill.3.x <- NULL
invitations$project.skill.4.x <- NULL
invitations$project.skill.5.x <- NULL
invitations$project.skill.6.x <- NULL
invitations$project.skill.7.x <- NULL
invitations$project.skill.8.x <- NULL
invitations$project.skill.9.x <- NULL
invitations$project.skill.10.x <- NULL
invitations$project.skill.11.x <- NULL
invitations$project.skill.12.x <- NULL
invitations$project.skill.1.y <- NULL
invitations$project.skill.2.y <- NULL
invitations$project.skill.3.y <- NULL
invitations$project.skill.4.y <- NULL
invitations$project.skill.5.y <- NULL
invitations$project.skill.6.y <- NULL
invitations$project.skill.7.y <- NULL
invitations$project.skill.8.y <- NULL
invitations$project.skill.9.y <- NULL
invitations$project.skill.10.y <- NULL
invitations$project.skill.11.y <- NULL
invitations$project.skill.12.y <- NULL
invitations$status <- 1

# Surivival analysis ------------------------------------------------------
fit1 <- coxph(Surv(progress, status) ~ ., data=invitations)
summary(fit1)

lm.1 <- lm(progress ~ ., data=invitations)
summary(lm.1)
