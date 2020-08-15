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
library(RMySQL)

# Data from MySQL ---------------------------------------------------------

# Credentials for MySQL
user = "root"
host = "localhost"
password = "root"
port = 3306
socket = "/Applications/MAMP/tmp/mysql/mysql.sock"

# Connect to the database
drv = dbDriver("MySQL")
mydb = dbConnect(drv, user=user, password=password, dbname='mdt_20191105', port= port, host=host, unix.socket=socket)
dbSendQuery(mydb, "SET NAMES utf8")

# Projects ids
projects.sids <- '11,14,88,91,92,101,120,124,130,131,133,136,137,138,139,221'
projects.ids <- c(11,14,88,91,92,101,120,124,130,131,133,136,137,138,139,221)

# Get the survey questions
q <- paste('select pp.id as project_id, qc.name as category, qqt.id as question_id, qq.question_type as type, qqt.content as question_text, qq.choices as choices
           from projects_project as pp
           left join questions_survey qs
           on qs.id = pp.survey_id
           left join questions_question_surveys qqs
           on qs.id = qqs.survey_id
           left join questions_question qq
           on qq.id = qqs.question_id
           left join questions_questiontext qqt
           on qqt.id = qq.text_id
           left join questions_category qc
           on qc.id = qq.category_id
           where pp.id in (',projects.sids,');',sep="")
survey.questions <- dbGetQuery(mydb, q)

# Get the survey data
q <- paste('select qr.projectid as project_id, au.id as user_id, qqt.id as question_id, qq.question_type as type, qc.name as category, qqt.content as question_text, CONVERT(qar.body USING utf8) as answer 
           from questions_answer qa
           left join questions_response qr on qr.id = qa.response_id
           left join auth_user au on au.id = qr.user_id
           left join profiles_userprofile as u on au.id = u.user_id
           left join questions_question qq on qq.id = qa.question_id
           left join questions_questiontext qqt on qqt.id = qq.text_id
           left join questions_answerradio qar on qa.id = qar.answer_ptr_id
           left join questions_category qc on qc.id = qq.category_id
           where qr.projectid in (',projects.sids,') and au.id not in (28,29) and u.status != "DR"
           order by qr.projectid asc;',sep="")
survey.data <- dbGetQuery(mydb, q)
survey.data <- survey.data[!duplicated(survey.data), ]
survey.data$answer_numeric <- NA

# Get users' public profiles
q <- paste('select u.project_id, u.user_id, u.team_id, u.summary, u.fav, u.goodat, u.six, u.message, i.date_of_birth, i.gender, i.nationality, au.last_name, au.first_name, au.email
           from profiles_userprofile u
           left join accounts_usercommoninfo i on u.user_id = i.user_id
           left join auth_user as au on au.id = u.user_id
           where u.project_id in (',projects.sids,') and u.user_id not in (28,29) and u.status != "DR"
           order by u.project_id, u.user_id', sep="")
user.profiles <- dbGetQuery(mydb, q)
user.profiles <- user.profiles[!duplicated(user.profiles), ]

# Get the search queries
q <- paste('select pe.id, pe.project_id as project_id, pe.tag as tag, u.user_id as user_id, u.team_id as team_id
           from preferences_preferences as pe 
           inner join auth_user as au on au.id = pe.user_id
           inner join profiles_userprofile as u on au.id = u.user_id
           where pe.project_id in (',projects.sids,') and u.status != "DR"
           group by pe.id
           order by pe.project_id, pe.id', sep="")
queries <- dbGetQuery(mydb, q)
queries <- queries[!duplicated(queries), ]

# Search preferences
q <- paste('select ps.*, pe.project_id as project_id, pe.tag as tag, qc.name as category_name, 
           pe.user_id as user_id, u.team_id as team_id
           from preferences_singlepreference as ps
           inner join preferences_preferences as pe on ps.preferences_id = pe.id
           inner join questions_category as qc on ps.category_id = qc.id
           inner join auth_user as au on au.id = pe.user_id
           inner join profiles_userprofile as u on au.id = u.user_id
           where pe.project_id in (',projects.sids,') and ps.decision != 0 and u.status != "DR"
           group by ps.id
           order by pe.project_id, ps.id', sep="")
queries.attr <- dbGetQuery(mydb, q)
queries.attr <- queries.attr[!duplicated(queries.attr), ]

# Get the recommendations
q <- paste('select r.id, r.preferences_id, r.score, r.invited, r.viewed, r.eval_score, r.diversity_score, pe.current_team_eval_score, pe.current_team_diversity_score, pe.user_id as sender, po.user_id as recommended, po.project_id as project_id
           from recommend_profilerank as r
           inner join profiles_userprofile as po on r.profile_id = po.id
           inner join preferences_preferences as pe on r.preferences_id = pe.id
           where po.project_id in (',projects.sids,') and po.status != "DR"
           order by r.preferences_id, r.score desc', sep="")
recommendations <- dbGetQuery(mydb, q)
recommendations <- recommendations[!duplicated(recommendations), ]

# Invitations
q <- paste('select po1.project_id as project_id, msg.id, msg.sent_at, msg.read_at, 
           msg.replied_at, msg.recipient_id, msg.sender_id, i.status, i.origin, i.valid
           from postman_message msg
           inner join MDTmessages_invitation i
           on msg.id = i.message_id
           inner join auth_user u1
           on msg.sender_id = u1.id
           inner join auth_user u2
           on msg.recipient_id = u2.id
           inner join profiles_userprofile as po1 
           on u1.id = po1.user_id
           inner join profiles_userprofile as po2 
           on u2.id = po2.user_id
           where po1.project_id in (',projects.sids,') and po2.project_id in (',projects.sids,') and po1.status != "DR" and po2.status != "DR"
           order by po1.project_id, msg.id', sep="")
invitations <- dbGetQuery(mydb, q)
invitations <- invitations[!duplicated(invitations), ]

# We disconnect
dbDisconnect(mydb)
rm(q, mydb, socket, password, host, user, port, drv)
detach("package:RMySQL")

# Save data
save.image("data_R/01_data_extraction.RData")
