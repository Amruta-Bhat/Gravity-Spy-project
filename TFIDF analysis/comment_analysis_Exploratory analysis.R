#CSCW Exploratory analysis#
#Amruta

library(plyr)

#import the comments file
file <- file.choose()

comments <- read.csv(file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
View(comments)

file <- file.choose()

user_promotion <- read.csv(file, sep = ",", stringsAsFactors = FALSE, header = TRUE)
View(user_promotion)


#Where do they get the most response? - most looked at talk board
unique(comments$board_id)

unique(comments$board_title)

df1 <- ddply(comments, c("board_title"),summarize, count = length(board_title))
df1 <- df1[order(-df1$count),] 
View(df1)

#Who gets the most responses? - popularity of a user based on their contributions
#define user types OR
#use the pre-defined user types

#merge both files based on the promotion date - user_promotion and comments
#all the users that were promoted to level 2 
lvl2_users <- user_promotion[which(!user_promotion$workflow_name == "1: Neutron Star Mountain"),]


#have only the users that exist in comments$user_id
lvl2_users_1 <- lvl2_users[lvl2_users$user_id %in% comments$comment_user_id,  ]

bkup_lvl2_users_1 <- lvl2_users_1
bkup_comments <- comments
comments$type_user <- NA

str(lvl2_users_1$Date)
str(comments$comment_created_at)

lvl2_users_1$Date <- as.POSIXct(
  strptime(lvl2_users_1$Date, "%m/%d/%y %H:%M")
)

lvl2_users$Date <- as.POSIXct(
  strptime(lvl2_users$Date, "%m/%d/%y %H:%M")
)

comments$comment_created_at <- as.POSIXct(
  strptime(comments$comment_created_at, "%Y-%m-%dT%H:%M:%OSZ")
)

#comments$comment_created_at[1] > lvl2_users_1$Date[1]
length(which(is.na(comments$comment_created_at)== TRUE))
length(which(is.na(lvl2_users_1$Date)== TRUE))

#length(which(comments$X== 27931))
#comments <-comments[-c(27931), ]
lvl2_users$user_id[1] == comments$comment_user_id[1]


#for each user in lvl2_users_1, compare the user in comments
for(i in 1:length(lvl2_users_1$user_id)) {
  for(j in 1:length(comments$comment_user_id)){
    
    if(comments$comment_user_id[j] == lvl2_users_1$user_id[i]){
      if(comments$comment_created_at[j] > lvl2_users_1$Date[i]){
        comments$type_user[j] <- "Advanced"
      }else{
        comments$type_user[j] <- "Beginner"
      }
        
    }
  }
  }

unique(comments$type_user)

#researcher users
res <- as.data.frame(c("Mcoughlin", "srallen-tester", "srallen", "crowston", "mzevin1", "trouille", 
         "cjackso3", "costerlusyr.edu", 
         "Mabi", "parrish", "citizenscientist1994", "camallen","sciencejedi",
         "jessiemcd", "ejm553", "RF45", "lcalian", "joeykey", "isapatane","SBC538"))


row_num <- which(comments$comment_user_login %in% res$`c("Mcoughlin", "srallen-tester", "srallen", "crowston", "mzevin1", "trouille", "cjackso3", "costerlusyr.edu", "Mabi", "parrish", "citizenscientist1994", "camallen", "sciencejedi", "jessiemcd", "ejm553", "RF45", "lcalian", "joeykey", "isapatane", "SBC538")`)

for(i in 1:length(row_num)){
  a <- row_num[i]
  comments[a,15] <- "Researcher"
}

#How often do the different types (e.g., advanced users, beginners, researcher) 
#of users interact with people? - number of interactions for different users. 
library(plyr)
df2 <- ddply(comments, c("type_user"),summarize, count = length(unique(comment_user_id)))
df2 <- df2[order(-df2$count),] 
View(df2)


researcher_activity <- comments[row_num,]


adv <- comments[ which(comments$type_user =="Advanced"), ]
beg <- comments[which(comments$type_user == "Beginner"), ]
res <- comments[which(comments$type_user == "Researcher"), ]


#Table of data with users along with their patterns of leaving comments according to months
#type_user, month
comments$comment_user_id
library(lubridate)
comments$month <- month(comments$comment_created_at)
df3 <- ddply(comments, c("type_user","month"),summarize, count = length(comment_user_id))
View(df3)

#for number of unique users
df4 <- ddply(comments, c("type_user","month"),summarize, count = length(unique(comment_user_id)))
View(df4)



#Frequency of comments made on each board before their promotion date for each level


comments$board_title
#average length of each comment on each board
df4 <- ddply(comments, c("board_title"),summarize, count = nchar(comments$board_description))
View(df4)

comments$comment_body
freq_comments <- comments[,c("board_id","board_title","comment_body")]
freq_comments$comment_length <- NA
attach(freq_comments)
for(i in 1: nrow(freq_comments))
{
  freq_comments$comment_length[i] <- nchar(freq_comments$comment_body[i])
}


df5 <- ddply(freq_comments, c("board_title"), summarize, length_comment = ceiling(mean(comment_length)))
View(df5)
ceiling()

#f = file.choose()
#comments <- read.csv(file = f, sep = ",", header = TRUE, stringsAsFactors = FALSE)
#comments$X.1 <- FALSE
