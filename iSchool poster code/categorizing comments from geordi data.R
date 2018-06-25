install.packages("plyr")
library(plyr)

#import comments file
getwd()

setwd("C:/Users/Amruta/Desktop/Research group/Comments unwind/Comments unwind/GS Comments")
comments <- read.csv("comments-09-04.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#comment_created_at
#comment_user_id
comments <- rename(comments, c("comment_user_id" = "user_id", "comment_created_at" = "time"))
comments <- rename(comments, c("user_id" = "userID"))
comments$userID <- as.character(comments$userID)
comments$time[1:3]

comments$time <- as.POSIXct(
  strptime(comments$time, "%Y-%m-%dT%H:%M:%OSZ")
)
comments$time[1:3]
str(comments)
comments$time <- as.character(comments$time)
str(comments$time)

#import the geordi file

geordi <- geordi <- read.csv("geordi-1-5-17.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#time
#user_id
#format the time in geordi
geordi$time[1:3]
geordi$time <- as.POSIXct(
  strptime(geordi$time, "%Y-%m-%d %H:%M:%S")
)
str(geordi$time)
geordi$time <- as.character(geordi$time)
str(geordi$time)


#check both dataframes before merge
str(comments)
str(geordi)

comments$time <- as.character(comments$time)

#merge both files

fr <- merge(geordi, comments, by = c("time","userID"), all.x = TRUE)


#clean the dataframe 

#keep only some of the columns in Comments dataframe
#columns to keep are: board_id, board_title, board_description, discussion_id, discussion_title, 
#comment_id, comment_body,comment_user_login

geordi_with_comments <- fr[,c(2:3,1,4:19,23:31)]


#adding a new column for the categories 
geordi_with_comments$new.categories <- paste(geordi_with_comments$type, geordi_with_comments$relatedID, geordi_with_comments$data, sep="-")


#renaming the categories in this dataframe 

df <- as.data.frame(geordi_with_comments$new.categories)
colnames(df) <- c("activity")
df$activity <- as.character(df$activity)
str(df)

#replacing COLLECTIONS as the category for all activities  
pos <- which(df$activity %in% c("about-menu-accountMenu.collections-NULL",
                                "about-menu-collab-collection-NULL",
                                "about-menu-settings-collection-NULL",
                                "about-menu-view-collection-NULL",
                                "collect-menu-collections.all-NULL",
                                "collect-menu-collections.allZoo-NULL",
                                "collect-menu-collections.my-NULL",
                                "profile-menu-view-projectcollection-NULL",
                                "about-menu-accountMenu.notifications-NULL"))
replace_string <- "collections"

df$activity <- replace(df$activity, pos, replace_string)


#replacing Favourites as the category for all activities  
pos <- which(df$activity %in% c("about-menu-accountMenu.favorites-NULL",
                                "collect-menu-favorites.all-NULL",
                                "collect-menu-favorites.allZoo-NULL",
                                "collect-menu-favorites.my-NULL",
                                "about-menu-view-favorite-NULL",
                                "favorite-NULL-NULL",
                                "profile-menu-view-projectfavorite-NULL"))
replace_string <- "favourites"

df$activity <- replace(df$activity, pos, replace_string)


#replacing About.Main.Page as the category for all activities  
pos <- which(df$activity %in% c("project-menu-project.nav.about-NULL",
                                "project-menu-NULL-\"project.nav.about\""))

replace_string <- "about.main.page"

df$activity <- replace(df$activity, pos, replace_string)


#replacing cat.search as the category for all activities  
pos <- which(df$activity %in% c("hashtag-sidebar",
                                "search-NULL",
                                "search-back-NULL-NULL",
                                "change-page-first-page-NULL",
                                "change-page-last-page-NULL",
                                "change-page-next-page-NULL",
                                "change-page-previous-page-NULL",
                                "change-page-select-page-NULL"))

replace_string <- "cat.search"

df$activity <- replace(df$activity, pos, replace_string)


#replacing cat.comment as the category for all activities  
pos <- which(df$activity %in% c("edit-post-NULL-NULL",
                                "link-post-NULL-NULL",
                                "message-user",
                                "reply-post-NULL-NULL",
                                "report-post-NULL-NULL",
                                "send-message",
                                "subscribe-NULL-NULL",
                                "unfavorite-NULL-NULL",
                                "update-comment-NULL-NULL"))

replace_string <- "cat.comment"

df$activity <- replace(df$activity, pos, replace_string)


#replacing cat.classification as the category for all activities
pos <- which(df$activity %in% c("metadata-NULL-NULL"))

replace_string <- "cat.classification"

df$activity <- replace(df$activity, pos, replace_string)

#replacing metadata as the category for all activities
pos <- which(df$activity %in% c("subject-image-NULL-NULL",
                                "view-subject-direct-NULL-NULL"))

replace_string <- "metadata"

df$activity <- replace(df$activity, pos, replace_string)

#replacing cat."view {name}" as the category for all activities
pos <- which(df$activity %in% c("talk-view-NULL-{\"board\":\"Chat\"}",
                                "talk-view-NULL-{\"board\":\"Help\"}",
                                "talk-view-NULL-{\"board\":\"Notes\"}",
                                "talk-view-NULL-{\"board\":\"Science\"}",
                                "talk-view-NULL-{\"board\":\"Test board\"}"))

replace_string <- "cat.\"view {name}\""

df$activity <- replace(df$activity, pos, replace_string)



#replacing open.field.guide as the category for all activities
pos <- which(df$activity %in% c("open-field-guide-NULL-NULL"))

replace_string <- "open.field.guide"

df$activity <- replace(df$activity, pos, replace_string)


#replacing like.post as the category for all activities
pos <- which(df$activity %in% c("like-post-NULL-NULL"))

replace_string <- "like.post"

df$activity <- replace(df$activity, pos, replace_string)


#replacing delete.post as the category for all activities
pos <- which(df$activity %in% c("delete-post-NULL-NULL"))

replace_string <- "delete.post"

df$activity <- replace(df$activity, pos, replace_string)

geordi_with_comments$new.categories <- df$activity

#categories to delete
pos <- which((df$activity %in% c("delete.post",
                           "like.post",
                           "open.field.guide",
                           "cat.\"view {name}\"",
                           "metadata",
                           "cat.classification",
                           "cat.comment",
                           "cat.search",
                           "about.main.page",
                           "favourites",
                           "collections")))


geordi_with_comments <- geordi_with_comments[pos,]

#the geordi_with_comments file has the names of the categories along with the geordi and comments file information

getwd()
write.csv(geordi_with_comments,"geordi_with_comments.csv")
