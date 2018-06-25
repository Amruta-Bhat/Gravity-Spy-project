library(plyr)


#import geordi file 
setwd("C:/Users/Amruta/Desktop/Research group/Comments unwind/Comments unwind/GS Comments")
geordi <- geordi <- read.csv("geordi-1-5-17.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#format the time in geordi
geordi$time[1:3]
geordi$time <- as.POSIXct(
  strptime(geordi$time, "%Y-%m-%d %H:%M:%S")
)
str(geordi$time)
geordi$time <- as.character(geordi$time)
str(geordi$time)


#import the comments file
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


colnames.geordi <- as.data.frame(colnames(geordi))
write.csv(colnames.geordi,"colnamesGeordi.csv")

write.csv(comments,"comments.csv")
  

#import the comments file that has the data for three columns - type, time and user_id

comments_1 <- read.csv("colnamesGeordi.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
str(comments_1)

#append both files
geordi_and_comments <- rbind(geordi, comments_1)

#adding a new column for the categories 
geordi_and_comments$newCategories <- paste(geordi_and_comments$type, geordi_and_comments$relatedID, geordi_and_comments$data, sep="-")


#renaming the categories in this dataframe 

df <- as.data.frame(geordi_and_comments$newCategories)
colnames(df) <- c("activity")
df$activity <- as.character(df$activity)
str(df)


#replacing COLLECTIONS as the category for all activities  
pos <- which(df$activity %in% c("about-menu-accountMenu.collections-NULL",
                                "about-menu-collab-collection-NULL",
                                "about-menu-settings-collection-NULL",
                                "about-menu-view-collection-NULL",
                                "collect-menu-collections.all-NULL",
                                "collect-menu-collections.my-NULL",
                                "profile-menu-view-projectcollection-NULL",
                                "about-menu-accountMenu.notifications-NULL"))
replace_string <- "collections"

df$activity <- replace(df$activity, pos, replace_string)


#replacing favorites as the category for all activities
pos <- which(df$activity %in% c("about-menu-accountMenu.favorites-NULL",
                                "collect-menu-favorites.all-NULL",
                                "collect-menu-favorites.my-NULL",
                                "about-menu-view-favorite-NULL",
                                "favorite-NULL-NULL",
                                "profile-menu-view-projectfavorite-NULL"))

replace_string <- "favourites"

df$activity <- replace(df$activity, pos, replace_string)


#replacing about as the category for all activities  
pos <- which(df$activity %in% c("project-menu-project.nav.about-NULL",
                                "project-menu-NULL-\"project.nav.about\""))

replace_string <- "about"

df$activity <- replace(df$activity, pos, replace_string)


#replacing search as the category for all activities  
pos <- which(df$activity %in% c("hashtag-sidebar",
                                "search-NULL",
                                "search-back-NULL-NULL",
                                "change-page-first-page-NULL",
                                "change-page-last-page-NULL",
                                "change-page-next-page-NULL",
                                "change-page-previous-page-NULL",
                                "change-page-select-page-NULL"))

replace_string <- "search"

df$activity <- replace(df$activity, pos, replace_string)


#replacing comment as the category for all activities  
pos <- which(df$activity %in% c("edit-post-NULL-NULL",
                                "link-post-NULL-NULL",
                                "like-post-NULL-NULL",
                                "report-post-NULL-NULL",
                                "subscribe-NULL-NULL",
                                "delete-post-NULL-NULL",
                                "update-comment-NULL-NULL"))

replace_string <- "comment"

df$activity <- replace(df$activity, pos, replace_string)


#replacing classification as the category for all activities
pos <- which(df$activity %in% c("metadata-NULL-NULL"))

replace_string <- "classification"

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
                                "talk-view-NULL-{\"board\":\"Test board\"}",
                                "Notes-NA-NA",
                                "Chat-NA-NA",
                                "Science-NA-NA",
                                "Help-NA-NA",
                                "Collections-NA-NA",
                                "Bug reports-NA-NA",
                                "New Glitch Classes-NA-NA"))

replace_string <- "View.name"

df$activity <- replace(df$activity, pos, replace_string)

#replacing open.field.guide as the category for all activities
pos <- which(df$activity %in% c("open-field-guide-NULL-NULL"))

replace_string <- "open.field.guide"

df$activity <- replace(df$activity, pos, replace_string)

#replacing view.discussion as the category for all activities
pos <- which(df$activity %in% c("view-discussion-NULL-NULL"))

replace_string <- "view.discussion"

df$activity <- replace(df$activity, pos, replace_string)


#replacing talk.view as the category for all activities
pos <- which(df$activity %in% c("talk-view-NULL-NULL"))

replace_string <- "talk.view"

df$activity <- replace(df$activity, pos, replace_string)


#replacing private.message as the category for all activities
pos <- which(df$activity %in% c("send-message",
                                "unfavorite-NULL-NULL"))

replace_string <- "private.message"

df$activity <- replace(df$activity, pos, replace_string)


#categories to delete

geordi_and_comments$Finalcategories <- df$activity

pos <- which((df$activity %in% c("private.message",
                                 "talk.view",
                                 "view.discussion",
                                 "open.field.guide",
                                 "View.name",
                                 "classification",
                                 "metadata",
                                 "comment",
                                 "search",
                                 "about",
                                 "favourites",
                                 "collections")))


geordi_and_comments <- geordi_and_comments[pos,]


write.csv(geordi_and_comments,"AppendedFile.csv")
