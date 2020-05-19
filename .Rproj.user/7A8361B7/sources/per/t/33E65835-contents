rm(list=ls())
library(dplyr)
library(gbm)
library(ggplot2)
load("movies.Rdata")
library(e1071)
attach(movies)
names(movies)
getwd()
movies=mutate(movies,  feature_film= ifelse(title_type=="Feature Film", "Yes", "No"))
movies=mutate(movies,  drama= ifelse(genre=="Drama", "Yes", "No"))
movies=mutate(movies,  R_rated= ifelse(mpaa_rating=="R", "Yes", "No"))

movies=mutate(movies,  oscar_season= ifelse(dvd_rel_month>9, "Yes", "No"))
movies=mutate(movies,  summer_season= ifelse(dvd_rel_month>4&dvd_rel_month<9, "Yes", "No"))
table(genre)
# feature_film= ifelse(title_type=="Feature Film",1,0)
# drama= ifelse(genre=="Drama", 1, 0)
# R_rated= ifelse(mpaa_rating=="R", 1, 0)
# oscar_season= ifelse(dvd_rel_month>9,1,0)
# summer_season= ifelse(dvd_rel_month>4&dvd_rel_month<9,1,0)
# 
# movies$feature_film=factor(feature_film,c(1,0),c("yes","n0"))
# movies$drama=factor(drama,c(1,0),c("yes","n0"))
# movies$R_rated=factor(R_rated,c(1,0),c("yes","no"))
# movies$oscar_season=factor(oscar_season,c(1,0),c("yes","n0"))
# movies$summer_season=factor(summer_season,c(1,0),c("yes","n0"))
install.packages("GGally")
library(GGally)


movies2=select(movies,c(runtime,drama,feature_film,R_rated,oscar_season,
                        summer_season,imdb_rating,imdb_num_votes,critics_score,
                        best_pic_nom,best_pic_win,best_actor_win,best_actress_win,
                        best_dir_win,top200_box,audience_score))

rm(drama,feature_film,oscar_season ,R_rated,summer_season)

str(movies2)
rm(movies)

str(movies2)
rowSums((apply(movies2,1,is.na)))
cbind(rowSums((apply(movies2,1,is.na))))

fac_var <- names(movies2)[which(sapply(movies2, is.factor))]

numeric_var <- names(movies2)[which(sapply(movies2, is.numeric))]

s1=apply(movies2[,numeric_var],2,log)
movies3=data.frame(s1,movies2[,fac_var])
summary(movies2[,numeric_var])

movies2[numeric_var]=(apply(movies2[,fac_var], 2,table))


plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}


doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}
doPlots(movies2[,fac_var], fun = plotHist, ii = 1:6, ncol = 2)
doPlots(movies2[,fac_var], fun = plotHist, ii = 7:11, ncol = 2)
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], audience_score = movies2$audience_score)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}
doPlots(movies2[,numeric_var], fun = plotDen, ii = 1:6, ncol = 2)

qplot(drama,audience_score,color=feature_film, data=movies2,geom = c("boxplot"))
qplot(best_actor_win,audience_score,color=best_actress_win, data=movies2,geom = c("boxplot"))
qplot(best_dir_win,audience_score,color=best_pic_win, data=movies2,geom = c("boxplot"))


pairs(as.matrix(numeric_var))

library(ggplot2)
names(movies2)
qplot(audience_score,audience_rating, data=movies2)
bY_drama<-movies2 %>%  group_by(drama) 
bY_drama %>% summarise(score= mean(audience_score) )

qplot(audience_score,data=movies2,,binwidth=5,
      facets = drama~.,color=thtr_rel_year)


qplot(audience_score,data=movies2,,binwidth=5,
      facets = summer_season~.,)
qplot(audience_score,critics_score, data=movies2,
      color=drama, facets = oscr_season ~.)


pairs(cbind(runtime,imdb_num_votes,critics_score,
                       best_pic_nom,best_pic_win,best_actor_win,best_actress_win,
                       best_dir_win,top200_box,audience_score))

qplot(drama,audience_score,data=movies2,geom = c("boxplot","jitter"))
qplot(drama,audience_score,color=feature_film, data=movies2,geom = c("boxplot"))

qplot(best_actor_win,audience_score,color=best_actress_win, data=movies2,geom = c("boxplot"))

qplot(drama,audience_score,colour=feature_film,facets =R_rated~. ,  data=movies2,geom = c("boxplot","jitter"))

cat_var <- names(movies2)[which(sapply(movies2, is.character))]

fac_var <- names(movies2)[which(sapply(movies2, is.factor))]

numeric_var <- names(movies2)[which(sapply(movies2, is.numeric))]

