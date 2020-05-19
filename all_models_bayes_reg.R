table(title_type)
feature_film1=ifelse(title_type=="Feature Film",1,0)
movies %>% mutate(feature_film,ifelse(title_type=="Feature Film", "Yes", "No"))
movies %>% mutate(R_rated,ifelse(mpaa_rating=="R", "Yes", "No")) 

movies=mutate(movies,  feature_film= ifelse(title_type=="Feature Film", "Yes", "No"));
movies=mutate(movies,  R_rated= ifelse(mpaa_rating=="R", "Yes", "No"));
movies=mutate(movies,  oscar_season= ifelse(dvd_rel_month>9, "Yes", "No"));
movies=mutate(movies,  summer_season= ifelse(dvd_rel_month>4&dvd_rel_month<9, "Yes", "No"));

movies2=select(movies,c(runtime,thtr_rel_year,feature_film,R_rated,oscar_season,
                       summer_season,imdb_rating,imdb_num_votes,critics_score,
                       best_pic_nom,best_pic_win,best_actor_win,best_actress_win,
                       best_dir_win,top200_box,audience_score))

documentry=ifelse(title_type=="Documentary",1,0)
table(genre)
drama1=ifelse(title_type=="Drama",1,0)
R_rated=ifelse(mpaa_rating=="R",1,0)
oscar_season=ifelse(dvd_rel_month>9,1,0)
summer_season=ifelse(dvd_rel_month>4&dvd_rel_month<9,1,0)
ELSE =T
movies1 <- movies %>% mutate(.,feature_film1= with(.,case_when(
  (title_type=="Feature Film") ~ 1,
  ELSE ~ 0
)))

fest=mutate(movies,feature_film1= with(title_type,case_when(
  (title_type=="Feature Film") ~ 1,
  ELSE ~ 0
)))

{r, Modelling}

# m1= lm(audience_score~., data = movies2)
# summary(m1)
# 
# result = data.frame(fitted_values = fitted.values(m1),
#                     residuals = residuals(m1))
# 
# library(ggplot2)
# ggplot(data = result, aes(x = fitted_values, y = residuals)) +
#   geom_point(pch = 1, size = 2) + 
#   geom_abline(intercept = 0, slope = 0) + 
#   xlab(expression(paste("fitted value ", widehat(audience_score)))) + 
#   ylab("residuals")
# 
# new_k = qnorm(0.5 + 0.5 * 0.95 ^ (1 / 651))
# new_k
# 
# outliers = Bayes.outlier(m1, k=3.94)
# p1=outliers$prob.outlier
# a1=which.max(p1)
# p1[a1]

# Import libary
library(BAS)

# Use `bas.lm` for regression
m2= bas.lm(audience_score~., 
           data = movies2,
           initprobs = "eplogp",
           prior = "BIC",
           modelprior = uniform())


round(summary(m2), 3)
print(m2)
image(m2, rotate = F)
c1=coef(m2,estimator = "BMA")

print(c1)
plot(m2, which = 1, add.smooth = F, 
     ask = F, pch = 16, sub.caption="", caption="")
abline(a = 0, b = 0, col = "darkgrey", lwd = 2)


plot(m2, which = 4, ask = F, caption = "", sub.caption = "", 
     col.in = "blue", col.ex = "darkgrey", lwd = 3)

#diagnostics(m2, type = "model", col = "blue", pch = 16, cex = 1.5)

round(confint(c1), 4)

par(mfrow = c(2,2))
plot(c1, subset = c(1:4), 
     col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)
par(mfrow = c(2,2))
plot(c1, subset = c(5:8), 
     col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)
par(mfrow = c(2,2))
plot(c1, subset = c(9:12), 
     col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)
par(mfrow = c(2,2))
plot(c1, subset = c(13:15), 
     col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)
library(pander)
pandoc.table(m2$namesx[m2$probne0 > .5])

# 
# m3= bas.lm(audience_score~.-imdb_rating-critics_score-thtr_rel_year, 
#                  data = movies2, prior = "BIC",
#                  modelprior = uniform())
# 
# 
# round(summary(m3), 3)
# print(m3)
# image(m3, rotate = F)
# coef(m3)
# 
# # Re-run regression using larger number of MCMC iterations
# m4= bas.lm(audience_score~ ., data = movies2,
#                   prior = "ZS-null", modelprior = uniform(),
#                   method = "MCMC", MCMC.iterations = 10 ^ 6)
# 
# # Plot diagnostics again
# diagnostics(m4, type = "model", col = "blue", pch = 16, cex = 1.5)
# image(m4, rotate = F)
# coef.m4=coef(m4)
# 
# par(mfrow = c(2,2))
# plot(coef.m4, subset = c(1:4), 
#      col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)
# 
# par(mfrow = c(2,2))
# plot(coef.m4, subset = c(5:8), 
#      col.lab = "darkgrey", col.axis = "darkgrey", col = "darkgrey", ask = F)
# 
# round(confint(coef.m4), 4)
```