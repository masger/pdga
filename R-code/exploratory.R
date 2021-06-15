# 
library(data.table)
library(ggplot2)
library(ggsci)
library(lubridate)
library(mgcv)
# time is weird - change and change back :) 



# high_rated = unique(pdga[rating>1040,player])
# high_dt = pdga[player %chin% high_rated,]

high_dt = pdga[,if(max(rating) > 1040) .SD , by = player]



ggplot(high_dt,aes(x = date, y = rating, group = player, col = player)) + geom_line(size = 1.3) + theme_minimal() + 
  scale_color_d3(palette = "category20")
ggsave("figures/1040_player.pdf")

ggplot(high_dt,aes(x = date0, y = rating, group = player, col = player)) + geom_line(size = 1.3) + theme_minimal() + 
  scale_color_d3(palette = "category20")
library(gamm4)

#### 
g1 = gamm4(rating~s(date0),random = ~(1|player),data = high_dt)
plot(g1$gam)
coef(g1$mer)
coef(g1$gam)

# predictions with random effects
pred = predict(g1$gam, newdata = high_dt[,"date0"]) 
pred = predict(g1$gam, newdata = high_dt[,c("player","date0")]) 


pred_df = data.frame(pred, high_dt[,c("player","date0","rating")])

ggplot(pred_df,aes(date0, pred)) + geom_line()

ggplot(pred_df,aes(rating,pred,col = player)) + geom_point()


test = coef(g1$mer)$player
setDT(test,keep.rownames = T)
names(test) = c("rn","re_inter","date0","fixed_intercept","fixed_date0")
test = test[rep(seq_len(nrow(test)), 20)]
test[, date0 := seq_len(.N)-1, by = rn]

test[,pred := re_inter+fixed_intercept+date0*(fixed_date0)]
ggplot(test,aes(date0, pred,col=rn)) + geom_line()

library(gamm4)
### predictions with random effect on slope and intercept
g1 = gamm4(rating~s(date0),random = ~(date0|player),data = high_dt)

test = coef(g1$mer)$player
setDT(test,keep.rownames = T)
names(test) = c("rn","re_inter","re_date0","date0","fixed_intercept","fixed_date0")
test = test[rep(seq_len(nrow(test)), 20)]
test[, date0 := seq_len(.N)-1, by = rn]

test[,pred := re_inter+fixed_intercept+date0*(re_date0+fixed_date0)]
ggplot(test,aes(date0, pred,col=rn)) + geom_line()
library(broom)



### try the conventional method
high_dt = pdga[,if(max(rating) > 1000) .SD , by = player]
high_dt[,fac := as.factor(player)]

rm1 <- bam(rating ~ s(date0)+s(fac,bs = "re")+s(date0,fac,bs="re"),data=high_dt,method = "fREML",
           discrete = TRUE,nthreads = 4)

fv0 <- predict(rm1,exclude=c("s(fac)","s(date0,fac)")) ## predictions setting r.e. to 0
fv1 <- predict(rm1,exclude="s(date0,fac)") ## predictions setting r.e. to 0
fv2 <- predict(rm1) ## predictions setting r.e. to predicted values
high_dt[,pred1 := fv0]
high_dt[,pred2 := fv1]
high_dt[,pred3 := fv2]



plot_df = melt(high_dt,id.vars = c("pdga","player","date0"), measure.vars = c("rating","pred1","pred2","pred3"))
ggplot(plot_df,aes(date0,value,col = player)) + facet_wrap(~variable) + geom_line(size = 1.01) + 
  guides(colour = F) + ggsci::scale_color_d3(palette = "category20b") + theme_bw() +
  coord_cartesian(xlim = c(0,10))
ggsave("disc_golf_1040.svg")

re_df = mixedup::extract_random_effects(rm1)
setDT(re_df)
re_df[effect == "date0"][order(-value)][value>10]

# some fast learners
fast = re_df[effect == "date0"][order(-value)][value>10]$group
ggplot(high_dt[player %chin% fast],aes(date0, rating,col=player)) + geom_line() + scale_color_d3(palette = "category20b")
