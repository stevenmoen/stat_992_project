##################################################### Load data and package ################################################################
library(glmnet)
library(ggplot2)
library(reshape2)
library(tidyr)
library(cowplot)
library(boot)
library(plyr)
# setwd("~/Users/stevenmoen/Documents/GitHub/stat_992_project")
# setwd('~/Documents/classes/stat992/')
dat = read.table('/Users/stevenmoen/Documents/GitHub/stat_992_project/games.csv', sep=',', header=T,  row.names = NULL, fill=T, quote='', stringsAsFactors = FALSE)
# dat = read.table('games.csv', sep=',', header=T,  row.names = NULL, fill=T, quote='', stringsAsFactors = FALSE)
dat = dat[dat$season>=2006&dat$season<2020,]
# remove playoff games
# dat = dat[dat$game_type=="REG",]
# remove games with ties
dat <- dat[dat$result!=0,]
# Teams change name in season 2016 and 2017. We use their old names for consistent comparison.
dat$home_team[dat$home_team=="LA"]="STL"
dat$away_team[dat$away_team=="LA"]="STL"
dat$home_team[dat$home_team=="LAC"]="SD"
dat$away_team[dat$away_team=="LAC"]="SD"


##########################################################################################################################################
############################################ Penalized logistic regression,  Train by season  ############################################
##########################################################################################################################################

season = (min(dat$season)+1):max(dat$season)
team = sort(unique(dat$home_team))
# test_acc if a matrix storing testing set classification accuracy of Bradley-Terry model, and spreadline accuracy.
test_acc = matrix(0, nrow=length(season), ncol=3)
colnames(test_acc) = c('test', 'spread', 'train')
rownames(test_acc) = paste0('season_', season)

convert_design<- function(team_name){
  mat= matrix(0, nrow=length(team_name), ncol=32)
  colnames(mat)=team
  for(i in 1:length(team_name)){
    mat[i, team_name[i]]=1
  }
  return(mat)
}

store_params = matrix(0, nrow=32, ncol = length(season))
rownames(store_params) = c('home', team[-1])
colnames(store_params)=season
for (i in season){
  # Training set
  dat_train=dat[dat$season==i-1,]
  y = 1* (dat_train$home_score > dat_train$away_score)
  home = convert_design(dat_train$home_team)
  away = convert_design(dat_train$away_team)
  train_design =data.frame(y, (home- away)[,-1] )  
  # Testing set
  dat_test=dat[dat$season==i,]
  test_design =data.frame( (convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1])
  
  if(i==season[1]){
    mod1 = glm(y~., data=train_design, family=binomial())
    pred = predict(mod1, newdata = (test_design))
    fit = predict(mod1, newdata = (train_design))
    param0 = mod1$coefficients
    
  }else{
    y = train_design$y
    train_design =as.matrix(train_design[,-1])
    offset= train_design%*%param0[-1]+param0[1]
    #fit.lasso.cv <- cv.glmnet(train_design, cbind(1-y,y), type.measure="class", alpha=0, family="binomial", offset = offset)
    fit.lasso <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=0, lambda =.1, offset = offset)
    param0 = c(fit.lasso$a0,as.vector(Matrix(fit.lasso$beta, sparse = F)))+param0
    pred = as.matrix(cbind(1,test_design))%*%param0
    fit = as.matrix(cbind(1,train_design))%*%param0
    #print(fit.lasso.cv$lambda.min)
    
  }
  store_params[,toString(i)]=param0
  
  test_acc[i-min(season)+1,]=c(
    mean((pred >0)*1 == (dat_test$result>0)*1),
    mean((dat_test$spread_line>0)*1 == (dat_test$result>0)*1),
    mean((fit>0)*1 == (dat_train$result>0)*1)
  )
}
# reshape the test_acc to plot
test_acc_long = gather(data.frame(test_acc, season=rownames(test_acc)), method, accuracy, "test":"train", factor_key=TRUE)
test_acc_long$season = as.integer(unlist(lapply(test_acc_long$season, FUN=function(i){substr(i,8,11)})))
ggplot(test_acc_long, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Classification Accuracy')+xlab("Season")
colMeans(test_acc)

##########################################################################################################################################
##################################### Penalized Logistic regression  Train by week  ######################################################
##########################################################################################################################################

# store_params

for (i in season){
  for (j in 1:17){
    # Subset the data frame by week and season
    dat_test = dat[dat$season==i&dat$week==j,]
    # Create a design matrix
    test_design =as.matrix(cbind(1, (convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1] ) )
    # test_design - use prior season unless we have at least 5 games in the season
    if (j<5){
      param = store_params[, toString(i)]
    }else{
      # Subset the training data
      dat_train = dat[dat$season==i&dat$week<j,]
      # Create a response variable
      y = 1* (dat_train$home_score > dat_train$away_score)
      # Create a design matrix
      train_design = as.matrix(cbind((convert_design(dat_train$home_team)- convert_design(dat_train$away_team))[,-1]))
      param0 =  store_params[, toString(i)]
      offset= train_design%*%param0[-1]+param0[1]
      #fit.lasso.cv <- cv.glmnet(train_design, cbind(1-y,y), type.measure="class", alpha=0, family="binomial", offset = offset)
      # Fir the lasso model
      fit.lasso <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=0, lambda = .1, offset = offset)
      param = c(fit.lasso$a0,as.vector(Matrix(fit.lasso$beta, sparse = F)))+param0
      # param
    }
    # temp
    # Let's add in the spread line
    temp = data.frame(cbind(season=i, week=j,
                            result= (dat_test$result>0),
                            prediction = as.vector(test_design%*%param>0),
                            # Tack on the original spread and the log odd predictions
                            spread=(dat_test$spread_line>0), spread_orig = dat_test$spread_line,
                            # home_team = dat_test$home_team,
                            mod_log_odds = as.vector(test_design%*%param)))
                            # mod_log_odds = as.vector(test_design%*%param),
                            # Bolt on more covariates
                            # roof = dat_test$roof, surface = dat_test$surface))
                            # div_game = dat_test$div_game, home_team = dat_test$home_team))
                            # Bolt on the team
    # temp = data.frame(cbind(season=i, week=j,
                            # result= (dat_test$result>0),
                            # prediction = as.vector(test_design%*%param>0),
                            # spread=(dat_test$spread_line>0)))
    if (i==season[1]&j==1){store_result=temp}else{store_result=rbind(store_result, temp)}
  }
}

head(dat_test)

# test_design%*%param
# store_result
# test_acc1
test_acc1 = data.frame(cbind(aggregate(cbind(store_result[,3]==store_result[,4], store_result[,3]==store_result[,5]), by = list(season=store_result$season),
                                       FUN=mean), test_acc[,1]))
names(test_acc1)=c('season', 'prediction.by.week', 'spread','prediction.by.season')
test_acc_long = gather(test_acc1, method, accuracy, "prediction.by.week":"prediction.by.season", factor_key=TRUE)
ggplot(test_acc_long, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Classification Accuracy')+xlab("Season")
colMeans(test_acc1)
df= aggregate(cbind(store_result[,3]==store_result[,4], store_result[,3]==store_result[,5]),
              by = list(week=store_result$week, season=store_result$season),
              FUN=mean)
ggplot(df, aes(x=week, y=V1, color=as.factor(season)))+geom_point()

####### Steven's Betting Model ######

# Let's see if we can summarize the data
# head(store_result)

# Bolt on the probability
store_result$mod_prob = inv.logit(store_result$mod_log_odds)
store_result

# Add a negative sign for the decision rule
store_result$neg_sign = ifelse(store_result$spread_orig < 0, 1, 0)
store_result

# Convert to absolut evalue
store_result$abv_ps = abs(store_result$spread_orig)
store_result$abv_ps

# Ok, we need to bolt on a mapping from the spread to the implied odds
spto_df = read.table('/Users/stevenmoen/Documents/GitHub/stat_992_project/spread_to_odds.csv', sep=',', header=T,  row.names = NULL, fill=T, quote='', stringsAsFactors = FALSE)
# Spread to odds dataframe
# spto_df$Point.Spread*2
head(spto_df)

colnames(spto_df)[colnames(spto_df) == 'Point.Spread'] <- 'abv_ps'


# Let's find a way to bolt this on
# store_result
# abc = merge(x = store_result, y = spto_df, by.x = "spread_orig",
            # by.y = "Point.Spread", all.x = TRUE)
# ?merge
# head(abc)

store_result = join(x = store_result, y = spto_df, by = "abv_ps")
head(store_result)

# Now, we simply need to find the spreadline odds
store_result$sl_prob = ifelse(store_result$neg_sign == 0, store_result$Favorite.Win.Chance, store_result$Underdog.Win.Chance)
head(store_result)

# Assign a probability of 0.999999 if abv_ps > 17, else 1 - this
num_infty = 0.999999

# Assign point spreads greater than or equal to 17 values of numerical infinity, else 1 - neg infinity
store_result$sl_prob[store_result$spread_orig >= 17] <- num_infty
store_result$sl_prob[store_result$spread_orig <= -17] <- (1-num_infty)

# store_result$sl_prob = ifelse(store_result$spread_orig >= 17, store_result$sl_prob = num_infty, store_result$sl_prob = store_result$sl_prob)

# store_result$sl_prob = ifelse(store_result$neg_sign == 0, sl_prob = num_infty, sl_prob = (1-num_))

# Check for missingness
sum(is.na(store_result$sl_prob))
# Yay none!
# store_result[170:179,]

# Now we need to find the difference between the two (experimental- theoretical)
# or, the implied odds from the spreadline versus the model probabilities
store_result$emt_pred = store_result$sl_prob - store_result$mod_prob

# par(mfrow=c(1,1))

# Visualize the data
hist(store_result$emt_pred, breaks = 20, xlab = "Vegas Prob. - Model Prob.")

# Sanity check
ggplot() + geom_histogram(aes(x=emt_pred), colour="red", data=store_result) 
# geom_density(aes(x=sl_prob), colour="blue", data=store_result)


# Summarize the data
summary(store_result$emt_pred)

# abc

# Sourece
# https://www.boydsbets.com/nfl-spread-to-moneyline-conversion/

# head(dat)

### Visualization

head(store_result)
p <- ggplot(data = store_result, aes(x = emt_pred)) + geom_histogram(binwidth = 0.05)
p + facet_wrap(~season)

head(store_result)

## What if we could plot both?
ggplot(store_result, aes(x=c)) + geom_density(aes(group=sl_prob))
# data_melt = melt(store_result)
# ?melt
# data_melt

ggplot() + geom_histogram(aes(x=emt_pred), colour="red", data=store_result) 
  # geom_density(aes(x=sl_prob), colour="blue", data=store_result)


# This is the one we want - 
p1 <- ggplot() + 
  geom_density(aes(x=mod_prob), colour="red", data=store_result) + 
  geom_density(aes(x=sl_prob), colour="blue", data=store_result)
# + scale_x_continuous("Test")
  # +labs(title="Win Probability",
  #     x ="Predicted Win Probability")


# Mess with the subtitle
special_sub = expression(paste("Our Model in Red, ", "Sports Book Odds in Blue"))
# special_sub = multiTitle(color="red","Hair color", color="black"," and ",color="blue","Eye color")

# abc = expression("Hair color" * phantom(" and Eye color"),col.main="red")


# title(expression(phantom("Hair color and ") *
#                    "Eye color"),col.main="blue")
# 
# title(expression(phantom("Hair color ") *
#                    
#                    "and " * phantom("Eye color"),col.main="black")) 

p2 <- p1 + scale_x_continuous("Win Probability") + scale_y_continuous("Density")  + ggtitle("Comparing Win Probabilities", subtitle = special_sub)

# Overall win probabilities
p2

# p2 <- p1 + scale_x_continuous("Win Probability") + scale_y_continuous("Density")  + ggtitle("Comparing Win Probabilities", subtitle = "Red is Our Model, Blue is Vegas")

# Mess with the subtitle

# p2 +  theme(
#   plot.title = element_text(color = "red", size = 12, face = "bold"),
#   plot.subtitle = element_text(color = "blue"),
#   plot.caption = element_text(color = "green", face = "italic")
# )

# By Season
season <- ggplot() + geom_density(aes(x=mod_prob), colour="red", data=store_result) + 
  geom_density(aes(x=sl_prob), colour="blue", data=store_result) + facet_wrap(~season)
# season
season + scale_x_continuous("Win Probability") + scale_y_continuous("Density")  + ggtitle("Comparing Win Probabilities by Season", subtitle = special_sub)

# By week
week <- ggplot() + geom_density(aes(x=mod_prob), colour="red", data=store_result) + 
  geom_density(aes(x=sl_prob), colour="blue", data=store_result) + facet_wrap(~week)

week + scale_x_continuous("Win Probability") + scale_y_continuous("Density")  + ggtitle("Comparing Win Probabilities by Week", subtitle = special_sub)

# Bolt on team


# p <- ggplot(data=store_result, aes(x=emt_pred, group=cut, fill=cut)) +
  # geom_density(adjust=1.5, alpha=.4) +
  # theme_ipsum()

### The Kelly Criterion ###
head(store_result)

# store_result$mod_prob
# Calculate spread line odds
store_result$sl_odds = (store_result$sl_prob)/(1- store_result$sl_prob)

# Calculate b assuming a fair casino
# store_result$kb = (1/(store_result$sl_prob))-1
store_result$kb = 1/store_result$sl_odds

# store_result$sl_odds


# Find out the Kelly bet
store_result$kelly_bet = store_result$mod_prob - ((1-store_result$mod_prob)/store_result$kb)

# Code up a negative dummy
store_result$kelly_result = ifelse(store_result$result == 0, -1, 1)

# head(store_result)
# Find out the total payoff
store_result$kb_payoff = ifelse(store_result$kelly_result*store_result$kelly_bet > 0, 
                                abs(store_result$kelly_bet) + 1, 1 - abs(store_result$kelly_bet))


head(store_result)
# Write a function
kb_func_rand = function(bankroll, df=store_result, niter= 100, samp_no = 100){
  # Initialize a storage matrix
  br_mat = matrix(0, samp_no, niter)
  # Loop through and populate
  for (j in 1:niter){
    # Take a sample
    t_samp = df[sample(nrow(df), samp_no), ]
    # Re-order the data frame
    t_samp$index <- as.numeric(row.names(t_samp))
    t_samp = t_samp[order(t_samp$index), ]
    # print(t_samp)
    # Initialize a storage vector
    # br_vec = rep(0, nrow(df))
    # Loop through
    for (i in 1:nrow(t_samp)){
      # If we have one, then we just simply extract the first value
      if (i ==1){
        br_mat[i,j] = t_samp$kb_payoff[i]*bankroll
        # If you're at 0, then you're at zero
        br_mat[i,j] = ifelse(br_mat[i,j] <0 ,0 , br_mat[i,j])
      } else {
        # Calculate the new payoff
        br_mat[i,j] = t_samp$kb_payoff[i]*br_mat[i-1,j]
        # If you're at 0, then you're at zero
        br_mat[i,j] = ifelse(br_mat[i,j] <0 ,0 , br_mat[i,j])
      }
      # print(br_vec)
    }
    # Add one to everyone to avoid problems with log scale
    br_mat = br_mat + 1 
    # Plot on the log scale
    if (j ==1){
      # Now we need to plot the data
      plot(log(br_mat[,j]), type = "l", xlab = "Number of Bets Placed",
      ylab = "Log Dollars", ylim = c(0,10),
      main = paste("Kelly Criterion Betting Results Over", niter, "Simulations"),
      sub = "$1 was Added to the Original Matrix of Dollars for Stability")
      # Add a horizonal line
      abline(h = log(100+1), col = "red", lty = 2)
      legend("topright", legend = c(paste0("Starting Bankroll of $",bankroll)), col = "red", lty = 2)
      # plot(br_mat[,j], type = "l", xlab = "Number of Bets Placed",
           # ylab = "Dollars", ylim = c(0,1000))
    } else {
      # lines(br_mat[,j])
      lines(log(br_mat[,j]))
    }
    
    # Add lines
    # lines(br_mat[,j])
  }
  # Plot the matrix
  # plot(br)
  # Add lines
  # Plot the data
  # plot(br_vec, type = "l", xlab = "Number of "
  return(br_mat)
}

def = kb_func_rand(100)
# def

# Append an index

# t_samp = sample(store_result, size = 10)
t_samp = store_result[sample(nrow(store_result), 5), ]

t_samp$index <- as.numeric(row.names(t_samp))
t_samp <- t_samp[order(t_samp$index), ]

t_samp$kb_payoff[1]

# sample(nrow(store_result), 3)
index(store_result)
order(t_samp)
# kb_func = function(bankroll, df=store_result){
#   # Print the data frame
#   # print(df)
#   # Initialize a storage vector
#   br_vec = rep(0, nrow(df))
#   # Loop through
#   for (i in 1:nrow(df)){
#     # If we have one, then we just simply extract the first value
#     if (i ==1){
#       br_vec[i] = df$kb_payoff[i]*bankroll
#     } else {
#       # Calculate the new payoff
#       br_vec[i] = df$kb_payoff[i]*br_vec[i-1]
#       # If you're at 0, then you're at zero
#       br_vec[i] = ifelse(br_vec[i] <0 ,0 , br_vec[i])
#     }
#     # print(br_vec)
#   }
#   # Plot the data
#   # plot(br_vec, type = "l", xlab = "Number of "
#   return(br_vec)
# }

store_result$kb_payoff[1]

abc = kb_func(100, df=store_result[1:100,])
plot(abc)


######################################################### Bradley Terry ####################################################################
for (i in season){
  # Training set
  dat_train=dat[dat$season==i-1,]
  y = 1* (dat_train$home_score > dat_train$away_score)
  home = convert_design(dat_train$home_team)
  away = convert_design(dat_train$away_team)
  train_design =data.frame(y, (home- away)[,-1])
  # Testing set
  dat_test=dat[dat$season==i,]
  test_design =data.frame( (convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1])
  
  mod1 = glm(y~., data=train_design, family=binomial())
  pred = predict(mod1, newdata = (test_design))
  fit = predict(mod1, newdata = (train_design))
  
  test_acc[i-min(season)+1,]=c(
    mean((pred >0)*1 == (dat_test$result>0)*1),
    mean((dat_test$spread_line>0)*1 == (dat_test$result>0)*1),
    mean((fit>0)*1 == (dat_train$result>0)*1)
  )
  store_params[,toString(i)]=mod1$coefficients
}
# reshape the test_acc to plot
test_acc_long = gather(data.frame(test_acc, season=rownames(test_acc)), method, accuracy, "test":"train", factor_key=TRUE)
test_acc_long$season = as.integer(unlist(lapply(test_acc_long$season, FUN=function(i){substr(i,8,11)})))
ggplot(test_acc_long, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Classification Accuracy')+xlab("Season")

# Add New Covariates - Steven

head(train_design)
dim(train_design)
    