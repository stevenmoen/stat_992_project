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
# season
for (i in 2007){
# for (i in season){
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
      fit.lasso <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=1, lambda = .1, offset = offset)
      fit.ridge <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=0, lambda = .1, offset = offset)
      # print(fit.lasso$beta)
      # print(fit.ridge$beta)
      # fit.lasso <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=0, lambda = .1, offset = offset)
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
head(store_result)

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

# sum(store_result$result)/nrow(store_result)

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


summary(store_result$kelly_bet)
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



##########################################################################################################################################
##################################### Penalized Logistic regression - Train by week  ######################################################
##########################################################################################################################################

# team

# STM Notes on 12.12.2020 - Let's fit a model on spreadline

# First step - modify the store_params matrix - adding a variable for the spread line at the end
store_params = matrix(0, nrow=33, ncol = length(season))
rownames(store_params) = c(c('home', team[-1]),"spread_line")
colnames(store_params)=season

# test_acc_spread if a matrix storing testing set classification accuracy of Bradley-Terry model, and spreadline accuracy.
test_acc_spread = matrix(0, nrow=length(season), ncol=3)
colnames(test_acc_spread) = c('test', 'spread', 'train')
rownames(test_acc_spread) = paste0('season_', season)

# store_params

# Modify the original code to work with spreadline

# store_params = matrix(0, nrow=32, ncol = length(season))
# rownames(store_params) = c('home', team[-1])
# colnames(store_params)=season
for (i in season){
# for (i in 2007){
  # Training set
  dat_train=dat[dat$season==i-1,]
  # Modify to be the result
  y = dat_train$result
  # train_design
  # y = 1* (dat_train$home_score > dat_train$away_score)
  home = convert_design(dat_train$home_team)
  away = convert_design(dat_train$away_team)
  # Bolt on the spread line and rename
  train_design =data.frame(y, (home- away)[,-1],dat_train$spread_line)
  # head(train_design)
  # names(train_design)
  # Rename the data
  names(train_design)[names(train_design) == 'dat_train.spread_line'] <- 'spread_line'
  # Testing set
  dat_test=dat[dat$season==i,]
  # Bolt on the spread line and rename
  test_design =data.frame((convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1],dat_test$spread_line)
  names(test_design)[names(test_design) == 'dat_test.spread_line'] <- 'spread_line'
  # Train using the simple GLM model
  if(i==season[1]){
    # ?glm
    mod1 = glm(y~., data=train_design, family=gaussian())
    pred = predict(mod1, newdata = (test_design))
    fit = predict(mod1, newdata = (train_design))
    param0 = mod1$coefficients
    
  }else{
    # Pull out the y variable
    y = train_design$y
    train_design =as.matrix(train_design[,-1])
    offset= train_design%*%param0[-1]+param0[1]
    #fit.lasso.cv <- cv.glmnet(train_design, cbind(1-y,y), type.measure="class", alpha=0, family="binomial", offset = offset)
    fit.ridge <- glmnet(train_design, y, family="gaussian", alpha=0, lambda =.1, offset = offset)
    # fit.lasso <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=0, lambda =.1, offset = offset)
    # param0 = c(fit.lasso$a0,as.vector(Matrix(fit.lasso$beta, sparse = F)))+param0
    param0 = c(fit.ridge$a0,as.vector(Matrix(fit.ridge$beta, sparse = F)))+param0
    pred = as.matrix(cbind(1,test_design))%*%param0
    fit = as.matrix(cbind(1,train_design))%*%param0
    #print(fit.lasso.cv$lambda.min)
    
  }
  # Store the parameters
  store_params[,toString(i)]=param0
  # Measure the testing accuracy
  test_acc[i-min(season)+1,]=c(
    mean((pred >0)*1 == (dat_test$result>0)*1),
    mean((dat_test$spread_line>0)*1 == (dat_test$result>0)*1),
    mean((fit>0)*1 == (dat_train$result>0)*1)
  )
  # Measure the testing accuracy against the spread
  # Define a variable that's the decision criterion - let's call it "bet_cover"
  # Do it for both train and test
  bet_cover_train = ifelse(fit > dat_train$spread_line, 1, -1)
  bet_cover_test = ifelse(pred > dat_test$spread_line, 1, -1)
  # Define cover the spread variables
  cts_train = ifelse(dat_train$result > dat_train$spread_line, 1, -1)
  cts_test = ifelse(dat_test$result > dat_test$spread_line, 1, -1)
  # cts_train = (dat_train$result - dat_train$spread_line >0)*1
  # cts_test = (dat_test$result - dat_test$spread_line >0)*1
  # Did we bet correctly on the training set?
  result_train = ifelse(bet_cover_train*cts_train > 0, 1, 0)
  # On the test set?
  result_test = ifelse(bet_cover_test*cts_test > 0, 1, 0)
  # What did the spread say?
  result_spread = ifelse(cts_test > 0, 1, 0)
  # The idea is that we would bet to cover the spread if we predict a higher
  test_acc_spread[i-min(season)+1,]=c(
    # This is whether we cover the spread in our test data
    # Did we bet correctly on our test set?
    mean(result_test),
    # How did the spread line do?
    mean(result_spread),
    # Did we bet correctly on our training set?
    mean(result_train)
    
    
    # mean((pred >0)*1 == (dat_test$result>0)*1),
    # mean((dat_test$spread_line>0)*1 == (dat_test$result>0)*1),
    # mean((fit>0)*1 == (dat_train$result>0)*1)
  )
}
# reshape the test_acc to plot
test_acc_long = gather(data.frame(test_acc, season=rownames(test_acc)), method, accuracy, "test":"train", factor_key=TRUE)
test_acc_long$season = as.integer(unlist(lapply(test_acc_long$season, FUN=function(i){substr(i,8,11)})))
ggplot(test_acc_long, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Classification Accuracy')+xlab("Season")
colMeans(test_acc)

# Plot the accuracy against the spread
test_acc_long2 = gather(data.frame(test_acc_spread, season=rownames(test_acc_spread)), method, accuracy, "test":"train", factor_key=TRUE)
test_acc_long2$season = as.integer(unlist(lapply(test_acc_long2$season, FUN=function(i){substr(i,8,11)})))
ggplot(test_acc_long2, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Spread Betting Accuracy')+xlab("Season")
colMeans(test_acc)

# store_params
# season
# for (i in 2007){
for (i in season){
  for (j in 1:17){
    # Subset the data frame by week and season
    dat_test = dat[dat$season==i&dat$week==j,]
    # head(dat_test)
    # Create a design matrix - bolt on spread line
    # test_design =as.matrix(cbind(1, (convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1] ) )
    # Bolt spreadline onto the end
    test_design =as.matrix(cbind(1, (convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1] ,dat_test$spread_line) )
    # names(test_design)[names(test_design) == 'dat_test.spread_line'] <- 'spread_line'
    # test_design =as.matrix(cbind(1, dat_test$spread_line, (convert_design(dat_test$home_team)-convert_design(dat_test$away_team))[,-1] ) )
    # test_design
    # test_design - use prior season unless we have at least 5 games in the season
    if (j<5){
      param = store_params[, toString(i)]
    }else{
      # Subset the training data
      dat_train = dat[dat$season==i&dat$week<j,]
      # Create a response variable
      # y = 1* (dat_train$home_score > dat_train$away_score)
      # Make a margin of victory variable
      # head(dat_train)
      y = dat_train$result
      # y = (dat_train$home_score > dat_train$away_score)
      # Create a design matrix
      train_design = as.matrix(cbind((convert_design(dat_train$home_team)- convert_design(dat_train$away_team))[,-1], dat_train$spread_line))
      # names(train_design)[names(train_design) == 'dat_train.spread_line'] <- 'spread_line'
      # train_design = as.matrix(cbind((convert_design(dat_train$home_team)- convert_design(dat_train$away_team))[,-1]))
      param0 =  store_params[, toString(i)]
      offset= train_design%*%param0[-1]+param0[1]
      # param0
      # train_design
      #fit.lasso.cv <- cv.glmnet(train_design, cbind(1-y,y), type.measure="class", alpha=0, family="binomial", offset = offset)
      # Fit the lasso model
      fit.lasso <- glmnet(train_design, y, family="gaussian", alpha=1, lambda = .1, offset = offset)
      # Fit the ridge model
      fit.ridge <- glmnet(train_design, y, family="gaussian", alpha=0, lambda = .1, offset = offset)
      # print(fit.lasso$beta)
      # print(fit.ridge$beta)
      # fit.lasso <- glmnet(train_design, cbind(1-y,y), family="binomial", alpha=0, lambda = .1, offset = offset)
      param = c(fit.ridge$a0,as.vector(Matrix(fit.ridge$beta, sparse = F)))+param0
      # param
    }
    # temp
    # Let's add in the spread line
    temp = data.frame(cbind(season=i, week=j,
                            result = dat_test$result,
                            # result= (dat_test$result>0),
                            prediction = as.vector(test_design%*%param),
                            # prediction = as.vector(test_design%*%param>0),
                            # Tack on the original spread and the log odd predictions
                            spread=(dat_test$spread_line>0), spread_line = dat_test$spread_line))
                            # home_team = dat_test$home_team,
                            # mod_log_odds = as.vector(test_design%*%param)))
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

# Bolt on accuracy variables
head(store_result)

# Measure the testing accuracy against the spread
# Define a variable that's the decision criterion - let's call it "bet_cover"
# Do it for both train and test
store_result$bet_cover_test = ifelse(store_result$prediction > store_result$spread_line, 1, -1)
# store_result$bet_cover_test = ifelse(pred > dat_test$spread_line, 1, -1)
# Define cover the spread variables
# store_result$cts_train = ifelse(store_result$result > store_result$spread_line, 1, -1)
store_result$cts_test = ifelse(store_result$result > store_result$spread_line, 1, -1)
# cts_train = (dat_train$result - dat_train$spread_line >0)*1
# cts_test = (dat_test$result - dat_test$spread_line >0)*1
# Did we bet correctly on the training set?
# result_train = ifelse(bet_cover_train*cts_train > 0, 1, 0)
# On the test set?
store_result$result_test = ifelse(store_result$bet_cover_test*store_result$cts_test > 0, 1, 0)
# What did the spread say?
store_result$result_spread = ifelse(store_result$cts_test > 0, 1, 0)
# The idea is that we would bet to cover the spread if we predict a higher
# test_acc_spread[i-min(season)+1,]=c(
#   # This is whether we cover the spread in our test data
#   # Did we bet correctly on our test set?
#   mean(result_test),
#   # How did the spread line do?
#   mean(result_spread),
#   # Did we bet correctly on our training set?
#   mean(result_train)
#   
#   
#   # mean((pred >0)*1 == (dat_test$result>0)*1),
#   # mean((dat_test$spread_line>0)*1 == (dat_test$result>0)*1),
#   # mean((fit>0)*1 == (dat_train$result>0)*1)
# )

# test_acc1

store_result$result_test
# test_acc

# Analyze the accuracy
test_acc1 = data.frame(cbind(aggregate(cbind(store_result$result_test, store_result$result_spread), by = list(season=store_result$season),
                                       FUN=mean), test_acc_spread[,1]))
names(test_acc1)=c('season', 'prediction.by.week', 'spread','prediction.by.season')
test_acc_long = gather(test_acc1, method, accuracy, "prediction.by.week":"prediction.by.season", factor_key=TRUE)
ggplot(test_acc_long, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Classification Accuracy')+xlab("Season")
colMeans(test_acc1)
df= aggregate(cbind(store_result$result_test, store_result$result_spread),
              by = list(week=store_result$week, season=store_result$season),
              FUN=mean)
ggplot(df, aes(x=week, y=V1, color=as.factor(season)))+geom_point()

# Find the accuracy by week
df_week = aggregate(cbind(store_result$result_test, store_result$result_spread),
                    by = list(week=store_result$week),
                    FUN=mean)
names(df_week)=c('season', 'prediction.by.week', 'spread')
# Transform the data frame
test_acc_week = gather(df_week, method, accuracy, "prediction.by.week":"spread", factor_key=TRUE)
ggplot(test_acc_week, aes(x=season, y=accuracy, color=method)) + theme_bw()+
  geom_line()+theme(legend.title = element_blank())+ylab('Classification Accuracy')+xlab("Season")

# df_week


###### Run the betting model #######

### The Kelly Criterion ###
head(store_result)

# store_result$mod_prob
# Calculate spread line odds
# store_result$sl_odds = (store_result$sl_prob)/(1- store_result$sl_prob)

# Calculate b assuming a fair casino - in other words, an even money bet
store_result$kb_fair = 1 
store_result$kb_105 = 100/105
store_result$kb_110 = 100/110

# store_result$kb = (1/(store_result$sl_prob))-1
# store_result$kb = 1/store_result$sl_odds

# store_result$sl_odds
store_result


# Find out the Kelly bet
store_result$kelly_bet = store_result$mod_prob - ((1-store_result$mod_prob)/store_result$kb)

# Code up a negative dummy
store_result$kelly_result = ifelse(store_result$result == 0, -1, 1)

# head(store_result)
# Find out the total payoff
store_result$kb_payoff = ifelse(store_result$kelly_result*store_result$kelly_bet > 0, 
                                abs(store_result$kelly_bet) + 1, 1 - abs(store_result$kelly_bet))


summary(store_result$kelly_bet)
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

# store_result

# season
# param
    