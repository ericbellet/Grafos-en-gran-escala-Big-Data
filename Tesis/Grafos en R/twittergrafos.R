library("twitteR")
library("igraph")
library("base64enc")
consumer_key <- "HzxyBssq3St8QVWNqEgcHO9KY"
consumer_secret <- "1e160TUCoB9rzbdIrfLvEYZHFV8kU96klPLtJ3o83Ej3ovIeHB"
access_token <- "371289232-ujkY6oV7IHjG1LmBZ4Qz1G48Q4JHLnfZ6r0waLeX"
access_secret <- "Q40XUryKBAY0Zx21CQ1dy014Qd281lUNtsp6YFn5PXIUH"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)



start <- getUser("andresz_s")


friends.object<-lookupUsers(start$getFriendIDs())
followers.object<-lookupUsers(start$getFollowerIDs())

# Retrieve the names of your friends and followers from the friend
# and follower objects. You can limit the number of friends and followers by adjusting the 
# size of the selected data with [1:n], where n is the number of followers/friends 
# that you want to visualize. If you do not put in the expression the maximum number of 
# friends and/or followers will be visualized.

n<-200
friends <- sapply(friends.object[1:n],name)
followers <- sapply(followers.object[1:n],name)

# Create a data frame that relates friends and followers to you for expression in the graph
relations <- merge(data.frame(User='andresz_s', Follower=friends), 
                   data.frame(User=followers, Follower='eric_bellet'), all=T)

# Create graph from relations.
g <- graph.data.frame(relations, directed = T)

# Assign labels to the graph (=people's names)
V(g)$label <- V(g)$name

# Plot the graph using plot() or tkplot(). Remember the HINT at the 
# beginning if you are using MAC OS/X
tkplot(g)
rglplot(g)





twitterGraph = function (username, password, userToPlot)
{
  sess <- start
  friends.object <- userFriends(userToPlot,n=20, sess)
  followers.object <- userFollowers(userToPlot,n=20, sess)
  friends <- sapply(friends.object,name)
  followers <- sapply(followers.object,name)
  
  relations <- merge(data.frame(User=userToPlot, Follower=friends), 
                     data.frame(User=followers,  Follower=userToPlot), 
                     all=T)
  
  g <- graph.data.frame(relations, directed = T)
  V(g)$label <- V(g)$name
  g
}

plot(g)
g= twitterGraph ('YOUR_TWITTER_USERNAME','PASSWORD','USER_TO_PLOT')