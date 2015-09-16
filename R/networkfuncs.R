#' Generate a twitter network
#' @description Generate a twitter network from a stored twitter database created by tweetDB
#' @param db the name of the database that holds all the tweets, it should include the full path
#' @param edgeType should be one of 'reply','retweet','favorite','mention' see details
#' @param findGroups if TRUE it will add groups for plotting.
#' @param simp TRUE or FALSE should the network be simplified?  If TRUE non-connected nodes are deleted and loops and mulitple edges are deleted
#' @details the edge type denotes how to connect users.
#' @return an igraph directed graph object
#' @import igraph stringr
#' @export


create_graph <- function(tweetDF,edgeType="retweet",findGroups = T,simp = T){
 allSN <- unique(tweetDF$screen_name)
  ## Create graph
  g <- graph.empty()
  g <- g + vertices(allSN)


  if(edgeType=="retweet"){
  for(i in 1:dim(tweetDF)[1]){
    if(grepl(pattern = 'RT @[a-zA-Z0-9_]{1,15}',x = tweetDF$tweet_text[i])){
      rt_name <- regmatches(tweetDF$tweet_text[i],regexpr('RT @[a-zA-Z0-9_]{1,15}',tweetDF$tweet_text[i]))
      rt_name <-  unlist(str_split(rt_name,"@"))[2]
      tryCatch({g <- g+ edges(c(tweetDF$screen_name[i],rt_name))},
      error = function(err) {
        err
      }
      )
    }
  }
}

  if(edgeType=="mention" || edgeType=="all"){
    for(i in 1:dim(tweetDF)[1]){
        ifelse(edgeType=="all",spttxt <- tweetDF$tweet_text[i],spttxt <- unlist(str_split(tweetDF$tweet_text[i],'RT @[a-zA-Z0-9_]{1,15}')) )
        txt <- spttxt[length(spttxt)]
        rt_name <- mention_ext(txt)
        edge_vec <- gsub(pattern = "@",replacement = paste(tweetDF$screen_name[i],",",sep=""),x = rt_name)
        edge_vec <- unlist(str_split(edge_vec,","))

        tryCatch({g <- g+ edges(edge_vec)},
                 error = function(err) {
                   err
                 }
        )
      }
  }

  if(simp){
    g <- delete.vertices(g, degree(g) == 0)
    g <- simplify(g)
  }

  if(findGroups){
    V(g)$color <- cluster_walktrap(g)$membership
  }

  return(g)
  }


#' Extract mentions
#' @description extract all the mentions of other twitter users from a tweet
#' @param txt the text of your tweet
#' @export

mention_ext <- function(txt){
  n <- vector()
  while(!is.na(txt)){
    n <- c(n,regmatches(txt,regexpr('@[a-zA-Z0-9_]{1,15}',txt)))
    txt <- unlist(regmatches(txt,regexpr('@[a-zA-Z0-9_]{1,15}',txt),invert=T))[2]

  }
  return(n)
}

#' Subset tweets
#' @description This will take user inputs and retrieve the data
#' @param db the name of the database that holds all the tweets, it should include the full path
#' @param tweetMin minimum number of tweets a user id should have to be included in the network
#' @param dt the date you want data for, could be 'all' if you want all days
#' @import RSQLite
#' @export

tweet_subset <- function(db,tweetMin=0,dt="all"){
conn <- dbConnect(SQLite(),db)

if(dt == "all"){
  q <- "SELECT screen_name, count(*) as tc from tweets group by screen_name order by tc desc"
} else {
q <- paste("SELECT screen_name, count(*) as tc from tweets
           WHERE rptg_dt = '",dt,"' group by screen_name order by tc desc",sep="")
}

sn <- dbGetQuery(conn,q)
## String it down to the tweet minimum
sn <- sn[sn$tc > tweetMin,]

if(dt == "all"){
  q <- "SELECT id,screen_name,tweet_text,in_reply_to_user_id from tweets"
} else {
  q <- paste("SELECT id,screen_name,tweet_text,in_reply_to_user_id from tweets
             WHERE rptg_dt = '",dt,"'",sep="")
}
all_tweets <- dbGetQuery(conn,q)

all_tweets <- all_tweets[grepl(paste(sn$screen_name,collapse="|"),all_tweets$screen_name),]

return(all_tweets)
}

#' tweets from API
#' @description Grab tweets from the API
#' @param tweetMin minimum number of tweets a user id should have to be included in the network
#' @param dt the date you want data for, could be 'all' if you want all days
#' @import dplyr httr
#' @export


tweet_from_api <- function(tweetMin,dt){

  baseURL <- paste("http://emhart.info/tweets/ESA/2015?date=",dt,sep="")
  dat <- GET(baseURL)
  tdf <- content(dat)

  tweetDF <- data.frame(do.call(rbind, lapply(tdf,function(x){unlist(lapply(x,function(x){ifelse(is.null(x),"None",x)}))})  ),stringsAsFactors = F)

  snc <- tweetDF %>% group_by(screen_name) %>% summarise(sncount = length(screen_name)) %>% filter(sncount > tweetMin)

  all_tweets <- left_join(snc,tweetDF)
  return(all_tweets)
}

#' Return D3 direct Graph list
#' @description This function will take an igraph object and return a list that can be plotted as a D3network graph
#' @param g a graph object that you want to plot
#' @return a list with numerical graph of links and a node list with names and groups
#' @import igraph
#' @export

convertD3 <- function(g){
d3g <- get.data.frame(g)

n  <- data.frame(cbind(sort(unique(unlist(d3g)))),stringsAsFactors = F)
n$n <- as.numeric(as.factor(n[,]))
colnames(n) <- c("name","id")
gm <- cluster_walktrap(g)
group_mem <- membership(gm)[order(names(membership(gm)))]
n$group <- group_mem[grepl(paste(n$name,collapse="|"),names(group_mem))]
for(i in 1:dim(n)[1]){
  d3g$from[d3g$from == n$name[i]] <- as.numeric(n$id[i]) - 1
  d3g$to[d3g$to == n$name[i]] <- as.numeric(n$id[i]) - 1

}
d3g$from <- as.numeric(d3g$from)
d3g$to <- as.numeric(d3g$to)

return(list(d3g = d3g,node=n))

}
