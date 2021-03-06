% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/networkfuncs.R
\name{create_graph}
\alias{create_graph}
\title{Generate a twitter network}
\usage{
create_graph(tweetDF, edgeType = "retweet", findGroups = T, simp = T)
}
\arguments{
\item{edgeType}{should be one of 'reply','retweet','favorite','mention' see details}

\item{findGroups}{if TRUE it will add groups for plotting.}

\item{simp}{TRUE or FALSE should the network be simplified?  If TRUE non-connected nodes are deleted and loops and mulitple edges are deleted}

\item{db}{the name of the database that holds all the tweets, it should include the full path}
}
\value{
an igraph directed graph object
}
\description{
Generate a twitter network from a stored twitter database created by tweetDB
}
\details{
the edge type denotes how to connect users.
}

