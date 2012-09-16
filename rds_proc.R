
#
# version:       0.2.0
# last modified: 08.08.12
#


#
# "rds_proc" - An application to process Respondent Driven
#              Sampling (RDS) data
#
# in particular ...
#   a) to validate RDS data (format validation)
#   b) to generate a graph in igraph format
#   c) to generate a graph in graphNEL format
#   d) to generate a layered graph (Sugiyama layout)
#
#


#--

# R packages
require(igraph)
require(Rgraphviz)


#--

#-----------------------------------------------------------------------#
#
#                     ++ to be edited by the user ++
#

# working directory
working_dir<- "..."
# name of the input file (RDS data)
file_name<- "data_120724-02.rds"
# number of columns (RDS data excluding the headers)
no_cols<- 9


#-----------------------------------------------------------------------#


findIdx1<- function(x, ds) which(ds==x, arr.ind=TRUE)


# function to generate a graph in igraph format
generateGraph_IGRAPH = function(rds_data, vert_pairs) {

  edges<- data.frame(rds_data[vert_pairs[,1],1], rds_data[vert_pairs[,2],1])
  
  # generating the graph
  graph<- graph.data.frame(edges)
  V(graph)$label<- lapply(V(graph)$name,
                          function(x) paste(strwrap(x, 12), collapse="\n"))
						  
  return(graph)
  
}


# function to get the vertex pairs of the graph;
# note that ...
#   - a participant can be either a seed or a respondent;
#     for a seed, the respondent ID is equal to -1;
#   - each vertex corresponds to a participant (e.g. vertex #1
#     corresponds to the seed #1 in the sorted list)
#   - each partecipant can be indentified either by the position
#     in the sorted list (row number) or by the respondent ID
#     (value in the first column of the RDS data set) 
# 
getVertexPairs = function(id_cfromrecr, id_ctoresp) {

  id_recr_t<- apply(id_cfromrecr, 1, findIdx1, ds=id_ctoresp)
  id_recr<- sapply(id_recr_t, "[", 1) 
  id_na<- which(is.na(id_recr))
  
  # vertex IDs (positions in the sorted list, row numbers)
  vert_pairs<- array(0, dim=c(length(id_recr)-length(id_na), 2))
  vert_pairs[,1]<- id_recr[-id_na]
  vert_pairs[,2]<- c((length(id_na)+1):length(id_recr))

  return(vert_pairs)

}


#--

# I/O processing - reading input
rds_data_h<- scan(file=paste(working_dir, file_name, sep=""),
                  what="character", nlines=2)

# sample size
size_s<- as.numeric(rds_data_h[2])
# maximum number of coupons given to each respondent
no_c<- as.numeric(rds_data_h[3])
# symbol for the missing values
flag_mv<- as.numeric(rds_data_h[4])

rds_data<- scan(file=paste(working_dir, file_name, sep=""),
                what="character", skip=2)
 
				
# --

# data validation
if(length(rds_data)!=(size_s*no_cols)) {
  cat("\n> validation error - check sample size and/or number of columns\n")
}

rds_data<- matrix(rds_data, nrow=size_s, ncol=no_cols, byrow=TRUE)
print(rds_data)

# sorting by the 3rd column (ID of the coupon received
# from recruiter), in ascending order 
rds_data<- rds_data[order(as.numeric(rds_data[,3])),]

# respondent IDs
id_resp<- rds_data[,1]

# self-reported network sizes
size_n<- as.numeric(rds_data[,2])

# IDs of the coupons received from recruiters
id_cfromrecr<- as.array(as.numeric(rds_data[,3]))

# IDs of the coupons given to respondents
id_ctoresp<- rds_data[,4:(3+no_c)]
id_ctoresp<- apply(id_ctoresp, 2, as.numeric)


#--

#
# graph generation
#

# vertex pairs of the graph
vert_pairs<- getVertexPairs(id_cfromrecr, id_ctoresp)
print(vert_pairs)

# generating a graph in igraph format
graph_01<- generateGraph_IGRAPH(rds_data, vert_pairs)

# generating a Sugiyama layout
layouts<- layout.sugiyama(graph_01)

# I/O processing
png("rds_tree_01.png")
plot(layouts$extd_graph,
     vertex.size=10,
	 vertex.label.cex=0.7,
     edge.arrow.size=0.5,
	 margin=c(0, 0, 0, 0))
dev.off()

# generating a graph in graphNEL format
graph_02<- igraph.to.graphNEL(graph_01)

# I/O processing
png("rds_tree_02.png")
plot(graph_02, attrs=list(node=list(shape="circle", height=0.1, width=0.1),
                          edge=list(arrowsize=0.5)))
dev.off()



#--

# memory management, releasing memory
rm(findIdx1, getVertexPairs, generateGraph_IGRAPH,
   id_cfromrecr, id_ctoresp,
   flag_mv,
   envir=globalenv())
gc()


#--

#
# -- References --
# http://www.respondentdrivensampling.org/
# http://igraph.sourceforge.net/
# http://igraph.sourceforge.net/doc/R/layout.sugiyama.html
# http://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html
#


# 
# -- `Rgraphviz' (installation) --
#
# To install the `Rgraphviz' package (`Bioconductor' project), follow these steps:
#
# 1) install `graphviz'
#    (see http://www.bioconductor.org/packages/release/bioc/readmes/Rgraphviz/README)
#
# 2) follow the standard procedure to install the `Bioconductor' packages
#    (see http://www.bioconductor.org/install/)
#
#
