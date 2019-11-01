## INITILIZATION ##
source("add_new_connection.R")
source("add_new_node.R")
source("nn_init.R")
source("nn_plot.R")
test <- nn_init(2)


## mutation operator ##
mutation_tracking <- data.frame(
  name = NULL, new_node_id = NULL,
  new_marker1 = NULL, In1= NULL, Out1 = NULL,
  new_marker2 = NULL, In2= NULL, Out2 = NULL
)
max_node <- 3
max_marker <- 2

my_mute <- function(node_df, connect_df, mutation_tracking,max_node, max_marker, type = 1){ # 1= add connection, 2= add node, 3= weight change
  
  #  node_df = test$nodes
  #  connect_df = test$connects
  if(type == 1){
    
    return(add_new_connection(node_df, connect_df, mutation_tracking, max_marker))
    
  }else if(type == 2){
    return(add_new_node(node_df, connect_df, max_node, mutation_tracking, max_marker))
  }else{
    return(0)
    
  }
  
  
} 



test_m <- my_mute(
  node_df = test$nodes, 
  connect_df = test$connects ,
  mutation_tracking,
  max_node,
  max_marker, 
  type = 2
)


test_m <- my_mute(
  node_df = test_m$node_df, 
  connect_df = test_m$connect_df ,
  mutation_tracking = test_m$mutation_tracking,
  max_node = test_m$max_node,
  max_marker= test_m$max_marker, 
  type = 1
)
test_m
nn_plot(test_m$connect_df)


