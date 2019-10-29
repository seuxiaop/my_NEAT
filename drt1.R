## INITILIZATION ##


my_act_fun <- function(x){
  return(1/(1+exp(-4.9*x)))
}

my_init <- function(n_input = 2){
  
  node_df <- data.frame(
                        node_id = 1:(n_input+1),
                        node_type = c(rep("sensor",n_input),"output")
                       )
  connect_df <- data.frame(In = (1:n_input), 
                           Out = rep(n_input + 1， n_input),
                           Weight = runif(n_input),
                           Marker = (1:n_input),
                           Disabled = rep("N",n_input)
                             )
  connect_df$Disabled <- as.character(connect_df$Disabled)
  
  return(list(nodes = node_df,connects = connect_df))
  
}

test <- my_init(4)



## mutation operator ##
mutation_tracking <- data.frame(
                                name = NULL, new_node_id = NULL,
                                new_marker1 = NULL, In1= NULL, Out1 = NULL,
                                new_marker2 = NULL, In2= NULL, Out2 = NULL
                                )
max_node <- 5
max_marker <- 4

my_mute <- function(node_df, connect_df, mutation_tracking,max_node, max_marker, type = 1){ # 1= add connection, 2= add node, 3= weight change
  node_df = test$nodes
  connect_df = test$connects
if(type == 1){
  # add connection
  
  node_in <- names(table($node_id[node_df$node_type != "output"]) <= nrow(node_df) )
  
  
  
  
}else{
  # add node
  index <- sample(nrow(connect_df) , 1)
  connect_df[index, "Disabled"] <- "Y"
  innovation_name = paste0("nn",connect_df$In[index],"-" ,connect_df$Out[index])
  inno_index <- which(mutation_tracking$name == innovation_name)
  if(length(inno_index) ==0 ){
    
    new_innovation <- data.frame(
                                 name = innovation_name, new_node_id = NA,
                                 new_marker1 = NA, In1= NA, Out1 = NA,
                                 new_marker2 = NA, In2= NA, Out2 = NA
                                 )
    
    ## add node
    new_node <- data.frame(node_id = max_node + 1, node_type = "hidden")
    new_innovation$new_node_id <- new_node$node_id
    
    ## update node genome list
    node_df <- rbind(node_df, new_node)
    
    ##update max_node traker
    max_node <- max_node + 1
    
    ## add connection
    new_marker <- max_marker + 1
    max_marker <- new_marker
    new_connect1 <- data.frame(In = connect_df$In[index], Out = new_node$node_id, Weight = 1, Marker = new_marker, Disabled = "N")
    
    new_innovation$new_marker1 <- new_marker
    new_innovation$In1 <-  connect_df$In[index]
    new_innovation$Out1 <- new_node$node_id
    
    
    new_marker <- max_marker + 1
    max_marker <- new_marker
    new_connect2 <- data.frame(In = new_node$node_id, Out = connect_df$Out[index], Weight = 1, Marker = new_marker, Disabled = "N")
    
    new_innovation$new_marker2 <- new_marker
    new_innovation$In2 <-   new_node$node_id
    new_innovation$Out2 <- connect_df$Out[index]
    
    ## update connection genome
    connect_df <- rbind(connect_df, new_connect1, new_connect2)
    
    ## update max_marker traker
    max_marker <- new_marker
    
    ## update mutation_tracker
    mutation_tracking <- rbind(mutation_tracking,new_innovation )
    
    
  }else{
    ## add node
    new_node <- data.frame(node_id = mutation_tracking$new_node_id[inno_index], node_type = "hidden")
    node_df <- rbind(node_df, new_node)
    ## add connection
    new_connect1 <- data.frame(
                               In = mutation_tracking$In1[inno_index],
                               Out = mutation_tracking$Out1[inno_index], 
                               Weight = 1, 
                               Marker = mutation_tracking$new_marker1[inno_index], 
                               Disabled = "N"
                              )
    
    new_connect2 <- data.frame(
                                In = mutation_tracking$In2[inno_index],
                                Out = mutation_tracking$Out2[inno_index], 
                                Weight = 1, 
                                Marker = mutation_tracking$new_marker2[inno_index], 
                                Disabled = "N"
                              )
    ## update connction genome
    connect_df <- rbind(connect_df, new_connect1, new_connect2)
}
  

}
  
    
} 
  