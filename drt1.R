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
                           Out = rep(n_input + 1, n_input),
                           Weight = runif(n_input),
                           Marker = (1:n_input),
                           Disabled = rep("N",n_input)
                             )
  connect_df$Disabled <- as.character(connect_df$Disabled)
  
  return(list(nodes = node_df,connects = connect_df))
  
}

test <- my_init(2)



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
  
  # random select a node with "in" capacity
  in_sum <- as.data.frame(table(connect_df$In))
  colnames(in_sum) <- c("node_id","current_in")
  in_sum$node_id <- as.numeric(as.character(in_sum$node_id))
  in_sum$max_in <- length(node_df$node_id[node_df$node_type != "sensor"])
  tem_index <- in_sum$node_id %in% node_df$node_id[node_df$node_type =="hidden"]
  in_sum$max_in[tem_index] <- in_sum$max_in[tem_index] - 1
  in_node_id <- sample(in_sum$node_id[in_sum$current_in < in_sum$max_in ], 1 )
  
  # random sample a node with "in" capacity and not yet connnected with the randomly selected in_node_id
  sample_pop <-  connect_df$Out[!connect_df$Out %in%   unique(connect_df$Out[connect_df$In == in_node_id])]
  if(length(sample_pop) == 1){
    out_node_id <- sample_pop
  }else{
    out_node_id <- sample(   sample_pop   ,1)
  }
 
  
  
  # add connection
  innovation_name = paste0("nc",in_node_id,"-" ,out_node_id)  # new connection
  inno_index <- which(mutation_tracking$name == innovation_name)
  
  if(length(inno_index) ==0){
    
    new_marker <- max_marker + 1
    max_marker <- new_marker
    
    new_innovation <- data.frame(
      name = innovation_name, new_node_id = NA,
      new_marker1 = new_marker, In1= in_node_id, Out1 = out_node_id,
      new_marker2 = NA, In2= NA, Out2 = NA
    )
    
    ## update mutation_tracker
    mutation_tracking <- rbind(mutation_tracking,new_innovation )
    
    ## update connect genome 
    new_connect <- data.frame(In = in_node_id, Out = out_node_id, Weight = 1, Marker = new_marker, Disabled = "N")
    connect_df <- rbind(connect_df, new_connect)
    
  }else{
    
    ## only update connect genome 
    new_connect <- data.frame(In = in_node_id, Out = out_node_id, Weight = 1, Marker = new_marker, Disabled = "N")
    connect_df <- rbind(connect_df, new_connect)
    
  }
  return(list(node_df=node_df,
              connect_df = connect_df, 
              mutation_tracking =mutation_tracking ,
              max_node = max_node, 
              max_marker = max_marker 
              ))
  
    
}else{
  # add node
  index <- sample(connect_df$Marker[connect_df$Disabled=='N'] , 1)
  index <- which(connect_df$Marker == index & connect_df$Disabled=='N')
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
  
  
  return(
          list(node_df=node_df,
              connect_df = connect_df, 
              mutation_tracking =mutation_tracking ,
              max_node = max_node, 
              max_marker = max_marker)
         )
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


nn_plot <- function(connect_df){
  
  node_list <- data.frame(node_id= unique(c(connect_df$In,connect_df$Out)))
  in_summary <- as.data.frame(table(connect_df$In))
  colnames(in_summary) <- c("node_id","in_sum")
  out_summary <- as.data.frame(table(connect_df$Out))
  colnames(out_summary) <- c("node_id","out_sum")
  
  node_list <- merge(node_list, in_summary, all.x = T )
  node_list <- merge(node_list, out_summary, all.x = T)
  node_list[is.na(node_list)] <- 0
  
  node_list$level <-  node_list$out_sum - node_list$in_sum 
  node_list$level[node_list$in_sum == 0] <- max(node_list$level) + 1
  node_list$level[node_list$out_sum == 0] <- min(node_list$level) - 1
  node_list$level <- rank(node_list$level, ties.method = "min")
  node_list$level <-as.numeric(as.factor(node_list$level ))
  
  level_df <- as.data.frame(table(node_list$level))
  colnames(level_df) <- c("level_id", "cnt")
  level_df$level_id <- as.numeric(level_df$level_id)
  
  x_lim <- prod(level_df$cnt + 1)
  y_lim <- nrow(level_df) * 10 + 10
  
  node_list$x <- NA
  node_list$y <- NA
  
  for(i in level_df$level_id){
    cnt_i <- level_df$cnt[i]
    x_pos <- (1:cnt_i)*(x_lim/(cnt_i + 1))
    y_pos <- i * 10
    node_list$x[node_list$level == i] <- x_pos
    node_list$y[node_list$level == i] <- y_pos
  }
  
  plot(node_list$x,node_list$y,cex = 4, axes=FALSE, frame.plot = F, ann=FALSE)
  text(y ~x, labels=node_id,data=node_list, cex=0.9, font=2)
  
  
  
  for(i in 1:nrow(connect_df)){
    
    if(connect_df$Disabled[i] == 'N'){
      x <- c(node_list$x[connect_df$In[i]] , node_list$x[connect_df$Out[i]] )
      y <- c(node_list$y[connect_df$In[i]] , node_list$y[connect_df$Out[i]] - 1 )
      lines(x,y,col= i)
    }
    
  }
  
  
  
}



  