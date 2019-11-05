## INITILIZATION ##
rm(list=ls())
source("add_new_connection.R")
source("add_new_node.R")
source("nn_init.R")
source("nn_plot.R")
source("get_node_info.R")
source("weight_mutation.R")
source("nn_mutation.R")

nn_test <- nn_init(2)


## mutation operator ##
mutation_tracking <- data.frame(
  name = NULL, new_node_id = NULL,
  new_marker1 = NULL, In1= NULL, Out1 = NULL,
  new_marker2 = NULL, In2= NULL, Out2 = NULL
)
max_node <- 3
max_marker <- 2

##
nn1 <- nn_mutation(
  nn = nn_test,
  mutation_tracking = mutation_tracking,
  max_node = max_node,
  max_marker= max_marker, 
  type = 2
)

mutation_tracking = nn1$mutation_tracking
max_node = nn1$max_node
max_marker= nn1$max_marker
nn1 <- nn1$nn


nn1 <- nn_mutation(
  nn = nn1,
  mutation_tracking = mutation_tracking,
  max_node = max_node,
  max_marker= max_marker, 
  type = 1
)
mutation_tracking = nn1$mutation_tracking
max_node = nn1$max_node
max_marker= nn1$max_marker
nn1 <- nn1$nn

nn1
nn_plot(nn1$connect_df)

### construct nn2

nn2 <- nn_mutation(
  nn = nn_test,
  mutation_tracking = mutation_tracking,
  max_node = max_node,
  max_marker= max_marker, 
  type = 2
)

mutation_tracking = nn2$mutation_tracking
max_node = nn2$max_node
max_marker= nn2$max_marker
nn2 <- nn2$nn
nn_plot(nn2$connect_df)

nn2 <- nn_mutation(
  nn = nn2,
  mutation_tracking = mutation_tracking,
  max_node = max_node,
  max_marker= max_marker, 
  type = 1
)

mutation_tracking_bk <- mutation_tracking
max_node_bk <- max_node
max_marker_bk <- max_node

mutation_tracking = nn2$mutation_tracking
max_node = nn2$max_node
max_marker= nn2$max_marker

nn2 <- nn2$nn
nn_plot(nn2$connect_df)



