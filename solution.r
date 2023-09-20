#this is assignment one solution code

# install.packages("~/uppsala/ai/solution/DeliveryMan_1.2.0.tar.gz", repos = NULL, type = "source")
# library("DeliveryMan")

#int, int*int -> (int,int)
#get package pickup point coords
packageSource <- function(package_num, packages){
    return (c(packages[package_num, 1], packages[package_num, 2]))  
}

#int, int*int -> (int,int)
#get package destination coords
packageDest <- function(package_num, packages){
  return (c(packages[package_num, 3], packages[package_num, 4])) 
}

#(int, int), (int, int) -> int
#gets direction to move toward destination node
getDirection <- function(curr, dest){
  if (curr$x<dest$x) {nextMove=6}
  else if (curr$x>dest$x) {nextMove=4}
  else if (curr$y<dest$x) {nextMove=8}
  else if (curr$y>dest$x) {nextMove=2}
  else {nextMove=5}
  return (nextMove)
}

#manhattan distance function
manhattan <- function(point1, point2) {
  return(abs(point1[1] - point2[1]) + abs(point1[2] - point2[2]))
}

#DM USED FOR TESTING A STAR
solDM=function(roads,car,packages) {
#   print(roads)
#   print(car)
print(roads)
  print("car coords:")
  print(car$x)
  print(car$y)
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) { #if have no package offset stays 0
    toGo=which(packages[,5]==0)[1]
    print("picking up package")
    print(toGo)
  } else { #if have a package offset is 2
    toGo=car$load
    print("delivering package")
    print(toGo)
    offset=2
   print(packageDest(toGo, packages))
   print(astarAlg(roads$hroads,roads$vroads,packageSource(toGo, packages),packageDest(toGo, packages)))
  }

  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove

  car$mem=list()
  return (car)
}

#astar pathfind for two points, start and end
astarAlg <- function(hroads, vroads, start, end) {
  nrow <- ncol(hroads)
  ncol <- nrow(vroads)
  
  #initializations
  open_list <- list()
  closed_list <- list()
  g_values <- matrix(Inf, nrow = nrow, ncol = ncol)
  f_values <- matrix(Inf, nrow = nrow, ncol = ncol)
  parent <- list()
  
  #add start node to open_list
  open_list[[1]] <- start
  g_values[start[1],start[2]] <- 0
  f_values[start[1],start[2]] <- manhattan(start, end)
  
  #while open_list is not empty
  while (length(open_list) > 0) {
    # find node with lowest f val
    current <- open_list[[which.min(sapply(open_list, function(cell) f_values[cell[1],cell[2]]))]]
    print("start")
    print(start)
    print(end)
    print("current")
    print(current)

    # Check if equals end node
    if (current[1] == end[1] && current[2] == end[2]) {
      print("end?")
      print(start)
      print(end)
      return(parent) #return path
    }
    
    # move current from open_list to closed_list
    open_list <- open_list[-which(names(open_list) == current)]
    closed_list[[length(closed_list)+1]] <- current
    
    #initialize neighbors as empty
    neighbors <- list()
    
    if (current[1] > 1) {neighbors[[length(neighbors)+1]] <- c(current[1] - 1, current[2])} #left 
    if (current[1] < nrow) {neighbors[[length(neighbors)+1]] <- c(current[1] + 1, current[2])} #right
    if (current[2] > 1) {neighbors[[length(neighbors)+1]] <- c(current[1], current[2] - 1)} #down
    if (current[2] < ncol) {neighbors[[length(neighbors)+1]] <- c(current[1], current[2] + 1)} #up
    
    #iterate through neighbours
    #TODO: make sure hroads and vroads are calibrated
    for (neighbor in neighbors) {
      print(neighbor)
      #if neighbor not in closed_list
      if (!(any(closed_list %in% list(neighbor)))) {
        print("neigbor not in closed list")
        if (neighbor[1] == current[1]) { #horizontal road
          tentative_g <- g_values[current[1], current[2]] + hroads[(neighbor[1]), neighbor[2]]
          # print(hroads)
          # print(hroads[neighbor[1], neighbor[2]])
        } else { #vertical road
          tentative_g <- g_values[current[1], current[2]] + vroads[neighbor[1], (neighbor[2])]
          # print(vroads)
          # print(vroads[neighbor[1],neighbor[2]])
        }
        print(tentative_g)
        
        #if neighbor not in the open list or has lower g (update g then)
        if (!(any(open_list %in% list(neighbor))) || (tentative_g < g_values[neighbor[1], neighbor[2]])) {
          "neighbor not in open list or g val is lower"
          parent[[length(parent)+1]] <- current
          g_values[neighbor[1],neighbor[2]] <- tentative_g
          f_values[neighbor[1], neighbor[2]] <- g_values[neighbor[1], neighbor[2]] + manhattan(neighbor, end)
          
          #if neighbor is not in the open list then add
          if (!(any(open_list %in% list(neighbor)))) {
            open_list[[length(open_list)+1]] <- neighbor
          }
        }
      }
    }
  }
  
  #no path found
  return(NULL)
}



runDeliveryMan ( carReady = solDM , doPlot = TRUE )