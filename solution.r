#this is assignment one solution code

initializeNodes <- function(curPackage)
{
  rows <- list()
  packx <- curPackage$x
  packy <- curPackage$y
  for (y in 1:10)
  {
    row = list()
    for (x in 1:10)
    {
      row[[x]] <- list(x = x, y = y, f = 0, g = 0, h = manhattanDistance(x, y, packx, packy), parent = NULL)
      #print(row)
    }
    rows[[y]] <- row
  }
  return (rows)
}

getNeighbors <- function(car, nodes)
{
  neighbors <- list()
  x <- car$x
  y <- car$y
  if (x-1 >= 1) neighbors[[length(neighbors)+1]] <- nodes[[y]][[x-1]]
  if (x+1 <= 10) neighbors[[length(neighbors)+1]] <- nodes[[y]][[x+1]]
  if (y-1 >= 1) neighbors[[length(neighbors)+1]] <- nodes[[y-1]][[x]]
  if (y+1 <= 10) neighbors[[length(neighbors)+1]] <- nodes[[y+1]][[x]]
  return (neighbors)
}

manhattanDistance <- function(x1, y1, x2, y2)
{
  return (abs(x2-x1) + abs(y2 - y1))
}

calculateNeighborsCost <- function(node, nodes, roads)
{
  x = node$x
  y = node$y
  hroads = roads$hroads
  vroads = roads$vroads
  neighbors = getNeighbors(node, nodes)
  
  for (neighbor in neighbors) {
    if (!is.null(neighbor))
    {
      dx <- neighbor$x - x
      dy <- neighbor$y - y
      # Move horizontally so get the horizontal cost
      if (abs(dx) == 1)
      {
        hcost <- hroads[x + dx-1, y]
        neighbor$g <- node$g + hcost
      }
      # Move vertically so get the vertical cost
      else if (abs(dy) == 1)
      {
        vcost <- vroads[x, y + dy-1]
        neighbor$g <- node$g + vcost
      }
      neighbor$f <- neighbor$g + neighbor$h
      nodes[[neighbor$y]][[neighbor$x]] <- neighbor
    }
  }
  return (getNeighbors(node, nodes))
}

getBestNode <- function(frontier, goal)
{
  bestNode <- NULL
  bestF <- Inf
  for (node in frontier)
  {
    if (length(node$f) == 0)
    {
      node$f = 0
    }
    if (node$f < bestF)
    {
      bestF <- node$f
      bestNode <- node
    }
    if (node$x == goal$x && node$y == goal$y)
    {
      bestNode <- goal
      break
    }
  }
  return (bestNode)
}

test <- function(roads, car, packages)
{
  # Testing
  nodes <- initializeNodes(list(x = 5, y = 7))
  cx = car$x
  cy = car$y
  curNode <- nodes[[cy]][[cx]]
  neighbors <- calculateNeighborsCost(curNode, nodes, roads)
  bestNode <- getBestNode(neighbors, list(x=5, y=7))
  neighbors <- calculateNeighborsCost(bestNode, nodes, roads)
  bestNode <- getBestNode(neighbors, list(x=5, y=7))
  print(bestNode)
  return (car)
}

