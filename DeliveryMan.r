# Install the package
install.packages("C://DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
#?testDM

Manhattan_Distance = function(x1,y1,x2,y2){
  dist = abs(x1-x2)+abs(y1-y2)
  return (dist)
}

FindClosestPackage = function(carInfo, packageMatrix){
    unpicked = which(packageMatrix[,5] == 0)
    if(length(unpicked)==0){
      print("All packages are picked")
      return(0)
    }
    else {
      nearestPackage=unpicked[1]
      minDistance=Manhattan_Distance(carInfo$x,carInfo$y,packageMatrix[nearestPackage,1],packageMatrix[nearestPackage,2])
      if(length(unpicked)>1){
        for (i in 2:length(unpicked)) {
          dist=Manhattan_Distance(carInfo$x,carInfo$y,packageMatrix[unpicked[i],1],packageMatrix[unpicked[i],2])
          if(dist < minDistance){
            minDistance=dist
            nearestPackage=unpicked[i]
          }
        }
      }
      # print(packageMatrix) 
      # print(nearestPackage)
      return (nearestPackage)
    }
}



myFunction = function(trafficMatrix, carInfo, packageMatrix) {
  unpicked = which(packageMatrix[,5] == 0)
  for (i in 1:length(unpicked)) {
    nearestPackage=FindClosestPackage(carInfo,packageMatrix)
    packageMatrix[nearestPackage,5]=1
  }
  # if(carInfo$load == 0) {
  #     carInfo$mem$goal <- nextPickup(trafficMatrix, 
  #                                    carInfo, 
  #                                    packageMatrix)
  #   } else {
  #     carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  # }
  # 
  # # How do we get there?
  # carInfo$nextMove <- nextMove(trafficMatrix,
  #                              carInfo,
  #                              packageMatrix)
  # return(carInfo)
}

# Find the nearest pickup location for an undelivered package
# nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
#   distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
#   distanceVector[packageMatrix[,5] != 0] = Inf
#   return(packageMatrix[which.min(distanceVector), c(1,2)])
# }
# 
# 
# # Find the move to get to carInfo$mem$goal
# nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
#   if(carInfo$x < carInfo$mem$goal[1]) {
#     return(6)
#   } else if (carInfo$x > carInfo$mem$goal[1]) {
#     return(4)
#   } else if (carInfo$y < carInfo$mem$goal[2]) {
#     return(8)
#   } else if (carInfo$y > carInfo$mem$goal[2]) {
#     return(2)
#   } else {
#     return(5)
#   }
# }

