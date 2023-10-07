#############################################################################
#                                                                           #
# Calculate cosine distance between two terms                               #
# Script written by Afonso Xavier Canosa Rodrigues                          #
# Example data from                                                         #
# Baroni, M., Bernardi, R., & Zamparelli, R. (2014).                        #
# Frege in space: A program of compositional distributional semantics.      # 
# LiLT (Linguistic Issues in Language Technology), 9.                       #
#                                                                           #
#############################################################################

library(CircStats)

occur<-function(occur1){
  pairs_t<-length(occur1)/2
  angles<-c(1:pairs_t)
  for (i in 1:pairs_t){
    den<-i*2
    den2<-den-1
  angles[i]<-atan(occur1[den]/occur1[den2])   # bring angle in radians
    print(deg(angles[i]))
  }
  return (angles)
}

# Data from example 
runs<-c(1,1,4)
barks<-c(5,2,0)
pairs<-length(runs)
angles2<-c(1:length(runs))

for (i in 1:pairs){
  angles2[i]<-atan(barks[i]/runs[i])   # bring angle in radians
  }

# cosine distance from term a to b and so on #
dif1_2<-cos(abs(ang[1]-ang[2]))
dif1_3<-cos(abs(ang[1]-ang[3]))
dif2_3<-cos(abs(ang[2]-ang[3]))

print(dif1_2)
print(dif1_3)
print(dif2_3)
