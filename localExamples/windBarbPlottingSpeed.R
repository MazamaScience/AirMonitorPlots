# How does the speed of plotting wind barbs compare to that of plotting icons?
# How long would it take to plot many wind barbs?
# Results: It takes about 5 seconds to plot 10000 wind barbs, which is significantly slower
# than just plotting circles but it is within a reasonable speed. The speed is about the same
# when plotting 100 vs 10000 barbs (as in it takes 100 times longer to plot 10000 barbs than 100 barbs.)
library(rbenchmark)
library(PWFSLSmokePlots)

# 100
# get a random set of lat, lon, speed, dir
longitude <- runif(100, -123, -113)
latitude <- runif(100, 33, 43)
speed <- runif(100, 0, 100)
dir <- runif(100, 0, 360)

maps::map('state', xlim = c(-123, -113), ylim = c(33, 43))
benchmark(
  points(longitude, latitude)
)
#                          test replications elapsed relative user.self sys.self user.child sys.child
# 1 points(longitude, latitude)          100   0.155        1     0.155        0          0         0

maps::map('state', xlim = c(-123, -113), ylim = c(33, 43))
benchmark(
  {
    addWindBarbs(longitude, latitude, speed, dir)
  }
)
#                                            test replications elapsed relative user.self sys.self user.child sys.child
# 1 addWindBarbs(longitude, latitude, speed, dir)          100   4.392        1      6.38    0.091          0         0

# It appears that addWindBarbs is about half as fast as points


# 1000
# get a random set of lat, lon, speed, dir
longitude <- runif(1000, -123, -113)
latitude <- runif(1000, 33, 43)
speed <- runif(1000, 0, 100)
dir <- runif(1000, 0, 360)

maps::map('state', xlim = c(-123, -113), ylim = c(33, 43))
benchmark(
  points(longitude, latitude),
  replications = 10
)
#                          test replications elapsed relative user.self sys.self user.child sys.child
# 1 points(longitude, latitude)           10   0.147        1     0.147        0          0         0

maps::map('state', xlim = c(-123, -113), ylim = c(33, 43))
benchmark(
  addWindBarbs(longitude, latitude, speed, dir),
  replications = 10
)
#                                            test replications elapsed relative user.self sys.self user.child sys.child
# 1 addWindBarbs(longitude, latitude, speed, dir)           10   4.213        1     4.175    0.036          0         0


# 10000
# get a random set of lat, lon, speed, dir
longitude <- runif(10000, -123, -113)
latitude <- runif(10000, 33, 43)
speed <- runif(10000, 0, 100)
dir <- runif(10000, 0, 360)

maps::map('state', xlim = c(-123, -113), ylim = c(33, 43))
benchmark(
  points(longitude, latitude),
  replications = 1
)
#                          test replications elapsed relative user.self sys.self user.child sys.child
# 1 points(longitude, latitude)            1   0.507        1     0.498    0.007          0         0

maps::map('state', xlim = c(-123, -113), ylim = c(33, 43))
benchmark(
  addWindBarbs(longitude, latitude, speed, dir),
  replications = 1
)
#                                            test replications elapsed relative user.self sys.self user.child sys.child
# 1 addWindBarbs(longitude, latitude, speed, dir)            1   4.145        1     4.139    0.005          0         0
