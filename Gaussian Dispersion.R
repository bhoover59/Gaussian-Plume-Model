#### About me ####
# Author: Bode Hoover (bodehoov@iu.edu)
# Calculate concentration at C(x,y,z)
# 2 special cases (y = 0 along plume line and x = 0 crosswind)
# The distances have a LARGE impact on concentration
# This script currently uses Class D stability but other values can easily be substituted
#### Dependencies ####
rm(list = ls()) 
library(ggplot2)
#### Constants ####
Q <- 2 # kg/s
u <- 15 # m/s
stack_height <- 120 # m 
plume_rise <- 10 # m 
height <- stack_height + plume_rise
Rz <- 0.14 # m 
rz <- 0.727 # m 
Ry = 0.219 # m 
ry = 0.764 # m

#### Along center line of plume C(x,0,0) ####
min <- 500
max <- 5000
increment <- 50
x_vals <- seq(min, max, by = increment)
df <- data.frame(x = x_vals)
df$sigma_y <- Ry * df$x ^ ry
df$sigma_z <- Rz * df$x ^ rz

df$pre_exp <- Q / (3.14 * u * df$sigma_y * df$sigma_z)
df$exp <- exp((-(height^2)) / (2 * df$sigma_z^2))
df$C_centerline <- df$pre_exp * df$exp
ggplot(df, aes(x = x, y = C_centerline * 1e9)) +
  geom_point() +
  ylab("[C] (ug/s)") + 
  xlab("Distance along plumeline (m)") + 
  ggtitle("Ground level Downwind distance along plumeline")
  
  
### Along crosswind C(x,y,0) ####
x1 <- 5000 # 5 km
x2 <- 50000 # 50 km
sigma_yx1 <- Ry * x1^ry
sigma_yx2 <- Ry * x2^ry
sigma_zx1 <- Rz * x1^rz
sigma_zx2 <- Rz * x2^rz
min <- -200
max <- 200
increment <- 10
y_vals <- seq(min, max, by = increment)
df_crosswind <- data.frame(y = y_vals)
df_crosswind$pre_exp <- Q / (3.14 * u * sigma_yx1 * sigma_zx1)
df_crosswind$exp1 <- exp((-(height^2)) / (2 * (sigma_zx1^2)))
df_crosswind$exp2 <- exp((-df_crosswind$y^2) / (2 * sigma_yx1^2))
df_crosswind$C_crosswind <- df_crosswind$pre_exp * df_crosswind$exp1 * df_crosswind$exp2
ggplot(df_crosswind, aes(x = y, y = C_crosswind * 1e9)) +
  geom_point() +
  ylab("[C] (ug/s)") + 
  xlab("Distance along crosswind (m)") + 
  ggtitle("Downwind distance along crosswind")

df_crosswind$pre_expx2 <- Q / (3.14 * u * sigma_yx2 * sigma_zx2)
df_crosswind$exp1x2 <- exp((-(height^2)) / (2 * (sigma_zx1^2)))
df_crosswind$exp2x2 <- exp((-(df_crosswind$y^2)) / (2 * sigma_yx2^2))
df_crosswind$C_crosswind_x2 <- df_crosswind$pre_exp * df_crosswind$exp1 * df_crosswind$exp2x2
ggplot(df_crosswind, aes(x = y, y = C_crosswind_x2 * 1e9)) +
  geom_point() +
  ylab("[C] (ug/s)") + 
  xlab("Distance along crosswind (m)") + 
  ggtitle("Downwind distance along crosswind")

#### Total concentration at any point x,y,z ####
x3 <- 500
sigma_yx3 <- Ry * x3^ry
sigma_zx3 <- Rz * x3^rz
min <- 0
max <- 300
increment <- 10
z_vals <- seq(min, max, by = increment)
y_vals <- seq(-150, 150, by = 10)
df_total <- data.frame(z = z_vals)
df_total$y <- y_vals
df_total$pre_exp <- Q / (3.14 * u * sigma_yx3 * sigma_zx3)
df_total$exp1 <- exp((-df_total$y^2) / (2 * sigma_yx3^2))
df_total$exp2 <- exp((-(df_total$z-height)^2 / (2 * sigma_zx3^2))) + exp((-(df_total$z+height)^2 / (2 * sigma_zx3^2)))
df_total$C_z <- df_total$pre_exp * df_total$exp1 * df_total$exp2
ggplot(df_total, aes(x = z, y = C_z * 1e9)) +
  geom_point() +
  ylab("[C] (ug/s)") + 
  xlab("Distance upward (m)") + 
  ggtitle("Downwind distance, concentration in z direction")


