x = seq(1, 100, by = 1)
y = seq(1, 100, by = 1)

z = rnorm(10000,1, 0.1)

xy = expand.grid(x,y)
xy$z = NA

zz = matrix(z, nrow = 100, ncol = 100)

library(ggplot2)

tt = aggregate(xy$z, by = list(xy$Var1, xy$Var2), FUN = sort)

min = sample(20:38, 1)
max = sample(60:88, 1)

for(i in 1:nrow(xy)){
  
  
  if(min < 20){
    direct = "add"
  } else if(min > 38){
    direct = "sub"
  } else {
    direct = sample(c("add", "sub"), 1)
  }
  
  
  if(direct == "add"){
    min = min + rnorm(1,1,0.1)
  } else{
    min = min - rnorm(1,1,0.1)
  }
  
  if(max > 88){
    direct = "sub"
  } else if(max < 60){
    direct = "add"
  } else {
    direct = sample(c("add", "sub"), 1)
  }
  
  if(direct == "add"){
    max = max + rnorm(1,1,0.1)
  } else{
    max = max - rnorm(1,1,0.1)
  }
  
  if(xy$Var1[i] > min & xy$Var1[i] < max){
    if(xy$Var2[i] > min & xy$Var2[i] < max){
      xy$z[i] = rnorm(1, 1, 0.1 )
    } else{
      xy$z[i] = rnorm(1,1.5,0.1)
    }
  } else if(xy$Var2[i] > min & xy$Var2[i] < max){
    if(xy$Var1[i] > min & xy$Var1[i] < max){
      xy$z[i] = rnorm(1, 1, 0.1 )
    } else{
      xy$z[i] = rnorm(1,1.5,0.1)
    }
  } else {
    xy$z[i] =rnorm(1,1.5, 0.1)
  }
  
  
}




ggplot(data = xy) +
  geom_tile (aes(x=Var1, y=Var2, fill = z))

lon = seq(-121.84290875645928, -121.83923021686712, length.out = 50)
lat = seq(38.26212850002531, 38.259323027126094, length.out = 50)

df = expand.grid(lon, lat)
colnames(df) = c("long", "lat")


#library(elevatr)
tt = elevatr::get_elev_point(df,prj="EPSG:4326")
tt$elevation

df = df[, 1:3]
df2 = df
df2$z = df$z + rnorm(1,0,1)
df3 = df2
df3$z = df$z + rnorm(1,0,1)
df4 = df3
df4$z = df$z + rnorm(1,0,1)
df5 = df4
df5$z = df$z + rnorm(1,0,1)
df6 = df5
df6$z = df$z + rnorm(1,0,1)


df$year = 1
df2$year = 2
df3$year = 3
df4$year = 4
df5$year = 5
df6$year = 6

df.all = rbind(df,df2,df3,df4,df5,df6)
colnames(df.all) = c("Longitude", "Latitude", "Variability", "year")

p = ggplot(df.all) +
  geom_tile (aes(x=Longitude, y=Latitude, fill = Variability)) +
  scale_fill_gradient2(low="blue", mid="green", high="yellow", 
                       midpoint=9) +
  theme_bw() +
  transition_time(year) +
  ease_aes('linear')

anim_save("/Users/Will/Desktop/v_heatmap.gif", p)


df$z2 = df$z + rnorm(1,0, 0.2)

ggplot() +
  geom_tile (aes(x=df$long, y=df$lat, fill = df$z2)) +
  scale_fill_gradient2(low="blue", mid="green", high="yellow", 
                       midpoint=8.75)

