dist2 <- function(lat_a, lon_a, lat_b, lon_b) 
{
  res=(acos(sin(lat_a * 0.01745329252) * sin(lat_b * 0.01745329252) + (cos(lat_a * 0.01745329252) * cos(lat_b * 0.01745329252) * cos(lon_a * 0.01745329252 - lon_b * 0.01745329252))) * 57.29577951 * 111.194)
  res
}

dist2(lat_a=-6.99,lon_a=-37.34, lat_b=48.19, lon_b=11.64)

