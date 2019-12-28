#' Smooth the 2D data.table fields.
#' 
#' @description 该程序调用fields程序库对二维场进行平滑, 首先将离散点插值到nx,ny的网格点上,
#'              如果输入场是等经纬度网格点, 也可看成离散点, 然后插值到(nx,ny)的格点上.之后
#'              调用image.smooth进行平滑, theta控制平滑度, 值越大越平滑. 返回为data.table
#'              数据, 以便于ggplot绘制图像.
#'
#' @param x : 1D vector, longitude.
#' @param y : 1D vector, latitude.
#' @param z : 1D vector, 
#' @param theta : the bandwidth or scale parameter, larger for smoother.
#' @param nx : Number of grid point in X coordinate.
#' @param ny : Number of grid points in Y coordinate.
#'
#' @return data.table, (x, y, z)
#' @export
#'
smooth2d <- function(x, y, z, theta=0.03, nx=128, ny=128){
  # convert to image field
  out <- fields::as.image(z, x=data.frame(lon=x, lat=y), nx=nx, ny=ny)
  
  # smooth the image filed
  out <- fields::image.smooth(out, theta=theta)
  
  # convert to data.table
  out <- data.table::data.table(
    x=rep(out$x, length(out$y)), y=rep(out$y, each=length(out$x)), 
    z=as.vector(out$z))
  return(out)
}


#' Convert data.table (lon, lat, var1) to matrix (lon, lat)
#'
#' @param data : data.table数据, like (lon, lat, var1)
#' @param formula : A formula of the form LHS ~ RHS to cast, 给出经纬度的列表名
#' @param value.var : 矩阵的数值变量
#'
#' @return : data.table matrix.
#' @export
#'
dt2matrix <- function(data, formula=lon~lat, value.var="var1"){
  return(data.table::dcast(data, formula, value.var=value.var))
}




