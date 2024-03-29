% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_cmadaas.R
\name{cmadass_model_grid}
\alias{cmadass_model_grid}
\title{Retrieve model grid data from CMADaSS service.}
\usage{
cmadass_model_grid(
  time,
  dataCode = "NAFP_CRA40_FTM_6HOR_ANA",
  fcstEle = "GPH",
  limit = NULL,
  fcstLevel = 500,
  levelType = "-",
  validTime = 0,
  outList = FALSE
)
}
\arguments{
\item{time:}{model initial or run time, like "2016081712" (format "\%Y\%m\%d\%H").}

\item{dataCode:}{MUSIC data code.
NAFP_CRA40_FTM_6HOR_ANA(default): 中国40年再分析资料
NAFP_ECMF_C1D_GLB_FOR: 欧洲中心数值预报产品-高分辨率C1D-全球
NAFP_ECMF_FTM_HIGH_ANEA_FOR: 欧洲中心数值预报产品-高分辨率C1D-亚洲地区
......}

\item{fcstEle:}{forecast element, like temperature "TEM"}

\item{limit:}{region limit, \link{minLat, minLon, maxLat, maxLon}}

\item{fcstLevel:}{vertical level, like 500}

\item{levelType:}{forecast level type, 表示Grib数据中的层次类型, 可在云平台上查询.}

\item{validTime:}{forecast hour, like 0 or 6}

\item{outList:}{if TRUE, return list type.}
}
\value{
data.table data.
}
\description{
\preformatted{Retrieve model grid data from CMADaaS service.
refer to: http://10.20.76.55/cimissapiweb/apidataclassdefine_list.action
}
}
\examples{
  data <- cmadass_model_grid("2020041900")

}
