# Meteorological Data IO Functions.

Functions used to retrieve meteorological data, such as MICAPS format, MICAPS Cassandra, CIMISS and so on.

## Installation

You can install the released version of nmcMetIO with:

```r
# install.packages("remotes")
remotes::install_github("nmcdev/nmcMetIO")
```

## 设置CIMISS和MICAPS服务器的地址及用户信息
在系统用户目录下("C:\Users\用户名"), 新建文本文件config_met_io.ini, 里面内容模板为:
```
[CIMISS]
DNS = xx.xx.xx.xx
USER_ID = xxxxxxxxx
PASSWORD = xxxxxxxx

[MICAPS]
GDS_IP = xx.xx.xx.xx
GDS_PORT = xxxx
```
这里xxxx用相应的地址, 接口和用户信息代替.
