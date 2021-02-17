# Meteorological Data IO Functions.

Functions used to retrieve meteorological data, such as MICAPS format, MICAPS Cassandra, CIMISS and so on.

## Installation

You can install the released version of nmcMetIO with:

```r
# install.packages("devtools")
devtools::install_github("nmcdev/nmcMetIO")
```

You can alse clone the repository and install the package based on the checkout.

```
# Change directory
cd <somewhere/on/your/local/disc>

# Clone repository
git clone https://github.com/nmcdev/nmcMetIO.git

# Start R
R
```
As soon as you are in the interactive R shell:
```r
# Loading the devtools library
library("devtools")

# Install package
devtools::install("nmcMetIO")

```

## 设置配置文件
若要访问CIMISS、CMADaaS或MICAPS服务器等, 需在配置文件中设置地址和用户信息(若不需要, 则相应项无需配置).

* 在系统用户目录下("C:\\Users\\用户名"(windows)或"/home/用户名/"(Linux)), 建立文件夹".nmcdev"(若Windows下无法直接创建, 在命令窗口中输入`mkdir .nmcdev`创建)
* 在".nmcdev"中创建文本文件"config.ini", 内容模板为:
  
```
[CIMISS]
DNS = xx.xx.xx.xx
USER_ID = xxxxxxxxx
PASSWORD = xxxxxxxx

[CMADaaS]
DNS = xx.xx.xx.xx
PORT = xx
USER_ID = xxxxxxxxx
PASSWORD = xxxxxxxx
serviceNodeId = NMIC_MUSIC_CMADAAS

[MICAPS]
GDS_IP = xx.xx.xx.xx
GDS_PORT = xxxx

# Cached file directory, if not set,
#   /home/USERNAME/.nmcdev/cache (linux) or C:/Users/USERNAME/.nmcdev/cache (windows) will be used.
[CACHE]
# CACHE_DIR = ~ 

[MAPBOX]
token = pk.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

这里xxxx用相应的地址, 接口和用户信息代替. 如果要用到MAPBOX地图, 可以申请[access token](https://docs.mapbox.com/help/glossary/access-token).

---
