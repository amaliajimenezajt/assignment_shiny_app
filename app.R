install.packages("devtools")
install.packages("usethis")

library(devtools)

devtools::install_git("https://github.com/bernardo-dauria/kaggler.git")
library(kaggler)
kgl_auth("username":"amaliajimenezt","key":"8e177825f7c983276080202675cc94f6")
