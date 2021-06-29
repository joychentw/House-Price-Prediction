# work by Joy

v1 <- read.csv('/Users/joychen/OneDrive - UC Irvine/288Predictive/course_project/house-prices-data/var01_20.csv',sep = ",")
v20 <- read.csv('/Users/joychen/OneDrive - UC Irvine/288Predictive/course_project/house-prices-data/var20_40.csv',sep = ",")
v40 <- read.csv('/Users/joychen/OneDrive - UC Irvine/288Predictive/course_project/house-prices-data/var41_60.csv',sep = ",")
v60 <- read.csv('/Users/joychen/OneDrive - UC Irvine/288Predictive/course_project/house-prices-data/var61_80.csv',sep = ",")

combined <- cbind(v20,v40[,-1],v60)
final <- merge(v1, combined, by.x = "Id", by.y = "X")

write.csv(final, 'cleaned_combined.csv')

rm(v1, v20, v40, v60, combined, final)
