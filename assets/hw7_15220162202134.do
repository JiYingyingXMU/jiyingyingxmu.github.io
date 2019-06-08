*HW7 for Microeconometrics
*By Yingying Ji, Stu.ID:15220162202134
*2019/06/08

clear //clear memory and remove data
set more off
cd "C:\Users\15068\Desktop\Microeconometrics\HW7" 
//Change the working directory to a specific folder


use hw7data.dta //load the data
des //read the data
sum //read the data


ttest income, by(small) unequal 
//Use t-test to compare the mean of income between two groups(small and regular class)
ttest girl, by(small) unequal 
//Use t-test to compare the mean of the share of girls between two groups.
regress girl small 
// or regress girl on the indicator of small classes
regress income small 
//or regress income on the indicator of small classes


reg testscore small girl income, cluster(classid) 
//regress testscore over small, girl, and income
//with standard errors allow to correlated within the same classid 

