View(HR_Employee_Attrition)

summary(HR_Employee_Attrition)


cor(HR_Employee_Attrition)
library(ggplot2)
#Problem statement 1: Are the people having less Monthly Income leaving the company?
ggplot(HR_Employee_Attrition   ,aes(y=Attrition,x=MonthlyIncome))+geom_boxplot(fill="Pink")+theme_minimal()
#Income
ab=select(HR_Employee_Attrition,Attrition,MonthlyIncome)
cb=filter(ab,Attrition=="Yes",MonthlyIncome<18000)
db=filter(ab,Attrition=="Yes",MonthlyIncome>18000)

rb=filter(ab,Attrition=="No",MonthlyIncome<18000)
kb=filter(ab,Attrition=="No",MonthlyIncome>18000)

#Problem statement 2:How does the monthly income vary with the job role ?


ggplot(HR_Employee_Attrition,aes(x=MonthlyIncome,fill=JobRole))+geom_histogram()

#Problem statement 3:  Does overtime affect a percent salary hike?

ggplot(HR_Employee_Attrition,aes(x=OverTime,y=PercentSalaryHike,fill=Gender))+geom_boxplot()
#Overtime

q=select(HR_Employee_Attrition,PercentSalaryHike,OverTime)

b=filter(q,OverTime=="Yes")
mean(b$PercentSalaryHike)
m=filter(q,OverTime=="No")
mean(m$PercentSalaryHike)

#Problem statement 4:Do the Employees are satisfied with their  Job role?


a=filter(HR_Employee_Attrition,JobRole=="Sales Executive")

b=filter(HR_Employee_Attrition,JobRole=="Research Scientist")
c=filter(HR_Employee_Attrition,JobRole=="Laboratory Technician")
d=filter(HR_Employee_Attrition,JobRole=="Manufacturing Director")
e=filter(HR_Employee_Attrition,JobRole=="Healthcare Representative")
f=filter(HR_Employee_Attrition,JobRole=="Manager")
g=filter(HR_Employee_Attrition,JobRole=="Sales Representative")
h=filter(HR_Employee_Attrition,JobRole=="Human Resources")
i=filter(HR_Employee_Attrition,JobRole=="Research Director")

p=ggplot(a,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
q=ggplot(b,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
r=ggplot(c,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
s=ggplot(d,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
t=ggplot(e,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
u=ggplot(f,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
v=ggplot(g,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
w=ggplot(h,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()
z=ggplot(i,aes(x=JobSatisfaction,fill=EducationField))+geom_bar()


dev.off()
library(grid)
viewport<- viewport(
  layout=grid.layout(3,3))
pushViewport(viewport)
print(x=p,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=1))
print(x=q,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=2))
print(x=r,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=3))
print(x=s,
      vp=viewport(
        layout.pos.row = 2,
        layout.pos.col=1))
print(x=t,
      vp=viewport(
        layout.pos.row = 2,
        layout.pos.col=2))
print(x=u,
      vp=viewport(
        layout.pos.row = 2,
        layout.pos.col=3))
print(x=v,
      vp=viewport(
        layout.pos.row = 3,
        layout.pos.col=1))
print(x=w,
      vp=viewport(
        layout.pos.row = 3,
        layout.pos.col=2))
print(x=z,
      vp=viewport(
        layout.pos.row = 3,
        layout.pos.col=3))



#Problem statement 5:Is there a relationship between employee turnover and specific  department?

ggplot(HR_Employee_Attrition, aes(x=JobRole, y=Attrition, fill=Department)) + geom_bar(stat="identity", position="dodge") + labs(title="Attrition  Rate by Job Title and Department", x="Job Role", y="Attrition Rate")



#Problem statement 6:Do certain demographic characteristics, such as age or gender, seem to be associated with higher or lower rates of employee Attrition?
ggplot(HR_Employee_Attrition, aes(x=Attrition,y=Age, fill=Gender ))+geom_boxplot()

a3=filter(HR_Employee_Attrition,Gender=="Male",Attrition =="No")

a2=filter(HR_Employee_Attrition,Gender=="Female",Attrition=="No")




#Problem statement 7:Find the correlation in all the factors affecting attrition ?
a=select(HR_Employee_Attrition , MonthlyIncome,DistanceFromHome,HourlyRate,DailyRate,Age,PercentSalaryHike,TotalWorkingYears,WorkLifeBalance)
cor(a)
d<-cor(a)




library(corrplot)
corrplot(d,method = "pie")

corrplot(d,method = "circle")

corrplot(d,type = "upper")

corrplot(d,type = "lower")

corrplot(d,method = "shade")


ggplot(HR_Employee_Attrition,aes(Attrition,fill=Department))+geom_bar()
ggplot(HR_Employee_Attrition,aes(WorkLifeBalance,fill=Attrition))+geom_bar()








ggplot(HR_Employee_Attrition,aes(x=MonthlyIncome,fill=JobRole))+geom_histogram()


