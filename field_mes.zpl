FOR J, 0, 5, 1
FOR Angle , 20+60*J, 55+60*J, 7
SETNSCPOSITION 14, 1, 5, Angle

 
FOR L, -1, 1, 2/31
SETOPERAND 9, 5, l
SETOPERAND 10, 5, l
SETOPERAND 11, 5, l
setoperand 5, 5, l
setoperand 6, 5, l
setoperand 7, 5, l



UPDATE all

a=oper(16,10)   #x坐标
b=oper(10,10)   #y坐标
c=oper(14,10)   #z坐标

d=oper(5,10)    #x坐标
e=oper(6,10)    #y坐标
f=oper(7,10)    #z坐标


p1=a-d
p2=b-e
p3=c-f

OUTPUT "D:\TEST\vec_0010.csv" ,APPEND
!PRINT x_angle
!PRINT y_angle
!PRINT x_angle,",",y_angle
PRINT p1 ,",", p2, "," ,p3,",",angle

NEXT
NEXT
NEXT

beep