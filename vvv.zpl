FOR J, 0, 5, 1
FOR angle , 15+60*J, 50+60*J, 7
SETNSCPOSITION 13, 1, 5, angle


FOR L, -1, 1, 2/31
!RAYTRACE 0, L, 0, 0

SETOPERAND 9, 5, l
SETOPERAND 10, 5, l
SETOPERAND 11, 5, l
setoperand 23, 5, l
setoperand 5, 5, l
setoperand 6, 5, l
setoperand 7, 5, l

raytrace 0, l, 0, 0
UPDATE all
#延长光程角度比
! a = RAGL(15)
! b = RAGM(15)
! c = RAGN(15)

#分离点到棱镜反射面得距离
! od=21.37

! x_ = a*od
! y_ = b*od
! z_ = c*od
#评价函数里边取值
#棱镜上的坐标点
a = oper(16,10)  
b = oper(10,10)  
c = oper(14,10)  

#反射镜上的坐标点
d = oper(5,10)
e = oper(6,10)
f = oper(7,10)


#out 坐标
p1 = a-d
p2 = b-e
P3 = c-f

OUTPUT "D:\TEST\vec_ray.csv" ,APPEND
print p1,",",p2,",",p3

NEXT
NEXT
NEXT