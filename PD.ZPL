FOR J, 0, 5, 1
FOR angle , 20+60*J, 55+60*J, 7
SETNSCPOSITION 14, 1, 5, angle


FOR L, -1, 1, 2/31
!RAYTRACE 0, L, 0, 0

SETOPERAND 9, 5, l
SETOPERAND 10, 5, l
SETOPERAND 11, 5, l
setoperand 23, 5, l

raytrace 0, l, 0, 0
UPDATE all
#延长光程角度比
a = RAGL(15)
b = RAGM(15)
c = RAGN(15)

#分离点到棱镜反射面得距离
od=21.37

x_ = a*od
y_ = b*od
z_ = c*od
#评价函数里边取值
a1 = oper(16,10)
b1 = oper(10,10)
c1 = oper(14,10)


x_end = a1-x_
y_end = b1-y_
z_end = c1-z_


OUTPUT "D:\TEST\pd_end.csv" ,APPEND
print x_end,",",y_end,",",z_end

NEXT
NEXT
NEXT