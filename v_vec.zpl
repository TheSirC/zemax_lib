!转镜旋转角度
FOR J, 0, 5, 1
FOR angle , 15+60*J, 50+60*J, 7
SETNSCPOSITION 13, 1, 5, angle

!32 通道的光束
FOR L, -1, 1, 2/31
!RAYTRACE 0, L, 0, 0
!设置评价函数里边的数据
SETOPERAND 9, 5, l
SETOPERAND 10, 5, l
SETOPERAND 11, 5, l
setoperand 12, 5, l
setoperand 13, 5, l
setoperand 14, 5, l
setoperand 5, 5, l
setoperand 6, 5, l
setoperand 7, 5, l

raytrace 0, l, 0, 0
UPDATE all

#光在转镜上的坐标点
a = oper(9,10)  
b = oper(10,10)  
c = oper(11,10)  


#出射光线
out_x = RAGL(14)
out_y = RAGM(14)
out_z = RAGN(14)

# 入射光向量（转镜坐标-反射镜坐标）
in_x = oper(12,10)
in_y = oper(13,10)
in_z = oper(14,10)


OUTPUT "D:\TEST\lens_pd3.csv" ,APPEND
print a,",",b,",",c,",",out_x,",",out_y,",",out_z,",",in_x,",",in_y,",",in_z

!print a,",",b,",",c

NEXT
NEXT
NEXT