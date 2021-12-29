! 使用宏语言计算失高

n = PVHX()
r1 = RADI(n)
d = SDIA(n)

sag=ABSO(r1)-SQRT(r1*r1-d*d)

OPTRETURN 0, sag
