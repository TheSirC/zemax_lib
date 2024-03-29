! by wys

deg_start = 15
deg_step = 5
deg_per_face = 35


DECLARE arr_fields, DOUBLE, 1, 32

FOR i = 1, 32, 1
    arr_fields(i) = -1+1/15.5*(i-1)
NEXT

OUTPUT "d:/test/hexa_dopler.csv"
PRINT " "
c = OCOD("NSST")

FOR deg_face_start = deg_start, deg_start+350, 60
! per face
    FOR deg = deg_face_start, deg_face_start+deg_per_face, deg_step
    !per deg
        SETNSCPOSITION 14, 1, 5, deg
        UPDATE
        OUTPUT SCREEN
        PRINT deg
        FOR field = 1, 32, 1
            ! the x,y,z direction cosines of the ray up to the prism face
            ray_cos_theta_x = OPEW(c,15,1,0,arr_fields(field),0,0,6,1)
            ray_cos_theta_y = OPEW(c,15,1,0,arr_fields(field),0,0,7,1)
            ray_cos_theta_z = OPEW(c,15,1,0,arr_fields(field),0,0,8,1)
            ! the x,y,z coordinate of the ray up to the prism face
            ray_coor_x = OPEW(c,15,1,0,arr_fields(field),0,0,0,1)
            ray_coor_y = OPEW(c,15,1,0,arr_fields(field),0,0,1,1)
            ray_coor_z = OPEW(c,15,1,0,arr_fields(field),0,0,2,1)

            OUTPUT SCREEN
            PRINT ray_cos_theta_x, ", ", ray_cos_theta_y, ", ", ray_cos_theta_z
            PRINT ray_coor_x, ", ", ray_coor_y, ", ", ray_coor_z
            print "================"
            OUTPUT "d:/test/hexa_dopler.csv", APPEND
            PRINT ray_cos_theta_x, ", ", ray_cos_theta_y, ", ", ray_cos_theta_z, ", ", ray_coor_x, ", ", ray_coor_y, ", ", ray_coor_z
        NEXT
    NEXT
NEXT

RELEASE arr_fields