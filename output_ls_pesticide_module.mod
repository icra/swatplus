  l*  C   k820309    �          2021.9.0    �mld                                                                                                          
       output_ls_pesticide_module.f90 OUTPUT_LS_PESTICIDE_MODULE                                                            #HRUOUT_PESTBAL_ADD                                                               #HRUOUT_PESTBAL_DIV                                                               #HRUOUT_PESTBAL_AVE                      @                             '8                    #PLANT    #SOIL    #SED    #SURQ    #LATQ 	   #TILEQ 
   #PERC    #APPLY_S    #APPLY_F    #DECAY_S    #DECAY_F    #WASH    #METAB_S    #METAB_F                �                                              	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                               	              	                                                 	                                 0.                �                               
              	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                            	  	                                                 	                                 0.                �                                    $       
  	                                                 	                                 0.                �                                    (         	                                                 	                                 0.                �                                    ,         	                                                 	                                 0.                �                                    0         	                                                 	                                 0.                �                                    4         	                                                 	                                 0.                                                     8       #PESTICIDE_BALANCE                      @              A                'H                    #PEST               �                                                   8             #PESTICIDE_BALANCE              &                                                                                                  H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                           H                        &                                           #OBJECT_PESTICIDE_BALANCE                                                     H       #OBJECT_PESTICIDE_BALANCE                                                     H       #OBJECT_PESTICIDE_BALANCE                                                      H       #OBJECT_PESTICIDE_BALANCE                                                !     H       #OBJECT_PESTICIDE_BALANCE                      @                         "     '                   #DAY #   #MO $   #DAY_MO %   #YRC &   #ISD '   #ID (   #NAME )   #PEST *   #PLANT +   #SOIL ,   #SED -   #SURQ .   #LATQ /   #TILEQ 0   #PERC 1   #APPLY_S 2   #APPLY_F 3   #DECAY_S 4   #DECAY_F 5   #WASH 6   #METAB_S 7   #METAB_F 8               �                              #                                                                                                          C jday                                �                              $                                                                                                         C   mon                                �                              %                                                                                                         C   day                                �                              &                                                                                                         C    yr                                �                              '                                                                                  	                       C    unit                                �                              (                                                                                  	                       C  gis_id                                �                              )            '                                                                                             C  name                                          �                              *            7                                                                                             C  pesticide                                     �                              +            G       	                                                                                      C  plant_kg/ha                                  �                              ,            V       
                                                                                      C    soil_kg/ha                                 �                              -            e                                                                                             C    sed_kg/ha                                  �                              .            t                                                                                             C   surq_kg/ha                                  �                              /            �                                                                                             C   latq_kg/ha                                  �                              0            �                                                                                             C  tileq_kg/ha                                  �                              1            �                                                                                             C   perc_kg/ha                                  �                              2            �                                                                                             C apply_s_kg/ha                                 �                              3            �                                                                                             C apply_f_kg/ha                                 �                              4            �                                                                                             C decay_s_kg/ha                                 �                              5            �                                                                                             C decay_f_kg/ha                                 �                              6            �                                                                                             C   wash_kg/ha                                  �                              7            �                                                                                             C metab_s_kg/ha                                 �                              8            
                                                                                            C metab_f_kg/ha                                                                 9           #OUTPUT_PESTBAL_HEADER "   &         @     X                                 8                      #HRU1 :   #HRU2 ;   #PESTICIDE_BALANCE              
                                  :     8              #PESTICIDE_BALANCE              
                                  ;     8              #PESTICIDE_BALANCE    &         @     X                                 8                      #HRU1 <   #CONST =   #PESTICIDE_BALANCE              
                                  <     8              #PESTICIDE_BALANCE              
                                  =     	      &         @     X                                 8                      #HRU1 >   #CONST ?   #PESTICIDE_BALANCE              
                                  >     8              #PESTICIDE_BALANCE              
                                  ?     	         �   B      fn#fn    �   X      i@    :  X      i@    �  X      i@ "   �  �       PESTICIDE_BALANCE (   �  �   a   PESTICIDE_BALANCE%PLANT '     �   a   PESTICIDE_BALANCE%SOIL &   %  �   a   PESTICIDE_BALANCE%SED '   �  �   a   PESTICIDE_BALANCE%SURQ '   q  �   a   PESTICIDE_BALANCE%LATQ (     �   a   PESTICIDE_BALANCE%TILEQ '   �  �   a   PESTICIDE_BALANCE%PERC *   c  �   a   PESTICIDE_BALANCE%APPLY_S *   	  �   a   PESTICIDE_BALANCE%APPLY_F *   �  �   a   PESTICIDE_BALANCE%DECAY_S *   U	  �   a   PESTICIDE_BALANCE%DECAY_F '   �	  �   a   PESTICIDE_BALANCE%WASH *   �
  �   a   PESTICIDE_BALANCE%METAB_S *   G  �   a   PESTICIDE_BALANCE%METAB_F    �  W       PESTBZ )   D  Z       OBJECT_PESTICIDE_BALANCE .   �  �   a   OBJECT_PESTICIDE_BALANCE%PEST    I  �       HPESTB_D    �  �       HPESTB_M    �  �       HPESTB_Y    G  �       HPESTB_A    �  �       RUPESTB_D    �  �       RUPESTB_M    E  �       RUPESTB_Y    �  �       RUPESTB_A    �  ^       BPESTB_D    �  ^       BPESTB_M    U  ^       BPESTB_Y    �  ^       BPESTB_A &     :      OUTPUT_PESTBAL_HEADER *   K  �   a   OUTPUT_PESTBAL_HEADER%DAY )     �   a   OUTPUT_PESTBAL_HEADER%MO -   �  �   a   OUTPUT_PESTBAL_HEADER%DAY_MO *   �  �   a   OUTPUT_PESTBAL_HEADER%YRC *   V  �   a   OUTPUT_PESTBAL_HEADER%ISD )     �   a   OUTPUT_PESTBAL_HEADER%ID +   �  �   a   OUTPUT_PESTBAL_HEADER%NAME +   �  �   a   OUTPUT_PESTBAL_HEADER%PEST ,   z  �   a   OUTPUT_PESTBAL_HEADER%PLANT +   F  �   a   OUTPUT_PESTBAL_HEADER%SOIL *     �   a   OUTPUT_PESTBAL_HEADER%SED +   �  �   a   OUTPUT_PESTBAL_HEADER%SURQ +   �  �   a   OUTPUT_PESTBAL_HEADER%LATQ ,   v  �   a   OUTPUT_PESTBAL_HEADER%TILEQ +   B   �   a   OUTPUT_PESTBAL_HEADER%PERC .   !  �   a   OUTPUT_PESTBAL_HEADER%APPLY_S .   �!  �   a   OUTPUT_PESTBAL_HEADER%APPLY_F .   �"  �   a   OUTPUT_PESTBAL_HEADER%DECAY_S .   r#  �   a   OUTPUT_PESTBAL_HEADER%DECAY_F +   >$  �   a   OUTPUT_PESTBAL_HEADER%WASH .   
%  �   a   OUTPUT_PESTBAL_HEADER%METAB_S .   �%  �   a   OUTPUT_PESTBAL_HEADER%METAB_F    �&  [       PESTB_HDR #   �&  {       HRUOUT_PESTBAL_ADD (   x'  _   a   HRUOUT_PESTBAL_ADD%HRU1 (   �'  _   a   HRUOUT_PESTBAL_ADD%HRU2 #   6(  |       HRUOUT_PESTBAL_DIV (   �(  _   a   HRUOUT_PESTBAL_DIV%HRU1 )   )  @   a   HRUOUT_PESTBAL_DIV%CONST #   Q)  |       HRUOUT_PESTBAL_AVE (   �)  _   a   HRUOUT_PESTBAL_AVE%HRU1 )   ,*  @   a   HRUOUT_PESTBAL_AVE%CONST 