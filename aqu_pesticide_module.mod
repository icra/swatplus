  q"  ;   k820309    �          2021.9.0     mld                                                                                                          
       aqu_pesticide_module.f90 AQU_PESTICIDE_MODULE                                                            #AQUPEST_ADD                                                              o #AQUPEST_ADD_ALL                                                               #AQUPEST_DIV                                                               #AQUPEST_AVE                      @                             '$              	      #TOT_IN    #SOL_FLO    #SOR_FLO    #SOL_PERC 	   #REACT 
   #METAB    #STOR_AVE    #STOR_INIT    #STOR_FINAL                �                                              	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                               	              	                                                 	                                 0.                �                               
              	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                             	                                                 	                                 0.                �                                            	  	                                                 	                                 0.                      @              A                'H                    #PEST               �                                                   $             #AQU_PESTICIDE_PROCESSES              &                                                                                            $       #AQU_PESTICIDE_PROCESSES                                                          H                        &                                           #AQU_PESTICIDE_OUTPUT                                                          H                        &                                           #AQU_PESTICIDE_OUTPUT                                                          H                        &                                           #AQU_PESTICIDE_OUTPUT                                                          H                        &                                           #AQU_PESTICIDE_OUTPUT                                                     H       #AQU_PESTICIDE_OUTPUT                                                     H       #AQU_PESTICIDE_OUTPUT                                                     H       #AQU_PESTICIDE_OUTPUT                                                     H       #AQU_PESTICIDE_OUTPUT                                                     H       #AQU_PESTICIDE_OUTPUT                                                     H       #AQU_PESTICIDE_OUTPUT                      @                              '�                    #DAY    #MO    #DAY_MO    #YRC     #ISD !   #ID "   #NAME #   #PEST $   #TOT_IN %   #SOL_OUT &   #SOR_OUT '   #SOL_PERC (   #REACT )   #METAB *   #STOR_AVE +   #STOR_INIT ,   #STOR_FINAL -               �                                                                                                                                        C  jday                                �                                                                                                                                       C   mon                                �                                                                                                                                       C   day                                �                                                                                                                                        C    yr                                �                              !                                                                                  	                       C   unit                                 �                              "                                                                                   	                       C gis_id                                 �                              #            (                                                                                             C name                                           �                              $            8                                                                                             C pesticide                                      �                              %            H       	                                                                                      C  tot_in_kg                                  �                              &            U       
                                                                                      C  sol_flo_kg                                 �                              '            b                                                                                             C  sor_flo_kg                                 �                              (            o                                                                                             Csol_perc_kg                                  �                              )            |                                                                                             Creact_kg                                     �                              *            �                                                                                             Cmetab_kg                                     �                              +            �                                                                                             Cstor_ave_kg                                  �                              ,            �                                                                                             Cstor_init_kg                                 �                              -            �                                                                                             Cstor_final_kg                                                                .     �       #AQU_PESTICIDE_HEADER    &         @     X                                 $                      #AQU1 /   #AQU2 0   #AQU_PESTICIDE_PROCESSES              
                                  /     $              #AQU_PESTICIDE_PROCESSES              
                                  0     $              #AQU_PESTICIDE_PROCESSES    &         @     X                                 $                      #AQU1 1   #AQU2 2   #AQU_PESTICIDE_PROCESSES              
                                  1     $              #AQU_PESTICIDE_PROCESSES              
                                  2     $              #AQU_PESTICIDE_PROCESSES    &         @     X                                 $                      #AQU1 3   #CONST 4   #AQU_PESTICIDE_PROCESSES              
                                  3     $              #AQU_PESTICIDE_PROCESSES              
                                  4     	      &         @     X                                 $                      #AQU1 5   #CONST 6   #AQU_PESTICIDE_PROCESSES              
                                  5     $              #AQU_PESTICIDE_PROCESSES              
                                  6     	         �   6      fn#fn    �   Q      i@    '  U      u@SUM    |  Q      i@    �  Q      i@ (     �       AQU_PESTICIDE_PROCESSES /   �  �   a   AQU_PESTICIDE_PROCESSES%TOT_IN 0   �  �   a   AQU_PESTICIDE_PROCESSES%SOL_FLO 0   1  �   a   AQU_PESTICIDE_PROCESSES%SOR_FLO 1   �  �   a   AQU_PESTICIDE_PROCESSES%SOL_PERC .   }  �   a   AQU_PESTICIDE_PROCESSES%REACT .   #  �   a   AQU_PESTICIDE_PROCESSES%METAB 1   �  �   a   AQU_PESTICIDE_PROCESSES%STOR_AVE 2   o  �   a   AQU_PESTICIDE_PROCESSES%STOR_INIT 3     �   a   AQU_PESTICIDE_PROCESSES%STOR_FINAL %   �  Z       AQU_PESTICIDE_OUTPUT *   	  �   a   AQU_PESTICIDE_OUTPUT%PEST    �	  ]       AQU_PESTBZ    #
  �       AQUPST_D    �
  �       AQUPST_M    o  �       AQUPST_Y      �       AQUPST_A    �  Z       BAQUPST_D      Z       BAQUPST_M    o  Z       BAQUPST_Y    �  Z       BAQUPST_A    #  Z       AQUPST    }  Z       AQUPSTZ %   �        AQU_PESTICIDE_HEADER )   �  �   a   AQU_PESTICIDE_HEADER%DAY (   �  �   a   AQU_PESTICIDE_HEADER%MO ,   o  �   a   AQU_PESTICIDE_HEADER%DAY_MO )   2  �   a   AQU_PESTICIDE_HEADER%YRC )   �  �   a   AQU_PESTICIDE_HEADER%ISD (   �  �   a   AQU_PESTICIDE_HEADER%ID *     �   a   AQU_PESTICIDE_HEADER%NAME *   L  �   a   AQU_PESTICIDE_HEADER%PEST ,     �   a   AQU_PESTICIDE_HEADER%TOT_IN -   �  �   a   AQU_PESTICIDE_HEADER%SOL_OUT -   �  �   a   AQU_PESTICIDE_HEADER%SOR_OUT .   w  �   a   AQU_PESTICIDE_HEADER%SOL_PERC +   A  �   a   AQU_PESTICIDE_HEADER%REACT +     �   a   AQU_PESTICIDE_HEADER%METAB .   �  �   a   AQU_PESTICIDE_HEADER%STOR_AVE /   �  �   a   AQU_PESTICIDE_HEADER%STOR_INIT 0   i  �   a   AQU_PESTICIDE_HEADER%STOR_FINAL    3  Z       AQUPEST_HDR    �  �       AQUPEST_ADD !     e   a   AQUPEST_ADD%AQU1 !   s  e   a   AQUPEST_ADD%AQU2     �  �       AQUPEST_ADD_ALL %   Y  e   a   AQUPEST_ADD_ALL%AQU1 %   �  e   a   AQUPEST_ADD_ALL%AQU2    #   �       AQUPEST_DIV !   �   e   a   AQUPEST_DIV%AQU1 "   
!  @   a   AQUPEST_DIV%CONST    J!  �       AQUPEST_AVE !   �!  e   a   AQUPEST_AVE%AQU1 "   1"  @   a   AQUPEST_AVE%CONST 