  c8  ]   k820309    �          2021.9.0    )mld                                                                                                          
       ch_pesticide_module.f90 CH_PESTICIDE_MODULE                                                     
       CS_DB                                                            #CHPEST_ADD                                                               #CHPEST_DIV                                                               #CHPEST_AVE                      @              E                '�                   #NUM_TOT    #NUM_PESTS    #PESTS    #PEST_NUM 	   #NUM_PATHS 
   #PATHS    #PATH_NUM    #NUM_METALS    #METALS    #METALS_NUM    #NUM_SALTS    #SALTS    #SALTS_NUM    #NUM_POLL    #POLL    #POLL_NUM                �                                                                                                                                0                �                                                                                                                               0    .           �                                                                       &                                                              �                               	            P                             &                                                       �                               
     �                                                                                           0    .           �                                          �                             &                                                              �                                           �                             &                                                       �                                    0                                                                                          0    .           �                                          8             	               &                                                              �                                           �             
               &                                                       �                                    �                                                                                          0    .           �                                          �                            &                                                              �                                                                       &                                                       �                                    `                                                                                          0    .           �                                          h                            &                                                              �                                           �                            &                                                             @             D                '�                   #PEST    #PATH    #HMET    #SALT    #SALT_MIN    #POLL               �                                                             	            &                                                      �                                           H                 	            &                                                      �                                           �                 	            &                                                      �                                           �                 	            &                                                      �                                                            	            &                                                      �                                           h                	            &                                                                                             �      #CONSTITUENTS                                                     	                                                        	                         @                              '8                    #TOT_IN !   #SOL_OUT "   #SOR_OUT #   #REACT $   #METAB %   #VOLAT &   #SETTLE '   #RESUS (   #DIFUS )   #REACT_BOT *   #METAB_BOT +   #BURY ,   #WATER -   #BENTHIC .               �                               !               	                                                 	                                 0.                �                               "              	                                                 	                                 0.                �                               #              	                                                 	                                 0.                �                               $              	                                                 	                                 0.                �                               %              	                                                 	                                 0.                �                               &              	                                                 	                                 0.                �                               '              	                                                 	                                 0.                �                               (              	                                                 	                                 0.                �                               )             	  	                                                 	                                 0.                �                               *     $       
  	                                                 	                                 0.                �                               +     (         	                                                 	                                 0.                �                               ,     ,         	                                                 	                                 0.                �                               -     0         	                                                 	                                 0.                �                               .     4         	                                                 	                                 0.                      @              A           /     'H                    #PEST 0              �                               0                    8             #CH_PESTICIDE_PROCESSES               &                                                                                       1     8       #CH_PESTICIDE_PROCESSES                                               2            H                        &                                           #CH_PESTICIDE_OUTPUT /                                             3            H                        &                                           #CH_PESTICIDE_OUTPUT /                                             4            H                        &                                           #CH_PESTICIDE_OUTPUT /                                             5            H                        &                                           #CH_PESTICIDE_OUTPUT /                                               6     H       #CH_PESTICIDE_OUTPUT /                                               7     H       #CH_PESTICIDE_OUTPUT /                                               8     H       #CH_PESTICIDE_OUTPUT /                                               9     H       #CH_PESTICIDE_OUTPUT /                                               :     H       #CH_PESTICIDE_OUTPUT /                                               ;     H       #CH_PESTICIDE_OUTPUT /                     @                         <     '                   #DAY =   #MO >   #DAY_MO ?   #YRC @   #ISD A   #ID B   #NAME C   #PEST D   #TOT_IN E   #SOL_OUT F   #SOR_OUT G   #REACT H   #METAB I   #VOLAT J   #SETTLE K   #RESUS L   #DIFUS M   #REACT_BOT N   #METAB_BOT O   #BURY P   #WATER Q   #BENTHIC R               �                              =                                                                                                          C  jday                                �                              >                                                                                                         C   mon                                �                              ?                                                                                                         C   day                                �                              @                                                                                                         C    yr                                �                              A                                                                                  	                       C   unit                                 �                              B                                                                                   	                       C gis_id                                 �                              C            (                                                                                             C name                                           �                              D            8                                                                                             C pesticide                                      �                              E            H       	                                                                                      Ctot_in_kg                                    �                              F            U       
                                                                                      Csol_out_kg                                   �                              G            b                                                                                             Csor_out_kg                                    �                              H            p                                                                                             Creact_h2o_kg                                 �                              I            }                                                                                             Cmetab_h2o_kg                                 �                              J            �                                                                                             Cvolat_kg                                    �                              K            �                                                                                             Csettle_kg                                   �                              L            �                                                                                             Cresuspend_kg                                 �                              M            �                                                                                             Cdiffuse_kg                                  �                              N            �                                                                                             Creact_benth_kg                                 �                              O            �                                                                                             Cmetab_benth_kg                                 �                              P            �                                                                                             Cbury_benth_kg                                 �                              Q            �                                                                                             Cwater_stor_kg                                 �                              R            �                                                                                             Cbenthic_kg                                                                  S           #CH_PESTICIDE_HEADER <   &         @     X                                 8                      #CHO1 T   #CHO2 U   #CH_PESTICIDE_PROCESSES               
                                  T     8              #CH_PESTICIDE_PROCESSES               
                                  U     8              #CH_PESTICIDE_PROCESSES     &         @     X                                 8                      #CH1 V   #CONST W   #CH_PESTICIDE_PROCESSES               
                                  V     8              #CH_PESTICIDE_PROCESSES               
                                  W     	      &         @     X                                 8                      #CH1 X   #CONST Y   #CH_PESTICIDE_PROCESSES               
                                  X     8              #CH_PESTICIDE_PROCESSES               
                                  Y     	         �   4      fn#fn (   �   F   J  CONSTITUENT_MASS_MODULE      P      i@    j  P      i@    �  P      i@ 5   
  (      CONSTITUENTS+CONSTITUENT_MASS_MODULE =   2  �   a   CONSTITUENTS%NUM_TOT+CONSTITUENT_MASS_MODULE ?   �  �   a   CONSTITUENTS%NUM_PESTS+CONSTITUENT_MASS_MODULE ;   |  �   a   CONSTITUENTS%PESTS+CONSTITUENT_MASS_MODULE >     �   a   CONSTITUENTS%PEST_NUM+CONSTITUENT_MASS_MODULE ?   �  �   a   CONSTITUENTS%NUM_PATHS+CONSTITUENT_MASS_MODULE ;   Q  �   a   CONSTITUENTS%PATHS+CONSTITUENT_MASS_MODULE >   �  �   a   CONSTITUENTS%PATH_NUM+CONSTITUENT_MASS_MODULE @   �  �   a   CONSTITUENTS%NUM_METALS+CONSTITUENT_MASS_MODULE <   &  �   a   CONSTITUENTS%METALS+CONSTITUENT_MASS_MODULE @   �  �   a   CONSTITUENTS%METALS_NUM+CONSTITUENT_MASS_MODULE ?   V	  �   a   CONSTITUENTS%NUM_SALTS+CONSTITUENT_MASS_MODULE ;   �	  �   a   CONSTITUENTS%SALTS+CONSTITUENT_MASS_MODULE ?   �
  �   a   CONSTITUENTS%SALTS_NUM+CONSTITUENT_MASS_MODULE >   +  �   a   CONSTITUENTS%NUM_POLL+CONSTITUENT_MASS_MODULE :   �  �   a   CONSTITUENTS%POLL+CONSTITUENT_MASS_MODULE >   l  �   a   CONSTITUENTS%POLL_NUM+CONSTITUENT_MASS_MODULE 9      �       CONSTITUENT_MASS+CONSTITUENT_MASS_MODULE >   �  �   a   CONSTITUENT_MASS%PEST+CONSTITUENT_MASS_MODULE >   $  �   a   CONSTITUENT_MASS%PATH+CONSTITUENT_MASS_MODULE >   �  �   a   CONSTITUENT_MASS%HMET+CONSTITUENT_MASS_MODULE >   L  �   a   CONSTITUENT_MASS%SALT+CONSTITUENT_MASS_MODULE B   �  �   a   CONSTITUENT_MASS%SALT_MIN+CONSTITUENT_MASS_MODULE >   t  �   a   CONSTITUENT_MASS%POLL+CONSTITUENT_MASS_MODULE .     R       CS_DB+CONSTITUENT_MASS_MODULE    Z  @       FRSOL    �  @       FRSRB '   �  �       CH_PESTICIDE_PROCESSES .   �  �   a   CH_PESTICIDE_PROCESSES%TOT_IN /   y  �   a   CH_PESTICIDE_PROCESSES%SOL_OUT /     �   a   CH_PESTICIDE_PROCESSES%SOR_OUT -   �  �   a   CH_PESTICIDE_PROCESSES%REACT -   k  �   a   CH_PESTICIDE_PROCESSES%METAB -     �   a   CH_PESTICIDE_PROCESSES%VOLAT .   �  �   a   CH_PESTICIDE_PROCESSES%SETTLE -   ]  �   a   CH_PESTICIDE_PROCESSES%RESUS -     �   a   CH_PESTICIDE_PROCESSES%DIFUS 1   �  �   a   CH_PESTICIDE_PROCESSES%REACT_BOT 1   O  �   a   CH_PESTICIDE_PROCESSES%METAB_BOT ,   �  �   a   CH_PESTICIDE_PROCESSES%BURY -   �  �   a   CH_PESTICIDE_PROCESSES%WATER /   A  �   a   CH_PESTICIDE_PROCESSES%BENTHIC $   �  Z       CH_PESTICIDE_OUTPUT )   A  �   a   CH_PESTICIDE_OUTPUT%PEST    �  \       CH_PESTBZ    M  �       CHPST_D    �  �       CHPST_M    �  �       CHPST_Y    <  �       CHPST_A    �  Y       BCHPST_D    :   Y       BCHPST_M    �   Y       BCHPST_Y    �   Y       BCHPST_A    E!  Y       CHPST    �!  Y       CHPSTZ $   �!  D      CH_PESTICIDE_HEADER (   ;#  �   a   CH_PESTICIDE_HEADER%DAY '   �#  �   a   CH_PESTICIDE_HEADER%MO +   �$  �   a   CH_PESTICIDE_HEADER%DAY_MO (   �%  �   a   CH_PESTICIDE_HEADER%YRC (   G&  �   a   CH_PESTICIDE_HEADER%ISD '   '  �   a   CH_PESTICIDE_HEADER%ID )   �'  �   a   CH_PESTICIDE_HEADER%NAME )   �(  �   a   CH_PESTICIDE_HEADER%PEST +   k)  �   a   CH_PESTICIDE_HEADER%TOT_IN ,   5*  �   a   CH_PESTICIDE_HEADER%SOL_OUT ,   �*  �   a   CH_PESTICIDE_HEADER%SOR_OUT *   �+  �   a   CH_PESTICIDE_HEADER%REACT *   �,  �   a   CH_PESTICIDE_HEADER%METAB *   ^-  �   a   CH_PESTICIDE_HEADER%VOLAT +   '.  �   a   CH_PESTICIDE_HEADER%SETTLE *   �.  �   a   CH_PESTICIDE_HEADER%RESUS *   �/  �   a   CH_PESTICIDE_HEADER%DIFUS .   �0  �   a   CH_PESTICIDE_HEADER%REACT_BOT .   O1  �   a   CH_PESTICIDE_HEADER%METAB_BOT )   2  �   a   CH_PESTICIDE_HEADER%BURY *   �2  �   a   CH_PESTICIDE_HEADER%WATER ,   �3  �   a   CH_PESTICIDE_HEADER%BENTHIC    z4  Y       CHPEST_HDR    �4  �       CHPEST_ADD     S5  d   a   CHPEST_ADD%CHO1     �5  d   a   CHPEST_ADD%CHO2    6  �       CHPEST_DIV    �6  d   a   CHPEST_DIV%CH1 !   �6  @   a   CHPEST_DIV%CONST    ?7  �       CHPEST_AVE    �7  d   a   CHPEST_AVE%CH1 !   #8  @   a   CHPEST_AVE%CONST 