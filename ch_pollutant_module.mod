  c8  ]   k820309    �          2021.9.0    �Zd                                                                                                          
       ch_pollutant_module.f90 CH_POLLUTANT_MODULE                                                     
       CS_DB                                                            #CHPOLL_ADD                                                               #CHPOLL_DIV                                                               #CHPOLL_AVE                      @              E                '�                   #NUM_TOT    #NUM_PESTS    #PESTS    #PEST_NUM 	   #NUM_PATHS 
   #PATHS    #PATH_NUM    #NUM_METALS    #METALS    #METALS_NUM    #NUM_SALTS    #SALTS    #SALTS_NUM    #NUM_POLL    #POLL    #POLL_NUM                �                                                                                                                                0                �                                                                                                                               0    .           �                                                                       &                                                              �                               	            P                             &                                                       �                               
     �                                                                                           0    .           �                                          �                             &                                                              �                                           �                             &                                                       �                                    0                                                                                          0    .           �                                          8             	               &                                                              �                                           �             
               &                                                       �                                    �                                                                                          0    .           �                                          �                            &                                                              �                                                                       &                                                       �                                    `                                                                                          0    .           �                                          h                            &                                                              �                                           �                            &                                                             @             D                '�                   #PEST    #PATH    #HMET    #SALT    #SALT_MIN    #POLL               �                                                             	            &                                                      �                                           H                 	            &                                                      �                                           �                 	            &                                                      �                                           �                 	            &                                                      �                                                            	            &                                                      �                                           h                	            &                                                                                             �      #CONSTITUENTS                                                     	                                                        	                         @                              '8                    #TOT_IN !   #SOL_OUT "   #SOR_OUT #   #REACT $   #METAB %   #VOLAT &   #SETTLE '   #RESUS (   #DIFUS )   #REACT_BOT *   #METAB_BOT +   #BURY ,   #WATER -   #BENTHIC .               �                               !               	                                                 	                                 0.                �                               "              	                                                 	                                 0.                �                               #              	                                                 	                                 0.                �                               $              	                                                 	                                 0.                �                               %              	                                                 	                                 0.                �                               &              	                                                 	                                 0.                �                               '              	                                                 	                                 0.                �                               (              	                                                 	                                 0.                �                               )             	  	                                                 	                                 0.                �                               *     $       
  	                                                 	                                 0.                �                               +     (         	                                                 	                                 0.                �                               ,     ,         	                                                 	                                 0.                �                               -     0         	                                                 	                                 0.                �                               .     4         	                                                 	                                 0.                      @              A           /     'H                    #POLL 0              �                               0                    8             #CH_POLLUTANT_PROCESSES               &                                                                                       1     8       #CH_POLLUTANT_PROCESSES                                               2            H                        &                                           #CH_POLLUTANT_OUTPUT /                                             3            H                        &                                           #CH_POLLUTANT_OUTPUT /                                             4            H                        &                                           #CH_POLLUTANT_OUTPUT /                                             5            H                        &                                           #CH_POLLUTANT_OUTPUT /                                               6     H       #CH_POLLUTANT_OUTPUT /                                               7     H       #CH_POLLUTANT_OUTPUT /                                               8     H       #CH_POLLUTANT_OUTPUT /                                               9     H       #CH_POLLUTANT_OUTPUT /                                               :     H       #CH_POLLUTANT_OUTPUT /                                               ;     H       #CH_POLLUTANT_OUTPUT /                     @                         <     '                   #DAY =   #MO >   #DAY_MO ?   #YRC @   #ISD A   #ID B   #NAME C   #POLL D   #TOT_IN E   #SOL_OUT F   #SOR_OUT G   #REACT H   #METAB I   #VOLAT J   #SETTLE K   #RESUS L   #DIFUS M   #REACT_BOT N   #METAB_BOT O   #BURY P   #WATER Q   #BENTHIC R               �                              =                                                                                                          C  jday                                �                              >                                                                                                         C   mon                                �                              ?                                                                                                         C   day                                �                              @                                                                                                         C    yr                                �                              A                                                                                  	                       C   unit                                 �                              B                                                                                   	                       C gis_id                                 �                              C            (                                                                                             C name                                           �                              D            8                                                                                             C pollutant                                      �                              E            H       	                                                                                      Ctot_in_kg                                    �                              F            U       
                                                                                      Csol_out_kg                                   �                              G            b                                                                                             Csor_out_kg                                    �                              H            p                                                                                             Creact_h2o_kg                                 �                              I            }                                                                                             Cmetab_h2o_kg                                 �                              J            �                                                                                             Cvolat_kg                                    �                              K            �                                                                                             Csettle_kg                                   �                              L            �                                                                                             Cresuspend_kg                                 �                              M            �                                                                                             Cdiffuse_kg                                  �                              N            �                                                                                             Creact_benth_kg                                 �                              O            �                                                                                             Cmetab_benth_kg                                 �                              P            �                                                                                             Cbury_benth_kg                                 �                              Q            �                                                                                             Cwater_stor_kg                                 �                              R            �                                                                                             Cbenthic_kg                                                                  S           #CH_POLLUTANT_HEADER <   &         @     X                                 8                      #CHO1 T   #CHO2 U   #CH_POLLUTANT_PROCESSES               
                                  T     8              #CH_POLLUTANT_PROCESSES               
                                  U     8              #CH_POLLUTANT_PROCESSES     &         @     X                                 8                      #CH1 V   #CONST W   #CH_POLLUTANT_PROCESSES               
                                  V     8              #CH_POLLUTANT_PROCESSES               
                                  W     	      &         @     X                                 8                      #CH1 X   #CONST Y   #CH_POLLUTANT_PROCESSES               
                                  X     8              #CH_POLLUTANT_PROCESSES               
                                  Y     	         �   4      fn#fn (   �   F   J  CONSTITUENT_MASS_MODULE      P      i@    j  P      i@    �  P      i@ 5   
  (      CONSTITUENTS+CONSTITUENT_MASS_MODULE =   2  �   a   CONSTITUENTS%NUM_TOT+CONSTITUENT_MASS_MODULE ?   �  �   a   CONSTITUENTS%NUM_PESTS+CONSTITUENT_MASS_MODULE ;   |  �   a   CONSTITUENTS%PESTS+CONSTITUENT_MASS_MODULE >     �   a   CONSTITUENTS%PEST_NUM+CONSTITUENT_MASS_MODULE ?   �  �   a   CONSTITUENTS%NUM_PATHS+CONSTITUENT_MASS_MODULE ;   Q  �   a   CONSTITUENTS%PATHS+CONSTITUENT_MASS_MODULE >   �  �   a   CONSTITUENTS%PATH_NUM+CONSTITUENT_MASS_MODULE @   �  �   a   CONSTITUENTS%NUM_METALS+CONSTITUENT_MASS_MODULE <   &  �   a   CONSTITUENTS%METALS+CONSTITUENT_MASS_MODULE @   �  �   a   CONSTITUENTS%METALS_NUM+CONSTITUENT_MASS_MODULE ?   V	  �   a   CONSTITUENTS%NUM_SALTS+CONSTITUENT_MASS_MODULE ;   �	  �   a   CONSTITUENTS%SALTS+CONSTITUENT_MASS_MODULE ?   �
  �   a   CONSTITUENTS%SALTS_NUM+CONSTITUENT_MASS_MODULE >   +  �   a   CONSTITUENTS%NUM_POLL+CONSTITUENT_MASS_MODULE :   �  �   a   CONSTITUENTS%POLL+CONSTITUENT_MASS_MODULE >   l  �   a   CONSTITUENTS%POLL_NUM+CONSTITUENT_MASS_MODULE 9      �       CONSTITUENT_MASS+CONSTITUENT_MASS_MODULE >   �  �   a   CONSTITUENT_MASS%PEST+CONSTITUENT_MASS_MODULE >   $  �   a   CONSTITUENT_MASS%PATH+CONSTITUENT_MASS_MODULE >   �  �   a   CONSTITUENT_MASS%HMET+CONSTITUENT_MASS_MODULE >   L  �   a   CONSTITUENT_MASS%SALT+CONSTITUENT_MASS_MODULE B   �  �   a   CONSTITUENT_MASS%SALT_MIN+CONSTITUENT_MASS_MODULE >   t  �   a   CONSTITUENT_MASS%POLL+CONSTITUENT_MASS_MODULE .     R       CS_DB+CONSTITUENT_MASS_MODULE    Z  @       FRSOL    �  @       FRSRB '   �  �       CH_POLLUTANT_PROCESSES .   �  �   a   CH_POLLUTANT_PROCESSES%TOT_IN /   y  �   a   CH_POLLUTANT_PROCESSES%SOL_OUT /     �   a   CH_POLLUTANT_PROCESSES%SOR_OUT -   �  �   a   CH_POLLUTANT_PROCESSES%REACT -   k  �   a   CH_POLLUTANT_PROCESSES%METAB -     �   a   CH_POLLUTANT_PROCESSES%VOLAT .   �  �   a   CH_POLLUTANT_PROCESSES%SETTLE -   ]  �   a   CH_POLLUTANT_PROCESSES%RESUS -     �   a   CH_POLLUTANT_PROCESSES%DIFUS 1   �  �   a   CH_POLLUTANT_PROCESSES%REACT_BOT 1   O  �   a   CH_POLLUTANT_PROCESSES%METAB_BOT ,   �  �   a   CH_POLLUTANT_PROCESSES%BURY -   �  �   a   CH_POLLUTANT_PROCESSES%WATER /   A  �   a   CH_POLLUTANT_PROCESSES%BENTHIC $   �  Z       CH_POLLUTANT_OUTPUT )   A  �   a   CH_POLLUTANT_OUTPUT%POLL    �  \       CH_POLLBZ    M  �       CHPOLL_D    �  �       CHPOLL_M    �  �       CHPOLL_Y    <  �       CHPOLL_A    �  Y       BCHPOLL_D    :   Y       BCHPOLL_M    �   Y       BCHPOLL_Y    �   Y       BCHPOLL_A    E!  Y       CHPOLL    �!  Y       CHPOLLZ $   �!  D      CH_POLLUTANT_HEADER (   ;#  �   a   CH_POLLUTANT_HEADER%DAY '   �#  �   a   CH_POLLUTANT_HEADER%MO +   �$  �   a   CH_POLLUTANT_HEADER%DAY_MO (   �%  �   a   CH_POLLUTANT_HEADER%YRC (   G&  �   a   CH_POLLUTANT_HEADER%ISD '   '  �   a   CH_POLLUTANT_HEADER%ID )   �'  �   a   CH_POLLUTANT_HEADER%NAME )   �(  �   a   CH_POLLUTANT_HEADER%POLL +   k)  �   a   CH_POLLUTANT_HEADER%TOT_IN ,   5*  �   a   CH_POLLUTANT_HEADER%SOL_OUT ,   �*  �   a   CH_POLLUTANT_HEADER%SOR_OUT *   �+  �   a   CH_POLLUTANT_HEADER%REACT *   �,  �   a   CH_POLLUTANT_HEADER%METAB *   ^-  �   a   CH_POLLUTANT_HEADER%VOLAT +   '.  �   a   CH_POLLUTANT_HEADER%SETTLE *   �.  �   a   CH_POLLUTANT_HEADER%RESUS *   �/  �   a   CH_POLLUTANT_HEADER%DIFUS .   �0  �   a   CH_POLLUTANT_HEADER%REACT_BOT .   O1  �   a   CH_POLLUTANT_HEADER%METAB_BOT )   2  �   a   CH_POLLUTANT_HEADER%BURY *   �2  �   a   CH_POLLUTANT_HEADER%WATER ,   �3  �   a   CH_POLLUTANT_HEADER%BENTHIC    z4  Y       CHPOLL_HDR    �4  �       CHPOLL_ADD     S5  d   a   CHPOLL_ADD%CHO1     �5  d   a   CHPOLL_ADD%CHO2    6  �       CHPOLL_DIV    �6  d   a   CHPOLL_DIV%CH1 !   �6  @   a   CHPOLL_DIV%CONST    ?7  �       CHPOLL_AVE    �7  d   a   CHPOLL_AVE%CH1 !   #8  @   a   CHPOLL_AVE%CONST 