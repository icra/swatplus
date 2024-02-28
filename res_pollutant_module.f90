      
      !ICRA copy of res_pesticide_module.f90
      
      
      module res_pollutant_module

      implicit none
              
      type res_pollutant_processes
        real :: tot_in = 0.             ! kg        !total pollutant into reservoir
        real :: sol_out = 0.            ! kg        !soluble pollutant out of reservoir
        real :: sor_out = 0.            ! kg        !sorbed pollutant out of reservoir
        real :: react = 0.              ! kg        !pollutant lost through reactions in water layer
        real :: metab = 0.              ! kg        !pollutant metabolized from parent in water layer
        real :: volat = 0.              ! kg        !pollutant lost through volatilization
        real :: settle = 0.             ! kg        !pollutant settling to sediment layer
        real :: resus = 0.              ! kg        !pollutant resuspended into lake water
        real :: difus = 0.              ! kg        !pollutant diffusing from sediment to water
        real :: react_bot = 0.          ! kg        !pollutant lost from benthic sediment by reactions
        real :: metab_bot = 0.          ! kg        !pollutant metabolized from parent in water layer
        real :: bury = 0.               ! kg        !pollutant lost from benthic sediment by burial
        real :: water = 0.              ! kg        !pollutant in water at end of day
        real :: benthic = 0.            ! kg        !pollutant in benthic sediment at end of day
      end type res_pollutant_processes
      
      type res_pollutant_output
        type (res_pollutant_processes), dimension (:), allocatable :: poll         !pollutant hydrographs
      end type res_pollutant_output
      type (res_pollutant_processes) :: res_pollbz
           
      type (res_pollutant_output), dimension(:), allocatable, save :: respoll_d
      type (res_pollutant_output), dimension(:), allocatable, save :: respoll_m
      type (res_pollutant_output), dimension(:), allocatable, save :: respoll_y
      type (res_pollutant_output), dimension(:), allocatable, save :: respoll_a
      type (res_pollutant_output) :: brespoll_d
      type (res_pollutant_output) :: brespoll_m
      type (res_pollutant_output) :: brespoll_y
      type (res_pollutant_output) :: brespoll_a
      type (res_pollutant_output) :: respoll, respollz
                 
      type res_pollutant_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name"
          character (len=16) :: poll =      " pollutant"
          character(len=13) :: tot_in =     "tot_in_mg "            ! (mg)
          character(len=13) :: sol_out =    "sol_out_mg "           ! (mg)
          character(len=14) :: sor_out =    "sor_out_mg "           ! (mg)
          character(len=13) :: react =      "react_h2o_mg"        	! (mg)
          character(len=13) :: metab =      "metab_h2o_mg"        	! (mg)
          character(len=10) :: volat =      "volat_mg"        		! (mg)
          character(len=10) :: settle =     "settle_mg"        		! (mg)
          character(len=13) :: resus =      "resuspend_mg"        	! (mg)
          character(len=11) :: difus =      "diffuse_mg "        	! (mg)
          character(len=15) :: react_bot =  "react_benth_mg "       ! (mg)
          character(len=14) :: metab_bot =  "metab_benth_mg "       ! (mg)
          character(len=14) :: bury =       "bury_benth_mg "        ! (mg)
          character(len=14) :: water =      "water_stor_mg "        ! (mg)
          character(len=13) :: benthic =    "benthic_mg"        	! (mg)
      end type res_pollutant_header
      type (res_pollutant_header) :: respoll_hdr
     
      interface operator (+)
        module procedure respoll_add
      end interface
      
      interface operator (/)
        module procedure respoll_div
      end interface
        
      interface operator (//)
        module procedure respoll_ave
      end interface 
             
      contains
!! routines for swatdeg_hru module

      function respoll_add(res1, res2) result (res3)
        type (res_pollutant_processes),  intent (in) :: res1
        type (res_pollutant_processes),  intent (in) :: res2
        type (res_pollutant_processes) :: res3
        res3%tot_in = res1%tot_in + res2%tot_in
        res3%sol_out = res1%sol_out + res2%sol_out
        res3%sor_out = res1%sor_out + res2%sor_out
        res3%react = res1%react + res2%react
        res3%metab = res1%metab + res2%metab
        res3%volat = res1%volat + res2%volat
        res3%settle = res1%settle + res2%settle
        res3%resus = res1%resus + res2%resus
        res3%difus = res1%difus + res2%difus
        res3%react_bot = res1%react_bot + res2%react_bot
        res3%metab_bot = res1%metab_bot + res2%metab_bot
        res3%bury = res1%bury + res2%bury
        res3%water = res1%water + res2%water
        res3%benthic = res1%benthic + res2%benthic
      end function respoll_add
      
      function respoll_div (res1, const) result (res2)
        type (res_pollutant_processes), intent (in) :: res1
        real, intent (in) :: const
        type (res_pollutant_processes) :: res2
          res2%tot_in = res1%tot_in / const
          res2%sol_out = res1%sol_out / const
          res2%sor_out = res1%sor_out / const
          res2%react = res1%react / const
          res2%metab = res1%metab / const
          res2%volat = res1%volat / const
          res2%settle = res1%settle / const
          res2%resus = res1%resus / const
          res2%difus = res1%difus / const
          res2%react_bot = res1%react_bot / const
          res2%metab_bot = res1%metab_bot / const
          res2%bury = res1%bury / const
          res2%water = res1%water / const
          res2%benthic = res1%benthic / const
      end function respoll_div
      
      function respoll_ave (res1, const) result (res2)
        type (res_pollutant_processes), intent (in) :: res1
        real, intent (in) :: const
        type (res_pollutant_processes) :: res2
          res2%tot_in = res1%tot_in
          res2%sol_out = res1%sol_out
          res2%sor_out = res1%sor_out
          res2%react = res1%react
          res2%metab = res1%metab
          res2%volat = res1%volat
          res2%settle = res1%settle
          res2%resus = res1%resus
          res2%difus = res1%difus
          res2%react_bot = res1%react_bot
          res2%metab_bot = res1%metab_bot
          res2%bury = res1%bury
          res2%water = res1%water
          res2%benthic = res1%benthic
      end function respoll_ave
      
      end module res_pollutant_module