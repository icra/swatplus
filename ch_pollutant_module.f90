      !fILE CREATED BY ICRA
      
      module ch_pollutant_module
    
      use constituent_mass_module, only : cs_db
      
      implicit none
              
      real :: poll_frsol             !none          |fraction of pollutant in reach that is soluble
      real :: poll_frsrb             !none          |fraction of pollutant in reach that is sorbed
      
      type ch_pollutant_processes
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
        real :: benthic = 0.            ! kg        !pollutant in benthic sediment at tend of day
      end type ch_pollutant_processes
      
      type ch_pollutant_output
        type (ch_pollutant_processes), dimension (:), allocatable :: poll         !pollutant hydrographs
      end type ch_pollutant_output
      type (ch_pollutant_processes) :: ch_pollbz
           
      type (ch_pollutant_output), dimension(:), allocatable, save :: chpoll_d
      type (ch_pollutant_output), dimension(:), allocatable, save :: chpoll_m
      type (ch_pollutant_output), dimension(:), allocatable, save :: chpoll_y
      type (ch_pollutant_output), dimension(:), allocatable, save :: chpoll_a
      type (ch_pollutant_output) :: bchpoll_d
      type (ch_pollutant_output) :: bchpoll_m
      type (ch_pollutant_output) :: bchpoll_y
      type (ch_pollutant_output) :: bchpoll_a
      type (ch_pollutant_output) :: chpoll, chpollz
                 
      type ch_pollutant_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name              "  
          character (len=16) :: poll =      " pollutant"
          character(len=13) :: tot_in =     "tot_in_mg "            ! (mg)
          character(len=13) :: sol_out =    "sol_out_mg "           ! (mg)
          character(len=14) :: sor_out =    "sor_out_mg "           ! (mg)
          character(len=13) :: react =      "react_h2o_mg "         ! (mg)
          character(len=13) :: metab =      "metab_h2o_mg "         ! (mg)
          character(len=12) :: volat =      "volat_mg "             ! (mg)
          character(len=12) :: settle =     "settle_mg "            ! (mg)
          character(len=13) :: resus =      "resuspend_mg "         ! (mg)
          character(len=12) :: difus =      "diffuse_mg "           ! (mg)
          character(len=15) :: react_bot =  "react_benth_mg "       ! (mg)
          character(len=15) :: metab_bot =  "metab_benth_mg "       ! (mg)
          character(len=14) :: bury =       "bury_benth_mg "        ! (mg)
          character(len=14) :: water =      "water_stor_mg "        ! (mg)
          character(len=12) :: benthic =    "benthic_mg "           ! (mg)
      end type ch_pollutant_header
      type (ch_pollutant_header) :: chpoll_hdr
     
      interface operator (+)
        module procedure chpoll_add
      end interface
      
      interface operator (/)
        module procedure chpoll_div
      end interface
        
      interface operator (//)
        module procedure chpoll_ave
      end interface 
             
      contains

      function chpoll_add(cho1, cho2) result (cho3)
        type (ch_pollutant_processes),  intent (in) :: cho1
        type (ch_pollutant_processes),  intent (in) :: cho2
        type (ch_pollutant_processes) :: cho3
        cho3%tot_in = cho1%tot_in + cho2%tot_in
        cho3%sol_out = cho1%sol_out + cho2%sol_out
        cho3%sor_out = cho1%sor_out + cho2%sor_out
        cho3%react = cho1%react + cho2%react
        cho3%metab = cho1%metab + cho2%metab
        cho3%volat = cho1%volat + cho2%volat
        cho3%settle = cho1%settle + cho2%settle
        cho3%resus = cho1%resus + cho2%resus
        cho3%difus = cho1%difus + cho2%difus
        cho3%react_bot = cho1%react_bot + cho2%react_bot
        cho3%metab_bot = cho1%metab_bot + cho2%metab_bot
        cho3%bury = cho1%bury + cho2%bury
        cho3%water = cho1%water + cho2%water
        cho3%benthic = cho1%benthic + cho2%benthic
      end function chpoll_add
      
      function chpoll_div (ch1, const) result (ch2)
        type (ch_pollutant_processes), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_pollutant_processes) :: ch2
          ch2%tot_in = ch1%tot_in / const
          ch2%sol_out = ch1%sol_out / const
          ch2%sor_out = ch1%sor_out / const
          ch2%react = ch1%react / const
          ch2%metab = ch1%metab / const
          ch2%volat = ch1%volat / const
          ch2%settle = ch1%settle / const
          ch2%resus = ch1%resus / const
          ch2%difus = ch1%difus / const
          ch2%react_bot = ch1%react_bot / const
          ch2%metab_bot = ch1%metab_bot / const
          ch2%bury = ch1%bury / const
          ch2%water = ch1%water / const
          ch2%benthic = ch1%benthic / const
      end function chpoll_div
      
      function chpoll_ave (ch1, const) result (ch2)
        type (ch_pollutant_processes), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_pollutant_processes) :: ch2
          ch2%tot_in = ch1%tot_in
          ch2%sol_out = ch1%sol_out
          ch2%sor_out = ch1%sor_out
          ch2%react = ch1%react
          ch2%metab = ch1%metab
          ch2%volat = ch1%volat
          ch2%settle = ch1%settle
          ch2%resus = ch1%resus
          ch2%difus = ch1%difus
          ch2%react_bot = ch1%react_bot
          ch2%metab_bot = ch1%metab_bot
          ch2%bury = ch1%bury
          ch2%water = ch1%water
          ch2%benthic = ch1%benthic
      end function chpoll_ave
      
      end module ch_pollutant_module