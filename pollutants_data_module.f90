      module pollutants_data_module
    
      implicit none    
          
      type pollutants_db
        character(len=16) :: name   !!                      |pollutant name
        real :: solub = 0.          !! mg/L (ppm)           |solubility of chemical in water
        real :: aq_hlife = 0.       !! days                 |aquatic half-life
        real :: aq_volat = 0.       !! m/day                |aquatic volatilization coeff
        real :: mol_wt = 0.         !! g/mol                |molecular weight - to calulate mixing velocity
        real :: aq_resus = 0.       !! m/day                |aquatic resuspension velocity for pollutant sorbed to sediment
        real :: aq_settle = 0.      !! m/day                |aquatic settling velocity for pollutant sorbed to sediment
        real :: ben_act_dep = 0.    !! m                    |depth of active benthic layer
        real :: ben_bury = 0.       !! m/day                |burial velocity in benthic sediment
        real :: ben_hlife = 0.      !! days                 |half-life of pest in benthic sediment
        character(len=32) :: descrip                        !pollutant description
      end type pollutants_db
      type (pollutants_db), dimension(:), allocatable, save :: polldb
      
      type daughter_decay_fractions
        character(len=16) :: name   !! daughter pollutant name
        integer :: num              !! sequential pollutant number in simulation
        real :: aq_fr               !! 0-1                  |fraction of parent aquatic degrading to daughter
        real :: ben_fr              !! 0-1                  |fraction of parent benthic degrading to daughter
      end type daughter_decay_fractions
      
      type pollutant_cp         !! calculated parameters from input parms
        integer :: num_metab = 0          !! number of metabolites
        type (daughter_decay_fractions), dimension(:), allocatable :: daughter
        real :: decay_a = 0.        !! none                 |exp of the rate const for degradation of the pollutant in aquatic
        real :: decay_b = 0.        !! none                 |exp of the rate const for degradation of the pollutant in benthic layer
      end type pollutant_cp
      type (pollutant_cp), dimension(:), allocatable, save:: pollcp
      
      end module pollutants_data_module 