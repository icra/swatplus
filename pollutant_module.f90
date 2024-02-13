      module pollutant_module
            
      implicit none
            
      type poll_output
        integer :: jday = 0.               
        integer :: mo = 0.              
        integer :: day_mo = 0.              
        integer :: yr = 0.              
        real :: load = 0.
      end type poll_output
              

      type (poll_output), dimension(:), allocatable :: exco        !export coefficient
      character(len=16), dimension(:), allocatable :: recall_rec
      character(len=16), dimension(:), allocatable :: pollutant_pth
      integer, dimension(:), allocatable :: poll_om_num
      type (poll_output), dimension(:, :), allocatable :: poll_om

    
              
      end module pollutant_module