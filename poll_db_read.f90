      !File created by ICRA
      
      subroutine poll_db_read

      use basin_module
      use input_file_module
      use constituent_mass_module
      use maximum_data_module
      use pollutants_data_module

  
      implicit none


      ! We do not have an entry of pollutants in "constituents.cs" file, so we simulate
      ! all the pollutants defined in pollutants.def file
      !These operations could have been done in 'constit_db_read', but we prefer to 
      !do it here to avoid to modidying the original structure of the code
           
      logical :: i_exist              !none       |check to determine if file exists
      integer :: ipoll                !none       |counter


      inquire (file=in_polldb%pollutant_pol, exist=i_exist)
      if (.not. i_exist .or. in_polldb%pollutant_pol == "null") then
        allocate (cs_db%poll(0:0))
      
      else
      do
        cs_db%num_poll = db_mx%pollparm
        allocate (cs_db%poll(0:cs_db%num_poll))
        allocate (cs_db%poll_num(0:cs_db%num_poll))
        exit
      end do
      end if

      do ipoll = 1, cs_db%num_poll
        cs_db%poll(ipoll) = polldb(ipoll)%name

        cs_db%poll_num(ipoll) = ipoll
      end do  
      
      cs_db%num_tot = cs_db%num_tot + cs_db%num_poll
      
      return
      end subroutine poll_db_read