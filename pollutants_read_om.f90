      subroutine pollutants_read_om
    
      use input_file_module
      use maximum_data_module
      use pollutant_module
      use pollutants_data_module
      use exco_module
      use hydrograph_module, only : sp_ob1, sp_ob, ob
      use constituent_mass_module, only : obcs


      
      character (len=80) :: titldum, header
      integer :: eof, imax, ob1, ob2
      logical :: i_exist              !none       |check to determine if file exists
      integer, dimension(:), allocatable :: recall_poll_om_to_exco
      integer, dimension(:), allocatable :: pollutant_poll_om_to_pollparm
      integer :: iexco

      
      
      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_polldb%pollutant_om, exist=i_exist)
      if (i_exist .or. in_polldb%pollutant_om /= "null") then
        do
          open (107,file=in_polldb%pollutant_om)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          db_mx%poll_om = imax
          
          allocate (recall_rec(0:imax))
          allocate (pollutant_pth(0:imax))
          allocate (exco(0:imax))
          allocate (poll_om(0:db_mx%exco_om, 0:db_mx%pollparm))
          allocate (recall_poll_om_to_exco(0:db_mx%poll_om))
          allocate (pollutant_poll_om_to_pollparm(0:db_mx%poll_om))
          
          
          rewind (107)
        
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%poll_om
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) recall_rec(ii), pollutant_pth(ii), exco(ii)
            if (eof < 0) exit
          end do
          
          
          do iexco_om = 1, db_mx%poll_om ! recall point in poll_om file
            recall_poll_om_to_exco(iexco_om) = -1
            pollutant_poll_om_to_pollparm(iexco_om) = -1

            do iexco = 1, db_mx%exco_om ! recall point in exco_om file
              if (exco_db(iexco)%om_file == recall_rec(iexco_om)) then
                recall_poll_om_to_exco(iexco_om) = iexco
                exit
              end if
            end do
            
            do ipoll = 1, db_mx%pollparm
              if (polldb(ipoll)%name == pollutant_pth(iexco_om)) then   ! ipoll is id of pollutant
                pollutant_poll_om_to_pollparm(iexco_om) = ipoll
                exit
              end if
            end do
            
            if ((recall_poll_om_to_exco(iexco_om) /= -1) .and. (pollutant_poll_om_to_pollparm(iexco_om) /= -1)) then
              poll_om(recall_poll_om_to_exco(iexco_om), pollutant_poll_om_to_pollparm(iexco_om)) = exco(iexco_om)
            end if
            
          end do
          
          close (107)
          
          exit
          
          
        end do
      end if

      
      !set exco_om object hydrograph
      do irec = 1, sp_ob%exco
        iob = sp_ob1%exco + irec - 1
        do ipoll = 1, db_mx%pollparm
          obcs(iob)%hd(1)%poll(ipoll) = poll_om(irec, ipoll)%load
        end do


      end do
      return
      end subroutine pollutants_read_om