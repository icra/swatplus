      !file created by ICRA
      
      subroutine exco_read_poll
    
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
      integer, dimension(:), allocatable :: recall_exco_poll_to_exco
      integer, dimension(:), allocatable :: pollutant_exco_poll_to_pollparm
      integer :: iexco

      
      
      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_polldb%exco_poll, exist=i_exist)
      if (i_exist .or. in_polldb%exco_poll /= "null") then
        do
          open (107,file=in_polldb%exco_poll)
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
          db_mx%exco_poll = imax
          
          allocate (recall_rec(0:imax))
          allocate (pollutant_pth(0:imax))
          allocate (exco(0:imax))
          allocate (exco_poll(0:db_mx%exco_om, 0:db_mx%pollparm))
          allocate (recall_exco_poll_to_exco(0:db_mx%exco_poll))
          allocate (pollutant_exco_poll_to_pollparm(0:db_mx%exco_poll))
          
          
          rewind (107)
        
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
      
          !read all export coefficient data
          do ii = 1, db_mx%exco_poll
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) recall_rec(ii), pollutant_pth(ii), exco(ii)
            if (eof < 0) exit
          end do
          
          
          do iexco_poll = 1, db_mx%exco_poll ! recall point in pollutats_om file
            recall_exco_poll_to_exco(iexco_poll) = -1
            pollutant_exco_poll_to_pollparm(iexco_poll) = -1

            do iexco = 1, db_mx%exco_om ! recall point in exco_om file
              if (exco_db(iexco)%om_file == recall_rec(iexco_poll)) then
                recall_exco_poll_to_exco(iexco_poll) = iexco
                exit
              end if
            end do
            
            do ipoll = 1, db_mx%pollparm
              if (polldb(ipoll)%name == pollutant_pth(iexco_poll)) then   ! ipoll is id of pollutant
                pollutant_exco_poll_to_pollparm(iexco_poll) = ipoll
                exit
              end if
            end do
            
            if ((recall_exco_poll_to_exco(iexco_poll) /= -1) .and. (pollutant_exco_poll_to_pollparm(iexco_poll) /= -1)) then
              exco_poll(recall_exco_poll_to_exco(iexco_poll), pollutant_exco_poll_to_pollparm(iexco_poll)) = exco(iexco_poll)
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
          obcs(iob)%hd(1)%poll(ipoll) = exco_poll(irec, ipoll)%load * 1000000
        end do


      end do
      return
      end subroutine exco_read_poll