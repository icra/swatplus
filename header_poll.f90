    ! ICRA copy of header_pest
    
     subroutine header_poll
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : res, sp_ob
     use output_ls_pesticide_module
     use constituent_mass_module
     use ch_pollutant_module
     use res_pollutant_module
     
     implicit none 

      
    !! CHANNEL_POLLUTANT - daily
     if (sp_ob%chandeg > 0) then
      if (pco%poll%d == "y" .and. cs_db%num_tot > 0) then
        open (2824,file="channel_poll_day.txt",recl=800)
        write (2824,*) bsn%name, prog
        write (9000,*) "CHANNEL_POLL              channel_poll_day.txt"
        write (2824,*) chpoll_hdr

          if (pco%csvout == "y") then
            open (2832,file="channel_poll_day.csv",recl=800)
            write (2832,*) bsn%name, prog
            write (2832,'(*(G0.3,:","))') chpoll_hdr
            write (9000,*) "CHANNEL_POLL              channel_poll_day.csv"
          end if
      end if
      
!! CHANNEL_POLLUTANT - monthly
      if (pco%poll%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2825,file="channel_poll_mon.txt",recl=800)
        write (2825,*) bsn%name, prog
        write (9000,*) "CHANNEL_POLL              channel_poll_mon.txt"
        write (2825,*) chpoll_hdr

          if (pco%csvout == "y") then
            open (2833,file="channel_poll_mon.csv",recl=800)
            write (2833,*) bsn%name, prog
            write (2833,'(*(G0.3,:","))') chpoll_hdr
            write (9000,*) "CHANNEL_POLL              channel_poll_mon.csv"
          end if
      end if
      
!! CHANNEL_POLLUTANT - yearly
      if (pco%poll%y == "y" .and. cs_db%num_tot > 0) then
        open (2826,file="channel_poll_yr.txt",recl=800)
        write (2826,*) bsn%name, prog
        write (9000,*) "CHANNEL_POLL              channel_poll_yr.txt"
        write (2826,*) chpoll_hdr

          if (pco%csvout == "y") then
            open (2834,file="channel_poll_yr.csv",recl=800)
            write (2834,*) bsn%name, prog
            write (2834,'(*(G0.3,:","))') chpoll_hdr
            write (9000,*) "CHANNEL_POLL              channel_poll_yr.csv"
          end if
      end if
      
!! CHANNEL_POLLUTANT - ave annual
      if (pco%poll%a == "y" .and. cs_db%num_tot > 0) then
        open (2827,file="channel_poll_aa.txt",recl=800)
        write (2827,*) bsn%name, prog
        write (9000,*) "CHANNEL_POLL              channel_poll_aa.txt"
        write (2827,*) chpoll_hdr

          if (pco%csvout == "y") then
            open (2835,file="channel_poll_aa.csv",recl=800)
            write (2835,*) bsn%name, prog
            write (2835,'(*(G0.3,:","))') chpoll_hdr
            write (9000,*) "CHANNEL_POLL              channel_poll_aa.csv"
          end if
      end if
     end if
      
!----------------------------------------
            
    !! RESERVOIR_POLLUTANT - daily
     if (sp_ob%res > 0) then
      if (pco%poll%d == "y" .and. cs_db%num_tot > 0) then
        open (2828,file="reservoir_poll_day.txt",recl=800)
        write (2828,*) bsn%name, prog
        write (9000,*) "RESERVOIR_POLL            reservoir_poll_day.txt"
        write (2828,*) respoll_hdr

          if (pco%csvout == "y") then
            open (2836,file="reservoir_poll_day.csv",recl=800)
            write (2836,*) bsn%name, prog
            write (2836,'(*(G0.3,:","))') respoll_hdr
            write (9000,*) "RESERVOIR_POLL            reservoir_poll_day.csv"           
          end if
      end if
      
!! RESERVOIR_POLLUTANT - monthly
      if (pco%poll%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2829,file="reservoir_poll_mon.txt",recl=800)
        write (2829,*) bsn%name, prog
        write (9000,*) "RESERVOIR_POLL            reservoir_poll_mon.txt"
        write (2829,*) respoll_hdr

          if (pco%csvout == "y") then
            open (2837,file="reservoir_poll_mon.csv",recl=800)
            write (2837,*) bsn%name, prog
            write (2837,'(*(G0.3,:","))') respoll_hdr
            write (9000,*) "RESERVOIR_POLL            reservoir_poll_mon.csv"
          end if
      end if
      
!! RESERVOIR_POLLUTANT - yearly
      if (pco%poll%y == "y" .and. cs_db%num_tot > 0) then
        open (2830,file="reservoir_poll_yr.txt",recl=800)
        write (2830,*) bsn%name, prog
        write (9000,*) "RESERVOIR_POLL            reservoir_poll_yr.txt"
        write (2830,*) respoll_hdr

          if (pco%csvout == "y") then
            open (2838,file="reservoir_poll_yr.csv",recl=800)
            write (2838,*) bsn%name, prog
            write (2838,'(*(G0.3,:","))') respoll_hdr
            write (9000,*) "RESERVOIR_POLL            reservoir_poll_yr.csv"
          end if
      end if
      
!! RESERVOIR_POLLUTANT - ave annual
      if (pco%poll%a == "y" .and. cs_db%num_tot > 0) then
        open (2831,file="reservoir_poll_aa.txt",recl=800)
        write (2831,*) bsn%name, prog
        write (9000,*) "RESERVOIR_POLL            reservoir_poll_aa.txt"
        write (2831,*) respoll_hdr

          if (pco%csvout == "y") then
            open (2839,file="reservoir_poll_aa.csv",recl=800)
            write (2839,*) bsn%name, prog
            write (2839,'(*(G0.3,:","))') respoll_hdr
            write (9000,*) "RESERVOIR_POLL            reservoir_poll_aa.csv"
          end if
      end if
     end if
         

      
      return
      end subroutine header_poll  