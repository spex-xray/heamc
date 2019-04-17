      ! HEAMC program to generate output directly comparable to other spectral codes
      ! Definition agreed on Lorentz Workshop: High Energy Astrophysical Model Comparison
      ! Leiden, August 8-12, 2016
      
      ! =================================
      ! Main program
      ! =================================
      program heamc
      use defheamc
      implicit none
      
      integer i,t
      
      write(*,'(a)') '============================================='
      write(*,'(a)') ' HEAMC program to generate output directly   '
      write(*,'(a)') ' comparable to other spectral codes.         '
      write(*,'(a)') ' '
      write(*,'(a)') ' Definition agreed on Lorentz Workshop:      '
      write(*,'(a)') '  High Energy Astrophysical Model Comparison '
      write(*,'(a)') '  Leiden, The Netherlands, August 8-12, 2016 '
      write(*,'(a)') '============================================='
      write(*,'(a)') ' '
      write(*,'(a)') ' Version 0.8 - October 10, 2016 '
      write(*,'(a)') ' '
      
      ! ===========================================
      ! CIE Model tests
      ! ===========================================      
      write(*,'(a)') '================================='
      write(*,'(a)') ' Calculate CIE tests             '
      write(*,'(a)') '================================='
      
      ! Perform calculation for CSD, lines and levels (CIE case)
      call spexlines('cie')
      
      !Write CSD to output format      
      call out_csd('cie')
      
      !Write Lines to output format
      call out_lines('cie')
      
      !Write Level populations to output
      call out_level()
      
      ! Perform power calculation for log temperature grid 
      ! between 10^4 and 10^9 K (temperature array)
      call spexpower() 
      call out_power() 
     
     
      ! ===========================================
      ! NEIJ Model tests
      ! ===========================================      
      write(*,'(a)') '================================='
      write(*,'(a)') ' Calculate NEIJ tests            '
      write(*,'(a)') '================================='
      
      ! Perform calculation for CSD, lines and levels (NEI case)
      call spexlines('nei')
      
      !Write CSD to output format      
      call out_csd('nei')
      
      !Write Lines to output format
      call out_lines('nei')
      
      ! Output of recombining NEI spectrum case
      call nei_cont()
      
      ! ===========================================
      ! PION Model tests
      ! ===========================================      
      write(*,'(a)') '================================='
      write(*,'(a)') ' Calculate PI models             '
      write(*,'(a)') '================================='
      
      ! Perform SPEX calculation for pion model
      call spexpion()
      
      ! Write pion CSD
      write(*,*) 'Write Pion CSD output...'
      call pi_csd()

      write(*,*) 'Write Pion Lines and absorption spectra...'
      do i=1,3
      
	! Write 100 strongest absorption lines to file
        call pi_lines(i)
      
        ! Write spectrum
	call pi_abs(i)
	
      enddo
      
      write(*,*) 'Write Pion Heating and Cooling contributions...'
      call pi_hc()
      
      write(*,'(a)') '================================='
      write(*,'(a)') ' Done.                           '
      write(*,'(a)') '================================='
                  
      end program
      
      
