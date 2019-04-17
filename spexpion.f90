      ! =================================
      ! SPEX calculations for PION model
      ! =================================
      
      subroutine spexpion()
      use defheamc
      implicit none
      
      character*64            :: out
      integer                 :: xi
      
      open(11,file='spexpion.sh',status='replace')
      write(11,*) '#!/bin/bash'
      write(11,*) '$SPEX90/bin/spex << EOF'
      
      ! -- Set up the pion model
      write(11,*) 'comp pow'
      write(11,*) 'comp pion'
      write(11,*) 'comp rel 1 2'
      
      ! Set-up the power-law
      write(11,*) 'par 1 gam val 2.0'
      write(11,*) 'par 1 1 type val 1'
      write(11,*) 'par 1 1 elow val 1e-4'
      write(11,*) 'par 1 1 eup val 1e3'
      write(11,*) 'par 1 1 lum val 2.76e6'
      write(11,*) 'par 2 hden val 1e-6'
      write(11,*) 'par 1 2 nh val 1e-4'
      write(11,*) 'egrid read custom_egrid'
      write(11,*) 'iba bry'
      
      do xi=1,3
        
	write(11,*) 'par 2 xil val ', xi
        write(11,*) 'calc'
	
	! -- PION CSD
	write(out,'(I1)') xi
	write(out,*) 'pion_'//trim(ppls)//trim(out)
	write(11,'("asc file ", a," 1 2 plas")') trim(out)
	write(out,'(I1)') xi
	write(out,*) 'pion_'//trim(pico)//trim(out)
	write(11,'("asc file ", a," 1 2 icon")') trim(out)
	
	! -- PION Lines
	write(out,'(I1)') xi
	write(out,*) 'pion_'//trim(plin)//trim(out)
	write(11,'("asc file ", a," 1 2 trans sort -tau")') trim(out)
	
	! -- PION HeatCool
	write(out,'(I1)') xi
	write(out,*) 'pion_'//trim(phea)//trim(out)
	write(11,'("asc file ", a," 1 2 heat")') trim(out)
	
	! -- PION Abs spectrum
	write(11,*) 'plot dev null'
	write(11,*) 'plot type model'
	write(11,*) 'plot fil disp f'
	write(11,*) 'plot x lin'
	write(11,*) 'plot y lin'
	write(11,*) 'plot ux kev'
	write(11,*) 'plot rx 0.1 10'
	write(11,*) 'plot uy k'
	write(11,*) 'plot'
	write(out,'(I1)') xi
	write(out,*) 'pion_'//trim(pspc)//trim(out)
	write(11,'("plot adum ", a," over")') trim(out)
	
      enddo

      write(11,*) 'quit'
      write(11,'(A3)') 'EOF'
      write(11,*) ''
      close(11)

      write(*,*) 'Running SPEX pion tests...'

      call system('chmod u+x spexpion.sh')
      call system('./spexpion.sh >> /dev/null')
      call system('rm -f spexpion.sh')

      end subroutine 
