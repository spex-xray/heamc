      ! =================================
      ! Calculate radiative power of 
      ! lines between 1-1000 Rydberg
      ! =================================
      
      subroutine spexpower()
      use defheamc
      implicit none
      character*128 :: out,tem,infile,outfile,command
      integer      :: i
      real         :: eh
      
      write(*,*) 'Run SPEX CIE line power calculation for temperature grid...'
      
      call setktarr()
      
      open(12,file='spexpow.sh',status='replace')
      write(12,*) '#!/bin/bash'
      write(12,*) '$SPEX90/bin/spex << EOF'
      write(12,*) 'com cie'
      write(12,*) 'var calc new'
      write(12,*) 'ions ignore all'
      do i=1,nel
        write(12,*) 'ions use z ', eli(i) 
      enddo
      do i=1,nkt
        write(out,*) 'par 1 1 t v ', ktarr(i)
        write(12,*) out
	write(12,*) 'calc'
        
	write(tem,'(F3.1)') log10(ktarr(i)/kttokev)
	write(out,'(a)') 'elhyd_'//trim(tem)
	write(12,'("asc file ",a," 1 1 plas")') trim(out)
	write(infile,'(a)') trim(out)//'.asc'
	
	write(out,*) trim(radp)//trim(tem)
        write(12,'("asc file ",a," 1 1 line")') trim(out)
      enddo  
      write(12,*) 'quit'
      write(12,'(A3)') 'EOF'
      
      close(12)
      call system('chmod u+x spexpow.sh')
      call system('./spexpow.sh > /dev/null')
      call system('rm -f spexpow.sh')
      
      end subroutine spexpower
