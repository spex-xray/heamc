      subroutine nei_cont()
      use defheamc
      implicit none
      
      character*64    :: out,s,cmp
      real            :: bc, bu, bl, fl, eh, factor
      logical         :: con
      
      ! Write spectrum to QDP file
      
      write(*,*) 'Calculating NEI Recombining plasma spectrum...'
      
      open(11,file='spexcont.sh',status='replace')
      write(11,'(a)') '#!/bin/bash'
      write(11,'(a)') '$SPEX90/bin/spex << EOF'
      
      write(11,*) 'com neij'
      write(11,*) 'var calc new'
      write(11,*) 'ibal u16'
      write(11,*) 'abun lodd'
      
      write(11,*) 'par 1 1 u v 1E-4'
      write(out,*) 'par 1 1 t1 v 3.5'
      write(11,*) out
      write(out,*) 'par 1 1 t2 v 1.5'
      write(11,*) out

      write(11,*) 'egrid lin 0.01:10 step 1E-3 kev'
      write(11,*) 'plot dev null'
      write(11,*) 'plot type model'
      write(11,*) 'plot x lin'
      write(11,*) 'plot y lin'
      write(11,*) 'calc'
      write(11,*) 'plot ux k'
      write(11,*) 'plot uy k'
      write(11,*) 'plot rx 0.01:10'
      write(11,*) 'plot adum nei-cont over'
      
      write(11,*) 'asc file nei-cont 1 1 plas'
      write(11,*) 'quit'
      write(11,'(A3)') 'EOF'
      write(11,*) ''
      
      close(11)
      
      call system('chmod u+x spexcont.sh')
      call system('./spexcont.sh > /dev/null')
      call system('rm -f spexcont.sh')
      
      ! Obtain electron/hydrogen density ratio
      
      con=.true.
      
      write(*,*) 'Writing NEI Recombining plasma spectrum to output file...'
      
      open(unit=10,file='nei-cont.asc',status='old')
      
      do while (con)
        read(10,'(a)') s
        read(s(11:13),*) cmp
	if (trim(cmp).eq.'Hyd') then
	  read(s(39:45),*) eh
	  con=.false.
	endif 
      enddo
      close(10)
      call system('rm -f nei-cont.asc') 
      
      factor = 1E-8*4*pi/eh  ! Conversion from flux to emissivity
      
      ! Write spectrum to agreed output format & units
      
      open(unit=13,file='spex_nei_cont.dat',status='replace')
      write(13,'(a)') '#  Full NEI spectrum for recombining plasma (3.5 -> 1.5 keV)'
      write(13,'(a)') '#  BinLo            BinHi            Flux'
      
      open(unit=12,file='nei-cont.qdp',status='old')
      read(12,'(a)') s
      do while (.true.)
        read(12,'(a)',end=1) s
	read(s,*) bc, bu, bl, fl
	write(13,*) bc+bl, bc+bu, factor*fl 
      enddo
      
1     close(12)
      close(13)
      call system('rm -f nei-cont.qdp')
      
      end subroutine nei_cont
