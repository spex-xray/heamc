      subroutine pi_lines(xi)
      use defheamc
      implicit none
      
      integer, intent(in)    :: xi
      integer                :: m, e, c, j
      character*128          :: out,s
      character*2            :: elem
      character*5            :: stage
      real                   :: lamb, tau

      write(out,'(I1)') xi
      write(*,'(a)') ' Write Pion Lines for Xi = '//trim(out)
      
      ! Open input file
      write(out,'(I1)') xi
      write(out,'(a)') 'pion_'//trim(plin)//trim(out)//'.asc'
      
      open(unit=13,file=trim(out),status='old')
      
      ! Open output file
      write(out,'(I1)') xi
      write(out,'(a)') 'spex_pi_lines'//trim(out)//'.dat'
      
      open(unit=14,file=trim(out),status='replace')
            
      read(13,'(a)') s
      read(13,'(a)') s
      write(14,*) '#      Indx   Lambda                   Z         Ion          Tau'
      
      m=0
      do while (m.lt.100)
        read(13,'(a)',end=2) s
	read(s(8:10),*) elem
	read(s(11:16),*) stage
	read(s(37:43),*) lamb
	read(s(52:63),*) tau 
	if (lamb.le.1000) then
	    do j=1,nel
	      if (trim(elem).eq.trim(elc(j))) then
	        e=j
	      endif 
	    enddo
	    do j=1,nel+1
	      if (trim(stage).eq.trim(roman(j))) then
	        c=j-1
	      endif
	    enddo
	    write(14,*) m, lamb, eli(e), c, tau
	    m=m+1
	endif
      enddo
2     close(13)
      close(14)
      
      end subroutine 
