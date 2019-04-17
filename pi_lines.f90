      subroutine pi_lines(xi)
      use defheamc
      implicit none
      
      integer, intent(in)    :: xi
      integer                :: m, e, c, j, io
      character*128          :: out,s,f1,f2
      character*2            :: elem
      character*5            :: stage
      real                   :: lamb, tau
      logical                :: break

      write(out,'(I1)') xi
      write(*,'(a)') ' Write Pion Lines for Xi = '//trim(out)
      
      ! Open input file
      write(out,'(I1)') xi
      write(f1,'(a)') 'pion_'//trim(plin)//trim(out)//'.asc'
      write(f2,'(a)') 'pion_'//trim(plin)//trim(out)//'.asc.rev'

      ! Reverse the order of the input file (to have highest tau up)
      write(out,'(a)') 'tac '//trim(f1)//' > '//trim(f2)
      call system(out)

      open(unit=13,file=trim(f2),status='old')
      
      ! Open output file
      write(s,'(I1)') xi
      write(out,'(a)') 'spex_pi_lines'//trim(s)//'.dat'
      
      open(unit=14,file=trim(out),status='replace')
            
      write(14,*) '#      Indx   Lambda                   Z         Ion          Tau'

      break=.true.
      do while (break)
        read(13,'(a)') s 
        if (s(1:3).eq.'ion') then
          break=.false.
        endif
      enddo

      read(13,'(a)',iostat=io)
      
      m=0
      do while (m.lt.100)
        read(13,'(a)') s
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
	    do j=1,31
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
