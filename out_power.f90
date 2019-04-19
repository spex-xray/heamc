      subroutine out_power()
      use defheamc
      implicit none
      
      character*128             :: infile,outfile
      character*256             :: s,in,tem
      character*1024            :: lin
      character*2               :: elnam
      
      real*8                                 :: ener, pow
      real*8, parameter                      :: kevtoerg=1.602176487D-9
      real                                   :: eh
      
      real*8, dimension(nkt,nel)             :: tab
      
      integer   :: i,j
      
      write(*,*) 'Reading SPEX CIE line powers from SPEX output...'
      
      call setktarr()
      
      tab=0.d0
      
      do i=1,nkt
        write(tem,'(F3.1)') log10(ktarr(i)/kttokev)
	write(s,'(a)') 'elhyd_'//trim(tem)//'.asc'

        call plas(s,eh)
      
        write(infile,'(a)') trim(radp)//trim(tem)//'.asc'
        
	open(unit=14,file=trim(infile),status='old')      
      
        read(14,'(a)') s
        read(14,'(a)') s
      
        do while (.true.)
          read(14,'(a)',end=5) s
	  read(s(11:13),'(a)') elnam
	  read(s(87:102),*) ener
	  read(s(117:128),*) pow
          if ((ener.ge.0.0136).and.(ener.le.13.6)) then
	    do j=1,nel
	      if (elnam.eq.elc(j)) then
	        ! Convert to requested units and emission measure
	        tab(i,j)=tab(i,j)+pow*ener*kevtoerg/(eh*1D52) 
	      endif
	    enddo
	  endif
	enddo
5	close(14)
        write(in,'(a)') 'rm -f '//trim(infile)
	call system(in)
      enddo
      
      write(s,'(a)') 'rm -f '//trim(infile)
      call system(trim(s))
      
      write(*,*) 'Writing SPEX CIE line powers to file...'
      
      ! Open output file
      open(unit=15,file='spex_cie_power.dat',status='replace')
      
      ! Set up first row of output file
      write(lin,'(a)') ' Z'
      do i=1,nkt
        write(tem,'(F3.1)') log10(ktarr(i)/kttokev)
        write(lin,'(a)') trim(lin)//'        '//trim(tem)
      enddo
      
      write(15,'(a)') trim(lin)
      
      do j=1,nel
        write(lin,'(I2)') eli(j)
	do i=1,nkt
	  if (tab(i,j).gt.0.0) then
	    write(s,'(F10.5)') log10(tab(i,j))
	  else
	    write(s,'(F10.5)') log10(tiny(tab(i,j)))
	  endif
	  write(lin,'(a)') trim(lin)//' '//trim(s)
	enddo
	write(15,'(a)') trim(lin)
      enddo
      
      close(15)
      
      end subroutine
