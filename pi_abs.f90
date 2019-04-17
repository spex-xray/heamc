      subroutine pi_abs(xi)
      use defheamc
      implicit none
      
      integer, intent(in)    :: xi
      
      character*128          :: out,s
      real                   :: binc, l, h, flx
      real                   :: binl, binh
      
      write(out,'(I1)') xi
      write(out,'(a)') 'pion_'//trim(pspc)//trim(out)//'.qdp'
      
      open(unit=12,file=trim(out),status='old')
      
      read(12,'(a)') s
      
      write(out,'(I1)') xi
      write(out,'(a)') 'spex_pi_abs'//trim(out)//'.dat'
      
      open(unit=13,file=trim(out),status='replace')
      
      write(13,*) 'BinLo     BinHi     Flux'
      
      do while (.true.)
        read(12,'(a)',end=2) s
	read(s(1:17),*) binc
	read(s(17:31),*) h
        read(s(32:46),*) l
	read(s(48:61),*) flx
	binl=binc+l
	binh=binc+h
	if ((binl.ge.1E-2).and.(binh.le.10)) then
	  ! Convert to photons/cm^2/s
	  flx=flx*1E-4*1E-3
          write(13,*) binl, binh, flx	
	endif
      enddo
2     close(12)
      close(13)
      
      end subroutine
