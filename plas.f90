      subroutine plas(fplas,eh)
      use defheamc
      implicit none
      character*128, intent(in)  :: fplas     
      real, intent(out)         :: eh
      
      integer n
      character*3  :: cmp
      character*64 :: out
      character*128:: s
      logical      :: con
      
      con=.true.
      
      open(unit=10,file=trim(fplas),status='old')
      
      do while (con)
        read(10,'(a)') s
        read(s(11:13),*) cmp
	if (trim(cmp).eq.'Hyd') then
	  read(s(38:48),*) eh
	  con=.false.
	endif 
      enddo
      close(10)
      
      write(s,'(a)') 'rm -f '//trim(fplas)
      call system(s)
      
      return
      end subroutine plas
