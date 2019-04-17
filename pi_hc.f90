      subroutine pi_hc()
      use defheamc
      implicit none
      
      character*128      :: out,s
      real               :: val
      real               :: hcomp, hpion, htot
      real               :: ccomp, cbrem, crecl, ctot
      integer            :: i,j
      
      ! Define output filename
      write(out,'(a)') 'spex_pi_heatcool.dat'
      open(unit=13,file=out,status='replace')
      
      ! Write first line of output file
      write(13,'(a)') '# LogXi    HeatCompton   HeatPhotoIon   HeatTotal   CoolCompton   CoolBrems  CoolRecAndLine CoolTotal' 
      
      ! Read input files and write line to output
      do i=1,3
        write(out,'(I1)') i
        write(out,'(a)') 'pion_'//trim(phea)//trim(out)//'.asc'
        
	open(unit=12,file=out,status='old')
        
	read(12,'(a)') s
	
	j=1
	hcomp=0.; hpion=0.; htot=0.
	ccomp=0.; cbrem=0.; crecl=0.; ctot=0.
	
	do while (.true.)
	  read(12,'(a)',end=2) s
	  if (j.ne.12) read(s(43:54),*) val
	  select case (j)
	    case (1)
	      hcomp=hcomp+val*0.1
	    !case (2)
	    !  hcomp=hcomp+val*0.1
	    case (3)
	      hpion=hpion+val*0.1
	    case (4)
	      hpion=hpion+val*0.1  
	    case (5)
	      hpion=hpion+val*0.1
	    case (7)
	      ccomp=ccomp+val*0.1         
	    case (9)
	      crecl=crecl+val*0.1
	    case (10)   
	      cbrem=cbrem+val*0.1
	    case (13)
	      htot=val*0.1
	    case (14) 
	      ctot=val*0.1   	  
	  end select
	  j=j+1
	enddo
2       close(12)
        write(13,*) i, hcomp, hpion, htot, ccomp, cbrem, crecl, ctot
      enddo
      
      close(13)
      
      end subroutine pi_hc
