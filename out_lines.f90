      subroutine out_lines(model)
      use defheamc
      implicit none
      character*3, intent(in)   :: model     
      
      integer                :: i,j,k,n,m
      character*128          :: out,s,infile,id
      character*2            :: elem
      character*5            :: stage
      real                   :: wav, eh
      real*8                 :: emis, scale 
      integer                :: e,c
      character*3, parameter :: hash=' # ' 
      
      write(*,'(a)') ' Write SPEX line list for model '//trim(model)//' to output file...'
      
      select case (model)
        case ('cie')
	  n=ncie
	case ('nei')
	  n=nnei
      end select
      
      do i=1,n 
        m=0
	
	! Open input file
	write(out,'(I1)') i
        write(out,'(a)') trim(model)//'_'//trim(line)//trim(out)//'.asc'
	infile=trim(out)
        open(unit=12,file=trim(infile),status='old')
        
	! Read electron/hydrogen ratio
	write(out,'(I1)') i
        write(out,'(a)') trim(model)//'_plas'//trim(out)//'.asc '
	call plas(out,eh)
	scale=eh*1D+64/1D+12
	
	! Open output file
	write(out,'(I1)') i
	write(out,'(a)') 'spex_'//trim(model)//'_'//trim(line)//trim(out)//'.dat'
        open(unit=13,file=out,status='replace')
	
	select case (model)
	  case ('cie')
	    write(out,'(F5.3)') ciekt(i)
	    write(13,'(a)') '#  100 Strongest lines for '//trim(out)//' keV CIE plasma'
	  case ('nei')
	    write(13,'(a)') '#  100 Strongest lines for NEI plasma'  
	end select      
	
	write(13,'(a)') '#  Lambda                  Z          Ion  Log(Flux)'    
	
	read(12,'(a)') s
        read(12,'(a)') s
        do while (m.lt.100)
          read(12,'(a)',end=2) s
	  read(s(11:12),*) elem
	  read(s(14:20),*) stage 
	  read(s(10:70),'(a)') id
          read(s(88:96),*) wav
	  read(s(102:111),*) emis
	  if (wav.le.1000) then
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
	    write(13,*) wav, eli(e), c, log10(emis/scale), hash, trim(id)
	    m=m+1
	  endif
	enddo
      
2       close(12)
        close(13)
        write(out,'(a)') 'rm -f '//trim(infile)
	call system(out)
      enddo
      
      end subroutine out_lines
