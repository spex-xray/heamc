      subroutine out_csd(model)
      use defheamc
      implicit none
      character*3, intent(in)   :: model
      
      real, allocatable         :: csd(:,:,:)
      integer i,j,k,n
      character*128 :: s
      character*2   :: elnam
      integer       :: charge
      real          :: conrel,conion
      real          :: val(3)
      
      character*128 :: out,v,infile
      
      ! Open input file for each temperature
      
      write(*,*) 'Read SPEX CSD output file...'
      
      allocate(csd(3,30,31))
      
      select case (model)
        case ('cie')
	  n=ncie
	case ('nei')
	  n=nnei
      end select	    
      
      csd=0.
      
      do i=1,n
        write(out,'(I1)') i
        write(infile,'(a)') trim(model)//'_'//trim(icon)//trim(out)//'.asc'
        open(unit=11,file=trim(infile),status='old')
        
	do j=1,4
	  read(11,'(a)') s
	enddo
	
	do while (.true.)
          read(11,'(a)',end=11) s
	  read(s(2:4),'(a)') elnam
	  read(s(29:31),*) charge
	  read(s(39:51),*) conrel
	  do j=1,nel
	    if (trim(elnam)==trim(elc(j))) then
	      csd(i,j,charge+1)=conrel
	    endif	
	  enddo  
	enddo  
11      close(11)
        write(out,'(a)') 'rm -f '//trim(infile)
        call system(out)
      enddo
      
      write(*,*) 'Write CSD to output file...'
      
      write(out,'(a)') 'spex_'//trim(model)//'_'//trim(ficon)//'.dat'
      
      open(unit=12,file=trim(out),status='replace')
      
      select case (model)
        case ('cie')
          write(12,'(a)') '# Charge state distribution for CIE model'
	  write(12,'(a)') '#          Z           ion     1e6K       6e6K       4.642e7K'
        case ('nei')
          write(12,'(a)') '# Charge state distribution for NEI model'
	  write(12,'(a)') '#          Z           ion     1e4K-2keV'
      end select
	  
      do i=1,nel
        do j=1,31
	  out=''
	  if (maxval(csd(:,i,j)).gt.0) then
	    do k=1,n
	      if (csd(k,i,j).gt.0) then
	        val(k)=log10(csd(k,i,j))
	      else
	        val(k)=log10(tiny(val))
	      endif
	      write(v,'(F10.6)') val(k)
	      write(out,*) trim(out)//' '//trim(v)
	    enddo
	    write(12,*) eli(i), j-1, out  
	  endif
        enddo
      enddo
      
      close(12)
      
      deallocate(csd)
      
      end subroutine out_csd
