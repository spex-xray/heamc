      subroutine pi_csd()
      use defheamc
      implicit none
      
      real, allocatable  :: csd(:,:,:)
      real               :: ekt(3), z
      integer            :: i,j,k,n
      character*128      :: out,s,v,infile
      logical            :: loop
      character*2        :: elnam
      integer            :: charge
      real               :: conrel,conion
      real               :: val(3)

      write(*,*) 'Write Pion CSD output...'

      ! Read electron temperature
      do i=1,3
        write(out,'(I1)') i
        write(out,'(a)') 'pion_'//trim(ppls)//trim(out)//'.asc'
        open(unit=12,file=out,status='old')
	loop=.true.
	do while (loop)
	  read(12,'(a)') s
	  if (s(11:22).eq.'temperature') then
	    read(s(38:49),*) ekt(i)
	    loop=.false.
	  endif
	enddo
	close(12)
      enddo
      
      allocate(csd(3,30,31))
      
      csd=0.
      
      do i=1,3
        write(out,'(I1)') i
        write(infile,'(a)') 'pion_'//trim(pico)//trim(out)//'.asc'
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
        !call system(out)
      enddo
      
      write(*,*) 'Write CSD to output file...'
      
      write(out,'(a)') 'spex_pi_csd.dat'
      
      open(unit=12,file=trim(out),status='replace')
      
      z=0.
      
      write(12,'(a)') '# Charge state distribution for PION model'
      write(12,'(a)') '#          Z           ion     1          2       3'
      write(12,*) z, z, ekt(1), ekt(2), ekt(3)
      
      do i=1,nel
        do j=1,31
	  out=''
	  if (maxval(csd(:,i,j)).gt.0) then
	    do k=1,3
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
      
      end subroutine pi_csd
