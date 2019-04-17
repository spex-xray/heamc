      subroutine out_level()
      use defheamc
      implicit none
      
      integer               :: i,j,k,l,nli,nion
      character*256         :: out,s
      character*16          :: ion,pion
      real                  :: ta(10,10)
      real                  :: dens
      integer               :: tidx(10)
      logical               :: loop
      
      ! Read key lines table
      
      open(unit=11,file='keyline.dat',status='old',err=100)
      
      read(11,'(a)') s
      read(11,'(a)') s
      read(s(19:22),*) nion
      read(11,'(a)') s
      read(s(19:22),*) nli
      read(11,'(a)') s
      
      allocate(keyl(nion))
      
      i=1  ! Number of ions
      l=1  ! Number of lines
      
      do k=1,nli 
        read(11,'(a)') s
        read(s(13:21),'(a)') ion
	if (k.gt.1) then
	  if (trim(ion).eq.trim(pion)) then
	    l=l+1
	  else
	    allocate(keyl(i)%lines(l))
	    keyl(i)%nlin=l
	    i=i+1 
	    l=1 
	  endif
	endif
	pion=ion
      enddo
      
      if (i.eq.nion) then
        allocate(keyl(i)%lines(l))
	keyl(i)%nlin=l
      endif
      
      
10    rewind(11)
      read(11,'(a)') s
      read(11,'(a)') s
      read(11,'(a)') s
      read(11,'(a)') s
      
      do i=1,nion
        do j=1,keyl(i)%nlin 
          read(11,'(a)') s
	  read(s(1:3),*) keyl(i)%lines(j)%num
	  read(s(6:8),*) keyl(i)%lines(j)%snum
	  read(s(13:15),*) keyl(i)%el
	  read(s(17:21),*) keyl(i)%ion
	  read(s(33:44),'(a)') keyl(i)%ground
	  read(s(48:62),'(a)') keyl(i)%lines(j)%upper
	enddo
      enddo
            
20    close(11)
      
      write(*,'(a)') ' Write SPEX level populations to output file...'
      
      call keynull(nion)
      
      open(unit=12,file='spex_levpop.dat',status='replace')
      
      write(12,'(a)') '# Num   Te   Ne    EExc   EDeExc    PExc   PDeexc   CascadeTo   RadiativeOut   RRin    DRin    ISion'
      
      do i=1,ncie
        ! Low density
	write(out,'(I1)') i
	write(out,'(a)') 'cie_'//trim(levl)//trim(out)//'.asc'
        open(unit=13,file=out,status='old')
        do while (.true.)
	  read(13,'(a)',end=30) s
	  if (s(1:12).eq.'level (keV)') then
	    do j=1,nion
	      do l=1,keyl(j)%nlin
	        if ((trim(s(30:45)).eq.trim(keyl(j)%lines(l)%upper))) then
	          if ((trim(s(55:57)).eq.trim(keyl(j)%el)).and.(trim(s(58:63)).eq.(trim(keyl(j)%ion)))) then
		  read(13,'(a)',end=30) s
		    loop=.true.
		    do while (loop)
		    read(13,'(a)',end=30) s
		      if (trim(s(140:160)).eq.trim(keyl(j)%ground)) then
		        read(s(19:29),*) keyl(j)%lines(l)%eexc
			read(s(30:40),*) keyl(j)%lines(l)%edeexc
			read(s(41:51),*) keyl(j)%lines(l)%pexc
			read(s(52:62),*) keyl(j)%lines(l)%pdeexc
			keyl(j)%lines(l)%casto = 0.
			read(s(63:73),*) keyl(j)%lines(l)%radout
			keyl(j)%lines(l)%ision = 0.
		      endif
		      if (trim(s(1:10)).eq.'Total gain') then
		        read(s(96:106),*) keyl(j)%lines(l)%rrin
			read(s(107:117),*) keyl(j)%lines(l)%drin
			loop=.false.
		      endif
		    enddo
		  endif
	        endif
	      enddo	
	    enddo  
	  else
	    cycle
	  endif
        enddo
30      close(13)
        
	dens=1.
	
	do j=1,nion
	  do l=1,keyl(j)%nlin
	    write(12,*) keyl(j)%lines(l)%num, ciekt(i), dens, keyl(j)%lines(l)%eexc, & 
     &                keyl(j)%lines(l)%edeexc, keyl(j)%lines(l)%pexc, keyl(j)%lines(l)%pdeexc, &
     &                keyl(j)%lines(l)%casto, keyl(j)%lines(l)%radout, keyl(j)%lines(l)%rrin, &
     &                keyl(j)%lines(l)%drin, keyl(j)%lines(l)%ision 	    
	  enddo
	enddo
	
	call keynull(nion)
	
	! High density
	write(out,'(I1)') i
	write(out,'(a)') 'cie_'//trim(levl)//trim(out)//'_hd.asc'
        open(unit=13,file=out,status='old')
        do while (.true.)
	  read(13,'(a)',end=40) s
	  if (s(1:12).eq.'level (keV)') then
	    do j=1,nion
	      do l=1,keyl(j)%nlin
	        if ((trim(s(30:45)).eq.trim(keyl(j)%lines(l)%upper))) then
	          if ((trim(s(55:57)).eq.trim(keyl(j)%el)).and.(trim(s(58:63)).eq.(trim(keyl(j)%ion)))) then
		  read(13,'(a)',end=40) s
		    loop=.true.
		    do while (loop)
		    read(13,'(a)',end=40) s
		      if (trim(s(140:160)).eq.trim(keyl(j)%ground)) then
		        read(s(19:29),*) keyl(j)%lines(l)%eexc
			read(s(30:40),*) keyl(j)%lines(l)%edeexc
			read(s(41:51),*) keyl(j)%lines(l)%pexc
			read(s(52:62),*) keyl(j)%lines(l)%pdeexc
			keyl(j)%lines(l)%casto = 0.
			read(s(63:73),*) keyl(j)%lines(l)%radout
			keyl(j)%lines(l)%ision = 0.
		      endif
		      if (trim(s(1:10)).eq.'Total gain') then
		        read(s(96:106),*) keyl(j)%lines(l)%rrin
			read(s(107:117),*) keyl(j)%lines(l)%drin
			loop=.false.
		      endif
		    enddo
		  endif
	        endif
	      enddo	
	    enddo
	  endif
        enddo
40      close(13)
      
        dens=1.0E+12
	
	do j=1,nion
	  do l=1,keyl(j)%nlin
	    write(12,*) keyl(j)%lines(l)%num, ciekt(i), dens, keyl(j)%lines(l)%eexc, & 
     &                keyl(j)%lines(l)%edeexc, keyl(j)%lines(l)%pexc, keyl(j)%lines(l)%pdeexc, &
     &                keyl(j)%lines(l)%casto, keyl(j)%lines(l)%radout, keyl(j)%lines(l)%rrin, &
     &                keyl(j)%lines(l)%drin, keyl(j)%lines(l)%ision 	    
	  enddo
	enddo
        
	call keynull(nion)
	
      enddo

        
	
      close(12)
      deallocate(keyl)
      
	
      return
100   write(*,'(a)') 'Error: Could not find file keyline.dat...'      
      
      end subroutine out_level
      
      subroutine keynull(nion)
      use defheamc
      implicit none
      
      integer, intent(in)  :: nion
      integer              :: i,j
      
      do i=1,nion
        do j=1,keyl(i)%nlin
	  keyl(i)%lines(j)%eexc=0.
	  keyl(i)%lines(j)%edeexc=0.
	  keyl(i)%lines(j)%pexc=0.
	  keyl(i)%lines(j)%pdeexc=0.
	  keyl(i)%lines(j)%casto=0.
	  keyl(i)%lines(j)%radout=0.
	  keyl(i)%lines(j)%rrin=0.
	  keyl(i)%lines(j)%drin=0.
	  keyl(i)%lines(j)%ision=0.
	enddo
      enddo	
      
      end subroutine

