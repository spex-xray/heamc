      ! =================================
      ! Subroutine to calculate
      ! - CSD per element
      ! - Strongest lines
      ! - Level populations
      ! =================================
      subroutine spexlines(model)
      use defheamc
      implicit none
      
      character*3,intent(in)  :: model
      character*64            :: out
      integer                 :: i,j,n
      
      write(*,'(a)') ' Running SPEX line list calculation for model '//trim(model)//'...'
      
      open(11,file='spexline.sh',status='replace')
      write(11,*) '#!/bin/bash'
      write(11,*) '$SPEX90/bin/spex << EOF'
      
      select case (model)
        case ('cie')
          write(11,*) 'com cie'
	  n=ncie
        case ('nei')
	  write(11,*) 'com neij'
	  n=nnei
      end select
      
      write(11,*) 'var calc new'
      write(11,*) 'ibal u17'
      write(11,*) 'abun lodd'
      
      do i=1,n
        select case (model)
	  case ('cie')
	    write(out,*) 'par 1 1 norm v ', em
            write(11,*) out
            write(out,*) 'par 1 1 t v ', ciekt(i) 
            write(11,*) out
	  case ('nei')
	    write(11,*) 'par 1 1 u v 1E-4'
	    write(out,*) 'par 1 1 t1 v ', neikt1(i)
	    write(11,*) out
	    write(out,*) 'par 1 1 t2 v ', neikt2(i)
	    write(11,*) out
	end select
	
	write(11,*) 'ions use all'
	write(11,*) 'calc'
	
	! Read electron/hydrogen ratio
	
	write(out,'(I1)') i
        write(out,*) trim(model)//'_plas'//trim(out)
        write(11,'("asc file ", a," 1 1 plas")') trim(out)
        
	! Calculate ion concentrations for selected temperature
        write(out,'(I1)') i
        write(out,*) trim(model)//'_'//trim(icon)//trim(out)
        write(11,'("asc file ", a," 1 1 icon")') trim(out)
        
	! Calculate lines for selected temperature
        write(out,'(I1)') i
        write(out,*) trim(model)//'_'//trim(line)//trim(out)
        write(11,'("asc file ", a," 1 1 line sort -pow")') trim(out)
        
	! Calculate level populations for selected temperature
	
	write(11,*) 'ions ignore all'
	write(11,*) 'ions use z 8'
	!write(11,*) 'ions use ion 8 8'
	!write(11,*) 'ions use ion 26 17'
	!write(11,*) 'ions use ion 26 25'
	write(11,*) 'ions use z 26'
	write(out,'(I1)') i
        write(out,*) trim(model)//'_'//trim(levl)//trim(out)
        write(11,'("asc file ", a," 1 1 lev")') trim(out)
        
	! Calculate level populations for high-density plasma
	
	write(11,*) 'par 1 1 ed v 1E-2'
	write(11,*) 'calc'
	write(out,'(I1)') i
        write(out,*) trim(model)//'_'//trim(levl)//trim(out)//'_hd'
        write(11,'("asc file ", a," 1 1 lev")') trim(out)
        
	
	
      enddo
      
      write(11,*) 'quit'
      write(11,'(A3)') 'EOF'
      write(11,*) ''
      close(11)
      
      call system('chmod u+x spexline.sh')
      call system('./spexline.sh >> /dev/null')
      call system('rm -f spexline.sh')
      
      end subroutine spexlines
      
