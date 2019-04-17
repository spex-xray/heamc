      ! HEAMC program to generate output directly comparable to other spectral codes
      ! Definition agreed on Lorentz Workshop: High Energy Astrophysical Model Comparison
      ! Leiden, August 8-12, 2016
      
      ! =================================
      ! Set agreed input parameters
      ! Set output file names
      ! =================================
      module defheamc
        ! Agreed parameters
	integer, parameter                     :: ncie = 3
	integer, parameter                     :: nnei = 1
        real, dimension(ncie), parameter       :: ciekt = (/0.0861732,0.517039,4.000000/)
	real, dimension(nnei), parameter       :: neikt1 = (/0.000861732/)
	real, dimension(nnei), parameter       :: neikt2 = (/2.0/)
        integer, parameter                     :: nel=13
	integer, dimension(nel), parameter     :: eli= (/1,2,6,7,8,10,12, &
                                                         14,16,18,20,26,28/)
        character*2, dimension(nel), parameter :: el = (/'01','02','06','07','08','10','12', &
                                                         '14','16','18','20','26','28'/)
        character*2, dimension(nel), parameter :: elc= (/'H ','He','C ','N ','O ','Ne','Mg', &
                                                         'Si','S ','Ar','Ca','Fe','Ni'/)
        character*6, dimension(31), parameter  :: roman=(/ &
         'I     ','II    ','III   ','IV    ','V     ','VI    ','VII   ', &
         'VIII  ','IX    ','X     ','XI    ','XII   ','XIII  ','XIV   ', &
         'XV    ','XVI   ','XVII  ','XVIII ','XIX   ','XX    ','XXI   ', &
         'XXII  ','XXIII ','XXIV  ','XXV   ','XXVI  ','XXVII ','XXVIII', &
         'XXIX  ','XXX   ','XXXI  '/)
	real, parameter                        :: em = 1.
        real, parameter                        :: kttokev = 8.617328149741e-8
        real*8, parameter                      :: pi = 3.14159265359D0
      
        integer, parameter                     :: nkt=51
        real, dimension(nkt)                   :: ktarr
      
        ! Intermediate file names
        character*7                           :: line = 'line_kt'
        character*7                           :: icon = 'icon_kt'
	character*7                           :: levl = 'levl_kt'
        character*7                           :: radp = 'radp_kt'
	
	! Intermediate file names pion        
	character*7                           :: plin = 'plin_xi'
	character*7                           :: ppls = 'ppls_xi'
	character*7                           :: pico = 'pico_xi'
	character*7                           :: phea = 'phea_xi'
	character*7                           :: pspc = 'pspc_xi'
         
        ! Final output file names
        character*3                           :: ficon= 'csd'
      
      end module
      
      subroutine setktarr()
      use defheamc
      implicit none
      
      integer i
      
      do i=1,nkt
        ktarr(i)=kttokev*(10**(3.9+(real(i)/10.)))
      enddo
      
      end subroutine
