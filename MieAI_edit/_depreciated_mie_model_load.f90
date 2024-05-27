module mo_art_radiation_mie_model_load
  use mo_kind, 		        only: wp  
  use mod_network, 	        only: network_type
  use mo_art_config, 	    only: art_config

  public :: MieAI
  public :: mie_model_load

  type, extends(network_type) :: MieAI
  !real(wp) :: lam, n1, n2, n3, n4, n5, n6, k1, k2, k3, k4, k5, k6

      ! Define refractive indices for components of mixed mode aerosol particles
  real(wp), dimension(:), allocatable :: dust_lam, dust_real_ri, dust_imag_ri
  real(wp), dimension(:), allocatable ::   ss_lam,   ss_real_ri,   ss_imag_ri
  real(wp), dimension(:), allocatable ::  ocb_lam,  ocb_real_ri,  ocb_imag_ri
  real(wp), dimension(:), allocatable ::  wat_lam,  wat_real_ri,  wat_imag_ri
  real(wp), dimension(:), allocatable :: sulf_lam, sulf_real_ri, sulf_imag_ri
  real(wp), dimension(:), allocatable ::  soa_lam,  soa_real_ri,  soa_imag_ri
      
      ! Scaling factors for quantile transformation
  real(wp), dimension(quantile_rows) :: ext_scale, sca_scale, asy_scale, qua_scale, ppf_scale

      ! Min and max values for normalization
      real(wp), dimension(min_max_rows)  :: min_vals, max_vals

      ! name of MieAI input and output variables
  CHARACTER(LEN=MAX_CHAR_LENGTH), dimension(min_max_rows) :: vname 

  contains  
         ! procedure for loading trained MieAI model including scaling factors for qunatile tranform and normalization
         procedure :: load_model => load_model_params   

         ! procedure for loading refractive indices from laboratory measurements
         procedure :: load_ri => load_ri_data

         ! procedure for converting aerosol concetration to refractive indices
         procedure :: chem2ri => estimate_ri	

     !Perform prediction using MieAI  
         procedure :: predict => get_prediction

         ! End-to-end pipeline for estimation of bulk AOPs by emulating Mie calculation using MieAI
     procedure :: emulate => perform_emulation
  end type MieAI
  
  private

  contains
  subroutine load_model_params(net, MieAI_file, min_max_file, quantile_transform_file)
	class(MieAI), intent(inout) :: net


             
        ! load saved MieAI model
        call net % load(MieAI_file)

        ! load saved min-max values for training data
        call read_min_max_data(min_max_file, min_max_rows, net%vname, net%max_vals, net%min_vals) 
        
        ! load parameters of quantile transformer
        call read_quantile_data(quantile_transform_file, net%ext_scale, net%sca_scale, net%asy_scale, net%qua_scale, net%ppf_scale, quantile_rows)
  end subroutine load_model_params

  subroutine load_ri_data(net, fold)
	class(MieAI), intent(inout) :: net 
 
	CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: fold  

        ! Define filenames
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: dust_file
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: ss_file 
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: ocb_file
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: wat_file 
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: sulf_file
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: soa_file

        dust_file = 'BI_USE_NorthSahara_newformat.txt'
        ss_file   = 'BI_USE_SS_RH70.txt'
        ocb_file  = 'RI_OCBC30.txt'
        wat_file  = 'BI_USE_H2O.txt'
        sulf_file = 'BI_USE_75Sulf215K.txt'
        soa_file  = 'BI_USE_SOA.txt' 

        ! Append folder path to filenames
        dust_file = trim(fold) // trim(dust_file)
        ss_file   = trim(fold) // trim(ss_file)
        ocb_file  = trim(fold) // trim(ocb_file)
        wat_file  = trim(fold) // trim(wat_file)
        sulf_file = trim(fold) // trim(sulf_file)
        soa_file  = trim(fold) // trim(soa_file)

        ! Read data from CSV files
	call read_csv(dust_file, net%dust_lam, net%dust_real_ri, net%dust_imag_ri)
	call read_csv(ss_file,     net%ss_lam,   net%ss_real_ri, net%ss_imag_ri)
	call read_csv(ocb_file,   net%ocb_lam,  net%ocb_real_ri,  net%ocb_imag_ri)
	call read_csv(wat_file,   net%wat_lam,  net%wat_real_ri,  net%wat_imag_ri)
	call read_csv(sulf_file, net%sulf_lam, net%sulf_real_ri, net%sulf_imag_ri)
	call read_csv(soa_file,   net%soa_lam,  net%soa_real_ri,  net%soa_imag_ri)
  end subroutine load_ri_data

  
  ! Change log: Cornelius
  ! 20.04.2024 1750PM: added f_dust, f_seas to equation by using if-logic art_config
  ! Added required argument: jg
  ! Assuming the order of tracer conainter: dust, seasalt, soot (if exist), shell...
    ! Note: Correct the interp_ri in the select case as the reference to dust and seasalt in net% method unknown
  ! Estimate refractive indices for the mixed mode core and shell using component concetrations	
  subroutine estimate_ri(net, tracer, lam, real_core, imag_core, real_shell, imag_shell, jg)
	class(MieAI), intent(inout) :: net 
        
        real(wp), INTENT(in)  :: tracer(:), lam
        real(wp), INTENT(out) :: real_core, imag_core, real_shell, imag_shell
        INTEGER,  INTENT(in)  :: jg ! Initialised jg as an input, reading art_config
        INTEGER               :: status_code ! Status code for the iart_seasalt/dust switch
        
        ! Local variables
        real(wp) :: core_part, shell_part 
        real(wp) :: f_soot, f_dust, f_wat, f_sul, f_salt, f_org
        real(wp) :: n1, n2, n3, n4, n5, n6, k1, k2, k3, k4, k5, k6



        ! Calculate fraction of components in the core
        ! Edit: Introduced status_code.
        ! Behaviour:
        !   status_code == 0: Soot only
        !   status_code == 1: Dust exists
        !   status_code == 2: Sea Salt exists
        !   status_code == 3: Dust & Sea Salt exists
        status_code = 0
        IF (art_config(jg)%iart_dust > 0) THEN
            status_code = status_code + 1
        ENDIF
        IF (art_config(jg)%iart_seasalt > 0) THEN
            status_code = status_code + 2
        END IF

        ! Match-Case behaviour: Compute different real_core and imag_core depending on status_code
        select case (status_code)
        case (0) ! Soot only
            ! Extract components
            core_part  = tracer(1)
            shell_part = SUM(tracer(2:5))

            ! Calculate fraction of components in the core
            f_soot = tracer(1) / core_part
            ! Calculate fraction of components in the shell
            f_wat  = tracer(2) / shell_part
            f_sul  = tracer(3) / shell_part
            f_org  = (tracer(4) + tracer(5)) / shell_part

            ! Interpolate refractive indices for components
	        call interp_ri( net%ocb_lam,  net%ocb_real_ri,  net%ocb_imag_ri, lam, n3, k3)

            ! Calculate real and imaginary parts for core
            real_core  =  f_soot * n3
            imag_core  =  f_soot * k3

        case (1) ! Dust and soot only
            ! Extract components
            core_part  = SUM(tracer(1:2))
            shell_part = SUM(tracer(3:6))

            ! Calculate fraction of components in the core
            f_dust = tracer(1) / core_part
            f_soot = tracer(2) / core_part
            ! Calculate fraction of components in the shell
            f_wat  = tracer(3) / shell_part
            f_sul  = tracer(4) / shell_part
            f_org  = (tracer(5) + tracer(6)) / shell_part

            ! Interpolate refractive indices for components
            call interp_ri( net%dust_lam,  net%dust_real_ri,  net%dust_imag_ri, lam, n1, k1)
	        call interp_ri( net%ocb_lam,  net%ocb_real_ri,  net%ocb_imag_ri, lam, n3, k3)

            ! Calculate real and imaginary parts for core
            real_core  =  f_dust * n1 + f_soot * n3
            imag_core  =  f_dust * k1 + f_soot * k3

        case (2) ! Seasalt and soot only
            ! Extract components
            core_part  = SUM(tracer(1:2))
            shell_part = SUM(tracer(3:6))

            ! Calculate fraction of components in the core
            f_seas = tracer(1) / core_part
            f_soot    = tracer(2) / core_part
            ! Calculate fraction of components in the shell
            f_wat     = tracer(3) / shell_part
            f_sul     = tracer(4) / shell_part
            f_org     = (tracer(5) + tracer(6)) / shell_part

            ! Interpolate refractive indices for components
            call interp_ri( net%seasalt_lam,  net%seasalt_real_ri,  net%seasalt_imag_ri, lam, n2, k2)
	        call interp_ri( net%ocb_lam,  net%ocb_real_ri,  net%ocb_imag_ri, lam, n3, k3)

            ! Calculate real and imaginary parts for core
            real_core  =  f_seas * n2 + f_soot * n3
            imag_core  =  f_seas * k2 + f_soot * k3
        case (3) ! Dust, seasalt and soot
            ! Extract components
            core_part  = SUM(tracer(1:3))
            shell_part = SUM(tracer(4:7))

            ! Calculate fraction of components in the core
            f_dust    = tracer(1) / core_part
            f_seas    = tracer(2) / core_part
            f_soot    = tracer(3) / core_part
            ! Calculate fraction of components in the shell
            f_wat     = tracer(4) / shell_part
            f_sul     = tracer(5) / shell_part
            f_org     = (tracer(6) + tracer(7)) / shell_part

            ! Interpolate refractive indices for components
            call interp_ri( net%dust_lam,  net%dust_real_ri,  net%dust_imag_ri, lam, n1, k1)
            call interp_ri( net%ss_lam,  net%ss_real_ri,  net%ss_imag_ri, lam, n2, k2)
	        call interp_ri( net%ocb_lam,  net%ocb_real_ri,  net%ocb_imag_ri, lam, n3, k3)

            ! Calculate real and imaginary parts for core
            real_core  =  f_dust * n1 + f_seas * n2 +f_soot * n3
            imag_core  =  f_dust * k1 + f_seas * k2 +f_soot * k3
        end select



        ! Interpolate refractive indices for components
    call interp_ri( net%wat_lam,  net%wat_real_ri,  net%wat_imag_ri, lam, n4, k4)
    call interp_ri(net%sulf_lam, net%sulf_real_ri, net%sulf_imag_ri, lam, n5, k5)
    call interp_ri( net%soa_lam,  net%soa_real_ri,  net%soa_imag_ri, lam, n6, k6)

        ! Calculate real and imaginary parts for the shell
        real_shell =  f_wat * n4 + f_sul * n5 + f_org * n6 
        imag_shell =  f_wat * k4 + f_sul * k5 + f_org * k6
  end subroutine estimate_ri

   subroutine get_prediction(net, mie_input, prediction)
	class(MieAI), intent(inout) :: net

	integer(ik), parameter :: ndim=7, nbins=15
        integer(ik) :: i, j, jj, k, idx

	real(wp), dimension(:, :), intent(in)  :: mie_input
	real(wp), dimension(:, :), intent(out) :: prediction
        
        real(wp), allocatable, dimension(:, :) :: input
        real(wp), allocatable, dimension(:)    :: mie_input1

        !real(rk), allocatable, dimension(:)    :: mie_input2
        !real(rk), dimension(nbins, ndim)       :: prediction1

        ! Indices for accessing min-max values
        integer, dimension(ndim) :: indices1 = [5, 13, 7, 8, 9, 10, 12]
        integer, dimension(3)     :: indices2 = [1, 2, 4]

        allocate(mie_input1(ndim), input(nbins, ndim))
                
        ! transform input data using min-max scaling and predict optical properties using MieAI 
        do i = 1, nbins
            do j = 1, ndim
                idx = indices1(j)
                mie_input1(j) = (mie_input(i, j) - net%min_vals(idx)) / (net%max_vals(idx) - net%min_vals(idx)) 
            end do
            input(i, :) = mie_input(i, :)        
            prediction(i, :) = real(net % output(real(mie_input1, kind=rk)), kind=wp)
        end do
            
        ! Inverse transform MieAI prediction using inverse quantile tranform    
        call inverse_quantile_transform(prediction, net%ext_scale, net%sca_scale, net%asy_scale, net%ppf_scale)
        
        ! denormalize optical properties using training min-max values
        do k = 1, 3
            idx = indices2(k)
            prediction(:, k) = prediction(:, k) * (net%max_vals(idx) - net%min_vals(idx)) + net%min_vals(idx)
        end do

        deallocate(mie_input1, input)
  end subroutine get_prediction

  subroutine perform_emulation(net, mu, sig, jspec, tracer1, opt)
	class(MieAI), intent(inout) :: net

        ! Parameters
	integer(ik), parameter :: num=11, nbins=15
        real(wp),    parameter :: c=299792458                     ! speed of light in m/s
        
        ! Inputs
	INTEGER,  INTENT(in) :: jspec
        real(wp), intent(in) :: mu, sig
        real(wp), intent(in), dimension(:)  :: tracer1

        ! Outputs
        real(wp), intent(out), dimension(4) :: opt
        
        ! Local variables
        real(wp), dimension(:,:), allocatable :: mie_input, prediction, eff
        real(wp), dimension(15) :: bins, x_range, dia        
        real(wp) :: real_core, imag_core, real_shell, imag_shell
        real(wp) :: dlogd, frac
	real(wp) :: wl, wavenum(30)
        integer(ik) :: i, idx(nbins)
        
        ! Initialize arrays
        allocate(mie_input(nbins, 7), prediction(nbins, 3), eff(nbins, num))

        wavenum = (/ 350.0_wp,  500.0_wp,  630.0_wp,  700.0_wp,  820.0_wp,  980.0_wp,  1080.0_wp, 1180.0_wp, &   ! thermal
        &        1390.0_wp, 1480.0_wp, 1800.0_wp, 2080.0_wp, 2250.0_wp, 2380.0_wp, 2600.0_wp, 3250.0_wp, &       ! thermal
        &        3250.0_wp, 4000.0_wp, 4650.0_wp, 5150.0_wp, 6150.0_wp, 7700.0_wp, 8050.0_wp,12850.0_wp, &       ! solar
        &        16000.0_wp,22650.0_wp, 29000.0_wp,38000.0_wp, 50000.0_wp, 2600.0_wp/)

	! convert wavenumber to wavelength with unit in nanometer
	wl = 1.0_wp * 1.0e-2 * 1.0e9 / wavenum(jspec)
        
        call mod2bin(mu, sig, nbins, bins, x_range, dlogd) 
        call get_shell_fraction(tracer1, frac)

        ! Compute complex refractive indices
	call net%chem2ri(tracer1, wl, real_core, imag_core, real_shell, imag_shell)
    
        ! Prepare input for prediction
        do i = 1, nbins
            idx(i) = i	
            mie_input(i, :) = [frac, pi * bins(i) / wl, real_core, imag_core, real_shell, imag_shell, wl]    
        end do
        
        ! Perform prediction
        call net%predict(mie_input, prediction) 
        
        !cols = ['tdia', 'pdf', 'dlogd', 'area', 'ncells', 'vol', 'const', 'scats', "Extinction", "Scattering", "Asym"]
        
        ! Compute efficiencies
        dia = x_range * 1.0e9
        eff(:, 9:11) = prediction

        eff(:, 1) = dia * (1 + mie_input(:, 1))                            ! tdia
        eff(:, 2) = bins                                                   ! pdf
        eff(:, 3) = dlogd                                                  ! dlogd
        eff(:, 4) = (pi / 4) * (dia ** 2)                                  ! area
        eff(:, 5) = idx                                                    ! ncells
        eff(:, 6) = (pi / 6) * (eff(:, 1) ** 3) * eff(:, 2) * eff(:, 3)    ! vol
        eff(:, 7) = eff(:, 4) * eff(:, 2) * eff(:, 3)                      ! const
        eff(:, 8) = eff(:, 10) * eff(:, 7)                                 ! scats
        !eff(:, 11) = eff(:, 9) / eff(:, 8)                                ! ssa
        
        ! integrate efficiencies to calculate coefficients for the mode
        call bin2mod(eff, opt)    

        ! Deallocate arrays
        deallocate(mie_input, prediction, eff)
  end subroutine perform_emulation

SUBROUTINE mie_model_load(net)
    CHARACTER(LEN=MAX_CHAR_LENGTH) :: fold, min_max_file, quantile_transform_file, MieAI_file
    type(MieAI), intent(inout) :: net
        ! File paths
        fold = art_config(jg)%cart_MieAI_files
        min_max_file = 'mlp_min_max.csv'
        quantile_transform_file = 'quantile_transform.csv'
        MieAI_file  = 'MieAI.txt'
        
        ! Concatenate folder path to filenames  
        min_max_file = trim(fold) // trim(min_max_file)
        quantile_transform_file = trim(fold) // trim(quantile_transform_file)
        MieAI_file  = trim(fold) // trim(MieAI_file)

        call net%load_model(net, MieAI_file, min_max_file, quantile_transform_file)
        call net%load_ri(fold)

end SUBROUTINE mie_model_load

end module mo_art_radiation_mie_model_load