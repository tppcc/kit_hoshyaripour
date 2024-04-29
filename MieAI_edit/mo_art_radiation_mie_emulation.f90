module mo_art_radiation_mie_emulation
  use mo_kind, 		    only: wp
  USE mo_exception,         ONLY: finish
  USE mo_impl_constants,    ONLY: SUCCESS, MAX_CHAR_LENGTH
  use mo_art_config, 	    only: art_config
  USE mo_art_modes,  	    ONLY: t_fields_2mom
  USE mo_art_data,   	    ONLY: t_art_data
  USE mo_art_data,          ONLY: p_art_data

  use mod_kinds, 	    only: ik, rk
  use mod_network, 	    only: network_type

  implicit none
  
  real(wp),    parameter :: pi=3.14159265358979323846
  integer(ik), parameter :: nbins=15, min_max_rows=13, quantile_rows=1000
  
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
  
  public :: get_opt_mie_ai

contains
  subroutine load_model_params(net, MieAI_file, min_max_file, quantile_transform_file)
	class(MieAI), intent(inout) :: net
	CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: min_max_file, quantile_transform_file, MieAI_file
             
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

SUBROUTINE get_opt_mie_ai(tracer, rho, jb, nlong, nshort, ks, ke, jcs, jce, jg, fields, tau_vr, tau_s_vr, tauasy_vr)
    TYPE(t_fields_2mom), INTENT(IN) :: fields
    type(MieAI) :: net

    ! Input 
    INTEGER, INTENT(IN)   :: jb, nlong, nshort, ks, ke, jcs, jce, jg
    REAL(wp), INTENT(IN)  :: tracer(:,:,:,:), rho(:,:,:)

    ! output
    REAL(wp), INTENT(OUT) :: tau_vr(:,:,:), tau_s_vr(:,:,:), tauasy_vr(:,:,:)

    !local variables
    INTEGER :: jk, jk_vr, jc, i, jspec
    CHARACTER(LEN=MAX_CHAR_LENGTH), ALLOCATABLE :: tracer_str(:)
    INTEGER, DIMENSION(:), ALLOCATABLE  :: tr_idx, ierror
    real(wp), DIMENSION(:), ALLOCATABLE :: tracer1, opt
    real(wp) :: ext, sca, ssa, asy, sig=2, mu, conc

    CHARACTER(LEN=MAX_CHAR_LENGTH) :: fold, min_max_file, quantile_transform_file, MieAI_file
    CHARACTER(LEN=MAX_CHAR_LENGTH) :: thisroutine = "mo_art_radiation_mie_emulation:get_opt_mie_ai"

    ! File paths
    fold = art_config(jg)%cart_MieAI_files
    min_max_file = 'mlp_min_max.csv'
    quantile_transform_file = 'quantile_transform.csv'
    MieAI_file  = 'MieAI.txt'
      
    ! Concatenate folder path to filenames  
    min_max_file = trim(fold) // trim(min_max_file)
    quantile_transform_file = trim(fold) // trim(quantile_transform_file)
    MieAI_file  = trim(fold) // trim(MieAI_file)

    ! Load MieAI model
    !write(*, *) 'MieAI started running!!! ' 
    call net%load_model(MieAI_file, min_max_file, quantile_transform_file)

    ! Load refractive indices
    fold = art_config(jg)%cart_ri
    call net%load_ri(fold)

    ! Define tracer strings based on fields%name
    SELECT CASE(TRIM(fields%name))
        CASE('mixed_acc')            
            tracer_str = [CHARACTER(LEN=MAX_CHAR_LENGTH) :: 'soot_mixed_acc', 'h2o_mixed_acc', 'so4_mixed_acc', 'nh4_mixed_acc', &
                         &'no3_mixed_acc']

        CASE('mixed_coa')
            tracer_str = [CHARACTER(LEN=MAX_CHAR_LENGTH) :: 'soot_mixed_coa', 'h2o_mixed_coa', 'so4_mixed_coa', 'nh4_mixed_coa', &
                	&'no3_mixed_coa']

        CASE DEFAULT
            tracer_str = [CHARACTER(LEN=MAX_CHAR_LENGTH) :: ]
	    !ALLOCATE(tracer_str(0)) ! Allocate empty array
    END SELECT

    ! Fetch tracer indices from dictionary
    IF (SIZE(tracer_str) /= 0) THEN
        ALLOCATE(tr_idx(SIZE(tracer_str)), ierror(SIZE(tracer_str)))
        DO i = 1, SIZE(tracer_str)
            CALL p_art_data(jg)%dict_tracer%get(tracer_str(i), tr_idx(i), ierror(i))
	    IF(ierror(i) /= SUCCESS) CALL finish (thisroutine, 'Tracer ' // trim(tracer_str(i)) // ' not found in dictionary.')
        END DO
    END IF
    
    ! Initialize AOPs arrays
    tau_vr(:,:,:) = 0.0_wp
    tau_s_vr(:,:,:) = 0.0_wp
    tauasy_vr(:,:,:) = 0.0_wp

    conc = 0.0_wp

    ! Loop over spectrums, layers, and bins
    DO jspec = 1, nlong + nshort
        DO jk = ks, ke
            jk_vr = ke + 1 - jk
            DO jc = jcs, jce
                IF (SIZE(tracer_str) /= 0) THEN
                    ALLOCATE(tracer1(SIZE(tracer_str)), opt(4))
                    DO i = 1, SIZE(tracer_str)
                        tracer1(i) = tracer(jc, jk, jb, tr_idx(i)) ! selecting tracer concentration using tracer indices
			conc = conc + tracer1(i)
                    END DO              

                    mu = fields%diameter(jc, jk, jb)

                    ! Emulate optical properties
                    call net%emulate(mu, sig, jspec, tracer1, opt)                    
		    !print *, opt
		    
		    !(ext, sca, ssa, asy) = opt  
                    ext = opt(1)
                    sca = opt(2)
                    ssa = opt(3)
                    asy = opt(4)

                    ! Conversion of mass specific coefficients to volume specific coefficients
                    ext = conc * ext * rho(jc, jk, jb) * 1.e-6_wp 

                    ! Calculation of AOPs coefficients in terms of per layer values
                    tau_vr(jc, jk_vr, jspec) = tau_vr(jc, jk_vr, jspec) + ext
                    tau_s_vr(jc, jk_vr, jspec) = tau_vr(jc, jk_vr, jspec) * ssa
                    tauasy_vr(jc, jk_vr, jspec) = tau_s_vr(jc, jk_vr, jspec) * asy		    

                    DEALLOCATE(tracer1, opt)
	        END IF
            END DO
        END DO
    END DO
    DEALLOCATE(tr_idx, ierror)
END SUBROUTINE get_opt_mie_ai 

        ! Edit: Cornelius
        ! 20.04.2024 1750PM: added f_dust, f_seas to equation by using if-logic art_config
        ! Added required argument: jg
        ! Assuming the order of tracer conainter: dust, seasalt, soot (if exist), shell...
SUBROUTINE get_shell_fraction(tracer, frac, jg)
        ! Estimate the shell diameter as the fraction of total diameter of the internally mixed aerosol

        real(wp), INTENT(in) :: tracer(:)
        real(wp), INTENT(out) :: frac

        real(wp) :: dens_core, dens_shell, total_mass, fc_mass, fc_vol, dc_dt
        real(wp) :: rho_dust, rho_soot, rho_seas, rho_sul, rho_wat, rho_org
        real(wp) :: core_part, shell_part, f_soot, f_dust, f_wat, f_sul, f_salt, f_org
        
        rho_dust = 2.60_wp
        rho_soot = 1.25_wp
        rho_seas = 1.70_wp

        rho_sul = 1.80_wp
        rho_wat = 1.0_wp
        rho_org = 1.35_wp

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
            total_mass = SUM(tracer(:))

            ! comute mass fraction
            fc_mass = core_part / total_mass

            f_dust    = tracer(1) / core_part
            ! Calculate fraction of components in the shell
            f_wat     = tracer(2) / shell_part
            f_sul     = tracer(3) / shell_part
            f_org     = (tracer(4) + tracer(5)) / shell_part

            ! Compute shell and core diameter
            dens_core =  f_soot * rho_soot
            dens_shell =  (f_org * rho_org + f_wat * rho_wat + f_sul * rho_sul)

        case (1) ! Dust and soot only
            core_part  = tracer(1:2)
            shell_part = SUM(tracer(3:6))
            total_mass = SUM(tracer(:))

            ! comute mass fraction
            fc_mass    = core_part / total_mass

            f_dust     = tracer(1) / core_part
            f_soot     = tracer(2) / core_part
            ! Calculate fraction of components in the shell
            f_wat      = tracer(3) / shell_part
            f_sul      = tracer(4) / shell_part
            f_org      = (tracer(5) + tracer(6)) / shell_part

            ! Compute shell and core diameter
            dens_core  =  f_dust * rho_dust + f_soot * rho_soot
            dens_shell =  (f_org * rho_org + f_wat * rho_wat + f_sul * rho_sul)
        case (2) ! Seasalt and soot only
            core_part  = tracer(1:2)
            shell_part = SUM(tracer(3:6))
            total_mass = SUM(tracer(:))

            ! comute mass fraction
            fc_mass    = core_part / total_mass

            f_seas     = tracer(1) / core_part
            f_soot     = tracer(2) / core_part
            ! Calculate fraction of components in the shell
            f_wat      = tracer(3) / shell_part
            f_sul      = tracer(4) / shell_part
            f_org      = (tracer(5) + tracer(6)) / shell_part

            ! Compute shell and core diameter
            dens_core  =  f_seas * rho_seas + f_soot * rho_soot
            dens_shell =  (f_org * rho_org + f_wat * rho_wat + f_sul * rho_sul)
        case (3) ! Dust, seasalt and soot
            core_part  = tracer(1:3)
            shell_part = SUM(tracer(4:7))
            total_mass = SUM(tracer(:))

            ! comute mass fraction
            fc_mass = core_part / total_mass

            f_dust     = tracer(1) / core_part
            f_seas     = tracer(2) / core_part
            f_soot     = tracer(3) / core_part
            ! Calculate fraction of components in the shell
            f_wat      = tracer(4) / shell_part
            f_sul      = tracer(5) / shell_part
            f_org      = (tracer(6) + tracer(7)) / shell_part

            ! Compute shell and core diameter
            dens_core  =  f_dust * rho_dust + f_seas * rho_seas + f_soot * rho_soot
            dens_shell =  (f_org * rho_org + f_wat * rho_wat + f_sul * rho_sul)
        end select

        ! Finally, compute the fraction of shell in terms of total diameter
        fc_vol = fc_mass / (fc_mass + (1 - fc_mass) * dens_core / dens_shell)
        dc_dt = fc_vol ** (1.0 / 3.0)
        frac = 1.0 - dc_dt
END SUBROUTINE get_shell_fraction
    
subroutine mod2bin(mu, sig, nbins, bins, x_range, dlogd)
        real(wp), intent(in) :: mu, sig
        integer(ik), intent(in)  :: nbins
        real(wp), dimension(:), intent(out) :: x_range, bins
        real(wp), dimension(size(x_range))  :: dx
        real(wp), intent(out)  :: dlogd
        real(wp) :: limit
        integer(ik)  :: i

        dlogd = log10(sig) * 0.25
        limit = floor(3.0_wp * log(sig) / dlogd) * dlogd

        do i = 1, nbins
            dx(i) = (10.0_wp)**(real(i - 1, wp) * (2.0_wp * limit / real(nbins - 1, wp)) - limit)
            x_range(i) = mu * dx(i)
            call LogNormal(x_range(i), mu, sig, bins(i))
        end do
end subroutine mod2bin

subroutine bin2mod(ex, opt)
      real(wp), intent(in)  :: ex(:,:)
      real(wp), intent(out) :: opt(4)
      real(wp) :: tvol
      integer(ik)  :: sz   

      sz = size(ex, 2)
      tvol = sum(ex(:, 6))
      opt(1) = sum(ex(:, 9)  * ex(:, 7)) / tvol * 1000_wp    ! extinction
      opt(2) = sum(ex(:, 10) * ex(:, 7)) / tvol * 1000_wp    ! scattering
      opt(4) = sum(ex(:, 11) * ex(:, 8)) / sum(ex(:, 8))     ! asym
      opt(3) = opt(2) / opt(1)                               ! ssa
end subroutine bin2mod
    
subroutine LogNormal(diam, mu, gsd, pdf)
        real(wp), intent(in) :: diam, mu, gsd
        real(wp), intent(out) :: pdf
        real(wp) :: loggsd, const, x

        loggsd = log10(gsd)
        const  = loggsd * sqrt(2.0 * acos(-1.0_wp))
        
        x   = diam / mu
        pdf = exp(-log10(x)**2 / (2.0 * loggsd**2)) / const  
end subroutine LogNormal  

subroutine inverse_quantile_transform(prediction, ext, sca, asy, ppf)
        real(wp), dimension(:), intent(in) :: ext, sca, asy, ppf
        real(wp), dimension(:,:), intent(inout) :: prediction
        integer(ik) :: i
        
        do i = 1, size(prediction, dim=1)
            call interp_linear(prediction(i, 1), ppf, ext, prediction(i, 1))
            call interp_linear(prediction(i, 2), ppf, sca, prediction(i, 2))
            call interp_linear(prediction(i, 3), ppf, asy, prediction(i, 3))
        end do
end subroutine inverse_quantile_transform

subroutine interp_ri(lams, rri, iri, lam, n, k)
        real(wp), dimension(:), intent(in) :: lams, rri, iri
        real(wp), intent(in) :: lam
        real(wp), intent(out) :: n, k

        call linear_interpolation(lams, rri, lam, n)
        call linear_interpolation(lams, iri, lam, k)
end subroutine interp_ri

subroutine linear_interpolation(xData, yData, xTarget, yInterp)
        implicit none
        real(wp), dimension(:), intent(in) :: xData, yData
        real(wp), intent(in) :: xTarget
        real(wp), intent(out) :: yInterp

        integer(ik) :: i, n
        real(wp) :: t

        n = size(xData)

        ! Check if xTarget is outside the range of xData
        if (xTarget < xData(1)) then
        ! Extrapolate to the left
        i = 1
        elseif (xTarget > xData(n)) then
        ! Extrapolate to the right
        i = n - 1
        else
        ! Find the index i such that xData(i) <= xTarget <= xData(i+1)
        i = 1
        do while (xData(i) < xTarget)
            i = i + 1
        end do
        i = i - 1
        end if

        ! Linear interpolation formula: y = y1 + (x - x1) * (y2 - y1) / (x2 - x1)
        t = (xTarget - xData(i)) / (xData(i + 1) - xData(i))
        yInterp = yData(i) + t * (yData(i + 1) - yData(i))
end subroutine linear_interpolation
    
subroutine interp_linear(x, ppf, references, interp)
        real(wp), intent(in) :: x
        real(wp), intent(out) :: interp
        real(wp), dimension(:), intent(in) :: ppf, references
        integer(ik) :: i

        do i = 3, size(ppf)
        if (x < ppf(i)) then
            !print *, i
            interp = references(i-1) + (references(i) - references(i-1)) * &
                                (x - ppf(i-1)) / (ppf(i) - ppf(i-1))
            exit
        end if
        end do
end subroutine interp_linear
    
subroutine read_csv(filename, data1, data2, data3)
        CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: filename
        real(wp), dimension(:), allocatable :: data1, data2, data3
        integer(ik) :: num_rows, io_status, i

        open(unit=1, file=filename, status='old', action='read', iostat=io_status)
        if (io_status /= 0) then
             write(*, *) 'Error opening file: ', filename
             stop
        end if

        num_rows = 0
        do
            read(1, *, iostat=io_status)
            if (io_status /= 0) exit
            num_rows = num_rows + 1
        end do

        rewind(1)

        if (allocated(data1)) deallocate(data1)
        if (allocated(data2)) deallocate(data2)
        if (allocated(data3)) deallocate(data3)

        allocate(data1(num_rows), data2(num_rows), data3(num_rows))

        do i = 1, num_rows
           read(1, *) data1(i), data2(i), data3(i)
        end do
        close(1)
end subroutine read_csv
    
subroutine read_min_max_data(filename, nrows, vname, max_vals, min_vals)
        implicit none    
        integer(ik) :: i, j

        CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: filename
        integer(ik), intent(in) :: nrows
        character(20), dimension(nrows), intent(out) :: vname
        real(wp), dimension(nrows), intent(out) :: min_vals, max_vals
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: line    

        open(1, file=filename, status='old', action='read')
        read(1, *) 

        j = 1
        do
            read(1, '(A)', iostat=i) line
            ! Check if the end of file is reached
            if (i /= 0) then
                exit
            end if

            read(line, *) vname(j), max_vals(j), min_vals(j)        
            j = j+1
        end do
        close(1)
end subroutine read_min_max_data
    
subroutine read_quantile_data(filename, ext, sca, asy, qua, ppf, nrows)
        implicit none    
        
        real(wp), dimension(:), intent(out) :: ext, sca, asy, qua, ppf
        CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: filename
        CHARACTER(LEN=MAX_CHAR_LENGTH) :: line
        integer(ik), intent(in) :: nrows
        integer(ik) :: i, j, num
        
        open(1, file=filename, status='old', action='read')
        read(1, *) 
        
        do j = 1, nrows  
            read(1, '(A)', iostat=i) line
            read(line, *) num, ext(j), sca(j), asy(j), qua(j), ppf(j)
        end do
        close(1)
end subroutine read_quantile_data
end module mo_art_radiation_mie_emulation
