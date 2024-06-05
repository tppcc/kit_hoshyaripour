module mo_art_nachmo_mlp
    USE ftorch
    
    IMPLICIT NONE

    private

    TYPE :: nachmo_coding
        ! Class object wraper of the the loaded model, load_model subroutine and emulation method.
        ! Attributes:
        !       model (torch_module)    : Model object, passing an empty model
        !                                 object and return a loaded object
        private
        type(torch_module) :: model
        CONTAINS
        procedure :: load_model => nachmo_model_load
        procedure :: emulate => nachmo_emulate
    END TYPE nachmo_coding

    public :: nachmo_coding

    CONTAINS

    subroutine load_model(self, path)
        ! Loading the torch model using FTorch, return the loaded object to self.model
        ! Input:
        !       path (char)             : Path to the torch model
        class(nachmo_coding), intent(inout) :: self
        CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: path

        self % model = torch_module_load(path)
    end subroutine load_model

    subroutine emulate(self, arr, arr_out, n_inputs)
        ! Emulate Chemical reaction based on nachmo_mlp.
        ! Input:
        !       arr (array, allocatable)        : Input array object, column major
        !       arr_out (array, allocatable)    : Empty output array object, column major
        !       n_inptus (integer)              : Number of input tensors
        ! Return:
        !       arr_out (array, allocatable)    : Output of the NN emulation, column major

        class(nachmo_coding), intent(inout) :: self
        real(wp), dimension(:), allocatable, intent(in) :: arr
        real(wp), dimension(:), allocatable, intent(inout) :: arr_out
        integer, intent(in) :: n_inputs

        type(torch_tensor), dimension(:), allocatable :: model_inputs_arr
        type(torch_tensor) :: model_output


        integer :: i, in_dims, out_dims
        integer, allocatable :: in_layout(:)
        integer, allocatable :: out_layout(:)

        in_dims = SIZE(SHAPE(arr))
        out_dims = SIZE(SHAPE(arr_out))

        allocate(in_layout(in_dims))
        allocate(out_layout(out_dims))

        ! Setting layout for dynamic dimensions
        do i = 1, in_dims
            in_layout(i) = i
        end do

        do i = 1, out_dims
            out_layout(i) = i
        end do

        ! Transpose the inpt array from column major (Fortran style) to row major (C++ style)
        model_inputs_arr(1) = torch_tensor_from_array(arr, in_layout, torch_kCUDA,   &
                                                        device_index=device_index)
        model_output = torch_tensor_from_array(arr_out, out_layout, torch_kCUDA,   &
                                                        device_index=device_index)

        ! Call the foward method of the loaded model, return the row major model_output object
        call torch_module_forward(self%model, model_input_arr, n_inputs, model_output)

    end subroutine emulate

end module mo_art_nachmo_mlp


