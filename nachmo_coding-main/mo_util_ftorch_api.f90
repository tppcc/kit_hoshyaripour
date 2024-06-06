module mo_util_ftorch_api
    USE ftorch

    IMPLICIT NONE

    private

    TYPE :: ftorch_net
        ! Universal class object for handling trained Torch Model in Torch Script form
        ! For details on FTorch see github repo FTorch
        ! Attributes:
        !       model (torch_module)    : Model object, passing an empty model
        !                                 object and return a loaded object
        ! Methods:
        !       model_load (method)     : Method to load the model, passing path to the trained
        !                                 Torch Script
        !       inference (method)      : Method to perform inference, pass input and output array,
        !                                 as well as n_input
        private
        type(torch_module) :: model     ! Model object of the Torch model
        CONTAINS
        procedure :: load_model => model_load
        procedure :: emulate => inference
    END TYPE ftorch_net
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

    subroutine emulate(self, arr, arr_out, n_inputs, device_type, device_index)
        ! Emulate Chemical reaction based on nachmo_mlp.
        ! Input:
        !       arr (array, allocatable)        : Input array object, column major
        !       arr_out (array, allocatable)    : Empty output array object, column major
        !       n_inptus (integer)              : Number of input tensors
        !       device_type (c_int)             : Device type for which the target tensor lives on
        !                                         ('torch_kCPU', 'torch_kCUDA')
        ! Return:
        !       arr_out (array, allocatable)    : Output of the NN emulation, column major

        use, intrinsic :: iso_c_binding, only : c_int
        ! Defining input types
        class(nachmo_coding), intent(inout) :: self
        real(wp), dimension(:), allocatable, intent(in) :: arr
        real(wp), dimension(:), allocatable, intent(inout) :: arr_out
        integer, intent(in) :: n_inputs
        integer(c_int), intent(in)     :: device_type
        integer(c_int), optional, intent(in) :: device_index 

        type(torch_tensor), dimension(:), allocatable :: model_inputs_arr
        type(torch_tensor) :: model_output


        integer :: i, in_dims, out_dims
        integer, allocatable :: in_layout(:)
        integer, allocatable :: out_layout(:)

        in_dims = SIZE(SHAPE(arr))
        out_dims = SIZE(SHAPE(arr_out))

        allocate(in_layout(in_dims))
        allocate(out_layout(out_dims))

        ! Setting layout for dynamic dimensions from arr and arr_out
        do i = 1, in_dims
            in_layout(i) = i
        end do

        do i = 1, out_dims
            out_layout(i) = i
        end do

        ! Process optional arguments
        if (present(device_index)) then
            device_index_value = device_index
        else if (device_type == torch_kCPU) then
            device_index_value = -1
        else
            device_index_value = 0
        endif

        ! Transpose the inpt array from column major (Fortran style) to row major (C++ style)
        model_inputs_arr(1) = torch_tensor_from_array(arr, in_layout, device_type,   &
                                                        device_index=device_index)
        model_output = torch_tensor_from_array(arr_out, out_layout, device_type,   &
                                                        device_index=device_index)

        ! Call the foward method of the loaded model, return the row major model_output object
        call torch_module_forward(self%model, model_input_arr, n_inputs, model_output)

    end subroutine emulate

end module mo_util_ftorch_api

