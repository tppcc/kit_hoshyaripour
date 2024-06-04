module nachmo_coding
    USE ftorch
    subroutine load_model(path, model)
        ! Loading the torch model using FTorch
        ! Input:
        !       path (char)             : Path to the torch model
        !       model (torch_module)    : Model object, passing an empty model 
        !                                 object and return a loaded object
        ! Output:
        !       model (torch_module)    : Model object, return inplace loaded model

        type(torch_module), intent(inout) :: model
        CHARACTER(LEN=MAX_CHAR_LENGTH), intent(in) :: path

        model = torch_module_load(path) ! "/my/saved/TorchScript/model.pt"
    end subroutine load_model

    subroutine emulate(arr, arr_out, n_inputs, model)
        real(wp), dimension(:), allocatable, intent(in) :: arr
        real(wp), dimension(:), allocatable, intent(inout) :: arr_out
        integer, intent(in) :: n_inputs

        type(torch_tensor), dimension(:), allocatable :: model_inputs_arr
        type(torch_tensor) :: model_output

        integer, parameter :: in_dims = 2
        integer :: in_layout(in_dims) = [1,2]
        integer, parameter :: out_dims = 1
        integer :: out_layout(out_dims) = [1]

        model_inputs_arr(1) = torch_tensor_from_array(arr, in_layout, torch_kCPU)
        model_output = torch_tensor_from_array(arr_out, out_layout, torch_kCPU)

        call torch_module_forward(model, model_input_arr, n_inputs, model_output)

    end subroutine emulate

end module nachmo_coding