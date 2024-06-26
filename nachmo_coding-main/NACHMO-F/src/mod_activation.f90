module mod_activation

  ! A collection of activation functions and their derivatives.

  use mod_kinds, only: ik, rk

  implicit none

  private

  public :: activation_function
  public :: gaussian, gaussian_prime
  public :: relu, relu_prime
  public :: prelu1, prelu2
  public :: sigmoid, sigmoid_prime
  public :: step, step_prime
  public :: tanhf, tanh_prime
  public :: sine, sine_prime
  public :: ini_sine, ini_sine_prime
  public :: lin, lin_prime

  interface
    pure function activation_function(x)
      import :: rk
      real(rk), intent(in) :: x(:)
      real(rk) :: activation_function(size(x))
    end function activation_function
  end interface

contains

  pure function gaussian(x) result(res)
    ! Gaussian activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = exp(-x**2)
  end function gaussian

  pure function gaussian_prime(x) result(res)
    ! First derivative of the Gaussian activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = -2 * x * gaussian(x)
  end function gaussian_prime

  pure function relu(x) result(res)
    !! REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = max(0., x)
  end function relu



  pure function prelu2(x) result(res)
    !! REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = max(0.000000001 * x, x)
  end function prelu2


  pure function prelu1(x) result(res)
    !! REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res =  max(0.1164203062653541565 * x, x)
  end function prelu1


  pure function relu_prime(x) result(res)
    ! First derivative of the REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    where (x > 0)
      res = 1
    elsewhere
      res = 0
    end where
  end function relu_prime

  pure function sigmoid(x) result(res)
    ! Sigmoid activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 1 / (1 + exp(-x))
  endfunction sigmoid

  pure function sigmoid_prime(x) result(res)
    ! First derivative of the sigmoid activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = sigmoid(x) * (1 - sigmoid(x))
  end function sigmoid_prime

  pure function step(x) result(res)
    ! Step activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    where (x > 0)
      res = 1
    elsewhere
      res = 0
    end where
  end function step

  pure function step_prime(x) result(res)
    ! First derivative of the step activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 0
  end function step_prime

  pure function tanhf(x) result(res)
    ! Tangent hyperbolic activation function. 
    ! Same as the intrinsic tanh, but must be 
    ! defined here so that we can use procedure
    ! pointer with it.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = tanh(x)
  end function tanhf

  pure function tanh_prime(x) result(res)
    ! First derivative of the tanh activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 1 - tanh(x)**2
  end function tanh_prime
  
  
  
  pure function ini_sine(x) result(res)
    ! Tangent hyperbolic activation function. 
    ! Same as the intrinsic tanh, but must be 
    ! defined here so that we can use procedure
    ! pointer with it.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = sin(6*x)
  end function ini_sine
  
  
  pure function ini_sine_prime(x) result(res)
    ! First derivative of the tanh activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 6*cos(6*x)
  end function ini_sine_prime
  
  pure function sine(x) result(res)
    ! Tangent hyperbolic activation function. 
    ! Same as the intrinsic tanh, but must be 
    ! defined here so that we can use procedure
    ! pointer with it.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = sin(4*x)
  end function sine

  pure function sine_prime(x) result(res)
    ! First derivative of the tanh activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = 4*cos(4*x)
  end function sine_prime
  
  
 
   pure function lin(x) result(res)
    !! REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))
    res = x
  end function lin

  pure function lin_prime(x) result(res)
    ! First derivative of the REctified Linear Unit (RELU) activation function.
    real(rk), intent(in) :: x(:)
    real(rk) :: res(size(x))

      res = 1

  end function lin_prime
 
 

end module mod_activation
