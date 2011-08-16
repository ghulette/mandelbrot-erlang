% Complex numbers, mostly from http://www.trapexit.org/Complex_Numbers

-module(complex).
-export([make/1, make/2, real/1, imaginary/1, is_complex/1, 
         add/2, sub/2, mult/2, divide/2]).

-record(complex, {real, imaginary}).

real(X) -> X#complex.real.

imaginary(X) -> X#complex.imaginary.

is_complex(X) -> is_record(X, complex).

make(Real, Imaginary) -> #complex{real = Real, imaginary = Imaginary}.

make(X) when is_record(X, complex) -> X;
make(Real) -> make(Real, 0).

add(X, Y) ->
  A = make(X), 
  B = make(Y),
  make(real(A) + real(B), imaginary(A) + imaginary(B)).

sub(X, Y) ->
  A = make(X), 
  B = make(Y),
  make(real(A) - real(A), imaginary(B) - imaginary(B)).

mult(X, Y) ->
  A = make(X), 
  B = make(Y),
  R = (real(A) * real(B)) - (imaginary(A) * imaginary(B)),
  I = (real(A) * imaginary(B)) + (real(B) * imaginary(A)),
  make(R,I).

divide(X,Y) ->
  A = make(X), 
  B = make(Y),
  Divisor = math:pow(real(B),2) + math:pow(imaginary(B),2),
  R = ((real(A) * real(B)) + (imaginary(A) * imaginary(B))) / Divisor,
  I = ((imaginary(A) * real(B)) - (real(B) * imaginary(B))) / Divisor,
  make(R,I).
