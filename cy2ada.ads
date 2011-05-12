with Interfaces.C;                     use Interfaces.C;
with Interfaces.C.Extensions;          use Interfaces.C.Extensions;
with Interfaces.C.Strings;             use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Standard_Complex_Polynomials;     use Standard_Complex_Polynomials;
with Generic_Polynomials;

package Cy2ada is
   type Int_Array is array ( Integer range <>) of aliased int;
   package Ints_Ptrs is
      new Interfaces.C.Pointers( Integer, int, Int_Array, 0);
   use Ints_Ptrs;
   type Int_Ptr is new Ints_Ptrs.Pointer;

   type Double_Array is array ( Integer range <>) of aliased double;
   package Doubles_Ptrs is
      new Interfaces.C.Pointers( Integer, double, Double_Array, 0.0);
   use Doubles_Ptrs;
   type Double_Ptr is new Doubles_Ptrs.Pointer;

   function New_Poly ( N : Integer; Input_String : Chars_Ptr ) return Poly;
   pragma Export ( C, New_Poly, "new_poly" );

   procedure Free_Poly ( Poly_Ptr : in Poly );
   pragma Export ( C, Free_Poly, "free_ada_poly" );

   function Is_Null_Poly( P : Poly) return Integer;
   pragma Export ( C, Is_Null_Poly, "is_null_poly" );

   function Poly_To_String ( P : in Poly) return Chars_Ptr;
   pragma Export ( C, Poly_To_String, "poly_to_string" );

   procedure Free_String ( Ptr : in Chars_Ptr );
   pragma Export ( C, Free_String, "free_ada_string" );

   function Num_Unknowns ( P : in Poly ) return Natural;
   pragma Export ( C, Num_Unknowns, "num_unknowns" );

   function Num_Terms ( P : in Poly ) return Natural;
   pragma Export ( C, Num_Terms, "num_terms" );

   procedure Poly_Coeff( P : in Poly;  Deg_Ptr : in Int_Ptr; Res_Ptr : in Double_Ptr);
   pragma Export ( C, Poly_Coeff, "poly_coeff" );

   procedure Call_Poly( P      : in Poly;
                        X_Real : in Double_Ptr;
                        X_Imag : in Double_Ptr;
                        Y      : in Double_Ptr);
   pragma Export ( C, Call_Poly , "call_poly" );

   procedure Get_Terms ( P : in Poly;
                         Degs : in Int_Ptr;
                         Reals : in Double_Ptr;
                         Imags : in Double_Ptr);
   pragma Export ( C, Get_Terms, "get_terms" );

end Cy2ada;
