with Interfaces.C;                     use Interfaces.C;
with Interfaces.C.Extensions;          use Interfaces.C.Extensions;
with Interfaces.C.Strings;             use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Standard_Floating_Numbers;        use Standard_Floating_Numbers;
with Standard_Integer_Vectors;
with Standard_Complex_Vectors;
with Standard_Complex_Polynomials;     use Standard_Complex_Polynomials;
with Generic_Polynomials;
with Generic_Polynomial_Systems;
with Standard_Complex_Poly_Systems;    use Standard_Complex_Poly_Systems;
with Symbol_Table;
with Floating_Mixed_Subdivisions;      use Floating_Mixed_Subdivisions;
with Standard_Complex_Solutions;       use Standard_Complex_Solutions;

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

   type Link_To_Solution_Array is access Solution_Array;

   type Solved_System is record
      System    : Link_To_Poly_Sys;
      Num_Solns : Natural;
      Solutions : Solution_List;
   end record;

   type Link_To_Solved_System is access Solved_System;

   procedure Reset_Symbols( Max : in Integer);
   pragma Export ( C, Reset_Symbols, "reset_symbols" );

   procedure Add_Symbol( Symbol_Str : in Chars_Ptr );
   pragma Export ( C, Add_Symbol, "add_symbol" );

   function New_Poly ( N : Integer;
                       Input_String : Chars_Ptr;
                       Return_Code : Int_Ptr ) return Poly;
   pragma Export ( C, New_Poly, "new_poly" );

   procedure Free_Poly ( Poly_Ptr : in Poly );
   pragma Export ( C, Free_Poly, "free_poly" );

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

   procedure Poly_Coeff
     ( P    : in Poly;
       Degs : in Int_Ptr;
       Res  : in Double_Ptr);
   pragma Export ( C, Poly_Coeff, "poly_coeff" );

   procedure Call_Poly
     ( P      : in Poly;
       X_Real : in Double_Ptr;
       X_Imag : in Double_Ptr;
       Y      : in Double_Ptr);
   pragma Export ( C, Call_Poly , "call_poly" );

   procedure Get_Terms
     ( P     : in Poly;
       Degs  : in Int_Ptr;
       Reals : in Double_Ptr;
       Imags : in Double_Ptr );
   pragma Export ( C, Get_Terms, "get_terms" );

   function Specialize_Poly
     ( P      : in Poly;
       X_Real : in Double_Ptr;
       X_Imag : in Double_Ptr;
       N      : in Integer) return Poly;
   pragma Export ( C, Specialize_Poly, "specialize_poly" );

   function Mixed_Volume_Algorithm
     ( N        : in  Natural; -- number of variables = number of polys
       M        : in  Natural; -- total size of support
       Indices  : in  Int_Ptr;
       Sizes    : in  Int_Ptr;
       Supports : in  Int_Ptr ) return Link_To_Solved_System;
   pragma Export ( C, Mixed_Volume_Algorithm, "mixed_volume_algorithm" );

   procedure Compute_Mixed_Volume
     (
      N        : in  Natural; -- number of variables = number of polys
      M        : in  Natural; -- total size of support
      Ind      : in  Standard_Integer_Vectors.Vector;
      Cnt      : in  Standard_Integer_Vectors.Vector;
      Supp     : in  Standard_Integer_Vectors.Vector;
      Stlb     : in  double_float;
      R        : out natural;
      Mix,Perm : out Standard_Integer_Vectors.Link_to_Vector;
      Sub      : out Mixed_Subdivision;
      Mixvol   : out natural );

   function New_Solved_System (N : in Natural) return Link_To_Solved_System;
   pragma Export ( C, New_Solved_System, "new_solved_system" );

   function Get_Poly(Sys : in Link_To_Solved_System; Index : in Natural )
                    return Poly;
   pragma Export ( C, Get_Poly, "get_poly" );

   procedure Set_Poly( Sys   : in Link_To_Solved_System;
                       Index : in Integer;
                       P     : in Poly );
   pragma Export ( C, Set_Poly, "set_poly" );

   function Get_Num_Solns (Sys : in Link_To_Solved_System) return Int;
   pragma Export ( C, Get_Num_Solns, "get_num_solns" );

   procedure Get_Solution ( Sys   : in Link_To_Solved_System;
                            Index : in Natural;
                            Mult  : in Int_Ptr;
                            Info  : in Double_Ptr;
                            Real  : in Double_Ptr;
                            Imag  : in Double_Ptr );
   pragma Export ( C, Get_Solution, "get_solution" );

   procedure Do_Homotopy (Q : in Link_To_Solved_System;  -- solved start system
                          P : in Link_To_Solved_System;  -- unsolved target system
                          Allow_Clustering : in Integer  -- collisions allowed if > 0
                         );
   pragma Export ( C, Do_Homotopy, "do_homotopy" );

   procedure Filter_Solns ( P : in Link_To_Solved_System ; Tolerance : in Double_Ptr);
   pragma Export ( C, Filter_Solns, "filter_solns" );

   function Is_Bad_Solution( Ls : in Link_To_Solution ; Tolerance : in Double_Float )
                           return Boolean;

end Cy2ada;
