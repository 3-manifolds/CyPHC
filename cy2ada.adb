with text_io,integer_io;               use text_io,integer_io;
with Interfaces.C;                     use Interfaces.C;
with Interfaces.C.Strings;             use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Interfaces.C.Extensions;          use Interfaces.C.Extensions;
with Symbol_Table;
with Strings_And_Numbers;              use Strings_And_Numbers;
with Generic_Polynomials;
with Parse_Polynomial_Exceptions;
with Standard_Natural_Vectors;
with Standard_Complex_Vectors;         use Standard_Complex_Vectors;
with Standard_Floating_Numbers;        use Standard_Floating_Numbers;
with Standard_Floating_Numbers_Io;     use Standard_Floating_Numbers_Io;
with Standard_Complex_Numbers;         use Standard_Complex_Numbers;
with Standard_Complex_Numbers_Io;      use Standard_Complex_Numbers_Io;
with Standard_Natural_Vectors_Io;      use Standard_Natural_Vectors_Io;
with Standard_Complex_Polynomials;     use Standard_Complex_Polynomials;
with Standard_Complex_Poly_Functions;  use Standard_Complex_Poly_Functions;
with Standard_Complex_Polynomials_io;  use Standard_Complex_Polynomials_io;
with Standard_Complex_Poly_Strings;    use Standard_Complex_Poly_Strings;

package body Cy2ada is

   function New_Poly ( N : in Integer; Input_String : in Chars_Ptr)
                     return Poly is
      V  : constant string := Value(Input_String);
   -- Create a new PHC Poly object from a C string.
   begin
      Symbol_Table.Init(N);
      return Parse(N ,V);
   exception
      when others => return Null_Poly;
   end New_Poly;

   function Is_Null_Poly( P : Poly )
                        return integer is
      -- Test if a PHC Poly is null
   begin
      if P = Null_Poly Then
         return 1;
      else return 0;
      end if;
   end Is_Null_Poly;

   procedure Free_Poly ( Poly_Ptr : in Poly ) is
      P : Poly := Poly_Ptr;
      -- Free a PHC Poly object.
   begin
      Clear(P);
   end Free_Poly;

   function Num_Unknowns ( P : in Poly )
                         return Natural is
      -- Return the number of variables of a Poly
   begin
      return Number_Of_Unknowns(P);
   end Num_Unknowns;

   function Num_Terms ( P : in Poly )
                      return Natural is
      -- Return the number of terms of a Poly
   begin
      return Number_Of_Terms(P);
   end Num_Terms;

   function Degrees_From_C(Ints : in Int_Ptr; N : in Integer)
                          return Degrees is
      Ptr : Int_Ptr := Ints;
      Result : Degrees := new Standard_Natural_Vectors.Vector'(1..N => 0);
      begin
         for K in 1..N loop
            Result(K) := Integer( Ptr.all );
            Increment(Ptr);
         end loop;
         return Result;
      end Degrees_From_C;

   procedure Poly_Coeff( P : in Poly;
                         Degs : in Int_Ptr;
                         Res : in Double_Ptr) is
      D    : Int_Ptr    := Degs;
      C    : Double_Ptr := Res;
      Dim  : Integer    := Number_Of_Unknowns(P);
      Z    : Complex_Number;
   begin
      Z := Coeff(P, Degrees_From_C(D, Dim));
      C.all := double(REAL_PART(Z));
      Increment(C);
      C.all := double(IMAG_PART(Z));
   end Poly_Coeff;

   procedure Get_Terms ( P : in Poly;
                         Degs : in Int_Ptr;
                         Reals : in Double_Ptr;
                         Imags : in Double_Ptr ) is
      D : Int_Ptr := Degs;
      R : Double_Ptr := Reals;
      I : Double_Ptr := Imags;
      Dim : Integer := Number_Of_Unknowns(P);
      procedure Visit_Term  ( T : in Term; Continue : out Boolean ) is
      begin
         for K in 1..Dim loop
            D.all := int(T.Dg(K));
            Increment(D);
         end loop;
         R.all := double(REAL_PART(T.cf));
         I.all := double(IMAG_PART(T.Cf));
         Increment(R); Increment(I);
         Continue := True;
      end Visit_Term;
      procedure Visit_Terms is new Visiting_Iterator(Visit_Term);
   begin
      Visit_Terms(P);
   end Get_Terms;

   procedure Call_Poly( P      : in Poly;
                        X_Real : in Double_Ptr;
                        X_Imag : in Double_Ptr;
                        Y      : in Double_Ptr) is
      R_Cursor : Double_Ptr := X_Real;
      I_Cursor : Double_Ptr := X_Imag;
      Y_Cursor : Double_Ptr := Y;
      Dim      : Integer := Number_Of_Unknowns(P);
      X        : Vector  := Vector'( 1..Dim => Create(0.0) );
      Value    : Complex_Number;
   begin
      for K in 1..Dim loop
         X(K) := Create(Double_Float(R_Cursor.all),
                        Double_Float(I_Cursor.all));
         Increment(R_Cursor);
         Increment(I_Cursor);
      end loop;
         Value := Eval(P, X);
         Y_Cursor.all := Double(REAL_PART(Value));
         Increment(Y_Cursor);
         Y_Cursor.all := Double(IMAG_PART(Value));
   end Call_Poly;

   function Poly_To_String ( P : in Poly ) return Chars_Ptr is
   -- Return a C string containing the canonical representation of a PHC Poly.
   begin
      return New_Char_Array(To_C(Write(P)));
   end Poly_To_String;

   procedure Free_String ( Ptr : in Chars_Ptr ) is
      S : Chars_Ptr := Ptr; -- pointer parameters must be copied to a variable.
      -- Free a character array generated from an Ada string.
   begin
      Free(S);
   end Free_String;





end Cy2ada;
