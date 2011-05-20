with Unchecked_Deallocation;
with Text_Io,Integer_Io;                 use Text_Io,Integer_Io;
with Interfaces.C;                       use Interfaces.C;
with Interfaces.C.Strings;               use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Interfaces.C.Extensions;            use Interfaces.C.Extensions;
with Symbol_Table;                       use Symbol_Table;
with Strings_And_Numbers;                use Strings_And_Numbers;
with Standard_Floating_Numbers;          use Standard_Floating_Numbers;
with Standard_Floating_Numbers_Io;       use Standard_Floating_Numbers_Io;
with Standard_Complex_Numbers;           use Standard_Complex_Numbers;
with Standard_Complex_Numbers_Io;        use Standard_Complex_Numbers_Io;
with Generic_Polynomials;
with Standard_Natural_Vectors;
with Standard_Natural_Vectors_Io;        use Standard_Natural_Vectors_Io;
with Standard_Integer_Vectors;           use Standard_Integer_Vectors;
with Standard_Integer_Vectors_Io;        use Standard_Integer_Vectors_Io;
with Standard_Floating_Vectors;
with Standard_Complex_Vectors;           use Standard_Complex_Vectors;
with Standard_Integer_VecVecs;
with Standard_Complex_VecVecs;
with Arrays_of_Integer_Vector_Lists;
with Arrays_of_Integer_Vector_Lists_io;
with Arrays_of_Floating_Vector_Lists;
with Arrays_of_Floating_Vector_Lists_io;
with Parse_Polynomial_Exceptions;        use Parse_Polynomial_Exceptions;
with Standard_Complex_Polynomials;       use Standard_Complex_Polynomials;
with Standard_Complex_Poly_Functions;    use Standard_Complex_Poly_Functions;
with Standard_Complex_Polynomials_io;    use Standard_Complex_Polynomials_io;
with Standard_Complex_Poly_Strings;      use Standard_Complex_Poly_Strings;
with Standard_Complex_Poly_Systems;      use Standard_Complex_Poly_Systems;
with Standard_Complex_Poly_Systems_io;   use Standard_Complex_Poly_Systems_io;
with Floating_Mixed_Subdivisions;        use Floating_Mixed_Subdivisions;
with Floating_Mixed_Subdivisions_io;     use Floating_Mixed_Subdivisions_io;
with Standard_Complex_Solutions;         use Standard_Complex_Solutions;
with Standard_Complex_Solutions_io;      use Standard_Complex_Solutions_io;
with Standard_System_and_Solutions_io;
with Floating_Integer_Convertors;
with Floating_Lifting_Utilities;
with Cell_Stack;                         use Cell_Stack;
with Mixedvol_Algorithm;                 use Mixedvol_Algorithm;
with Drivers_For_Mixedvol_Algorithm;     use Drivers_For_Mixedvol_Algorithm;
package body Cy2ada is

   procedure Reset_Symbols( Max : in Integer ) is
      -- Free the current symbol table and create an empty one of size Max.
   begin
      Symbol_Table.Clear;
      Symbol_Table.Init(Max);
   end Reset_Symbols;

   procedure Add_Symbol( Symbol_Str : in Chars_Ptr ) is
      -- Append a symbol to the current table.
      S : constant String := Value(Symbol_Str);
   begin
      Symbol_Table.Add_String(S);
   end Add_Symbol;

   function New_Poly ( N : in Integer;
                       Input_String : in Chars_Ptr;
                       Return_Code : in Int_Ptr)
                     return Poly is
      -- Create a new PHC Poly object from a C string.
      Code : Int_Ptr := Return_Code;
      V  : constant string := Value(Input_String);
   begin
      return Parse(N, V);
   exception
      when ILLEGAL_CHARACTER => Code.all := 1; return Null_Poly;
      when ILLEGAL_OPERATION => Code.all := 2; return Null_Poly;
      when OVERFLOW_OF_UNKNOWNS => Code.all := 3; return Null_Poly;
      when BAD_BRACKET => Code.all := 4; return Null_Poly;
      when INVALID_SYMBOL => Code.all :=5; return Null_Poly;
      when others => Code.all := 6; return Null_Poly;
   end New_Poly;

   procedure Free_Poly ( Poly_Ptr : in Poly ) is
      P : Poly := Poly_Ptr;
      -- Free a PHC Poly object.
   begin
      Clear(P);
   end Free_Poly;

   function Is_Null_Poly( P : Poly )
                        return integer is
      -- Test if a PHC Poly is null.
   begin
      if P = Null_Poly Then
         return 1;
      else return 0;
      end if;
   end Is_Null_Poly;

   function Num_Unknowns ( P : in Poly )
                         return Natural is
      -- Return the number of variables of a PHC Poly.
   begin
      return Number_Of_Unknowns(P);
   end Num_Unknowns;

   function Num_Terms ( P : in Poly )
                      return Natural is
      -- Return the number of terms in a PHC Poly.
   begin
      return Number_Of_Terms(P);
   end Num_Terms;

   function Degrees_From_C(Ints : in Int_Ptr; N : in Integer)
                          return Degrees is
      -- Convert a C array of ints to an Ada vector of Naturals.
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
      -- Get the coefficient with the given multi-degree from a Poly.
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
      -- Get arrays of degrees and coefficients.
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
      -- Evaluate a PHC Poly on a vector of complex numbers.
      R_Cursor : Double_Ptr := X_Real;
      I_Cursor : Double_Ptr := X_Imag;
      Y_Cursor : Double_Ptr := Y;
      Dim      : Integer := Number_Of_Unknowns(P);
      X        : Standard_Complex_Vectors.Vector
               := Standard_Complex_Vectors.Vector'( 1..Dim => Create(0.0) );
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

   function Specialize_Poly (P      : in Poly;
                             X_Real : in Double_Ptr;
                             X_Imag : in Double_Ptr;
                             N      : in Integer) return Poly is
      -- Specialize one variable of a Poly, returning a Poly with that variable gone.
      X : Complex_Number := Create(Double_Float(X_Real.all),
                                   Double_Float(X_Imag.all));
      Result : Poly;
   begin
      Result := Eval(P, X, N);
      return Result;
   end Specialize_Poly;

   function Poly_To_String ( P : in Poly ) return Chars_Ptr is
      -- Return a C string containing the canonical representation of a PHC Poly.
      Poly_String : constant String := Write(P);
   begin
      return New_Char_Array(To_C(Poly_String));
   end Poly_To_String;

   procedure Free_String ( Ptr : in Chars_Ptr ) is
      -- Free a character array generated from an Ada string.
      S : Chars_Ptr := Ptr;
   begin
      Free(S);
   end Free_String;

   procedure Mixed_Volume_Algorithm
     (
      N        : in  Natural; -- number of variables = number of polys
      M        : in  Natural; -- total size of support
      Indices  : in  Int_Ptr;
      Sizes    : in  Int_Ptr;
      Supports : in  Int_Ptr
     ) is
      Index_Ptr : Int_Ptr := Indices;
      Size_Ptr  : Int_Ptr := Sizes;
      Supp_Ptr  : Int_Ptr := Supports;
      R         : Natural;
      Mix,Perm  : Standard_Integer_Vectors.Link_to_Vector;
      Ind       : Standard_Integer_Vectors.Vector(1..N);
      Cnt       : Standard_Integer_Vectors.Vector(1..N);
      Supp      : Standard_Integer_Vectors.Vector(1..N*M);
      Sub       : Mixed_Subdivision;
      Mixvol    : Natural;
      Q         : Link_To_Poly_Sys := new Poly_Sys(1..N);
      Qsols     : Solution_List;
   begin
      for I in 1..N loop
         Ind(I) := Integer(Index_Ptr.all);
         Cnt(I) := Integer(Size_Ptr.all);
         Increment(Index_Ptr);
         Increment(Size_Ptr);
      end loop;
      for I in 1..N*M loop
         Supp(I) := Integer(Supp_Ptr.all);
         Increment(Supp_Ptr);
      end loop;
      Put("Computing mixed volume"); New_Line;
      Compute_Mixed_Volume(N, M, Ind,  Cnt, Supp, 0.0,
                           R, Mix, Perm, Sub, MixVol);
      declare
         LS : Arrays_of_Floating_Vector_Lists.Array_of_Lists(Mix'range)
            := Floating_Lifting_Utilities.Lifted_Supports(mix'last,sub);
      begin
         Put("Solving the random system:"); New_Line;
         Random_Coefficient_System(N, Mix.all, LS, Sub, Q.all, Qsols);
         Put(Q.all); New_Line;
         Put(Qsols); New_Line;
      end;
   end Mixed_Volume_Algorithm;

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
      Mixvol   : out natural ) is

      Size,Nb     : natural;
      Mtype,Idx     : Standard_Integer_Vectors.Link_to_Vector;
      Vtx           : Standard_Integer_VecVecs.Link_to_VecVec;
      Lift          : Standard_Floating_Vectors.Link_to_Vector;
      Cells         : CellStack;

   begin
      Put("Number of variables: "); Put(N,1); New_Line;
      Put("Size of support: "); Put(M,1); New_Line;
      Put("Indices: ");
      Standard_Integer_Vectors_IO.Put(Ind); New_Line;
      Put("Counts: ");
      Standard_Integer_Vectors_IO.Put(Cnt); New_Line;
      Put("Support: ");
      Standard_Integer_Vectors_IO.Put(Supp); New_Line;

      mv(N, M, Ind, Cnt, Supp, Stlb,
         R, Mtype, Perm, Idx, Vtx, Lift, Size, Nb, Cells, Mixvol);
      Put("The mixed volume is "); Put(mixvol, 1); Put_Line(".");
      Put("There are "); Put(nb, 1); Put_Line(" mixed cells.");
      put_line("Creating a regular mixed-cell configuration ...");
      Put("R = "); Put(R,1); New_Line;
      if R < N
      then
         Put("Using the permutation: ");
         for I in 1..N loop
            Put(Perm.all(I),1); Put(", ");
         end loop;
         New_Line;
         Create_Mixed_Cell_Configuration
        (N, R, Size, Nb, Mtype, Perm, Vtx, Lift, Cells, Sub );
      else
         Put("Not using the  permutation.");New_Line;
         Create_Mixed_Cell_Configuration
        (N, R, Size, Nb, Mtype, Vtx, Lift, Cells, Sub );
      end if;
      Mix := new Standard_Integer_Vectors.Vector( 1..R );
      Put("Mix: ");
      for I in 1..R loop
         Mix(I ) := Mtype(I-1);
         Put(Mix(I),1);Put(", ");
      end loop;
      New_Line;
   end Compute_Mixed_Volume;

end Cy2ada;
