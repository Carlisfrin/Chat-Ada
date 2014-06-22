
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Sender_Buffering is
   package ASU renames Ada.Strings.Unbounded;

   use type Ada.Calendar.Time;
   use type Types.Seq_N_T;
   use type Llu.End_Point_Type;

   type Tree;
   type Tree_A is access Tree;
   type Tree is record
      Key   : Ada.Calendar.Time;
      Value : Value_T;
      Left  : Tree_A;
      Right : Tree_A;
   end record;


   procedure Free is new Ada.Unchecked_Deallocation (Tree, Tree_A);


   function Image (EP: LLU.End_Point_Type) return String is
      S : ASU.Unbounded_String := ASU.To_Unbounded_String(LLU.Image (EP));
      IP, Port  : ASU.Unbounded_String;
   begin
      if LLU.Is_Null (EP) then
         return "null";
      else
         S := ASU.Tail (S, ASU.Length (S) - ASU.Index (S, "IP:") + 1 - 4);

         IP := ASU.Head (S, ASU.Index (S, ", Port"));
         Port := ASU.Tail (S, ASU.Length (S) - ASU.Index (S, "Port:") + 1 - 7);

         return "(" & ASU.To_String(IP) & " " & ASU.To_String(Port) & ")";
      end if;
   end Image;


   protected P is
      function Tree_Size return Natural;

      procedure Delete (Key : Ada.Calendar.Time);

      function Min return Ada.Calendar.Time;

      function Is_Empty return Boolean;

      procedure Get (Key     : Ada.Calendar.Time;
                     Value   : out Value_T;
                     Success : out Boolean);

      procedure Get (Ep      : LLU.End_Point_Type;
                     Seq_N  : Types.Seq_N_T;
                     Key     : out Ada.Calendar.Time;
                     Value   : out Value_T;
                     Success : out Boolean);

      procedure Put (Key    : Ada.Calendar.Time;
                     Value  : Value_T);

      function Delete (P_Tree : Tree_A;
                       Key    : Ada.Calendar.Time)
                      return Tree_A;

      procedure Print_Tree;

   private
      P_Root : Tree_A;
   end P;


   protected body P is

      function Tree_Size (P_Tree : Tree_A) return Natural is
      begin
         if P_Tree /= null then
            return 1 + Tree_Size (P_Tree.Left) + Tree_Size (P_Tree.Right);
         else
            return 0;
         end if;
      end Tree_Size;

      function Tree_Size return Natural is
      begin
         return Tree_Size (P_Root);
      end Tree_Size;

      function Min (P_Tree : Tree_A) return Tree_A is
      begin
         if P_Tree = null then
            return null;
         end if;

         if P_Tree.Left = null then
            return P_Tree;
         else
            return Min (P_Tree.Left);
         end if;

      end Min;

      function Min return Ada.Calendar.Time is
      begin
         return Min (P_Root).key;
      end;

      function Is_Empty return Boolean is
      begin
         return P_Root = null;
      end Is_Empty;

      procedure Get (P_Tree  : Tree_A;
                     EP      : in  Llu.End_Point_Type;
                     Seq_N  : in  Types.Seq_N_T;
                     Key     : out Ada.Calendar.Time;
                     Value   : out Value_T;
                     Success : out Boolean) is
      begin
         Success := False;
         Value := Null_Value;

         if P_Tree /= null then
            if P_Tree.Value.Ep = Ep and then P_Tree.Value.Seq_N = Seq_N then
               Value := P_Tree.Value;
               Key   := P_Tree.Key;
               Success := True;
            else
               if P_Tree.Left /= null then
                  Get (P_Tree.Left, Ep, Seq_N, Key, Value, Success);
               end if;
               if not Success and then P_Tree.Right /= null then
                  Get (P_Tree.Right, Ep, Seq_N, Key, Value, Success);
               end if;
            end if;
         end if;
      end Get;

      procedure Get (ep       : in  Llu.End_Point_Type;
                     Seq_N   : in  Types.Seq_N_T;
                     Key      : out Ada.Calendar.Time;
                     Value    : out Value_T;
                     Success  : out Boolean) is
      begin
         Get (P_Root, Ep, Seq_N, Key, Value, Success);
      end Get;


      procedure Get (P_Tree  : Tree_A;
                     Key     : in  Ada.Calendar.Time;
                     Value   : out Value_T;
                     Success : out Boolean) is
      begin
         Value := Null_Value;
         if P_Tree = null then
            Success := False;
         elsif P_Tree.Key = Key then
            Value := P_Tree.Value;
            Success := True;
         elsif Key > P_Tree.Key then
            Get (P_Tree.Right, Key, Value, Success);
         else
            Get (P_Tree.Left, Key, Value, Success);
         end if;
      end Get;


      procedure Get (Key     : in  Ada.Calendar.Time;
                     Value   : out Value_T;
                     Success : out Boolean) is
      begin
         Get (P_Root, Key, Value, Success);
      end Get;



      function Put (P_Tree : in Tree_A;
                    Key    : Ada.Calendar.Time;
                    Value  : Value_T)
                   return Tree_A is

      begin

         if P_Tree = null then
            return new Tree'(Key, Value, null, null);
         end if;

         if Key = P_Tree.Key then
            P_Tree.Value := Value;
         elsif Key < P_Tree.Key then
            P_Tree.Left := Put (P_Tree.Left, Key, Value);
         elsif Key > P_Tree.Key then
            P_Tree.Right := Put (P_Tree.Right, Key, Value);
         end if;

         return P_Tree;
      end Put;

      procedure Put (Key   : Ada.Calendar.Time;
                     Value : Value_T) is
      begin
         P_Root := Put (P_Root, Key, Value);
      end Put;



      function Delete_Min (P_Tree : Tree_A)  return Tree_A  is
      begin
         -- Código intencionadamente eliminado
      end Delete_Min;



      function Delete (P_Tree : Tree_A;
                       Key : Ada.Calendar.Time) return Tree_A is
      begin
         -- Código intencionadamente eliminado
      end Delete;

      procedure Delete (Key : Ada.Calendar.Time) is
      begin
         -- Código intencionadamente eliminado
      end Delete;

      function Image (T: Ada.Calendar.Time) return String is
         use type ASU.Unbounded_String;

         S_Decimals: constant Integer := 4;
         D: Duration;
         H, M: Integer;
         S: Duration;
         Hst, Mst, Sst, Tst: ASU.Unbounded_String;
      begin
         D := Ada.Calendar.Seconds(T);
         H := Integer(D)/3600;
         D := D - Duration(H)*3600;
         M := Integer(D)/60;
         S := D - Duration(M)*60;
         Hst := ASU.To_Unbounded_String(Integer'Image(H));
         Mst := ASU.To_Unbounded_String(Integer'Image(M));
         Sst := ASU.To_Unbounded_String(Duration'Image(S));
         Hst := ASU.Tail(Hst, ASU.Length(Hst)-1);
         Mst := ASU.Tail(Mst, ASU.Length(Mst)-1);
         Sst := ASU.Tail(Sst, ASU.Length(Sst)-1);
         Sst := ASU.Head(Sst, ASU.Length(Sst)-(9-S_Decimals));
         Tst := Hst & "h:" & Mst & "m:" & Sst & "s:";
         return ASU.To_String(Tst);
      end Image;

      procedure Print_Tree (P_Tree : Tree_A) is
      begin
         if P_Tree /= null then
            if P_Tree.Left /= null then
               Print_Tree (P_Tree.Left);
            end if;


            Ada.Text_IO.Put_Line
              ( Image(P_Tree.Key ) &
                  " (" & Image(P_Tree.Value.Ep) & ":" &
                  P_Tree.Value.Seq_N'Img & ")");

            if P_Tree.Right /= null then
               Print_Tree (P_Tree.Right);
            end if;
         end if;
      end Print_Tree;

      procedure Print_Tree is
      begin
         Ada.Text_IO.Put_Line ("Sender_Buffering");
         Ada.Text_IO.Put_Line ("----------------");

         Print_Tree (P_Root);
      end Print_Tree;

   end P;

   procedure Put (Key    : in Ada.Calendar.Time;
                  Value  : in Value_T) is
      New_Value : Value_T;
   begin
      New_Value := Value;

      New_value.P_Buffer := new LLU.Buffer_Type (1024);
      LLU.Copy (New_Value.P_Buffer, Value.P_Buffer);

      P.Put(Key,New_Value);
   end Put;


   procedure Get (Key     : in  Ada.Calendar.Time;
                  Value   : out Value_T;
                  Success : out Boolean) is
   begin
      P.Get (Key, Value, Success);
   end Get;

   procedure Get (EP      : in  Llu.End_Point_Type;
                  Seq_N   : in  Types.Seq_N_T;
                  Key     : out Ada.Calendar.Time;
                  Value   : out Value_T;
                  Success : out Boolean) is
   begin
      P.Get (Ep, Seq_N, Key, Value, Success);
   end Get;


   function Is_Empty return Boolean is
   begin
      return P.Is_Empty;
   end Is_Empty;

   function Min return Ada.Calendar.Time is
   begin
      return P.Min;
   end;

   procedure Delete (Key : in Ada.Calendar.Time) is
   begin
      P.Delete (Key);
   end Delete;


   function Size return Natural is
   begin
      return P.Tree_Size;
   end Size;


   procedure Print is
   begin
      P.Print_Tree;
   end Print;



end Sender_Buffering;
